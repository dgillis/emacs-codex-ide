;;; codex-ide-status-mode.el --- Project status overview for codex-ide -*- lexical-binding: t; -*-

;;; Commentary:

;; Project overview buffer for Codex buffers and stored threads.

;;; Code:

(require 'codex-ide)
(require 'codex-ide-renderer)
(require 'codex-ide-section)
(require 'codex-ide-session-list)

;;;###autoload
(defcustom codex-ide-status-mode-preview-max-width 120
  "Maximum width for preview text shown in status section headings."
  :type 'integer
  :group 'codex-ide)

(defvar-local codex-ide-status-mode--directory nil
  "Project directory displayed by the current status buffer.")

(defvar codex-ide-status-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map codex-ide-section-mode-map)
    map)
  "Keymap for `codex-ide-status-mode'.")

(define-key codex-ide-status-mode-map (kbd "+") #'codex-ide)
(define-key codex-ide-status-mode-map (kbd "D") #'codex-ide-status-mode-delete-thing-at-point)
(define-key codex-ide-status-mode-map (kbd "l") #'codex-ide-status-mode-refresh)
(define-key codex-ide-status-mode-map (kbd "RET") #'codex-ide-status-mode-visit-thing-at-point)

(define-derived-mode codex-ide-status-mode codex-ide-section-mode "Codex-Status"
  "Major mode for the Codex project status buffer."
  (setq-local hl-line-face 'codex-ide-session-list-current-row-face)
  (hl-line-mode 1)
  (setq-local revert-buffer-function #'codex-ide-status-mode-refresh))

(defun codex-ide-status-mode--section-identity (section)
  "Return a stable identity for SECTION across rerenders."
  (pcase (codex-ide-section-type section)
    ('buffers 'buffers)
    ('threads 'threads)
    ('buffer
     (when-let* ((session (codex-ide-section-value section))
                 (buffer (and (codex-ide-session-p session)
                              (codex-ide-session-buffer session))))
       (buffer-name buffer)))
    ('thread
     (let ((thread (codex-ide-section-value section)))
       (or (alist-get 'id thread)
           (alist-get 'name thread)
           (alist-get 'preview thread))))
    (_ (codex-ide-section-type section))))

(defun codex-ide-status-mode--section-path (section)
  "Return SECTION's path from the root section list."
  (let (path)
    (while section
      (push (codex-ide-status-mode--section-identity section) path)
      (setq section (codex-ide-section-parent section)))
    path))

(defun codex-ide-status-mode--map-sections (fn)
  "Call FN for every status section in the current buffer."
  (cl-labels ((walk (section)
                (funcall fn section)
                (dolist (child (codex-ide-section-children section))
                  (walk child))))
    (dolist (section codex-ide-section--root-sections)
      (walk section))))

(defun codex-ide-status-mode--find-section-by-path (path)
  "Return the section identified by PATH, or nil when absent."
  (cl-labels ((find-in (sections remaining)
                (when-let ((key (car remaining)))
                  (when-let ((section
                              (cl-find-if
                               (lambda (candidate)
                                 (equal (codex-ide-status-mode--section-identity candidate)
                                        key))
                               sections)))
                    (if (cdr remaining)
                        (find-in (codex-ide-section-children section) (cdr remaining))
                      section)))))
    (find-in codex-ide-section--root-sections path)))

(defun codex-ide-status-mode--section-containing-point (&optional pos)
  "Return the deepest status section containing POS or point."
  (setq pos (or pos (point)))
  (cl-labels ((find-in (sections)
                (cl-find-if
                 #'identity
                 (mapcar
                  (lambda (section)
                    (when (and (<= (codex-ide-section-heading-start section) pos)
                               (< pos (codex-ide-section-end section)))
                      (or (find-in (codex-ide-section-children section))
                          section)))
                  sections))))
    (find-in codex-ide-section--root-sections)))

(defun codex-ide-status-mode--capture-view-state ()
  "Capture the current view state of the status buffer."
  (let ((section (codex-ide-status-mode--section-containing-point))
        (collapsed nil))
    (codex-ide-status-mode--map-sections
     (lambda (candidate)
       (push (cons (codex-ide-status-mode--section-path candidate)
                   (codex-ide-section-hidden candidate))
             collapsed)))
    `((collapsed . ,collapsed)
      (point-path . ,(and section
                          (codex-ide-status-mode--section-path section)))
      (point-offset . ,(and section
                            (- (point)
                               (codex-ide-section-heading-start section))))
      (point . ,(point)))))

(defun codex-ide-status-mode--restore-view-state (state)
  "Restore the status buffer view STATE after rerendering."
  (dolist (entry (alist-get 'collapsed state))
    (when-let ((section (codex-ide-status-mode--find-section-by-path (car entry))))
      (if (cdr entry)
          (codex-ide-section-hide section)
        (codex-ide-section-show section))))
  (if-let* ((path (alist-get 'point-path state))
            (section (codex-ide-status-mode--find-section-by-path path)))
      (let* ((offset (max 0 (or (alist-get 'point-offset state) 0)))
             (target (min (+ (codex-ide-section-heading-start section) offset)
                          (max (codex-ide-section-heading-start section)
                               (1- (codex-ide-section-end section))))))
        (goto-char target))
    (goto-char (min (or (alist-get 'point state) (point-min))
                    (point-max)))))

(defun codex-ide-status-mode--actionable-section-at-point ()
  "Return the actionable status section at point.
Only child `buffer' and `thread' sections support visit and delete actions."
  (let ((section (codex-ide-section-at-point)))
    (unless section
      (user-error "No status entry at point"))
    (unless (memq (codex-ide-section-type section) '(buffer thread))
      (user-error "No status entry at point"))
    section))

(defun codex-ide-status-mode--selected-actionable-sections ()
  "Return actionable sections at point or every unique one touched by the active region."
  (if (use-region-p)
      (let ((sections nil)
            (end (max (region-beginning) (1- (region-end)))))
        (save-excursion
          (goto-char (region-beginning))
          (beginning-of-line)
          (while (<= (point) end)
            (when-let ((section (codex-ide-status-mode--section-containing-point)))
              (when (and (memq (codex-ide-section-type section) '(buffer thread))
                         (not (memq section sections)))
                (push section sections)))
            (forward-line 1)))
        (or (nreverse sections)
            (user-error "No status entries in region")))
    (list (codex-ide-status-mode--actionable-section-at-point))))

(defun codex-ide-status-mode--visit-section (section)
  "Visit SECTION using the same underlying behavior as the session list modes."
  (pcase (codex-ide-section-type section)
    ('buffer
     (let ((session (codex-ide-section-value section)))
       (unless (and (codex-ide-session-p session)
                    (buffer-live-p (codex-ide-session-buffer session)))
         (user-error "Session buffer is no longer live"))
       (let ((codex-ide-display-buffer-options '(:reuse-buffer-window
                                                 :reuse-mode-window)))
         (codex-ide--show-session-buffer session))))
    ('thread
     (codex-ide--prepare-session-operations)
     (let ((codex-ide-display-buffer-options '(:reuse-buffer-window
                                               :reuse-mode-window)))
       (codex-ide--show-or-resume-thread (alist-get 'id (codex-ide-section-value section))
                                         codex-ide-status-mode--directory)))))

(defun codex-ide-status-mode--delete-buffer-session (session)
  "Delete SESSION's live buffer with list-mode-consistent confirmation."
  (let ((buffer (and (codex-ide-session-p session)
                     (codex-ide-session-buffer session))))
    (unless (buffer-live-p buffer)
      (user-error "Session buffer is no longer live"))
    (when (y-or-n-p
           (format "Kill Codex session buffer %s? " (buffer-name buffer)))
      (let ((kill-buffer-query-functions nil))
        (kill-buffer buffer))
      (codex-ide-status-mode-refresh))))

(defun codex-ide-status-mode--delete-thread (thread)
  "Delete THREAD with list-mode-consistent confirmation and refresh."
  (codex-ide--prepare-session-operations)
  (let ((codex-home (abbreviate-file-name (codex-ide--codex-home))))
    (when (yes-or-no-p
           (format "Permanently remove 1 Codex thread from %s? " codex-home))
      (codex-ide-delete-session-thread (alist-get 'id thread) t)
      (codex-ide-status-mode-refresh))))

(defun codex-ide-status-mode--confirm-delete-sections (sections)
  "Return non-nil when the user confirms deleting SECTIONS."
  (let* ((count (length sections))
         (buffer-count (seq-count
                        (lambda (section)
                          (eq (codex-ide-section-type section) 'buffer))
                        sections))
         (thread-count (- count buffer-count))
         (codex-home (abbreviate-file-name (codex-ide--codex-home)))
         (prompt nil)
         (confirm-function nil))
    (cond
     ((= buffer-count count)
      (setq prompt
            (if (= count 1)
                (format "Kill Codex session buffer %s? "
                        (buffer-name
                         (codex-ide-session-buffer
                          (codex-ide-section-value (car sections)))))
              (format "Kill %d Codex session buffers? " count))
            confirm-function #'y-or-n-p))
     ((= thread-count count)
      (setq prompt
            (if (= count 1)
                (format "Permanently remove 1 Codex thread from %s? " codex-home)
              (format "Permanently remove %d Codex threads from %s? "
                      count
                      codex-home))
            confirm-function #'yes-or-no-p))
     (t
      (setq prompt (format "Delete %d Codex status entries? " count)
            confirm-function #'y-or-n-p)))
    (funcall confirm-function prompt)))

(defun codex-ide-status-mode--delete-sections (sections)
  "Delete SECTIONS after one confirmation and refresh once."
  (when (codex-ide-status-mode--confirm-delete-sections sections)
    (when (seq-some (lambda (section)
                      (eq (codex-ide-section-type section) 'thread))
                    sections)
      (codex-ide--prepare-session-operations))
    (dolist (section sections)
      (pcase (codex-ide-section-type section)
        ('buffer
         (let* ((session (codex-ide-section-value section))
                (buffer (and (codex-ide-session-p session)
                             (codex-ide-session-buffer session))))
           (unless (buffer-live-p buffer)
             (user-error "Session buffer is no longer live"))
           (let ((kill-buffer-query-functions nil))
             (kill-buffer buffer))))
        ('thread
         (codex-ide-delete-session-thread
          (alist-get 'id (codex-ide-section-value section))
          t))))
    (codex-ide-status-mode-refresh)))

(defun codex-ide-status-mode-visit-thing-at-point ()
  "Visit the actionable status entry at point."
  (interactive)
  (codex-ide-status-mode--visit-section
   (codex-ide-status-mode--actionable-section-at-point)))

(defun codex-ide-status-mode-delete-thing-at-point ()
  "Delete the actionable status entry at point or every entry in the active region."
  (interactive)
  (codex-ide-status-mode--delete-sections
   (codex-ide-status-mode--selected-actionable-sections)))

(defun codex-ide-status-mode--status-face (status)
  "Return the face used for STATUS."
  (or (codex-ide--status-face status) 'default))

(defun codex-ide-status-mode--section-title (title count)
  "Return a section heading from TITLE and COUNT."
  (propertize (format "%s (%d)" title count) 'face 'bold))

(defun codex-ide-status-mode--user-prompt-face-p (face)
  "Return non-nil when FACE includes `codex-ide-user-prompt-face'."
  (if (listp face)
      (memq 'codex-ide-user-prompt-face face)
    (eq face 'codex-ide-user-prompt-face)))

(defun codex-ide-status-mode--last-submitted-prompt-text (session)
  "Return the last submitted non-empty prompt text from SESSION."
  (let ((search-end (or (and (codex-ide-session-input-prompt-start-marker session)
                             (marker-position
                              (codex-ide-session-input-prompt-start-marker session)))
                        (point-max))))
    (plist-get (codex-ide-status-mode--last-prompt-data-before session search-end)
               :text)))

(defun codex-ide-status-mode--active-prompt-data (session)
  "Return plist describing SESSION's current non-empty editable prompt."
  (when-let ((buffer (codex-ide-session-buffer session)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when-let ((prompt-start (and (codex-ide-session-input-prompt-start-marker session)
                                      (marker-position
                                       (codex-ide-session-input-prompt-start-marker session))))
                   (input-start (and (codex-ide-session-input-start-marker session)
                                     (marker-position
                                      (codex-ide-session-input-start-marker session)))))
          (let ((text (string-trim
                       (buffer-substring-no-properties input-start (point-max)))))
            (unless (string-empty-p text)
              (list :text text
                    :start prompt-start
                    :end (point-max)))))))))

(defun codex-ide-status-mode--last-prompt-data-before (session position)
  "Return plist describing the last non-empty prompt in SESSION before POSITION.
The plist contains `:text', `:start', and `:end'."
  (when-let ((buffer (codex-ide-session-buffer session)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (save-excursion
          (let ((pos (max (point-min)
                          (min (point-max) position)))
                candidate
                candidate-start
                candidate-end)
            (while (and (> pos (point-min)) (not candidate-start))
              (setq pos (previous-single-char-property-change pos 'face nil (point-min)))
              (when (codex-ide-status-mode--user-prompt-face-p
                     (get-char-property pos 'face))
                (let* ((end (next-single-char-property-change pos 'face nil position))
                       (start pos)
                       text)
                  (while (and (> start (point-min))
                              (codex-ide-status-mode--user-prompt-face-p
                               (get-char-property (1- start) 'face)))
                    (setq start (1- start)))
                  (setq text (string-remove-prefix
                              "> "
                              (buffer-substring-no-properties start end)))
                  (setq text (string-trim text))
                  (unless (string-empty-p text)
                    (setq candidate text
                          candidate-start start
                          candidate-end end))
                  (setq pos start))))
            (when candidate-start
              (list :text candidate
                    :start candidate-start
                    :end candidate-end))))))))

(defun codex-ide-status-mode--last-prompt-data (session)
  "Return plist describing the last non-empty prompt in SESSION.
The plist contains `:text', `:start', and `:end'."
  (or (codex-ide-status-mode--active-prompt-data session)
      (codex-ide-status-mode--last-prompt-data-before session (point-max))))

(defun codex-ide-status-mode--last-submitted-prompt-data (session)
  "Return plist describing SESSION's last submitted prompt."
  (when-let ((buffer (codex-ide-session-buffer session)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let ((search-end (if (codex-ide--input-prompt-active-p session)
                              (or (and (codex-ide-session-input-prompt-start-marker session)
                                       (marker-position
                                        (codex-ide-session-input-prompt-start-marker session)))
                                  (point-max))
                            (point-max))))
          (codex-ide-status-mode--last-prompt-data-before session search-end))))))

(defun codex-ide-status-mode--preview-line (value)
  "Return a one-line preview for VALUE."
  (let ((preview (replace-regexp-in-string
                  "[\n\r]+"
                  "↵"
                  (codex-ide--thread-choice-preview (or value "")))))
    (if (string-empty-p preview)
        "Untitled"
      preview)))

(defun codex-ide-status-mode--truncate-preview (value)
  "Return VALUE truncated for section-heading previews."
  (let* ((limit (max 3 codex-ide-status-mode-preview-max-width))
         (preview (codex-ide-status-mode--preview-line value)))
    (if (> (length preview) limit)
        (concat (substring preview 0 (- limit 3)) "...")
      preview)))

(defun codex-ide-status-mode--last-agent-response-range (session)
  "Return the start and end positions of SESSION's last agent response."
  (when-let ((buffer (codex-ide-session-buffer session)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (save-excursion
          (let* ((search-end (if (codex-ide--input-prompt-active-p session)
                                 (or (and (codex-ide-session-input-prompt-start-marker session)
                                          (marker-position
                                           (codex-ide-session-input-prompt-start-marker session)))
                                     (point-max))
                               (point-max)))
                 (pos search-end)
                 response-start
                 response-end)
            (while (and (> pos (point-min)) (not response-end))
              (setq pos (previous-single-char-property-change
                         pos
                         codex-ide-agent-item-type-property
                         nil
                         (point-min)))
              (when (and (< pos search-end)
                         (get-text-property pos codex-ide-agent-item-type-property))
                (setq response-end
                      (next-single-char-property-change
                       pos
                       codex-ide-agent-item-type-property
                       nil
                       search-end))
                (setq response-start pos)
                (while (and (> response-start (point-min))
                            (get-text-property
                             (1- response-start)
                             codex-ide-agent-item-type-property))
                  (setq response-start
                        (previous-single-char-property-change
                         response-start
                         codex-ide-agent-item-type-property
                         nil
                         (point-min))))))
            (when (and response-start response-end)
              (cons response-start response-end))))))))

(defun codex-ide-status-mode--copy-buffer-region-for-status (session start end)
  "Return SESSION buffer text from START to END."
  (ignore session)
  (buffer-substring start end))

(defun codex-ide-status-mode--insert-prefixed-lines
    (text &optional content-face prefix prefix-face)
  "Insert TEXT with PREFIX on each line.
When CONTENT-FACE is non-nil, apply it to each inserted line body.
PREFIX defaults to a dimmed `└ '.  PREFIX-FACE defaults to `shadow'."
  (when (stringp text)
    (let ((target (current-buffer))
          (prefix (or prefix "└ "))
          (prefix-face (or prefix-face 'shadow)))
      (with-temp-buffer
        (insert text)
        (goto-char (point-min))
        (while (< (point) (point-max))
          (let ((line-start (point))
                (line-end (line-end-position))
                (has-newline (< (line-end-position) (point-max)))
                line-text)
            (setq line-text (buffer-substring line-start line-end))
            (with-current-buffer target
              (let (content-start content-end)
                (insert (propertize prefix 'face prefix-face))
                (setq content-start (point))
                (insert line-text)
                (setq content-end (point))
                (when content-face
                  (add-text-properties content-start content-end
                                       (list 'face content-face)))
                (when has-newline
                  (insert "\n"))))
            (forward-line 1)))))))

(defun codex-ide-status-mode--last-output-block-range (start end)
  "Return the last separator-delimited output block between START and END."
  (save-excursion
    (goto-char end)
    (let ((separator (codex-ide--output-separator-string)))
      (if-let ((separator-start (search-backward separator start t)))
          (progn
            (goto-char (+ separator-start (length separator)))
            (while (and (< (point) end)
                        (eq (char-after) ?\n))
              (forward-char 1))
            (cons (point) end))
        (cons start end)))))

(defun codex-ide-status-mode--current-turn-transcript-range (session)
  "Return the visible transcript range for SESSION's in-progress turn."
  (when-let* ((prompt-data (codex-ide-status-mode--last-submitted-prompt-data session))
              (buffer (codex-ide-session-buffer session)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (save-excursion
          (let* ((turn-start (plist-get prompt-data :end))
                 (turn-end (if (codex-ide--input-prompt-active-p session)
                               (or (and (codex-ide-session-input-prompt-start-marker session)
                                        (marker-position
                                         (codex-ide-session-input-prompt-start-marker session)))
                                   (point-max))
                             (point-max)))
                 (block-range
                  (codex-ide-status-mode--last-output-block-range
                   turn-start
                   turn-end))
                 (start (car block-range))
                 (end (cdr block-range)))
            (goto-char start)
            (while (and (< (point) end)
                        (eq (char-after) ?\n))
              (forward-char 1))
            (setq start (point))
            (goto-char end)
            (while (and (> (point) start)
                        (memq (char-before) '(?\n ?\s ?\t)))
              (backward-char 1))
            (setq end (point))
            (when (< start end)
              (cons start end))))))))

(defun codex-ide-status-mode--buffer-transcript-slice (session)
  "Return the last relevant transcript slice for SESSION.
This includes the in-progress turn transcript or the last completed reply block.
Return nil when there is no agent reply."
  (when-let ((buffer (codex-ide-session-buffer session)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (if (and (codex-ide--input-prompt-active-p session)
                 (string= (codex-ide-session-status session) "idle"))
            (when-let* ((response-range
                         (codex-ide-status-mode--last-agent-response-range session))
                        (block-range
                         (codex-ide-status-mode--last-output-block-range
                          (car response-range)
                          (cdr response-range))))
              (codex-ide-status-mode--copy-buffer-region-for-status
               session
               (car block-range)
               (cdr block-range)))
          (when-let ((turn-range
                      (codex-ide-status-mode--current-turn-transcript-range session)))
            (codex-ide-status-mode--copy-buffer-region-for-status
             session
             (car turn-range)
             (cdr turn-range))))))))

(defun codex-ide-status-mode--project-sessions (directory)
  "Return live session buffers for DIRECTORY."
  (sort
   (seq-filter
    (lambda (session)
      (equal (codex-ide-session-directory session) directory))
    (codex-ide--session-buffer-sessions))
   (lambda (left right)
     (string-lessp (buffer-name (codex-ide-session-buffer left))
                   (buffer-name (codex-ide-session-buffer right))))))

(defun codex-ide-status-mode--thread-status (thread directory)
  "Return the display status for THREAD in DIRECTORY."
  (if-let ((session (codex-ide--session-for-thread-id
                     (alist-get 'id thread)
                     directory)))
      (codex-ide-session-status session)
    "stored"))

(defun codex-ide-status-mode--thread-buffer-name (thread directory)
  "Return the linked live buffer name for THREAD in DIRECTORY, if any."
  (when-let ((session (codex-ide--session-for-thread-id
                       (alist-get 'id thread)
                       directory)))
    (buffer-name (codex-ide-session-buffer session))))

(defun codex-ide-status-mode--insert-thread-metadata-line (label value)
  "Insert a dimmed thread metadata line with LABEL and VALUE."
  (insert (propertize (format "└ %s: %s\n" label (or value "")) 'face 'shadow)))

(defun codex-ide-status-mode--thread-preview-body (full-preview)
  "Return FULL-PREVIEW normalized for status display."
  (or (codex-ide--thread-read-display-user-text full-preview)
      ""))

(defun codex-ide-status-mode--insert-thread-preview-body (full-preview)
  "Insert FULL-PREVIEW as plain status preview text."
  (ignore full-preview))

(defun codex-ide-status-mode--insert-buffer-section (session)
  "Insert a child section for SESSION."
  (let* ((status (codex-ide-session-status session))
         (label (codex-ide--status-label status))
         (full-prompt (or (codex-ide-status-mode--last-submitted-prompt-text session) ""))
         (preview (codex-ide-status-mode--truncate-preview full-prompt))
         (buffer-name (buffer-name (codex-ide-session-buffer session)))
         (title (concat
                 (propertize label 'face (codex-ide-status-mode--status-face status))
                 "  "
                 (propertize buffer-name 'face 'shadow)
                 "  "
                 preview)))
    (codex-ide-section-insert
     'buffer session title
     (lambda (_section)
       (when-let ((transcript (codex-ide-status-mode--buffer-transcript-slice session)))
         (codex-ide-status-mode--insert-prefixed-lines transcript nil "  ")
         (unless (bolp)
           (insert "\n"))))
     t)))

(defun codex-ide-status-mode--insert-thread-section (thread directory)
  "Insert a child section for THREAD in DIRECTORY."
  (let* ((status (codex-ide-status-mode--thread-status thread directory))
         (label (codex-ide--status-label status))
         (thread-id (alist-get 'id thread))
         (raw-preview (or (alist-get 'name thread)
                          (alist-get 'preview thread)
                          "Untitled"))
         (thread-title (codex-ide-status-mode--preview-line raw-preview))
         (preview (codex-ide-status-mode--truncate-preview raw-preview))
         (title (concat
                 (propertize label 'face (codex-ide-status-mode--status-face status))
                 "  "
                 (propertize (codex-ide--thread-choice-short-id thread)
                             'face '(shadow font-lock-constant-face))
                 "  "
                 preview)))
    (codex-ide-section-insert
     'thread thread title
     (lambda (_section)
       (codex-ide-status-mode--insert-thread-metadata-line "Thread" thread-id)
       (codex-ide-status-mode--insert-thread-metadata-line
        "Buffer"
        (or (codex-ide-status-mode--thread-buffer-name thread directory) "None"))
       (codex-ide-status-mode--insert-thread-metadata-line
        "Created"
        (codex-ide--format-thread-updated-at (alist-get 'createdAt thread)))
       (codex-ide-status-mode--insert-thread-metadata-line
        "Updated"
        (codex-ide--format-thread-updated-at (alist-get 'updatedAt thread)))
       (codex-ide-status-mode--insert-thread-preview-body raw-preview))
     t)))

(defun codex-ide-status-mode--render-sections (directory)
  "Render status sections for DIRECTORY."
  (let* ((sessions (codex-ide-status-mode--project-sessions directory))
         (query-session nil)
         (threads nil))
    (codex-ide--prepare-session-operations)
    (setq query-session (codex-ide--ensure-query-session-for-thread-selection directory))
    (setq threads (codex-ide--thread-list-data query-session))
    (codex-ide-section-insert
     'buffers sessions
     (codex-ide-status-mode--section-title "Buffers" (length sessions))
     (lambda (_section)
       (dolist (session sessions)
         (codex-ide-status-mode--insert-buffer-section session))))
    (insert "\n")
    (codex-ide-section-insert
     'threads threads
     (codex-ide-status-mode--section-title "Threads" (length threads))
     (lambda (_section)
       (dolist (thread threads)
         (codex-ide-status-mode--insert-thread-section thread directory))))))

(defun codex-ide-status-mode--render-buffer (directory)
  "Render the status buffer for DIRECTORY."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (codex-ide-section-reset)
    (insert (propertize "Project: " 'face 'shadow))
    (insert (propertize (abbreviate-file-name directory) 'face 'bold))
    (insert "\n\n")
    (codex-ide-status-mode--render-sections directory)
    (goto-char (point-min))))

;;;###autoload
(defun codex-ide-status-mode-refresh (&optional _ignore-auto _noconfirm)
  "Refresh the current Codex status buffer."
  (interactive)
  (unless codex-ide-status-mode--directory
    (user-error "No Codex project is associated with this buffer"))
  (let ((state (codex-ide-status-mode--capture-view-state)))
    (codex-ide-status-mode--render-buffer codex-ide-status-mode--directory)
    (codex-ide-status-mode--restore-view-state state)))

;;;###autoload
(defun codex-ide-status ()
  "Show the Codex status buffer for the current project."
  (interactive)
  (let* ((directory (codex-ide--normalize-directory
                     (codex-ide--get-working-directory)))
         (buffer-name (format "codex-ide: %s"
                              (codex-ide--project-name directory)))
         (buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (codex-ide-status-mode)
      (setq-local default-directory directory)
      (setq-local codex-ide-status-mode--directory directory)
      (codex-ide-status-mode--render-buffer directory))
    (pop-to-buffer buffer)))

(provide 'codex-ide-status-mode)

;;; codex-ide-status-mode.el ends here
