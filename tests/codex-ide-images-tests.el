;;; codex-ide-images-tests.el --- Tests for codex-ide image input -*- lexical-binding: t; -*-

;;; Commentary:

;; Focused tests for local image and clipboard image attachment helpers.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'codex-ide-test-fixtures)
(require 'codex-ide)

(defun codex-ide-images-test--display-image-file (display)
  "Return DISPLAY's image file, or nil."
  (and (consp display)
       (eq (car display) 'image)
       (plist-get (cdr display) :file)))

(defun codex-ide-images-test--string-has-image-display-p (string path)
  "Return non-nil when STRING has an image display for PATH."
  (catch 'found
    (dotimes (index (length string))
      (when (equal (codex-ide-images-test--display-image-file
                    (get-text-property index 'display string))
                   path)
        (throw 'found t)))
    nil))

(defun codex-ide-images-test--buffer-has-image-display-p (path)
  "Return non-nil when the current buffer has an image display for PATH."
  (catch 'found
    (let ((pos (point-min)))
      (while (< pos (point-max))
        (when (equal (codex-ide-images-test--display-image-file
                      (get-text-property pos 'display))
                     path)
          (throw 'found t))
        (setq pos (or (next-single-property-change pos 'display nil (point-max))
                      (point-max)))))
    nil))

(defun codex-ide-images-test--emacs-program ()
  "Return the Emacs executable used by the test suite."
  (or (getenv "RUN_TESTS_EMACS_EXECUTABLE")
      (expand-file-name invocation-name invocation-directory)))

(defmacro codex-ide-images-test--with-session (status &rest body)
  "Run BODY in a temporary session buffer with STATUS."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (codex-ide-session-mode)
     (let ((session (make-codex-ide-session
                     :buffer (current-buffer)
                     :status ,status
                     :item-states (make-hash-table :test 'equal))))
       (setq-local codex-ide--session session)
       ,@body)))

(defun codex-ide-images-test--assert-render-context (context session)
  "Assert CONTEXT owns SESSION's current buffer."
  (should (codex-ide-transcript-render-context-p context))
  (should (eq (codex-ide-transcript-render-context-session context)
              session))
  (should (eq (codex-ide-transcript-render-context-buffer context)
              (current-buffer))))

(ert-deftest codex-ide-images-module-loads-runtime-dependencies ()
  (let ((buffer (generate-new-buffer " *codex-ide-images-module-load*")))
    (unwind-protect
        (let ((exit-code
               (call-process
                (codex-ide-images-test--emacs-program)
                nil
                buffer
                nil
                "-Q"
                "--batch"
                "-L"
                codex-ide-test--root-directory
                "--eval"
                (concat
                 "(progn"
                 " (setq load-prefer-newer t)"
                 " (require 'codex-ide-images)"
                 " (unless (and"
                 "          (fboundp 'codex-ide--ensure-session-for-current-project)"
                 "          (fboundp 'codex-ide--ensure-input-prompt)"
                 "          (fboundp 'codex-ide--add-pending-local-image))"
                 "   (kill-emacs 1)))"))))
          (should
           (equal exit-code 0)))
      (kill-buffer buffer))))

(ert-deftest codex-ide-submit-image-attaches-local-image-without-submitting ()
  (let* ((project-dir (codex-ide-test--make-temp-project))
         (image-path (codex-ide-test--make-project-file
                      project-dir
                      "screenshot.png"
                      "fake-png")))
    (codex-ide-test-with-fixture project-dir
				 (codex-ide-test-with-fake-processes
				  (let ((session (codex-ide--create-process-session)))
				    (with-current-buffer (codex-ide-session-buffer session)
				      (codex-ide--insert-input-prompt session nil)
				      (cl-letf (((symbol-function 'codex-ide--session-for-current-project)
						 (lambda () session))
						((symbol-function 'codex-ide--ensure-session-for-current-project)
						 (lambda () session))
						((symbol-function 'codex-ide--submit-prompt)
						 (lambda (&rest _)
						   (ert-fail "Attaching an image should not submit"))))
					(codex-ide-submit-image image-path))
				      (should (equal (codex-ide--pending-local-images session)
						     (list image-path)))
				      (should (equal (codex-ide--current-input session)
						     ""))
				      (should (string-match-p
					       "\\[Image #1\\]"
					       (overlay-get
						(codex-ide--session-metadata-get
						 session
						 :pending-local-images-overlay)
						'before-string)))))))))

(ert-deftest codex-ide-submit-image-ensures-session-before-attaching ()
  (let* ((project-dir (codex-ide-test--make-temp-project))
         (image-path (codex-ide-test--make-project-file
                      project-dir
                      "screenshot.png"
                      "fake-png")))
    (codex-ide-test-with-fixture project-dir
				 (codex-ide-test-with-fake-processes
				  (let ((session (codex-ide--create-process-session)))
				    (with-current-buffer (codex-ide-session-buffer session)
				      (cl-letf (((symbol-function 'codex-ide--session-for-current-project)
						 (lambda ()
						   (ert-fail
						    "Attaching an image should ensure a session directly")))
						((symbol-function 'codex-ide--ensure-session-for-current-project)
						 (lambda () session)))
					(codex-ide-submit-image image-path))
				      (should (equal (codex-ide--pending-local-images session)
						     (list image-path)))))))))

(ert-deftest codex-ide-pending-local-images-display-includes-thumbnail ()
  (let* ((project-dir (codex-ide-test--make-temp-project))
         (image-path (codex-ide-test--make-project-file
                      project-dir
                      "screenshot.png"
                      "fake-png")))
    (codex-ide-test-with-fixture project-dir
				 (codex-ide-test-with-fake-processes
				  (let ((session (codex-ide--create-process-session)))
				    (with-current-buffer (codex-ide-session-buffer session)
				      (codex-ide--insert-input-prompt session nil)
				      (cl-letf (((symbol-function 'create-image)
						 (lambda (file &rest _args)
						   `(image :file ,file))))
					(codex-ide--add-pending-local-image session image-path))
				      (let ((display (overlay-get
						      (codex-ide--session-metadata-get
						       session
						       :pending-local-images-overlay)
						      'before-string)))
					(should (string-match-p "\\[Image #1\\]" display))
					(should (codex-ide-images-test--string-has-image-display-p
						 display
						 image-path)))))))))

(ert-deftest codex-ide-submit-clipboard-image-attaches-image-without-submitting ()
  (let ((project-dir (codex-ide-test--make-temp-project)))
    (codex-ide-test-with-fixture project-dir
				 (codex-ide-test-with-fake-processes
				  (let ((session (codex-ide--create-process-session)))
				    (with-current-buffer (codex-ide-session-buffer session)
				      (codex-ide--insert-input-prompt session nil)
				      (cl-letf (((symbol-function 'codex-ide--save-clipboard-image)
						 (lambda (&optional _session) "/tmp/clipboard.png"))
						((symbol-function 'codex-ide--session-for-current-project)
						 (lambda () session))
						((symbol-function 'codex-ide--ensure-session-for-current-project)
						 (lambda () session))
						((symbol-function 'codex-ide--submit-prompt)
						 (lambda (&rest _)
						   (ert-fail "Attaching an image should not submit"))))
					(codex-ide-submit-clipboard-image))
				      (should (equal (codex-ide--pending-local-images session)
						     '("/tmp/clipboard.png")))
				      (should (string-match-p
					       "\\[Image #1\\]"
					       (overlay-get
						(codex-ide--session-metadata-get
						 session
						 :pending-local-images-overlay)
						'before-string)))))))))

(ert-deftest codex-ide-save-clipboard-image-requires-swift ()
  (let ((system-type 'darwin))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (command)
                 (and (not (equal command "swift"))
                      (executable-find command))))
              ((symbol-function 'make-temp-file)
               (lambda (&rest _args)
                 (ert-fail "Missing swift should fail before creating a temp file"))))
      (let ((error (should-error
                    (codex-ide--save-clipboard-image nil)
                    :type 'user-error)))
        (should (string-match-p
                 "requires the macOS Swift command line tool"
                 (error-message-string error)))))))

(ert-deftest codex-ide-delete-backward-keeps-temporary-clipboard-image-file ()
  (let* ((project-dir (codex-ide-test--make-temp-project))
         (image-path (codex-ide-test--make-project-file
                      project-dir
                      "clipboard.png"
                      "fake-png")))
    (codex-ide-test-with-fixture project-dir
				 (codex-ide-test-with-fake-processes
				  (let ((session (codex-ide--create-process-session)))
				    (with-current-buffer (codex-ide-session-buffer session)
				      (codex-ide--insert-input-prompt session nil)
				      (cl-letf (((symbol-function 'codex-ide--save-clipboard-image)
						 (lambda (&optional _session) image-path))
						((symbol-function 'codex-ide--session-for-current-project)
						 (lambda () session))
						((symbol-function 'codex-ide--ensure-session-for-current-project)
						 (lambda () session)))
					(codex-ide-submit-clipboard-image))
				      (goto-char (codex-ide--input-end-position session))
				      (codex-ide-delete-backward-or-remove-attached-image)
				      (should-not (codex-ide--pending-local-images session))
				      (should (file-exists-p image-path))))))))

(ert-deftest codex-ide-delete-backward-removes-last-attached-image ()
  (let* ((project-dir (codex-ide-test--make-temp-project))
         (first-image (codex-ide-test--make-project-file
                       project-dir
                       "first.png"
                       "fake-png"))
         (second-image (codex-ide-test--make-project-file
                        project-dir
                        "second.png"
                        "fake-png")))
    (codex-ide-test-with-fixture project-dir
				 (codex-ide-test-with-fake-processes
				  (let ((session (codex-ide--create-process-session)))
				    (with-current-buffer (codex-ide-session-buffer session)
				      (codex-ide--insert-input-prompt session nil)
				      (codex-ide--add-pending-local-image session first-image)
				      (codex-ide--add-pending-local-image session second-image)
				      (goto-char (codex-ide--input-end-position session))
				      (codex-ide-delete-backward-or-remove-attached-image)
				      (should (equal (codex-ide--pending-local-images session)
						     (list first-image)))
				      (should (equal (codex-ide--current-input session)
						     ""))
				      (let ((display (overlay-get
						      (codex-ide--session-metadata-get
						       session
						       :pending-local-images-overlay)
						      'before-string)))
					(should (string-match-p "\\[Image #1\\]" display))
					(should-not (string-match-p "\\[Image #2\\]" display)))))))))

(ert-deftest codex-ide-delete-backward-removes-image-token-at-input-end ()
  (let* ((project-dir (codex-ide-test--make-temp-project))
         (image-path (codex-ide-test--make-project-file
                      project-dir
                      "screenshot.png"
                      "fake-png")))
    (codex-ide-test-with-fixture project-dir
				 (codex-ide-test-with-fake-processes
				  (let ((session (codex-ide--create-process-session)))
				    (with-current-buffer (codex-ide-session-buffer session)
				      (codex-ide--insert-input-prompt session "abc")
				      (codex-ide--add-pending-local-image session image-path)
				      (goto-char (codex-ide--input-end-position session))
				      (codex-ide-delete-backward-or-remove-attached-image)
				      (should (equal (codex-ide--current-input session)
						     "abc"))
				      (should-not (codex-ide--pending-local-images session))))))))

(ert-deftest codex-ide-delete-backward-inside-text-keeps-attached-image ()
  (let* ((project-dir (codex-ide-test--make-temp-project))
         (image-path (codex-ide-test--make-project-file
                      project-dir
                      "screenshot.png"
                      "fake-png")))
    (codex-ide-test-with-fixture project-dir
				 (codex-ide-test-with-fake-processes
				  (let ((session (codex-ide--create-process-session)))
				    (with-current-buffer (codex-ide-session-buffer session)
				      (codex-ide--insert-input-prompt session "abc")
				      (codex-ide--add-pending-local-image session image-path)
				      (goto-char (1- (codex-ide--input-end-position session)))
				      (codex-ide-delete-backward-or-remove-attached-image)
				      (should (equal (codex-ide--current-input session)
						     "ac"))
				      (should (equal (codex-ide--pending-local-images session)
						     (list image-path)))))))))

(ert-deftest codex-ide-delete-backward-after-text-following-image-keeps-image ()
  (let* ((project-dir (codex-ide-test--make-temp-project))
         (image-path (codex-ide-test--make-project-file
                      project-dir
                      "screenshot.png"
                      "fake-png")))
    (codex-ide-test-with-fixture project-dir
				 (codex-ide-test-with-fake-processes
				  (let ((session (codex-ide--create-process-session)))
				    (with-current-buffer (codex-ide-session-buffer session)
				      (codex-ide--insert-input-prompt session "aaa")
				      (codex-ide--add-pending-local-image session image-path)
				      (insert "bbb")
				      (should (< (overlay-start
						  (codex-ide--session-metadata-get
						   session
						   :pending-local-images-overlay))
						 (codex-ide--input-end-position session)))
				      (goto-char (codex-ide--input-end-position session))
				      (codex-ide-delete-backward-or-remove-attached-image)
				      (should (equal (codex-ide--current-input session)
						     "aaabb"))
				      (should (equal (codex-ide--pending-local-images session)
						     (list image-path)))))))))

(ert-deftest codex-ide-delete-backward-falls-back-to-text-deletion ()
  (let ((project-dir (codex-ide-test--make-temp-project)))
    (codex-ide-test-with-fixture project-dir
				 (codex-ide-test-with-fake-processes
				  (let ((session (codex-ide--create-process-session)))
				    (with-current-buffer (codex-ide-session-buffer session)
				      (codex-ide--insert-input-prompt session "abc")
				      (goto-char (codex-ide--input-end-position session))
				      (codex-ide-delete-backward-or-remove-attached-image)
				      (should (equal (codex-ide--current-input session)
						     "ab"))))))))

(ert-deftest codex-ide-reset-session-buffer-clears-pending-local-images-and-session-temp-files ()
  (let* ((project-dir (codex-ide-test--make-temp-project))
         (prefix "codex-ide-clipboard-test-reset-")
         (image-path (make-temp-file prefix nil ".png")))
    (codex-ide-test-with-fixture project-dir
				 (codex-ide-test-with-fake-processes
				  (let ((session (codex-ide--create-process-session)))
				    (codex-ide--session-metadata-put
				     session
				     :local-image-temp-prefix
				     prefix)
				    (with-current-buffer (codex-ide-session-buffer session)
				      (codex-ide--insert-input-prompt session nil)
				      (cl-letf (((symbol-function 'codex-ide--save-clipboard-image)
						 (lambda (&optional _session) image-path))
						((symbol-function 'codex-ide--session-for-current-project)
						 (lambda () session))
						((symbol-function 'codex-ide--ensure-session-for-current-project)
						 (lambda () session)))
					(codex-ide-submit-clipboard-image))
				      (codex-ide--reset-session-buffer session)
				      (should-not (codex-ide--pending-local-images session))
				      (should-not (codex-ide--session-metadata-get
						   session
						   :pending-local-images-overlay))
				      (should-not (file-exists-p image-path))))))))

(ert-deftest codex-ide-submit-prompt-keeps-temporary-clipboard-image-after-send ()
  (let* ((project-dir (codex-ide-test--make-temp-project))
         (image-path (codex-ide-test--make-project-file
                      project-dir
                      "clipboard.png"
                      "fake-png")))
    (codex-ide-test-with-fixture project-dir
				 (codex-ide-test-with-fake-processes
				  (let ((session (codex-ide--create-process-session))
					submitted)
				    (setf (codex-ide-session-thread-id session) "thread-clipboard-image")
				    (with-current-buffer (codex-ide-session-buffer session)
				      (codex-ide--insert-input-prompt session "describe")
				      (cl-letf (((symbol-function 'codex-ide--save-clipboard-image)
						 (lambda (&optional _session) image-path))
						((symbol-function 'codex-ide--session-for-current-project)
						 (lambda () session))
						((symbol-function 'codex-ide--ensure-session-for-current-project)
						 (lambda () session))
						((symbol-function 'codex-ide--request-sync)
						 (lambda (_session method params)
						   (when (equal method "turn/start")
						     (setq submitted params))
						   nil)))
					(codex-ide-submit-clipboard-image)
					(codex-ide--submit-prompt))
				      (should submitted)
				      (should-not (codex-ide--pending-local-images session))
				      (should (file-exists-p image-path))))))))

(ert-deftest codex-ide-queued-prompt-keeps-temporary-image-after-send ()
  (let* ((project-dir (codex-ide-test--make-temp-project))
         (image-path (codex-ide-test--make-project-file
                      project-dir
                      "clipboard.png"
                      "fake-png"))
         (codex-ide-running-submit-action 'queue)
         requests)
    (codex-ide-test-with-fixture project-dir
				 (codex-ide-test-with-fake-processes
				  (let ((session (codex-ide--create-process-session)))
				    (setf (codex-ide-session-thread-id session) "thread-queued-image"
					  (codex-ide-session-current-turn-id session) "turn-current"
					  (codex-ide-session-output-prefix-inserted session) t
					  (codex-ide-session-status session) "running")
				    (with-current-buffer (codex-ide-session-buffer session)
				      (codex-ide--insert-input-prompt session "describe later")
				      (cl-letf (((symbol-function 'codex-ide--save-clipboard-image)
						 (lambda (&optional _session) image-path))
						((symbol-function 'codex-ide--session-for-current-project)
						 (lambda () session))
						((symbol-function 'codex-ide--ensure-session-for-current-project)
						 (lambda () session))
						((symbol-function 'codex-ide--request-sync)
						 (lambda (_session method params)
						   (push (cons method params) requests)
						   '((turn . ((id . "turn-next")))))))
					(codex-ide-submit-clipboard-image)
					(codex-ide-submit)
					(should (file-exists-p image-path))
					(should-not (codex-ide--pending-local-images session))
					(codex-ide--handle-notification
					 session
					 '((method . "turn/completed")
					   (params . ((turn . ((id . "turn-current"))))))))
				      (should (= (length requests) 1))
				      (should (equal (caar requests) "turn/start"))
				      (should (file-exists-p image-path))))))))

(ert-deftest codex-ide-insert-input-prompt-refreshes-pending-images-in-render-transaction ()
  (let* ((project-dir (codex-ide-test--make-temp-project))
         (image-path (codex-ide-test--make-project-file
                      project-dir
                      "screenshot.png"
                      "fake-png")))
    (codex-ide-test-with-fixture project-dir
				 (codex-ide-images-test--with-session "idle"
								      (codex-ide--session-metadata-put
								       session
								       :pending-local-images
								       (list image-path))
								      (let ((original
									     (symbol-function 'codex-ide--refresh-pending-local-images-display))
									    contexts)
									(cl-letf (((symbol-function
										    'codex-ide--refresh-pending-local-images-display)
										   (lambda (&rest args)
										     (push codex-ide--transcript-render-context contexts)
										     (apply original args))))
									  (codex-ide--insert-input-prompt session nil))
									(codex-ide-images-test--assert-render-context
									 (car contexts)
									 session)
									(should (string-match-p
										 "\\[Image #1\\]"
										 (overlay-get
										  (codex-ide--session-metadata-get
										   session
										   :pending-local-images-overlay)
										  'before-string))))))))

(ert-deftest codex-ide-replace-current-input-refreshes-pending-images-in-render-transaction ()
  (let* ((project-dir (codex-ide-test--make-temp-project))
         (image-path (codex-ide-test--make-project-file
                      project-dir
                      "screenshot.png"
                      "fake-png")))
    (codex-ide-test-with-fixture project-dir
				 (codex-ide-images-test--with-session "idle"
								      (codex-ide--insert-input-prompt session "draft")
								      (codex-ide--add-pending-local-image session image-path)
								      (let ((original
									     (symbol-function 'codex-ide--refresh-pending-local-images-display))
									    contexts)
									(cl-letf (((symbol-function
										    'codex-ide--refresh-pending-local-images-display)
										   (lambda (&rest args)
										     (push codex-ide--transcript-render-context contexts)
										     (apply original args))))
									  (codex-ide--replace-current-input session "replacement"))
									(codex-ide-images-test--assert-render-context
									 (car contexts)
									 session)
									(should (equal (codex-ide--current-input session) "replacement"))
									(should (string-match-p
										 "\\[Image #1\\]"
										 (overlay-get
										  (codex-ide--session-metadata-get
										   session
										   :pending-local-images-overlay)
										  'before-string))))))))

(ert-deftest codex-ide-begin-turn-display-renders-local-images-in-render-transaction ()
  (let* ((project-dir (codex-ide-test--make-temp-project))
         (image-path (codex-ide-test--make-project-file
                      project-dir
                      "screenshot.png"
                      "fake-png")))
    (codex-ide-test-with-fixture project-dir
				 (codex-ide-images-test--with-session "idle"
								      (codex-ide--insert-input-prompt session "describe this")
								      (let ((original
									     (symbol-function 'codex-ide--insert-local-image-attachments))
									    contexts)
									(cl-letf (((symbol-function 'codex-ide--insert-local-image-attachments)
										   (lambda (&rest args)
										     (push codex-ide--transcript-render-context contexts)
										     (apply original args))))
									  (codex-ide--begin-turn-display
									   session
									   nil
									   nil
									   (list image-path)))
									(codex-ide-images-test--assert-render-context
									 (car contexts)
									 session)
									(save-excursion
									  (goto-char (point-min))
									  (should (search-forward "Attached images:" nil t))
									  (should (get-text-property (match-beginning 0) 'read-only)))
									(should (string-match-p "\\[Image #1\\]" (buffer-string))))))))

(ert-deftest codex-ide-freeze-active-input-renders-local-images-in-render-transaction ()
  (let* ((project-dir (codex-ide-test--make-temp-project))
         (image-path (codex-ide-test--make-project-file
                      project-dir
                      "screenshot.png"
                      "fake-png")))
    (codex-ide-test-with-fixture project-dir
				 (codex-ide-images-test--with-session "running"
								      (codex-ide--insert-input-prompt session "steer with image")
								      (let ((original
									     (symbol-function 'codex-ide--insert-local-image-attachments))
									    contexts)
									(cl-letf (((symbol-function 'codex-ide--insert-local-image-attachments)
										   (lambda (&rest args)
										     (push codex-ide--transcript-render-context contexts)
										     (apply original args))))
									  (codex-ide--freeze-active-input-prompt
									   session
									   nil
									   'steering
									   (list image-path)))
									(codex-ide-images-test--assert-render-context
									 (car contexts)
									 session)
									(should-not (codex-ide--input-prompt-active-p session))
									(save-excursion
									  (goto-char (point-min))
									  (should (search-forward "Attached images:" nil t))
									  (should (get-text-property (match-beginning 0) 'read-only)))
									(should (string-match-p "\\[Image #1\\]" (buffer-string))))))))

(provide 'codex-ide-images-tests)

;;; codex-ide-images-tests.el ends here
