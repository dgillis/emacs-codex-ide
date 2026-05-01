;;; codex-ide-session-mode-tests.el --- Tests for codex-ide session mode -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for `codex-ide-session-mode'.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'codex-ide)
(require 'codex-ide-session-mode)

(defun codex-ide-session-mode-test--flatten-imenu-labels (index)
  "Return INDEX labels in depth-first order for assertions."
  (let (labels)
    (cl-labels ((walk (entry)
                  (push (car entry) labels)
                  (unless (markerp (cdr entry))
                    (mapc #'walk (cdr entry)))))
      (mapc #'walk index))
    (nreverse labels)))

(ert-deftest codex-ide-session-mode-imenu-indexes-user-prompts ()
  (with-temp-buffer
    (codex-ide-session-mode)
    (let ((inhibit-read-only t)
          first-pos
          second-pos
          index)
      (insert "> first prompt\nassistant reply\n> second prompt\nmore detail\n")
      (goto-char (point-min))
      (setq first-pos (+ (line-beginning-position) 2))
      (codex-ide-renderer-style-user-prompt-region
       (line-beginning-position)
       (line-end-position))
      (forward-line 2)
      (setq second-pos (+ (line-beginning-position) 2))
      (let ((start (line-beginning-position)))
        (forward-line 2)
        (codex-ide-renderer-style-user-prompt-region start (point)))
      (setq index (codex-ide-session-mode--imenu-create-index))
      (should (equal (codex-ide-session-mode-test--flatten-imenu-labels index)
                     '("first prompt" "second prompt↵more detail")))
      (should (= (marker-position (cdr (nth 0 index))) first-pos))
      (should (= (marker-position (cdr (nth 1 index))) second-pos)))))

(ert-deftest codex-ide-session-mode-imenu-uses-numbered-empty-prompt-labels ()
  (with-temp-buffer
    (codex-ide-session-mode)
    (let ((inhibit-read-only t)
          index)
      (insert "> \n")
      (goto-char (point-min))
      (codex-ide-renderer-style-user-prompt-region
       (line-beginning-position)
       (line-end-position))
      (setq index (codex-ide-session-mode--imenu-create-index))
      (should (equal (codex-ide-session-mode-test--flatten-imenu-labels index)
                     '("Prompt 1"))))))

(ert-deftest codex-ide-session-mode-imenu-normalizes-label-whitespace ()
  (with-temp-buffer
    (codex-ide-session-mode)
    (let ((inhibit-read-only t)
          index)
      (insert ">   prompt\twith\n spaced   words  \n# Heading\twith   spaces\n")
      (goto-char (point-min))
      (let ((start (line-beginning-position)))
        (forward-line 2)
        (codex-ide-renderer-style-user-prompt-region start (point)))
      (add-text-properties
       (point)
       (point-max)
       `(,codex-ide-agent-item-type-property "agentMessage"))
      (setq index (codex-ide-session-mode--imenu-create-index))
      (should (equal (codex-ide-session-mode-test--flatten-imenu-labels index)
                     '("prompt with↵spaced words"))))))

(ert-deftest codex-ide-session-mode-imenu-ignores-agent-messages ()
  (with-temp-buffer
    (codex-ide-session-mode)
    (let ((inhibit-read-only t)
          index)
      (insert "> do work\n# Implementation\nDetails\n## Validation\nDone\n")
      (goto-char (point-min))
      (codex-ide-renderer-style-user-prompt-region
       (line-beginning-position)
       (line-end-position))
      (forward-line 1)
      (let ((start (point)))
        (goto-char (point-max))
        (add-text-properties
         start
         (point)
         `(,codex-ide-agent-item-type-property "agentMessage")))
      (setq index (codex-ide-session-mode--imenu-create-index))
      (should (equal (codex-ide-session-mode-test--flatten-imenu-labels index)
                     '("do work"))))))

(ert-deftest codex-ide-session-mode-imenu-keeps-prompts-in-buffer-order ()
  (with-temp-buffer
    (codex-ide-session-mode)
    (let ((inhibit-read-only t)
          index)
      (insert "> first prompt\nAgent one\n> second prompt\nAgent two\n")
      (goto-char (point-min))
      (codex-ide-renderer-style-user-prompt-region
       (line-beginning-position)
       (line-end-position))
      (forward-line 1)
      (add-text-properties
       (line-beginning-position)
       (line-end-position)
       `(,codex-ide-agent-item-type-property "agentMessage"))
      (forward-line 1)
      (codex-ide-renderer-style-user-prompt-region
       (line-beginning-position)
       (line-end-position))
      (forward-line 1)
      (add-text-properties
       (line-beginning-position)
       (line-end-position)
       `(,codex-ide-agent-item-type-property "agentMessage"))
      (setq index (codex-ide-session-mode--imenu-create-index))
      (should (equal (codex-ide-session-mode-test--flatten-imenu-labels index)
                     '("first prompt" "second prompt"))))))

(ert-deftest codex-ide-session-mode-theme-refresh-subscribes-and-tears-down-hooks ()
  (let ((refresh-count 0)
        (codex-ide-session-mode--theme-refresh-buffers nil)
        (buffer (generate-new-buffer " *codex-ide-session-theme-test*")))
    (unwind-protect
        (with-current-buffer buffer
          (cl-letf (((symbol-function 'codex-ide-renderer-refresh-theme-faces)
                     (lambda ()
                       (setq refresh-count (1+ refresh-count)))))
            (codex-ide-session-mode)
            (should (memq (current-buffer) codex-ide-session-mode--theme-refresh-buffers))
            (should (memq #'codex-ide-session-mode--handle-theme-change
                          enable-theme-functions))
            (should (memq #'codex-ide-session-mode--handle-theme-change
                          disable-theme-functions))
            (run-hook-with-args 'enable-theme-functions 'test-theme)
            (run-hook-with-args 'disable-theme-functions 'test-theme)
            (should (= refresh-count 2))
            (fundamental-mode)
            (should-not (memq (current-buffer) codex-ide-session-mode--theme-refresh-buffers))
            (should-not (memq #'codex-ide-session-mode--handle-theme-change
                              enable-theme-functions))
            (should-not (memq #'codex-ide-session-mode--handle-theme-change
                              disable-theme-functions))
            (codex-ide-session-mode)
            (should (memq (current-buffer) codex-ide-session-mode--theme-refresh-buffers))
            (should (memq #'codex-ide-session-mode--handle-theme-change
                          enable-theme-functions))
            (should (memq #'codex-ide-session-mode--handle-theme-change
                          disable-theme-functions))
            (run-hook-with-args 'enable-theme-functions 'test-theme)
            (should (= refresh-count 3))
            (kill-buffer buffer)
            (should-not (memq buffer codex-ide-session-mode--theme-refresh-buffers))
            (should-not (memq #'codex-ide-session-mode--handle-theme-change
                              enable-theme-functions))
            (should-not (memq #'codex-ide-session-mode--handle-theme-change
                              disable-theme-functions))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(provide 'codex-ide-session-mode-tests)

;;; codex-ide-session-mode-tests.el ends here
