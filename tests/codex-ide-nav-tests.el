;;; codex-ide-nav-tests.el --- Tests for codex-ide navigation -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for shared focal-point navigation in status and session buffers.

;;; Code:

(require 'ert)
(require 'codex-ide-test-fixtures)
(require 'codex-ide)
(require 'codex-ide-status-mode)

(ert-deftest codex-ide-status-mode-nav-traverses-sections-and-visible-buttons ()
  (with-temp-buffer
    (codex-ide-status-mode)
    (codex-ide-section-reset)
    (let ((inhibit-read-only t))
      (insert "Intro\n"))
    (let (top-button-position
          hidden-button-position)
      (let* ((top-section
              (codex-ide-section-insert
               'group 'group "Group"
               (lambda (_section)
                 (insert "Body text\n")
                 (setq top-button-position (point))
                 (insert "Visible action")
                 (make-text-button top-button-position (point) 'follow-link t)
                 (insert "\n")
                 (codex-ide-section-insert
                  'hidden 'hidden "Hidden"
                  (lambda (_child)
                    (setq hidden-button-position (point))
                    (insert "Hidden action")
                    (make-text-button hidden-button-position (point) 'follow-link t)
                    (insert "\n"))
                  t))
               nil))
             (hidden-section (car (codex-ide-section-children top-section)))
             (other-section
              (codex-ide-section-insert
               'other 'other "Other"
               (lambda (_section)
                 (insert "Tail\n"))
               nil)))
	(goto-char (point-min))
	(codex-ide-status-mode-nav-forward)
	(should (= (point) (codex-ide-section-heading-start top-section)))
	(codex-ide-status-mode-nav-forward)
	(should (= (point) top-button-position))
	(codex-ide-status-mode-nav-forward)
	(should (= (point) (codex-ide-section-heading-start hidden-section)))
	(codex-ide-status-mode-nav-forward)
	(should (= (point) (codex-ide-section-heading-start other-section)))
	(should (invisible-p hidden-button-position))
	(codex-ide-status-mode-nav-backward)
	(should (= (point) (codex-ide-section-heading-start hidden-section)))
	(codex-ide-status-mode-nav-backward)
	(should (= (point) top-button-position))
	(codex-ide-status-mode-nav-backward)
	(should (= (point) (codex-ide-section-heading-start top-section)))))))

(ert-deftest codex-ide-session-mode-nav-traverses-buttons-then-active-prompt ()
  (let ((project-dir (codex-ide-test--make-temp-project)))
    (codex-ide-test-with-fixture project-dir
				 (codex-ide-test-with-fake-processes
				  (let ((session (codex-ide--create-process-session))
					first-button-position
					second-button-position
					prompt-position)
				    (with-current-buffer (codex-ide-session-buffer session)
				      (let ((inhibit-read-only t))
					(erase-buffer)
					(insert "Codex session\n\n")
					(setq first-button-position (point))
					(insert "First action")
					(make-text-button first-button-position (point)
							  'follow-link t
							  'keymap (codex-ide-nav-button-keymap))
					(insert "\n")
					(setq second-button-position (point))
					(insert "Second action")
					(make-text-button second-button-position (point)
							  'follow-link t
							  'keymap (codex-ide-nav-button-keymap))
					(insert "\n\n"))
				      (codex-ide--insert-input-prompt session "draft")
				      (setq prompt-position
					    (marker-position (codex-ide-session-input-start-marker session)))
				      (let ((keymap (button-get (button-at first-button-position) 'keymap)))
					(should (eq (lookup-key keymap (kbd "TAB"))
						    #'codex-ide-nav-button-forward))
					(should (eq (lookup-key keymap (kbd "<backtab>"))
						    #'codex-ide-nav-button-backward)))
				      (goto-char (point-min))
				      (codex-ide-session-mode-nav-forward)
				      (should (= (point) first-button-position))
				      (codex-ide-session-mode-nav-forward)
				      (should (= (point) second-button-position))
				      (codex-ide-session-mode-nav-forward)
				      (should (= (point) prompt-position))
				      (goto-char (1+ prompt-position))
				      (should-error (codex-ide-session-mode-nav-forward) :type 'user-error)
				      (codex-ide-session-mode-nav-backward)
				      (should (= (point) second-button-position))
				      (codex-ide-session-mode-nav-backward)
				      (should (= (point) first-button-position))))))))

(ert-deftest codex-ide-session-mode-nav-includes-submitted-prompts ()
  (let ((project-dir (codex-ide-test--make-temp-project)))
    (codex-ide-test-with-fixture project-dir
				 (codex-ide-test-with-fake-processes
				  (let ((session (codex-ide--create-process-session))
					first-prompt-position
					first-prompt-input-position
					button-position
					second-prompt-position
					second-prompt-input-position
					active-prompt-position)
				    (with-current-buffer (codex-ide-session-buffer session)
				      (let ((inhibit-read-only t))
					(erase-buffer)
					(insert "Codex session\n\n")
					(setq first-prompt-position (point))
					(insert "> first prompt\n")
					(setq first-prompt-input-position
					      (+ first-prompt-position 2))
					(codex-ide-renderer-style-user-prompt-region
					 first-prompt-position
					 (1- (point)))
					(insert "Agent response\n")
					(setq button-position (point))
					(insert "Open file")
					(make-text-button button-position (point)
							  'follow-link t
							  'keymap (codex-ide-nav-button-keymap))
					(insert "\n")
					(setq second-prompt-position (point))
					(insert "> second prompt\n")
					(setq second-prompt-input-position
					      (+ second-prompt-position 2))
					(codex-ide-renderer-style-user-prompt-region
					 second-prompt-position
					 (1- (point)))
					(insert "\n"))
				      (codex-ide--insert-input-prompt session "draft")
				      (setq active-prompt-position
					    (marker-position
					     (codex-ide-session-input-start-marker session)))
				      (goto-char (point-min))
				      (codex-ide-session-mode-nav-forward)
				      (should (= (point) first-prompt-input-position))
				      (codex-ide-session-mode-nav-forward)
				      (should (= (point) button-position))
				      (codex-ide-session-mode-nav-forward)
				      (should (= (point) second-prompt-input-position))
				      (codex-ide-session-mode-nav-forward)
				      (should (= (point) active-prompt-position))
				      (codex-ide-session-mode-nav-backward)
				      (should (= (point) second-prompt-input-position))))))))

(ert-deftest codex-ide-session-mode-link-tab-does-not-use-button-wrap ()
  (with-temp-buffer
    (codex-ide-session-mode)
    (let ((inhibit-read-only t)
	  first-link-position
	  second-link-position
	  command)
      (insert "Intro\n")
      (setq first-link-position (point))
      (insert "First link")
      (make-text-button first-link-position (point)
			'follow-link t
			'keymap codex-ide-renderer-link-keymap)
      (insert "\n")
      (setq second-link-position (point))
      (insert "Second link")
      (make-text-button second-link-position (point)
			'follow-link t
			'keymap codex-ide-renderer-link-keymap)
      (goto-char second-link-position)
      (setq command (key-binding (kbd "TAB")))
      (should (eq command #'codex-ide-renderer-button-nav-forward))
      (should-error (call-interactively command) :type 'user-error)
      (should (= (point) second-link-position))
      (goto-char first-link-position)
      (setq command (key-binding (kbd "<backtab>")))
      (should (eq command #'codex-ide-renderer-button-nav-backward))
      (should-error (call-interactively command) :type 'user-error)
      (should (= (point) first-link-position)))))

(ert-deftest codex-ide-session-mode-action-button-tab-visits-prompts-and-stops-at-edges ()
  (with-temp-buffer
    (codex-ide-session-mode)
    (let ((inhibit-read-only t)
	  first-button-position
	  prompt-position
	  prompt-input-position
	  second-button-position
	  command)
      (insert "Intro\n")
      (setq first-button-position (point))
      (codex-ide-renderer-insert-action-button
       "first"
       (lambda () nil)
       nil
       codex-ide-item-result-map)
      (insert "\n")
      (setq prompt-position (point))
      (insert "> submitted prompt\n")
      (setq prompt-input-position (+ prompt-position 2))
      (codex-ide-renderer-style-user-prompt-region
       prompt-position
       (1- (point)))
      (setq second-button-position (point))
      (codex-ide-renderer-insert-action-button
       "second"
       (lambda () nil)
       nil
       codex-ide-item-result-map)
      (goto-char first-button-position)
      (setq command (key-binding (kbd "TAB")))
      (should (eq command #'codex-ide-renderer-button-nav-forward))
      (call-interactively command)
      (should (= (point) prompt-input-position))
      (call-interactively command)
      (should (= (point) second-button-position))
      (should-error (call-interactively command) :type 'user-error)
      (should (= (point) second-button-position))
      (setq command (key-binding (kbd "<backtab>")))
      (should (eq command #'codex-ide-renderer-button-nav-backward))
      (call-interactively command)
      (should (= (point) prompt-input-position))
      (call-interactively command)
      (should (= (point) first-button-position))
      (should-error (call-interactively command) :type 'user-error)
      (should (= (point) first-button-position)))))

(provide 'codex-ide-nav-tests)

;;; codex-ide-nav-tests.el ends here
