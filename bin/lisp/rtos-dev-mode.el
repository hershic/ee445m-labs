;;; rtos-mode.el --- A minor mode to augment the development of ee445m-labs

;;; Commentary: TODO
;;
;; dependencies: https://github.com/abo-abo/hydra

;;; Code:

(defvar rtos-dev-mode-map (make-keymap)
  "The keymap for `rtos-dev-mode'.")

;;;###autoload
(define-minor-mode rtos-dev-mode
  "A minor mode to augment the development of
https://github.com/hershic/ee445m-labs."
  :group 'rtos
  :version "0.1"
  :init-value nil
  :lighter " rtos"
  :keymap rtos-dev-mode-map)

(defun rtos/git-root ()
  "Root dir of ee445m-labs.git."
  (if (string-equal (user-login-name) "eric")
      "~/workspace/ee445m-labs/"
    "~/ee445m-labs/"))

(defun rtos/ocd-buffer ()
  "Name of the buffer running openocd in debug mode."
  "*ocd*")

(defun rtos/ocd-debug-command ()
  "Invocation of /bin/ocd in debug mode."
  "ocd -d")

(defmacro rtos/exec-comint-command (command)
  "Macro to simplify the execution of COMMAND in a comint
buffer."
  `(progn
     (end-of-buffer)
     (insert ,command)
     (comint-send-input)))

(defun rtos/ocd-debugger ()
  "Open a buffer named `rtos/ocd-buffer' and run `rtos/ocd-debug-command'."
  ;; todo: Hershal's machines won't like this (visudo permissions)
  ;; so either find a way to kill cleanly upon exit of rtos/ocd-buffer
  ;; or find a way to kill in batch mode on hershal's
  ;; machine. hopefully this won't be a problem as this command
  ;; doesn't have to be invoked often.
  (interactive)
  (when (buffer-live-p (rtos/ocd-buffer)) (kill-buffer (rtos/ocd-buffer)))
  (shell (rtos/ocd-buffer))
  (rtos/exec-comint-command "sudo killall openocd")
  (rtos/exec-comint-command (concat "source " (rtos/git-root) "bin/setenv"))
  (rtos/exec-comint-command (rtos/ocd-debug-command))
  (bury-buffer)
  ;; todo: check for errors
  (message (concat "Buffer " (rtos/ocd-buffer)
		   " is running " (rtos/ocd-debug-command))))

(defun rtos/buffers-matching-regexp (regexp)
  "Return a list of buffers matching REGEXP."
  (remq nil
	(mapcar (lambda (buf)
		  (let ((name (buffer-name buf)))
		    (when (string-match regexp name)
		      buf)))
		(buffer-list))))

(defmacro rtos/exec-gdb-command (command)
  "Macro to ensure COMMAND will only be executed in the contents
of a `gud-mode' buffer."
  `(if (not (equal major-mode 'gud-mode))
       (let ((gud (car (rtos/buffers-matching-regexp "^\*gud-"))))
	 (if (not (buffer-live-p gud))
	     (message "You must be in a gdb buffer first.")
	   (switch-to-buffer gud)))
     (rtos/exec-comint-command ,command)))

(defmacro rtos/define-gdb-command (function)
  (let ((gdb-command   (cdr function))
	(funsymbol (intern (format "rtos/gdb-%s" (car function)))))
    `(defun ,funsymbol () (interactive) (rtos/exec-gdb-command ,gdb-command))))

(defun rtos/gdb-reset-load-continue ()
  ""
  ;; TODO: Document this bitch
  (interactive)
  (rtos/gdb-reset)
  (rtos/gdb-load)
  (rtos/gdb-continue))

(mapcar* (lambda (function)
	   (eval `(rtos/define-gdb-command ,function)))
	 '(;; openocd functions
	   (load  . "load")
	   (reset . "monitor reset halt")

	   ;; gdb functions
	   (target   . "target remote localhost:3333")
	   (step     . "step")
	   (next     . "next")
	   (continue . "continue")))

;; todo: determine why lv isn't working. probably an esc-system thing
(setq hydra-lv nil)

(defhydra rtos/hydra-gdb (rtos-dev-mode-map "M-e" :color red)
  "gdb"
  ("o" rtos/ocd-debugger             "ocd -d")
  ("g" gdb                           "gdb")
  ("l" rtos/gdb-load                 "load")
  ("r" rtos/gdb-reset                "reset")
  ("t" rtos/gdb-target               "target")
  ("s" rtos/gdb-step                 "step")
  ("n" rtos/gdb-next                 "next")
  ("c" rtos/gdb-continue             "continue")
  ("a" rtos/gdb-reset-load-continue  "refresh"))

;; font-lock
(font-lock-add-keywords
 'rtos-dev-mode
 '(("\\<\\(<immutable\\|atomic\\)\\>" . font-lock-keyword-face)))

;; add /bin/lisp to load path
(let ((default-directory (concat (rtos/git-root) "/bin/lisp")))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;; associated mode: c-eldoc
(require 'c-eldoc)
(defun rtos/eldoc-hook()
  (when (equal major-mode 'c-mode) 'c-turn-on-eldoc-mode))
(add-hook 'rtos-dev-mode-hook 'rtos/eldoc-hook)

;; associated mode: disaster-arm
(require 'disaster-arm)

;; rtos-dev-mode: associated with c-mode and gud-mode
(add-hook 'c-mode-hook 'rtos-dev-mode)
(add-hook 'gud-mode-hook 'rtos-dev-mode)

(provide 'rtos-dev-mode)

;;; rtos-dev-mode.el ends here
