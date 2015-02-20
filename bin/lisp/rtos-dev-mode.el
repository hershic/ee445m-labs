;;; rtos-mode.el --- A minor mode to augment the development of ee445m-labs

;; dependencies: https://github.com/abo-abo/hydra


;;; Commentary:
;;

;;; Code:

(defvar rtos-dev-mode-map (make-keymap)
  "The keymap for `rtos-dev-mode'.")

(define-minor-mode rtos-dev-mode
  "A minor mode to augment the development of
https://github.com/hershic/ee445m-labs."
  :group 'rtos
  :version "0.1"
  :init-value nil
  :lighter " rtos"
  :keymap rtos-dev-mode-map)

(defun rtos/git-root ()
  (if (string-equal (user-login-name) "eric")
      "~/workspace/ee445m-labs/"
    "/ee445m-labs/"))

(defun rtos/ocd-buffer ()
  "*ocd*")

(defun rtos/ocd-debug-command ()
  "ocd -d")

(defmacro rtos/exec-comint-command (command)
  `(progn
     (end-of-buffer)
     (insert ,command)
     (comint-send-input)))

(defun rtos/ocd-debugger ()
  ;; - todo: some sort of error handling would be nice
  ;; - todo: Hershal won't like this (visudo permissions) so either
  ;; find a way to kill cleanly upon exit of rtos/ocd-buffer or find a
  ;; way to kill in batch mode on hershal's machine. hopefully this
  ;; won't be a problem as this command doesn't have to be invoked
  ;; much.
  (interactive)
  (when (buffer-live-p (rtos/ocd-buffer)) (kill-buffer (rtos/ocd-buffer)))
  (shell (rtos/ocd-buffer))
  (rtos/exec-comint-command "sudo killall openocd")
  (rtos/exec-comint-command (concat "source " (rtos/git-root) "bin/setenv"))
  (rtos/exec-comint-command (rtos/ocd-debug-command))
  (bury-buffer)
  (message (concat "Buffer " (rtos/ocd-buffer)
		   " is running " (rtos/ocd-debug-command))))

(defmacro rtos/generate-gdb-command (command)
  (if (not (equal major-mode 'gud-mode))
      (message "You must be in a gdb buffer first.")
    (insert ,command)
    (comint-send-input)))

(defun rtos/gdb-target-remote ()
  (interactive)
  (rtos/generate-gdb-command "target remote localhost:3333"))

(defun rtos/gdb-reset ()
  (interactive)
  (rtos/generate-gdb-command "monitor reset halt"))

(defun rtos/gdb-load ()
  (interactive)
  (rtos/generate-gdb-command "load"))

(defun rtos/gdb-continue ()
  (interactive)
  (rtos/generate-gdb-command "continue"))

;; todo: determine why lv isn't working. probably an esc-system thing
(setq hydra-lv nil)
(defhydra rtos/hydra-gdb (rtos-dev-mode-map "M-e" :color red)
  "gdb"
  ("o" rtos/ocd-debugger      "ocd -d")
  ("g" gdb                    "gdb")
  ("t" rtos/gdb-target-remote "target")
  ("r" rtos/gdb-reset         "reset")
  ("l" rtos/gdb-load          "load")
  ("c" rtos/gdb-continue      "continue" :color blue))

;; font-lock
(font-lock-add-keywords
 'rtos-dev-mode
 '(("\\<\\(<immutable\\)\\>" . font-lock-keyword-face)))

;; add /bin/lisp to load path
(let ((default-directory (concat (rtos/git-root) "/bin/lisp")))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;; associated mode: c-eldoc
(require 'c-eldoc)
(add-hook 'rtos-dev-mode-hook 'c-turn-on-eldoc-mode)

;; associated mode: disaster-arm
(require 'disaster-arm)

;; rtos-dev-mode: associated with c-mode
(add-hook 'c-mode-hook 'rtos-dev-mode)

(provide 'rtos-dev-mode)

;;; rtos-mode.el ends here
