
;; TODO: create rtos mode

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
  (interactive)
  ;; todo: Hershal won't like this (visudo permissions) so either find
  ;; a way to kill cleanly upon exit of rtos/ocd-buffer or find a way
  ;; to kill in batch mode on hershal's machine. hopefully this won't
  ;; be a problem as this command doesn't have to be invoked much.
  (when (buffer-live-p (rtos/ocd-buffer))
    (kill-buffer (rtos/ocd-buffer)))
  (shell (rtos/ocd-buffer))
  (rtos/exec-comint-command "sudo killall openocd")
  (rtos/exec-comint-command (concat "source " (rtos/git-root) "bin/setenv"))
  (rtos/exec-comint-command "ocd -d")
  (bury-buffer)
  (message (concat "Buffer " (rtos/ocd-buffer) " is running " (rtos/ocd-debug-command))))

(defmacro rtos/generate-gdb-command (command)
  `(progn
     (if (not (equal major-mode 'gud-mode))
	 (message "You must be in a gdb buffer first.")
       (insert ,command)
       (comint-send-input))))

(defun rtos/gdb-target-remote ()
  (interactive)
  (rtos/generate-gdb-command "target remote localhost:3333"))

(defun rtos/gdb-reset ()
  (interactive)
  (rtos/generate-gdb-command "monitor reset halt"))

(defun rtos/gdb-load ()
  (interactive)
  (rtos/generate-gdb-command "load"))

;; todo: determine why lv isn't working. probably an esc-system thing
(defhydra rtos/hydra-gdb (esc-mode-map "M-e" :color red)
  "gdb"
  ("o" rtos/ocd-debugger      (rtos/ocd-debug-command))
  ("t" rtos/gdb-target-remote "target")
  ("r" rtos/gdb-reset         "reset")
  ("l" rtos/gdb-load          "load")
  ("c" rtos/gdb-continue      "continue" :color blue))
