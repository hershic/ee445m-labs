;;; rtos-interrupt-generator.el --- Generate interrupt handlers
;;; following the naming convention used by startup_gcc.c

;;; Commentary:
;;

;; Created by Eric Crosson on Saturday February 21, 2015

;;; Code:

(defvar rtos/interrupt-channels nil
  "Templates of interruptable device channel numbers on the
  TM4C123G Cortex M4.")
(setq rtos/interrupt-channels '(0 1 2))

(defvar rtos/interrupt-devices nil
  "Templates of interruptable device device numbers on the
  TM4C123G Cortex M4.")
(setq rtos/interrupt-devices '(uart timer))

(defun rtos/combinations (&rest lists)
  "Return a list of all possible combinations of the elements of LISTS."
  (if (car lists)
      (mapcan (lambda (inner-val)
                (mapcar (lambda (outer-val)
                          (cons outer-val
                                inner-val))
                        (car lists)))
              (apply #'rtos/combinations (cdr lists)))
    (list nil)))

(defun rtos/isr-template (file sub)
  "Return a list of lines of a file at FILE (symbol) with format
strings %s substituted with (concat FILE SUB)."
  (let ((c-tab "    ")
	(interface (symbol-name file)))
    (with-temp-buffer
      (insert-file-contents (format "templates/%s.c" interface))
      (format "%s%s"
	      c-tab
	      (mapconcat 'identity
			 (split-string (buffer-string) "\n" t)
		 (format "\n%s" c-tab))))))

;;;###autoload
(defun rtos/generate-interrupts()
  "Insert at point interrupt handlers for every permutation of
device and channel number in `rtos/interrupt-devices' and
`rtos/interrupt-channels', respectively."
  (interactive)
  (save-excursion
    (mapcar (lambda (template)
	      (let* ((device (cadr template))
		     (id     (car template))
		     (isr-handler (upcase (symbol-name device)))
		     (fn-body (format "%s\n" (rtos/isr-template device id))))
		;; todo wrap in fn prototype and interpolate
		(insert
		 (replace-regexp-in-string
		  "%s"
		  (format "%s%d%s" isr-handler id (if (eq device 'timer) "A" ""))
		  (format "%s%s"
			 "\nvoid %s_Handler(void) {\n\n"
			 (replace-regexp-in-string
			  "%s"
			  (format "%s%d" isr-handler id)
			  (format "%s}\n" fn-body)))))))
	    (rtos/combinations rtos/interrupt-channels rtos/interrupt-devices)))
  (kill-line))


(provide 'rtos-interrupt-generator)

;;; rtos-interrupt-generator.el ends here
