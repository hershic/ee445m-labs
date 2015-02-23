;;; rtos-interrupt-generator.el --- Generate interrupt handlers
;;; following the naming convention used by startup_gcc.c

;;; Commentary:
;;

;; Created by Eric Crosson on Saturday February 21, 2015

;;; Code:

(defun read-lines (file)
  "Return FILE's file content."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

;; todo: pull from a file to avoid this nonsense, use a macro to make it clean
(defvar rtos/interrupt-template-alist nil
  "Contains (DEVICE . TEMPLATE) of interrupt handlers.")
(setq rtos/interrupt-template-alist
      ;; TODO: insert '    ' after new lines
      `((uart . ,(read-lines "templates/uart.c"))
	(timer . ,(read-lines "templates/timer.c"))))

(defvar rtos/interrupt-devices nil
  "Templates of interruptable device names on the TM4C123G Cortex
  M4.")
(setq rtos/interrupt-devices '("UART%s" "TIMER%s"))

(defvar rtos/interrupt-channels nil
  "Templates of interruptable device channel numbers on the
  TM4C123G Cortex M4.")
(setq rtos/interrupt-channels '(0 1 2))

(defun combinations (&rest lists)
  "Return a list of all possible combinations of the elements of LISTS."
  (if (car lists)
      (mapcan (lambda (inner-val)
                (mapcar (lambda (outer-val)
                          (cons outer-val
                                inner-val))
                        (car lists)))
              (apply #'combinations (cdr lists)))
    (list nil)))

(defun rtos/generate-interrupt-handler(device template)
  "Use information about DEVICE to return a populated device TEMPLATE."
  (let ((body (format
	       (concat "\nvoid %s_Handler(void) {\n\n    " template "\n}\n")
	       device)))
    (insert body)))

(defun rtos/interrupt-template (dev)
  "Populate and return an interrupt service routine template for
device DEV."
  (cond ((string-match "UART" dev)
	 ;; TODO: more meta processing, remove repeated dev calls by
	 ;; counting occurances of '%s' in the template alist
	 (format (cdr (assoc 'uart rtos/interrupt-template-alist)) dev dev dev))
	((string-match "TIMER" dev)
	 (format (cdr (assoc 'timer rtos/interrupt-template-alist)) dev dev))))

;;;###autoload
(defun rtos/generate-interrupts()
  "Insert at point interrupt handlers for every permutation of
device and channel number in `rtos/interrupt-devices' and
`rtos/interrupt-channels', respectively."
  (interactive)
  (save-excursion
    (mapcar (lambda (data)
	    (let* ((device (format (cadr data) (car data)))
		   (template (rtos/interrupt-template device)))
	      (rtos/generate-interrupt-handler device template)))
	    (combinations rtos/interrupt-channels rtos/interrupt-devices)))
  (kill-line))

(provide 'rtos-interrupt-generator)

;;; rtos-interrupt-generator.el ends here
