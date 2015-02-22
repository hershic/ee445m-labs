;;; rtos-interrupt-generator.el --- Generate interrupt handlers
;;; following the naming convention used by startup_gcc.c

;;; Commentary:
;;

;; Created by Eric Crosson on Saturday February 21, 2015

;;; Code:

(defvar rtos/interrupt-template-alist nil
  "Contains (DEVICE . TEMPLATE) of interrupt handlers.")
(setq rtos/interrupt-template-alist
      '((uart . "unsigned short i;
    hw_notification notification;
    /* TODO: determine which bit to clear:  */
    unsigned long look_at_me = UARTIntStatus();
    /* UARTIntClear(%s_BASE, ); */

    while(UARTCharsAvail(%s_BASE)) {

	/* Notify every subscribed task of each incoming character
	 * (but schedule them for later so we can return from this ISR
	 * asap). */
	notification._char = uart_get_char();

	/* TODO: schedule this thread instead of running it immediately */
	hw_notify(HW_UART, %s_BASE, notification);
    }")
	(timer . "TimerIntClear(%s_BASE, TIMER_TIMA_TIMEOUT);
    hw_notification notification;
    notification._int = 1;
    hw_notify(HW_TIMER, %s_BASE, notification);")
	))

(defun combinations (&rest lists)
  ""
  (if (car lists)
      (mapcan (lambda (inner-val)
                (mapcar (lambda (outer-val)
                          (cons outer-val
                                inner-val))
                        (car lists)))
              (apply #'combinations (cdr lists)))
    (list nil)))

(defun rtos/generate-interrupt-handler(device template)
  ""
  (let ((body (format
	       (concat "\nvoid %s_Handler(void) {\n\n    " template "\n}\n")
	       device)))
    (insert body)))

(defun rtos/interrupt-template (dev)
  ""
  (cond ((string-match "UART" dev)
	 (format (cdr (assoc 'uart rtos/interrupt-template-alist)) dev dev dev))
	((string-match "TIMER" dev)
	 (format (cdr (assoc 'timer rtos/interrupt-template-alist)) dev dev))))

;;;###autoload
(defun rtos/generate-interrupts()
  ""
  (interactive)
  (save-excursion
    (mapcar (lambda (data)
	    (let* ((device (format (cadr data) (car data)))
		   (template (rtos/interrupt-template device)))
	      (rtos/generate-interrupt-handler device template)))
	    (combinations '(0 1 2) '("UART%s" "TIMER%sA"))))
  (kill-line))

(provide 'rtos-interrupt-generator)

;;; rtos-interrupt-generator.el ends here
