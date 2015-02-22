;;; rtos-interrupt-generator.el --- Generate interrupt handlers
;;; following the naming convention used by startup_gcc.c

;;; Commentary:
;;

;; Created by Eric Crosson on Saturday February 21, 2015

;;; Code:

(defvar rtos/interrupt-devices nil
  "Templates of interruptable device names on the TM4C123G Cortex
  M4.")
(setq rtos/interrupt-devices '("UART%s" "TIMER%s"))

(defvar rtos/interrupt-channels nil
  "Templates of interruptable device channel numbers on the
  TM4C123G Cortex M4.")
(setq rtos/interrupt-channels '(0 1 2))

;; todo: pull from yaml or a file to avoid this nonsense
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
