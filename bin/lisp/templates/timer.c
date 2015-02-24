/*! %sA isr responsible for notifying all subscriptions with
 * information describing the interrupt.
 *
 * This isr was generated
 * automatically by bin/lisp/rtos-interrupt-generator.el
 */
void %sA_Handler(void) {

  TimerIntClear(%s_BASE, TIMER_TIMA_TIMEOUT);
  notification_init(int, 1);
  timer_metadata_init(%s_BASE, NULL, NULL, NULL);
  hw_notify(HW_TIMER, timer_metadata, note);
}
