/*! %s isr responsible for notifying all subscriptions with information
 * describing the interrupt.
 *
 * This isr was generated
 * automatically by bin/lisp/rtos-interrupt-generator.el
 */
void %s_Handler(void) {

  unsigned short i;
  notification note;
  hw_metadata metadata;

  /* TODO: determine which bit to clear:  */
  unsigned long look_at_me = UARTIntStatus();
  /* UARTIntClear(%s_BASE, ); */

  metadata.uart.channel = %s_BASE;

  while(UARTCharsAvail(%s_BASE)) {

    /* Notify every subscribed task of each incoming character
     * (but schedule them for later so we can return from this ISR
     * asap). */
    note._char = uart_get_char();

    /* TODO: schedule this thread instead of running it immediately */
    hw_notify(HW_UART, metadata, note);
  }
}
