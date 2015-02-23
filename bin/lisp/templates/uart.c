void %s_Handler(void) {

  unsigned short i;
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
    hw_notify_subscriber(HW_UART, %s_BASE, notification);
  }
}
