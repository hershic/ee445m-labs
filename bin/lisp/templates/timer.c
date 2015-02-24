void %sA_Handler(void) {

  TimerIntClear(%s_BASE, TIMER_TIMA_TIMEOUT);
  notification note;
  note._int = 1;

  hw_metadata metadata;
  metadata.timer.base = %s_BASE;
  hw_notify(HW_TIMER, metadata, note);
}
