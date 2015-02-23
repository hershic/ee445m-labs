TimerIntClear(%s_BASE, TIMER_TIMA_TIMEOUT);
hw_notification notification;
notification._int = 1;
hw_notify_subscriber(HW_TIMER, %s_BASE, notification);
