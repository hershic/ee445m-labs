#include <stdbool.h>

void uart_set_active_channel(const long channel);
void uart_clear_active_channel();
bool uart_has_active_channel();

void uart_init();
void uart_init_(const long channel);

void uart_send(const char* text);
void uart_send_(const long channel, const char* text);

char uart_get_char();
char uart_get_char_(const long channel);

char* uart_get_string(const long string_length);
char* uart_get_string_(const long channel, const long string_length);
