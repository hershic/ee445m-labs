#include "shellpp.hpp"

extern "C" {
#include "libos/system.h"
}

shell::shell() {}

shell::shell(uart u) {

    this->pos = 0;
    this->uart0 = u;
    clear_buffer();
}

void* umemset(void* b, int c, int len) {

    int i;
    unsigned char *p = (unsigned char *) b;
    i = 0;
    while(len > 0) {
        *p = c;
        ++p;
        --len;
    }
    return b;
}

uint32_t strlen(const char* s) {
    uint32_t len = 0;
    while(s[len]) { ++len; }
    return(len);
}

void* umemcpy(void *str1, const void *str2, long n) {

    long i = 0;
    uint8_t *dest8 = (uint8_t*)str1;
    uint8_t *source8 = (uint8_t*)str2;
    for (i=0; i<n; ++i) {
        dest8[i] = source8[i];
    }
}

void shell::clear_buffer() {

    umemset(buf, 0, sizeof(buf));
    pos = 0;
}

void shell::set_ps1(char* new_ps1) {

    umemcpy(ps1, new_ps1, strlen(new_ps1));
}

void shell::print_ps1() {

    uart0.printf(ps1);
}

exit_status_t shell::execute_command() {

    /* Null terminate to separate the cmd from the args */
    uint8_t idx = 0;
    while(idx < pos && buf[idx] != ' ') {
	++idx;
    }
    buf[idx] = 0;

    /* Waldo says this line requires the extra char to be a 0 */
    return system_exec((const char*) buf, (const char*) &buf[idx+1]);
}

void shell::shell_uart_handler(const char ch) {

    char recv = (char) ch;
    exit_status_t exit_code;

    switch(recv) {
    case 13:
        uart0.printf("\r");

        exit_code = execute_command();
        if(UART_VERBOSE && exit_code != 0) {
            uart0.printf("%d", exit_code);
        }
        clear_buffer();
        uart0.send_newline();
        print_ps1();
        break;

    case 127:
    case 8:
        buf[pos--] = (char) 0;
        uart0.printf("\b \b");
        break;

    default:
        if (SHELL_BUFFER_LENGTH > pos) {
            buf[pos++] = recv;
            /* Echo char to terminal for user */
            uart0.printf("%c", recv);
        }
        break;
    }
}
