#include "shellpp.hpp"
#include "blinker.hpp"

#include "inc/hw_memmap.h"

char shell::str_doc[10] = "doctor   ";
char shell::str_witch[10] = "witch   ";

char shell::system_command_names[SHELL_COMMANDS][SYSTEM_MAX_NAME_LENGTH];
sys_cmd shell::system_command_funcs[SHELL_COMMANDS];

exit_status_t shell::doctor(const char* args) {

    blinker blink = blinker(GPIO_PORTF_BASE);
    blink.toggle(PIN_RED);
}

exit_status_t shell::witch(const char* args) {

    blinker blink = blinker(GPIO_PORTF_BASE);
    blink.toggle(PIN_GREEN);
}

int shell::ustrncmp(const char *s1, const char *s2, uint32_t n) {

    /* Loop while there are more characters. */
    while(n) {
        /* If we reached a NULL in both strings, they must be equal so
         * we end the comparison and return 0 */
        if((!*s1 || (*s1 == ' ')) && (!*s2 || (*s2 == ' '))) {
            return(0);
        }

        /* Compare the two characters and, if different, return the
         * relevant return code. */
        if(*s2 < *s1) {
            return(1);
        }
        if(*s1 < *s2) {
            return(-1);
        }

        /* Move on to the next character. */
        s1++;
        s2++;
        n--;
    }
    /* If we fall out, the strings must be equal for at least the
     * first n characters so return 0 to indicate this. */
    return 0;
}

exit_status_t shell::system_exec(const char* cmd, const char* args) {

    sys_cmd sys_command = 0;
    int16_t i = 0;
    while(i<sizeof(system_command_funcs)/4 &&
          0 != ustrncmp(system_command_names[i], cmd, (uint32_t)-1)) {
        ++i;
        /* resume: the strncmp didnt' work */
    }
    sys_command = system_command_funcs[i];

#ifdef SHELL_LOG
    UINT bytes_written;
    if (logging_ready) {
        f_write(&logfilehandle, "\r\n", 2, &bytes_written);
        f_write(&logfilehandle, command, strlen(command),  &bytes_written);
        f_write(&logfilehandle, " ", 1,  &bytes_written);
        f_write(&logfilehandle, args, strlen(args),  &bytes_written);
        f_sync(&logfilehandle);
    }
#endif

    exit_status_t ret;
    if (0 == sys_command) {
        ret = sys_command(args);
    } else {
        ret = -1;
    }
    return ret;
}

void shell::ustrcpy(char* dest, const char* source) {
    uint32_t i = 0;
    while (1) {
        dest[i] = source[i];
        if (dest[i++] == '\0') { break; }
    }
}


shell::shell() {}

shell::shell(uart u) {

    pos = 0;
    uart0 = u;

    ustrcpy(system_command_names[0], str_doc);    system_command_funcs[0] = doctor;
    ustrcpy(system_command_names[1], str_witch);  system_command_funcs[1] = witch;
    clear_buffer();
    print_ps1();
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

exit_status_t shell::execute_command(char* cmd_and_args) {

    /* Null terminate to separate the cmd from the args */
    uint8_t len = strlen(cmd_and_args);
    uint8_t idx = 0;
    while((idx < len) && (cmd_and_args[idx] != ' ')) {
	++idx;
    }
    cmd_and_args[idx] = 0;

    /* Waldo says this line requires the extra char to be a 0 */
    return system_exec((const char*) cmd_and_args, (const char*) &cmd_and_args[idx+1]);
}

exit_status_t shell::execute_command(char* cmd, char* args) {

     /* Waldo says this line requires the extra char to be a 0 */
    return system_exec((const char*) cmd, (const char*) args);
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
