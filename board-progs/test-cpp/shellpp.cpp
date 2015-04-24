#include "shellpp.hpp"
#include "blinker.hpp"

#include "inc/hw_memmap.h"

#include "libio/kbd.h"

#define EXIT_SUCCESS 0

#define SHELL_VERBOSE

#define shell_command_is(a)                     \
    0 == ustrncmp(a, (const char*) buf.buf, buf.length())

char shell::system_command_names[SHELL_COMMANDS][SYSTEM_MAX_NAME_LENGTH];
sys_cmd shell::system_command_funcs[SHELL_COMMANDS];

semaphore* shell::m_start;
semaphore* shell::m_stop;

exit_status_t shell::doctor(const char* args) {

    blinker blink = blinker(GPIO_PORTF_BASE);
    blink.toggle(PIN_RED);
    return EXIT_SUCCESS;
}

exit_status_t shell::witch(const char* args) {

    blinker blink = blinker(GPIO_PORTF_BASE);
    blink.toggle(PIN_GREEN);
    return EXIT_SUCCESS;
}

exit_status_t shell::jester(const char* args) {

    blinker blink = blinker(GPIO_PORTF_BASE);
    blink.toggle(PIN_BLUE);
    return EXIT_SUCCESS;
}

/*! \note this implementation does not compare length, thus unique
 *  abbreviations are fair game (gdb style) */
int32_t shell::ustrncmp(const char *s1, const char *s2, uint32_t n) {

    /* Loop while there are more characters. */
    while(n) {
        /* If we reached a NULL in both strings, they must be equal so
         * we end the comparison and return 0 */
        if((!*s1 || (*s1 == ' ')) && (!*s2 || (*s2 == ' '))) {
            return(0);
        }

        if(*s2 < *s1) { return(1); }
        if(*s1 < *s2) { return(-1); }

        s1++;
        s2++;
        n--;
    }
    /* If we fall out, the strings must be equal for at least the
     * first n characters so return 0 to indicate this. */
    return 0;
}

void shell::ustrcpy(char* dest, const char* source) {

    uint32_t i = 0;
    while (1) {
        dest[i] = source[i];
        if (source[i++] == '\0') { break; }
    }
}

void shell::init() {

    buf = buffer<char, SHELL_BUFFER_LENGTH>();
    init_ps1();
}

shell::shell() {

    init();
}

shell::shell(uart* u) {

    uart0 = u;
    init();
}

shell::shell(uart* u, semaphore* m_start, semaphore* m_stop) {

    uart0 = u;

    m_start = m_start;
    m_stop = m_stop;

    init();
}

void shell::init_ps1() {

    /* KLUDGE: shitty way to set PS1 */
    ps1[0] = '>';
    ps1[1] = ' ';
    ps1[2] = ' ';
    ps1[3] = 0;

    print_ps1();
}

void* shell::memset(void* b, int c, int len) {

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

uint32_t shell::strlen(const char* s) {
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

    buf.clear();
}

void shell::set_ps1(char* new_ps1) {

    umemcpy(ps1, new_ps1, strlen(new_ps1));
}

/*! \note has the side effect of clearing the shell buffer */
void shell::print_ps1() {

    buf.clear();
    uart0->atomic_printf("\n\n\r%s", ps1);
}

bool shell::type(char ch) {

    bool ret;
    if (buf.full()) {
        ret = false;
    } else {
        ret = true;
        buf.add((const char) ch);
    }
    if(ret) {
    uart0->atomic_printf("%c", ch);
    }
    return ret;
}

void shell::accept(char ch) {

    switch(ch) {
    case SC_CR:
        execute_command();
        break;

    case 127:
    case SC_BACKSPACE:
        backspace();
        break;

    default:
        type(ch);
        break;
    }
}

void shell::backspace() {

    bool ok;
    buf.get(ok);
    if (ok) {
        uart0->atomic_printf("\b \b");
    }
}

exit_status_t shell::motor_start(const char* args) {

    shell::m_start->post();
    return EXIT_SUCCESS;
}

exit_status_t shell::motor_stop(const char* args) {

    shell::m_stop->post();
    return EXIT_SUCCESS;
}

exit_status_t shell::execute_command() {

    /* Null terminate to separate the cmd from the args */
    uint8_t len = buf.length();
    uint8_t idx = 0;
    while((idx < len) && (buf.buf[idx] != ' ')) {
        ++idx;
    }
    buf.buf[idx] = 0;

    /* Clear some space between the user input and this cmd output */
    uart0->atomic_printf("\r\n");

    exit_status_t exit_code = (exit_status_t) 0xDEADBEEF;
    /* Waldo says this line requires the extra char to be a 0 */
    if(shell_command_is("doctor")) {
        exit_code = doctor((const char*) &buf.buf[idx+1]);
    } else if(shell_command_is("witch")) {
        exit_code = witch((const char*) &buf.buf[idx+1]);
    } else if(shell_command_is("jester")) {
        exit_code = jester((const char*) &buf.buf[idx+1]);
        /* begin motor control commands */
    } else if(shell_command_is("start")) {
        exit_code = motor_start((const char*) &buf.buf[idx+1]);
    } else if(shell_command_is("stop")) {
        exit_code = motor_stop((const char*) &buf.buf[idx+1]);
    } else {
        uart0->atomic_printf("%s is not a recognized command.\n\r", buf.buf);
    }

#ifdef SHELL_VERBOSE
    if (exit_code != EXIT_SUCCESS) {
        uart0->atomic_printf("\n\rnonzero exit code: %d", exit_code);
    }
#endif

    /* Prepare for the next command */
    print_ps1();

    return exit_code;
}
