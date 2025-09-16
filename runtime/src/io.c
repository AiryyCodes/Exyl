#include <stddef.h>
#include <stdarg.h>
#include <stdio.h>

void print(const char *format, ...)
{
    va_list args;
    va_start(args, format);

    vprintf(format, args);
    putchar('\n');

    va_end(args);
}
