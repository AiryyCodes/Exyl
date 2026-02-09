#include <stdint.h>
#include <stdio.h>

typedef struct
{
    const char *ptr;
    uint64_t len;
} string;

void print(string msg)
{
    fwrite(msg.ptr, 1, msg.len, stdout);
    fflush(stdout);
}

void println(string msg)
{
    fwrite(msg.ptr, 1, msg.len, stdout);
    fwrite("\n", 1, 1, stdout);
    fflush(stdout);
}
