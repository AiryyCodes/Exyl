#include <cstdio>
// #include <cstring>
// #include <unistd.h>

extern "C" void print(const char *str)
{
    // write(1, str, strlen(str));
    printf("%s\n", str);
}
