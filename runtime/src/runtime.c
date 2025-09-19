#include "io.c"
#include "math.c"

static int exyl_argc = 0;
static char **exyl_argv = NULL;

void exyl_runtime_init(int argc, char **argv)
{
    exyl_argc = argc;
    exyl_argv = argv;
}

// Returns number of arguments
int get_argc(void) { return exyl_argc; }

// Returns argument pointer at index
const char *get_argv(int i)
{
    if (i < 0 || i >= exyl_argc)
        return NULL;
    return exyl_argv[i];
}
