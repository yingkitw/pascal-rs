#include "poscal-rs_runtime.h"
#include <stdio.h>

int main(int argc, char** argv) {
    pas_argc = argc;
    pas_argv = argv;
    printf("Param count: %d\n", pas_paramcount());
    if (argc > 0) {
        char buf[256];
        pas_paramstr(0, buf, sizeof(buf));
        printf("Program name: %s\n", buf);
    }
    pas_writeln("Hello from runtime test!");
    return 0;
}
