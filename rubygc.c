#include <stdio.h>
#include <stdlib.h>
#include "rubygc.h"

RUBY_FUNC_EXPORTED void
GC_Init(void) {
    if (getenv("CUSTOM_GC")) {
        fprintf(stderr, "Loading our GC file\n");
    }
}
