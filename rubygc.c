#include <stdio.h>
#include <stdlib.h>
#include "ruby/internal/config.h"

RUBY_FUNC_EXPORTED void
GC_Init(void) {
    fprintf(stderr, "Loading our GC file\n");
}
