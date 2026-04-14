
#include <sys/wait.h>
#include <stdlib.h>
#include <unistd.h>
#include "try-both.h"

bool is_parent = true;

bool try_both(void) {
    bool result = !!fork();
    is_parent &= result;
    return result;
}

bool wait_all(bool ret) {
    int subprocess_return_code;
    while(wait(&subprocess_return_code) != -1) {
        ret |= subprocess_return_code;
    }
    if (!is_parent) {
        exit(ret);
    }
    return ret;
}
