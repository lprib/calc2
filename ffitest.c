#include <stdio.h>


typedef void rl_voidfunc_t (void);

rl_voidfunc_t *callback_fn;

void set_callback(rl_voidfunc_t * fn) {
    callback_fn = fn;
    printf("setting callback_fn %lx\n", (unsigned long)(void*)callback_fn);
}

int test(int a) {
    printf("hello %d\n", a);
    if (callback_fn)
        callback_fn();
    return a + 1;
}

