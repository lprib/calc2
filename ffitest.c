#include <stdio.h>


typedef void rl_voidfunc_t (void);
rl_voidfunc_t *callback_fn;

int test(int a) {
    printf("hello %d\n", a);
    if (callback_fn)
        callback_fn();

    return a + 1;
}

