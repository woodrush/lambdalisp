#include <stdio.h>

int main (void) {
    char c;
    while ((c = getchar()) != EOF) {
        for (int mask = (1 << 7); mask; mask >>= 1) {
            putchar('0' + (c & mask ? 1 : 0));
        }
    }
    return 0;
}
