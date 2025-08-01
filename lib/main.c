#include <stdio.h>

int Cmain(void);
int Cballs(void);

int Cmod(int x, int y) {
    return x % y;
}

int main(void) {
    int n0 = Cballs();
    int n1 = Cmain();
    printf("n0 = %d\n", n0);
    printf("n1 = %d\n", n1);
}
