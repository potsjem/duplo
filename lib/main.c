#include <stdio.h>

typedef struct Foo {
    int x, y, z;
} Foo;

extern int X;
int Dmain(void);
int balls(void);
int add(int, int);
int addb(signed char, signed char);
void existance(int *);
int structo(Foo*);

int mod(int x, int y) {
    return x % y;
}

int main(void) {
    X = 64;
    Foo foo = { .x = 19, .y = 69, .z = 67 };

    int n0 = balls();
    int n1 = Dmain();
    int n2 = add(34, 35);
    int n4 = 5;
    int n5 = structo(&foo);

    existance(&n4);

    printf("n0 = %d\n", n0);
    printf("n1 = %d\n", n1);
    printf("n2 = %d\n", n2);
    printf("n3 = %d\n", addb(127, 1));
    printf("n4 = %d\n", n4);
    printf("n5 = %d\n", n5);
}
