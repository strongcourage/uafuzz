# include <stdio.h>
# include <stdlib.h>
# include <string.h>
# include <unistd.h>
# include <fcntl.h>

int *p, *p_alias;
char buf[10];

void bad_func() {
    free(p); // exit() is missing
}

int main (int argc, char *argv[]) {
    int f = open(argv[1], O_RDONLY);
    read(f, buf, 10);
    p = malloc(sizeof(int));
    
    if (buf[0] == 'A') {
        p_alias = malloc(sizeof(int));   
        p = p_alias;    
    }
    if (buf[1] == 'F')
        bad_func();
    if (buf[2] == 'U')
       *p = 1;
    return 0;
}