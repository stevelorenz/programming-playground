#include <stdio.h>
#include <stdlib.h>

class Parent {
   public:
	char c;
	char d;
};

class Child : public Parent {
   public:
	char str[5];
};

int main(int argc, char *argv[]) {
	Child c;
	c.c = 'A';
	c.d = 'B';
	snprintf(c.str, 5, "%s", "1234");
	return 0;
}
