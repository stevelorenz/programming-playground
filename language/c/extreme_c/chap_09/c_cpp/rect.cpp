#include <stdio.h>

class Rect {
   public:
	int Area() { return width * length; }

	int width;
	int length;
};

int main(int argc, char *argv[]) {
	Rect r;
	r.width = 10;
	r.length = 25;
	int area = 0;
	area = r.Area();
	printf("The area : %d\n", area);
	return 0;
}
