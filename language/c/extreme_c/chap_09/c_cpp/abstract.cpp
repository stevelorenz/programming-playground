#include <stdio.h>

enum class Taste { Sweet, Sour };

class Eatable {
   public:
	virtual Taste GetTaste() = 0;
};

class Apple : public Eatable {
   public:
	Taste GetTaste() override { return Taste::Sweet; }
};

int main(int argc, char *argv[]) {
	Apple a;
	Taste t;
	t = a.GetTaste();

	return 0;
}
