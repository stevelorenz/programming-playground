#include <fstream>

int main()
{
	std::ofstream myfile;
	myfile.open("./squares.txt");
	int i = 0;
	for (i = 0; i < 10; ++i) {
		myfile << i << "^2 = " << i * i << std::endl;
	}
	myfile.close();
}
