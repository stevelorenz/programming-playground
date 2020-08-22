#include <iostream>

int main()
{
	const float r1 = 3.5, pi = 3.1415;
	float area1 = pi * r1 * r1;
	std::cout << "The area of a circle with radius " << r1 << " is "
		  << area1 << std::endl;
}
