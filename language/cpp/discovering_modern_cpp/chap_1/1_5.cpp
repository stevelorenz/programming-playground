#include <iostream>
#include <memory>

std::shared_ptr<double> f()
{
	std::shared_ptr<double> p1{ new double };
	std::shared_ptr<double> p2{ new double }, p3 = p2;
	std::cout << "p3.use_count =" << p3.use_count() << std::endl;
	return p3;
}

int main()
{
	// When the function returns, the pointers are destroyed and the memory
	// referred by p1 is released. The second memory still exists since p
	// from the main function is still referring to it.
	std::shared_ptr<double> p = f();
	std::cout << "p.use_count()" << p.use_count() << std::endl;
	return 0;
}
