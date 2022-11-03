#include <boost/coroutine/all.hpp>
#include <iostream>

using namespace boost::coroutines;

void cooperative(coroutine<void>::push_type &sink) {
	std::cout << "hello";
	sink();
	std::cout << "coroutine";
}

int main() {
	coroutine<void>::pull_type source{cooperative};
	std::cout << ",";
	source();
	std::cout << std::endl;
}
