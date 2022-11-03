#include <boost/algorithm/string.hpp>
#include <iostream>
#include <string>

namespace ba = boost::algorithm;

int main() {
	// std::string is just an array of bytes...
	std::string s = "a simple string";
	std::cout << ba::to_upper_copy(s) << std::endl;

	std::string s2 = "你好";
	std::cout << s2 << std::endl;
}
