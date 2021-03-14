#include <iostream>
#include <format>

int main(int argc, char *argv[])
{
	std::cout << std::format("Hello, {}!", "world");
	return 0;
}
