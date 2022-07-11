#include <fmt/core.h>
#include <vector>
#include <tuple>
#include <iostream>
#include <cstdlib>

#include <fmt/format.h>

#include "modern/lib.hpp"

int main(int argc, char *argv[])
{
	std::vector<double> input = {1.2, 2.3, 3.4, 4.5};
	auto [mean, moment] = accumulate_vector(input);
	fmt::print("Mean: {}, Moment: {}\n", mean, moment);

	return EXIT_SUCCESS;
}
