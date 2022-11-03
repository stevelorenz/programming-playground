#include <fmt/core.h>
#include <fmt/format.h>

#include <cstdlib>
#include <iostream>
#include <tuple>
#include <vector>

#include "modern/lib.hpp"

int main(int argc, char *argv[]) {
	std::vector<double> input = {1.2, 2.3, 3.4, 4.5};
	auto [mean, moment] = accumulate_vector(input);
	fmt::print("Mean: {}, Moment: {}\n", mean, moment);

	return EXIT_SUCCESS;
}
