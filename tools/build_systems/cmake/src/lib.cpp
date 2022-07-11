#include "modern/lib.hpp"

#include <algorithm>
#include <tuple>
#include <vector>

#include <boost/accumulators/accumulators.hpp>
#include <boost/accumulators/statistics/mean.hpp>
#include <boost/accumulators/statistics/moment.hpp>
#include <boost/accumulators/statistics/stats.hpp>

namespace ba = boost::accumulators;

std::tuple<double, double>
accumulate_vector(const std::vector<double> &values) {
	ba::accumulator_set<double, ba::stats<ba::tag::mean, ba::tag::moment<2>>> acc;
	std::for_each(values.begin(), values.end(), std::ref(acc));
	return {ba::mean(acc), ba::moment<2>(acc)};
}
