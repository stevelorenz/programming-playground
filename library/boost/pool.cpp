/*
 * Simple test of Boost.Pool.
 */

#include <iostream>
#include <chrono>

#include <boost/pool/object_pool.hpp>

void benchmark_object_pool(uint32_t pool_size)
{
}

int main()
{
	// The first para is the size of the memory block.
	// The second para is the maximum number of memory block to allocate.
	boost::object_pool<int> pool{ 32, 64 };

	int *i = pool.malloc();
	*i = 1;
	auto j = pool.construct(2);

	std::cout << pool.get_next_size() << std::endl;

	pool.destroy(i);
	pool.destroy(j);
}
