/*
 * Example to use steady timers with Boost.Asio
 * */

#include <boost/asio/io_service.hpp>
#include <boost/asio/steady_timer.hpp>
#include <chrono>
#include <iostream>
#include <thread>
#include <vector>

#define PROJECT_NAME "asio"

void test_single_thread_timer() {
	std::cout << "* Test single thread steady timer" << std::endl;
	boost::system::error_code ec;
	boost::asio::io_service io_service;
	boost::asio::steady_timer timer{io_service, std::chrono::seconds{1}};
	timer.async_wait([](const boost::system::error_code &ec) {
		if (not ec) {
			std::cout << "1 seconds passed" << std::endl;
		} else {
			std::cerr << ec.message() << std::endl;
		}
	});
	io_service.run(ec);
	if (ec) {
		std::cerr << "Failed to start IO service with error: " << ec.message()
				  << std::endl;
	}
}

void test_two_threads_timers() {
	std::cout << "* Test multi-threads steady timers" << std::endl;
	boost::asio::io_context io_context;
	boost::asio::steady_timer timer1{io_context, std::chrono::seconds(3)};
	timer1.async_wait([](const boost::system::error_code &ec) {
		std::cout << "3 seconds passed from timer1" << std::endl;
	});
	boost::asio::steady_timer timer2{io_context, std::chrono::seconds(3)};
	timer2.async_wait([](const boost::system::error_code &ec) {
		std::cout << "3 seconds passed from timer2" << std::endl;
	});

	std::thread thread1{[&io_context]() { io_context.run(); }};
	std::thread thread2{[&io_context]() { io_context.run(); }};
	thread1.join();
	thread2.join();
}

int main(int argc, char **argv) {
	std::vector<int> vec{1, 2, 3};
	if (argc != 1) {
		std::cout << argv[0] << "takes no arguments.\n";
		return 1;
	}
	std::cout << "This is project " << PROJECT_NAME << ".\n";

	test_single_thread_timer();
	test_two_threads_timers();

	return 0;
}
