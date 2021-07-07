/*
 * Simple example to use coroutines with Boost.Asio.
 * */

#include <list>
#include <string>
#include <ctime>
#include <iostream>

#include <boost/asio/io_context.hpp>
#include <boost/asio/spawn.hpp>
#include <boost/asio/write.hpp>
#include <boost/asio/buffer.hpp>
#include <boost/asio/ip/tcp.hpp>

using namespace boost::asio;
using namespace boost::asio::ip;

io_context iocontext;
tcp::endpoint tcp_endpoint{ tcp::v4(), 2014 };
tcp::acceptor tcp_acceptor{ iocontext, tcp_endpoint };
std::list<tcp::socket> tcp_sockets;

void do_write(tcp::socket &tcp_socket, yield_context yield)
{
	std::time_t now = std::time(nullptr);
	std::string data = std::ctime(&now);
	async_write(tcp_socket, buffer(data), yield);
	tcp_socket.shutdown(tcp::socket::shutdown_send);
}

void do_accept(yield_context yield)
{
	for (int i = 0; i < 2; ++i) {
		tcp_sockets.emplace_back(iocontext);
		tcp_acceptor.async_accept(tcp_sockets.back(), yield);
		spawn(iocontext, [](yield_context yield) {
			do_write(tcp_sockets.back(), yield);
		});
	}
}

int main()
{
	tcp_acceptor.listen();
	spawn(iocontext, do_accept);
	std::cout << "* Start running IO context. Press ctrl+c to terminate."
		  << std::endl;
	iocontext.run();
}
