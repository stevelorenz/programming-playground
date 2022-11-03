/*
 * Simple TCP server.
 * */

#include <boost/asio/buffer.hpp>
#include <boost/asio/io_service.hpp>
#include <boost/asio/ip/tcp.hpp>
#include <boost/asio/write.hpp>
#include <ctime>
#include <iostream>
#include <string>

using namespace boost::asio;

io_context iocontext;
ip::tcp::endpoint tcp_endpoint{ip::tcp::v4(), 2014};
ip::tcp::acceptor tcp_acceptor{iocontext, tcp_endpoint};
ip::tcp::socket tcp_socket{iocontext};
std::string data;

void write_handler(const boost::system::error_code &ec,
				   std::size_t bytes_transferred) {
	if (not ec) {
		tcp_socket.shutdown(ip::tcp::socket::shutdown_send);
	}
}

void accepte_handler(const boost::system::error_code &ec) {
	if (not ec) {
		std::time_t now = std::time(nullptr);
		data = std::ctime(&now);
		async_write(tcp_socket, buffer(data), write_handler);
	}
}

int main() {
	tcp_acceptor.listen();
	tcp_acceptor.async_accept(tcp_socket, accepte_handler);

	std::cout << "* Start IO context... Press Ctrl+C to exit." << std::endl;
	iocontext.run();
}
