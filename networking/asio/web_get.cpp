/*
 * Use Boost.Asio to perform a simple HTTP GET request.
 */

#include <array>
#include <boost/asio/buffer.hpp>
#include <boost/asio/io_context.hpp>
#include <boost/asio/ip/tcp.hpp>
#include <boost/asio/write.hpp>
#include <iostream>
#include <memory>
#include <string>

using namespace boost::asio;

io_context iocontext;
ip::tcp::resolver tcp_resolver{iocontext};
ip::tcp::socket tcp_socket{iocontext};
std::array<char, 4096> bytes;

void read_handler(const boost::system::error_code &ec,
				  std::size_t bytes_transferred) {
	if (not ec) {
		std::cout.write(bytes.data(), bytes_transferred);
		tcp_socket.async_read_some(buffer(bytes), read_handler);
	} else {
		std::cerr << ec.message() << std::endl;
	}
}

void connect_handler(const boost::system::error_code &ec) {
	if (not ec) {
		std::string req =
			"GET / HTTP/1.1\r\nHost: theboostcpplibraries.com\r\n\r\n";
		write(tcp_socket, buffer(req));
		tcp_socket.async_read_some(buffer(bytes), read_handler);
	} else {
		std::cerr << "Failed to connect to the server: " << ec.message()
				  << std::endl;
	}
}

void resolve_handler(const boost::system::error_code &ec,
					 ip::tcp::resolver::iterator it) {
	if (not ec) {
		// Connect to the resolved address.
		tcp_socket.async_connect(*it, connect_handler);
	} else {
		std::cerr << "Failed to resolve the address." << std::endl;
	}
}

int main() {
	ip::tcp::resolver::query q{"theboostcpplibraries.com", "80"};
	tcp_resolver.async_resolve(q, resolve_handler);
	iocontext.run();
}
