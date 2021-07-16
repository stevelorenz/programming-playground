#include <spdlog/spdlog.h>
#include <spdlog/sinks/stdout_color_sinks.h>

int main()
{
	spdlog::set_level(spdlog::level::debug);
	spdlog::set_pattern("[%H:%M:%S %z] [%n] [%^---%L---%$] [thread %t] %v");

	spdlog::info("This is an info message");
	spdlog::debug("Debug message, {:03.2f}", 1.234567);

	SPDLOG_TRACE("Some trace message with param {}", 42);
	SPDLOG_DEBUG("Some debug message");

	auto console = spdlog::stdout_color_mt("console");
	auto err_logger = spdlog::stderr_color_mt("stderr");
	spdlog::get("console")->info(
		"loggers can be retrieved from a global registry using the spdlog::get(logger_name)");

	spdlog::enable_backtrace(32);
	for (int i = 0; i < 3; i++) {
		spdlog::debug("Backtrace message {}", i); // not logged yet..
	}
	spdlog::dump_backtrace();
}
