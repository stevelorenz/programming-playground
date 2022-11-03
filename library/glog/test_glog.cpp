#include <fmt/core.h>
#include <glog/logging.h>

int main(int argc, char *argv[]) {
	// Configure the glog library
	FLAGS_logtostderr = true;
	FLAGS_colorlogtostderr = true;

	FLAGS_minloglevel = 0;
	FLAGS_stderrthreshold = 0;
	FLAGS_v = 3;

	google::InitGoogleLogging(argv[0]);

	LOG(ERROR) << "Error message";
	LOG(WARNING) << "Warning message";
	LOG(INFO) << "Info message";

	auto s = fmt::format("Current vlog level: {}", FLAGS_v);
	LOG(INFO) << s;

	VLOG(0) << "I’m printed when you run the program with --v=0 or higher";
	VLOG(1) << "I’m printed when you run the program with --v=1 or higher";
	VLOG(2) << "I’m printed when you run the program with --v=2 or higher";

	// This will print stack trace.
	CHECK_NE(1, 1) << ": The world must be ending!";
}
