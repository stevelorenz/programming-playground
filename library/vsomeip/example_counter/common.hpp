/*
 * Common code shared between service and client
 * */

constexpr unsigned int SAMPLE_SERVICE_ID = 0x1234;
constexpr unsigned int SAMPLE_INSTANCE_ID = 0x5678;
constexpr unsigned int SAMPLE_METHOD_ID = 0x0421;

// Naja, this can be std::string or even one bit because there's ONLY two
// commands now. Use a unsigned integer just to keep it simple
constexpr unsigned int CMD_INCRESE_COUNTER = 0;
constexpr unsigned int CMD_DECRESE_COUNTER = 1;

/**
 * Get payload data from an unsigned integer, just for demo
 *
 * @param cmd
 */
std::vector<vsomeip::byte_t> get_payload_data(unsigned int cmd);
