/*
 * Common code shared between service and client
 * */

#include <iostream>
#include <vsomeip/vsomeip.hpp>

#include "common.hpp"

std::vector<vsomeip::byte_t> get_payload_data(unsigned int cmd) {
    std::vector<vsomeip::byte_t> its_payload_data;
    unsigned int i = 0;

    // May not be the most elegant approach, just keep it simple
    for (i = 0; i < sizeof(unsigned int); ++i) {
        uint8_t byte = static_cast<uint8_t>((cmd >> (i * 8)) & 0xFF);
        its_payload_data.push_back(byte);
    }
    return its_payload_data;
}
