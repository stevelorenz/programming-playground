/*
 * Sample vsomeip service
 *
 * NOTE: Sample/Test code, NOT production code!
 * */

#include <iomanip>
#include <iostream>
#include <sstream>
#include <vsomeip/vsomeip.hpp>

#include "common.hpp"

/**
 * @class CounterService
 * @brief
 *
 * NOTE: Graceful termination and deactivation of services has not yet been
 * implemented!
 *
 */
class CounterService {
  public:
    CounterService()
        : app_(vsomeip::runtime::get()->create_application("CounterService")),
          counter_(0) {}

    /**
     * Callback to handle client request
     *
     * @param _request
     */
    void on_message(const std::shared_ptr<vsomeip::message> &_request) {
        // vsomeip::payload is an array of byte_t, namely uint8_t
        std::shared_ptr<vsomeip::payload> its_payload = _request->get_payload();
        vsomeip::length_t l = its_payload->get_length();

        // Handle service request
        if (l != sizeof(unsigned int)) {
            // NOTE: Use a logger instead of cout for production code
            std::cout << "Invalid request payload size: " << l << "! Ignore it!"
                      << std::endl;
            return;
        }

        unsigned int cmd = 0;
        cmd = (unsigned int)*(its_payload->get_data());

        std::cout << "SERVICE: Received cmd message with Client/Session ["
                  << std::setw(4) << std::setfill('0') << std::hex
                  << _request->get_client() << "/" << std::setw(4)
                  << std::setfill('0') << std::hex << _request->get_session()
                  << "] " << cmd << std::endl;

        if (CMD_INCRESE_COUNTER == cmd) {
            counter_ += 1;
        } else if (CMD_DECRESE_COUNTER == cmd) {
            if (counter_ > 0) {
                counter_ -= 1;
            }
        } else {
            std::cout << "Invalid request CMD: " << cmd << std::endl;
            return;
        }

        // Create service response
        std::shared_ptr<vsomeip::message> its_response =
            vsomeip::runtime::get()->create_response(_request);
        its_payload = vsomeip::runtime::get()->create_payload();
        auto its_payload_data = get_payload_data(counter_);

        its_payload->set_data(its_payload_data);
        its_response->set_payload(its_payload);
        app_->send(its_response);
    }

    bool init() {
        // Init the vsomeip application
        if (not app_->init()) {
            return false;
        }
        // Register request handler callback
        app_->register_message_handler(SAMPLE_SERVICE_ID, SAMPLE_INSTANCE_ID,
                                       SAMPLE_METHOD_ID,
                                       std::bind(&CounterService::on_message,
                                                 this, std::placeholders::_1));
        app_->offer_service(SAMPLE_SERVICE_ID, SAMPLE_INSTANCE_ID);
        return true;
    }

    void start() { app_->start(); }

  private:
    std::shared_ptr<vsomeip::application> app_;
    unsigned int counter_;
};

int main() {
    auto service = CounterService();

    if (not service.init()) {
        return EXIT_FAILURE;
    };

    service.start();

    return EXIT_SUCCESS;
}
