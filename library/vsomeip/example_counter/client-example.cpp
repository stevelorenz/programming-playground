/*
 * Sample vsomeip client
 *
 * NOTE: Sample/Test code, NOT production code!
 * */

#include <chrono>
#include <condition_variable>
#include <iomanip>
#include <iostream>
#include <sstream>
#include <thread>
#include <vsomeip/vsomeip.hpp>

#include "common.hpp"

std::shared_ptr<vsomeip::application> app;
std::mutex mutex;
std::condition_variable condition;
constexpr unsigned int TEST_REQ_NUM = 10;

void run() {
    std::unique_lock<std::mutex> its_lock(mutex);
    condition.wait(its_lock);

    std::shared_ptr<vsomeip::message> request;
    request = vsomeip::runtime::get()->create_request();
    request->set_service(SAMPLE_SERVICE_ID);
    request->set_instance(SAMPLE_INSTANCE_ID);
    request->set_method(SAMPLE_METHOD_ID);
    std::shared_ptr<vsomeip::payload> its_payload =
        vsomeip::runtime::get()->create_payload();

    // Just increase and decrease counters for basic examples
    auto its_payload_data_inc = get_payload_data(CMD_INCRESE_COUNTER);
    unsigned int i = 0;
    its_payload->set_data(its_payload_data_inc);
    request->set_payload(its_payload);
    for (i = 0; i < TEST_REQ_NUM; ++i) {
        app->send(request);
        std::this_thread::sleep_for(std::chrono::milliseconds(500));
    }

    its_payload_data_inc = get_payload_data(CMD_DECRESE_COUNTER);
    its_payload->set_data(its_payload_data_inc);
    request->set_payload(its_payload);
    for (i = 0; i < TEST_REQ_NUM; ++i) {
        app->send(request);
        std::this_thread::sleep_for(std::chrono::milliseconds(500));
    }
}

void on_message(const std::shared_ptr<vsomeip::message> &_response) {
    std::shared_ptr<vsomeip::payload> its_payload = _response->get_payload();
    vsomeip::length_t l = its_payload->get_length();

    // Get response payload
    if (l != sizeof(unsigned int)) {
        std::cout << "Invalid response payload size: " << l << "! Ignore it!"
                  << std::endl;
        return;
    }

    unsigned int counter = 0;
    counter = (unsigned int)*(its_payload->get_data());

    std::cout << "CLIENT: Received counter with Client/Session ["
              << std::setw(4) << std::setfill('0') << std::hex
              << _response->get_client() << "/" << std::setw(4)
              << std::setfill('0') << _response->get_session() << "] "
              << std::dec << counter << std::endl;
}

void on_availability(vsomeip::service_t _service, vsomeip::instance_t _instance,
                     bool _is_available) {
    std::cout << "CLIENT: Service [" << std::setw(4) << std::setfill('0')
              << std::hex << _service << "." << _instance << "] is "
              << (_is_available ? "available." : "NOT available.") << std::endl;
    condition.notify_one();
}

int main() {
    app = vsomeip::runtime::get()->create_application("Hello");
    app->init();
    app->register_availability_handler(SAMPLE_SERVICE_ID, SAMPLE_INSTANCE_ID,
                                       on_availability);
    app->request_service(SAMPLE_SERVICE_ID, SAMPLE_INSTANCE_ID);
    app->register_message_handler(SAMPLE_SERVICE_ID, SAMPLE_INSTANCE_ID,
                                  SAMPLE_METHOD_ID, on_message);

    // The send operation can be performed ONLY after app is started and
    // initialized properly. So here a separate send thread is used.
    // condition_variable is used for the synchronization.
    std::thread sender(run);

    app->start();
}
