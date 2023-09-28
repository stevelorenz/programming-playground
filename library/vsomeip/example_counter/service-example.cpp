/*
 * Sample vsomeip service
 *
 * NOTE: Sample/Test code, NOT production code!
 * */

#include <condition_variable>
#include <iomanip>
#include <iostream>
#include <mutex>
#include <sstream>
#include <string>
#include <thread>

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
          is_registered_(false), is_offered_(false), running_(true),
          notify_thread_(std::bind(&CounterService::notify, this)),
          counter_(0) {}

    void on_state(vsomeip::state_type_e _state) {
        std::cout << "Application " << app_->get_name() << " is "
                  << (_state == vsomeip::state_type_e::ST_REGISTERED
                          ? "registered."
                          : "deregistered.")
                  << std::endl;

        if (_state == vsomeip::state_type_e::ST_REGISTERED) {
            if (!is_registered_) {
                is_registered_ = true;
            }
        } else {
            is_registered_ = false;
        }
    }

    /**
     * Callback method of the notify thread
     */
    void notify() {
        while (running_) {
            std::unique_lock<std::mutex> its_lock(notify_mutex_);
            while (not is_offered_ && running_) {
                std::cout << "[INFO] Wait main thread enter blocked state!"
                          << std::endl;
                notify_condition_.wait(its_lock);
            }
            // std::cout << "[DEBUG] Running notify loop!" << std::endl;

            // Protect the critical section!
            {
                std::lock_guard<std::mutex> its_lock(counter_mutex_);
                // std::cout << "[DEBUG] Get lock and check "
                //              "counter value!"
                //           << std::endl;

                // NOTE: Just an example here! Notify payload can be defined as
                // a class field, namely instead using local variables!
                if (counter_ == 5) {
                    std::cout << "[INFO] Event is triggered! Counter is "
                                 "now 5 ! Notify subscribers !!!"
                              << std::endl;
                    std::string notify_msg = "Counter is now 5 !!!";
                    std::vector<vsomeip::byte_t> notify_payload_data(
                        notify_msg.begin(), notify_msg.end());
                    std::shared_ptr<vsomeip::payload> notify_payload;
                    // Alloc memory for notify_payload!
                    notify_payload = vsomeip::runtime::get()->create_payload();
                    notify_payload->set_data(notify_payload_data);
                    app_->notify(SAMPLE_SERVICE_ID, SAMPLE_INSTANCE_ID,
                                 SAMPLE_EVENT_ID, notify_payload);
                }
            }
            // The event check cycle here is 500ms
            std::this_thread::sleep_for(std::chrono::milliseconds(500));
        }
    }

    /**
     * Callback to handle client request
     *
     * @param _request
     */
    void on_message(const std::shared_ptr<vsomeip::message> &_request) {
        std::lock_guard<std::mutex> its_lock(counter_mutex_);

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
        std::lock_guard<std::mutex> its_lock(notify_mutex_);
        // Init the vsomeip application
        if (not app_->init()) {
            return false;
        }
        // Register state handler
        app_->register_state_handler(
            std::bind(&CounterService::on_state, this, std::placeholders::_1));

        // Register request handler callback
        app_->register_message_handler(SAMPLE_SERVICE_ID, SAMPLE_INSTANCE_ID,
                                       SAMPLE_METHOD_ID,
                                       std::bind(&CounterService::on_message,
                                                 this, std::placeholders::_1));
        app_->offer_service(SAMPLE_SERVICE_ID, SAMPLE_INSTANCE_ID);

        // Offer event to subscriber
        std::set<vsomeip::eventgroup_t> its_groups;
        its_groups.insert(SAMPLE_EVENTGROUP_ID);
        app_->offer_event(
            SAMPLE_SERVICE_ID, SAMPLE_INSTANCE_ID, SAMPLE_EVENT_ID, its_groups,
            vsomeip::event_type_e::ET_EVENT, std::chrono::milliseconds::zero(),
            false, true, nullptr, vsomeip::reliability_type_e::RT_UNKNOWN);

        is_offered_ = true;
        notify_condition_.notify_one();
        return true;
    }

    void start() { app_->start(); }

  private:
    // The vsomeip application
    std::shared_ptr<vsomeip::application> app_;
    // Resource counter
    unsigned int counter_;
    // Mutex to protect the shared counter between main and notify thread
    std::mutex counter_mutex_;
    // Flag: if the service is registered
    bool is_registered_;
    // Flag: if the counter service is already offered
    bool is_offered_;
    // Flag: if the service is still running
    bool running_;
    // Condition variable to sync the notify thread
    std::mutex notify_mutex_;
    std::condition_variable notify_condition_;
    // Notify thread MUST be initialized after all flags
    std::thread notify_thread_;
};

int main() {
    CounterService service = CounterService();

    if (not service.init()) {
        return EXIT_FAILURE;
    };

    service.start();

    return EXIT_SUCCESS;
}
