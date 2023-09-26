#include <chrono>
#include <condition_variable>
#include <iomanip>
#include <iostream>
#include <sstream>
#include <thread>

#include <vsomeip/vsomeip.hpp>

#include "common.hpp"

class Subscriber {
  public:
    Subscriber() : app_(vsomeip::runtime::get()->create_application()) {}

    void on_state(vsomeip::state_type_e _state) {
        if (_state == vsomeip::state_type_e::ST_REGISTERED) {
            app_->request_service(SAMPLE_SERVICE_ID, SAMPLE_INSTANCE_ID);
        }
    }

    void on_availability(vsomeip::service_t _service,
                         vsomeip::instance_t _instance, bool _is_available) {
        std::cout << "Service [" << std::setw(4) << std::setfill('0')
                  << std::hex << _service << "." << _instance << "] is "
                  << (_is_available ? "available." : "NOT available.")
                  << std::endl;
    }

    void on_message(const std::shared_ptr<vsomeip::message> &_response) {
        std::stringstream its_message;
        its_message << "Received a notification for Event ["
                    << std::setfill('0') << std::hex << std::setw(4)
                    << _response->get_service() << "." << std::setw(4)
                    << _response->get_instance() << "." << std::setw(4)
                    << _response->get_method() << "] to Client/Session ["
                    << std::setw(4) << _response->get_client() << "/"
                    << std::setw(4) << _response->get_session() << "] = ";
        std::shared_ptr<vsomeip::payload> its_payload =
            _response->get_payload();
        its_message << "(" << std::dec << its_payload->get_length() << ") "
                    << std::hex << std::setw(2);
        std::string notify_msg;
        for (uint32_t i = 0; i < its_payload->get_length(); ++i) {
            notify_msg += static_cast<char>(its_payload->get_data()[i]);
        }
        std::cout << notify_msg << std::endl;
    }

    bool init() {
        if (not app_->init()) {
            std::cerr << "Failed to init application!" << std::endl;
            return false;
        }
        app_->register_state_handler(
            std::bind(&Subscriber::on_state, this, std::placeholders::_1));

        app_->register_availability_handler(
            SAMPLE_SERVICE_ID, SAMPLE_INSTANCE_ID,
            std::bind(&Subscriber::on_availability, this, std::placeholders::_1,
                      std::placeholders::_2, std::placeholders::_3));

        app_->register_message_handler(
            vsomeip::ANY_SERVICE, SAMPLE_INSTANCE_ID, vsomeip::ANY_METHOD,
            std::bind(&Subscriber::on_message, this, std::placeholders::_1));

        std::set<vsomeip::eventgroup_t> its_groups;
        its_groups.insert(SAMPLE_EVENTGROUP_ID);
        app_->request_event(SAMPLE_SERVICE_ID, SAMPLE_INSTANCE_ID,
                            SAMPLE_EVENT_ID, its_groups,
                            vsomeip::event_type_e::ET_FIELD);
        std::cout
            << "[INFO] Subscriber starts to subscribe the event group with ID: "
            << SAMPLE_EVENTGROUP_ID << std::endl;
        app_->subscribe(SAMPLE_SERVICE_ID, SAMPLE_INSTANCE_ID,
                        SAMPLE_EVENTGROUP_ID);

        return true;
    }

    void start() { app_->start(); }

  private:
    std::shared_ptr<vsomeip::application> app_;
};

int main(int argc, char *argv[]) {
    Subscriber sub = Subscriber();
    if (sub.init()) {
        std::cout << "[INFO] Subscriber starts" << std::endl;
        sub.start();
        return EXIT_SUCCESS;
    } else {
        return EXIT_FAILURE;
    }
}
