/*
 * test.cpp
 */

#include <yaml-cpp/node/node.h>
#include <yaml-cpp/yaml.h>

#include <cstdlib>
#include <iostream>
#include <string>

int main() {
    std::string name = "Default";
    YAML::Node config = YAML::LoadFile("test_config.yaml");
    if (config["name"]) {
        name = config["name"].as<std::string>();
    }

    std::cout << "Hello: " << name << std::endl;

    return EXIT_SUCCESS;
}
