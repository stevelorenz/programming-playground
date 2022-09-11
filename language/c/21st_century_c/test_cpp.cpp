/*
 * test.cpp
 */

#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <yaml-cpp/node/node.h>
#include <yaml-cpp/yaml.h>

#include <cstdlib>
#include <iostream>
#include <random>
#include <string>

#include "gmock/gmock.h"

TEST(TEST_CPP, BasicAssertions) {
    // Expect two strings not to be equal.
    EXPECT_STRNE("hello", "world");
    // Expect equality.
    EXPECT_EQ(7 * 6, 42);
}

TEST(TEST_CPP, YAML_CPP) {
    std::string name = "Default";
    YAML::Node config = YAML::LoadFile("test_config.yaml");
    if (config["name"]) {
        name = config["name"].as<std::string>();
        EXPECT_EQ(name, "Steve");
    } else {
        EXPECT_EQ(name, "Default");
    }
}

/*
 * Try to learn and use GMock mechanism provided by GTest ----------------------
 * */

class RandomNumberGenerator {
   public:
    virtual int Get() = 0;
    // The deconstructor MUST be virtual but I'm not sure about default...
    virtual ~RandomNumberGenerator() = default;
};

class RandomNumberGeneratorMock : public RandomNumberGenerator {
   public:
    MOCK_METHOD(int, Get, (), (override));
};

class RandomNumberGeneratorMt19937 : public RandomNumberGenerator {
   public:
    int Get() override;
};

int RandomNumberGeneratorMt19937::Get() {
    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_int_distribution<> distrib(1, 6);
    return distrib(gen);
}

class Calc {
   private:
    RandomNumberGenerator* rng_;

   public:
    Calc(RandomNumberGenerator* rng);
    int Sum(int a, int b);
    int Multiply(int a, int b);
    int AddRandomNumber(int a);
};

Calc::Calc(RandomNumberGenerator* rng) { rng_ = rng; }

int Calc::Sum(int a, int b) { return a + b; }

int Calc::Multiply(int a, int b) { return a * b; }

int Calc::AddRandomNumber(int a) { return a + rng_->Get(); }

class CalcTestSuite : public testing::Test {
   protected:
    RandomNumberGeneratorMock rng_mock_;
    // The syntax looks very cryptic...
    Calc sut_{&rng_mock_};
};

TEST_F(CalcTestSuite, GMOCK_TEST) {
    auto rng = new RandomNumberGeneratorMt19937();
    Calc calc(rng);
    // Everytime you run it, it will return a random number.
    std::cout << "AddRandomNumber + 1 with Mt19937: " << calc.AddRandomNumber(1)
              << std::endl;
    delete rng;

    EXPECT_EQ(4, sut_.Sum(2, 2));

    // The Get() function should be called only once with the return value of 3!
    EXPECT_CALL(rng_mock_, Get()).Times(1).WillOnce(testing::Return(3));
    EXPECT_EQ(4, sut_.AddRandomNumber(1));
}
