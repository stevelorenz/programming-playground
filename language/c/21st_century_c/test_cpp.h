/*
 * test_cpp.h
 */

#ifndef TEST_CPP_H
#define TEST_CPP_H

#pragma once

#include <gmock/gmock.h>

#include <cstdlib>
#include <iostream>
#include <random>
#include <string>

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

class Calc {
   private:
    RandomNumberGenerator* rng_;

   public:
    Calc(RandomNumberGenerator* rng);
    int Sum(int a, int b);
    int Multiply(int a, int b);
    int AddRandomNumber(int a);
};

#endif /* !TEST_CPP_H */
