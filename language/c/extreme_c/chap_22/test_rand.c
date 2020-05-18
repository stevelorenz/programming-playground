#include <stdarg.h>
#include <stddef.h>
#include <setjmp.h>
#include <cmocka.h>

#include "rand.h"

/**
 * @brief The wrapper function for the real rand function.
 */
int __wrap_rand()
{
	return mock_type(int);
}

void test_even_random_number(void **state)
{
	will_return(__wrap_rand, 10);
	assert_false(random_boolean());
}

void test_odd_random_number(void **state)
{
	will_return(__wrap_rand, 11);
	assert_true(random_boolean());
}

int main(int argc, char *argv[])
{
	const struct CMUnitTest tests[] = {
		cmocka_unit_test(test_even_random_number),
		cmocka_unit_test(test_odd_random_number),
	};
	return cmocka_run_group_tests(tests, NULL, NULL);
}
