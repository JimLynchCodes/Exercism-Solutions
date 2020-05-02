#include <stddef.h>
#include "vendor/unity.h"
#include "../src/hello_world.h"

// Notes from Jim!
// If it's not working you may need to run `make clean` and then just `make`.

void setUp(void)
{
}

void tearDown(void)
{
}

static void test_hello(void)
{
   TEST_ASSERT_EQUAL_STRING("Hello, World!", hello());
}

int main(void)
{
   UnityBegin("test/test_hello_world.c");

   RUN_TEST(test_hello);

   return UnityEnd();
}
