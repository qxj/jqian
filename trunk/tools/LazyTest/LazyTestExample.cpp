#include "LazyTest.h"

TESTCASE(test1)
{
    ASSERT_TRUE(1 + 1 ==  2);
}

TESTCASE(test2)
{
    ASSERT_TRUE(1 + 1 !=  2);
}

TESTCASE(test3)
{
#if defined(_WIN32)
    int* p = NULL;
    *p = 10;
#endif
    ASSERT_TRUE(1 + 1 >  2);
}

int main()
{
    RUN_ALL_CASES();
    return 0;
}

