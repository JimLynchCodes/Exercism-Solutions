#include "armstrong_numbers.h"

const is_armstrong_number(int)
{
    unsigned int num = 123; //for example
    unsigned int dig = count(num);
    char arr[dig];
    while (dig--)
    {
        arr[dig] = num % 10;
        num /= 10;
    }

    printf(arr)

    return num
}
