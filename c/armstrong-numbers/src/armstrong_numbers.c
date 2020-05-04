#include "armstrong_numbers.h"
#include "stdlib.h"
#include "string.h"
#include "stdio.h"
#include "math.h"

int is_armstrong_number(int n)
{
    char buffer[100];
    snprintf(buffer, 100, "%d", n);

    int len = strlen(buffer);
    int i;
    int sum = 0;

    for (i = 0; i < len; i++)
    {
        sum = sum + pow((buffer[i] - 48), len);
    }

    printf("ella sum: %d\n", sum);

    return sum == n;
}
