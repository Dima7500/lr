#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <math.h>

typedef enum {
    STATUS_OK,
    STATUS_ERR_INVALID,
    STATUS_ERR_RANGE,
    STATUS_ERR_OVERFLOW,
    STATUS_ERR_NULL_ARG
} status_t;

status_t parse_int(const char *str, long long *result) {
    if (str == NULL || result == NULL) {
        return STATUS_ERR_NULL_ARG;
    }
    char *end;
    long long value = strtoll(str, &end, 10);

    if (end == str || *end != '\0') {
        return STATUS_ERR_INVALID;
    }
    if (value > INT_MAX || value < INT_MIN) {
        return STATUS_ERR_OVERFLOW;
    }
    *result = value;
    return STATUS_OK;
}

status_t find_multiples(const int x, int arr[], int *count) {
    if (arr == NULL || count == NULL) {
        return STATUS_ERR_NULL_ARG;
    }
    if (x == 0) {
        return STATUS_ERR_RANGE;
    }
    long long step = (x < 0) ? -(long long)x : x;
    *count = 0;
    for (long long i = step; i <= 100; i += step) {
        arr[*count] = (int)i;
        (*count)++;
    }
    return STATUS_OK;
}

status_t check_prime(const int x, int *is_prime) {
    if (is_prime == NULL) {
        return STATUS_ERR_NULL_ARG;
    }
    if (x < 2) {
        return STATUS_ERR_RANGE;
    }
    *is_prime = 1;
    int limit = (int)sqrt((double)x);
    for (int d = 2; d <= limit; d++) {
        if (x % d == 0) {
            *is_prime = 0;
            break;
        }
    }
    return STATUS_OK;
}

status_t to_hex_digits(const int x, char digits[], int *len, int *negative) {
    if (digits == NULL || len == NULL || negative == NULL) {
        return STATUS_ERR_NULL_ARG;
    }
    const char hex[] = "0123456789ABCDEF";
    long long n = x;
    *negative = (n < 0);
    if (n < 0) {
        n = -n;
    }
    if (n == 0) {
        digits[0] = '0';
        *len = 1;
        return STATUS_OK;
    }
    char tmp[16];
    int k = 0;
    while (n > 0) {
        tmp[k++] = hex[n % 16];
        n /= 16;
    }
    for (int i = 0; i < k; i++) {
        digits[i] = tmp[k - 1 - i];
    }
    *len = k;
    return STATUS_OK;
}

status_t power_table(const int x, long long table[10][10]) {
    if (table == NULL) {
        return STATUS_ERR_NULL_ARG;
    }
    if (x < 1 || x > 10) {
        return STATUS_ERR_RANGE;
    }
    for (int base = 1; base <= 10; base++) {
        for (int p = 1; p <= x; p++) {
            table[base - 1][p - 1] = llround(pow(base, p));
        }
    }
    return STATUS_OK;
}

status_t sum_to_x(const int x, unsigned long long *sum) {
    if (sum == NULL) {
        return STATUS_ERR_NULL_ARG;
    }
    if (x < 1) {
        return STATUS_ERR_RANGE;
    }
    unsigned long long n = (unsigned long long)x;
    *sum = n * (n + 1) / 2;
    return STATUS_OK;
}

status_t factorial(const int x, unsigned long long *result) {
    if (result == NULL) {
        return STATUS_ERR_NULL_ARG;
    }
    if (x < 0) {
        return STATUS_ERR_RANGE;
    }
    unsigned long long f = 1;
    for (int i = 2; i <= x; i++) {
        if (f > ULLONG_MAX / i) {
            return STATUS_ERR_OVERFLOW;
        }
        f *= i;
    }
    *result = f;
    return STATUS_OK;
}

int main(int argc, char *argv[]) {
    if (argc != 3) {
        fprintf(stderr, "Формат: ./[<программа] [число] [флаг]\n");
        fprintf(stderr, "Флаги: -h -p -s -e -a -f (или /h /p ...)\n");
        return 1;
    }

    long long value;
    status_t st = parse_int(argv[1], &value);
    if (st == STATUS_ERR_INVALID) {
        fprintf(stderr, "Ошибка: '%s' не является целым числом\n", argv[1]);
        return 1;
    } else if (st == STATUS_ERR_OVERFLOW) {
        fprintf(stderr, "Ошибка: слишком большое число\n");
        return 1;
    } else if (st != STATUS_OK) {
        fprintf(stderr, "Неизвестная ошибка\n");
        return 1;
    }
    int x = (int)value;

    const char *flag = argv[2];
    if ((flag[0] != '-' && flag[0] != '/') || flag[1] == '\0' || flag[2] != '\0') {
        fprintf(stderr, "Ошибка: флаг должен иметь вид -X или /X\n");
        return 1;
    }

    switch (flag[1]) {
        case 'h': {
            int arr[100];
            int count;
            st = find_multiples(x, arr, &count);
            if (st == STATUS_ERR_RANGE) {
                fprintf(stderr, "Ошибка: x не должно быть равно 0\n");
                return 1;
            }
            if (count == 0) {
                printf("Нет чисел до 100, кратных %d\n", x);
            } else {
                for (int i = 0; i < count; i++) {
                    printf("%d ", arr[i]);
                }
                printf("\n");
            }
            break;
        }
        case 'p': {
            int is_prime;
            st = check_prime(x, &is_prime);
            if (st == STATUS_ERR_RANGE) {
                printf("%d не является ни простым, ни составным\n", x);
            } else if (is_prime) {
                printf("%d - простое число\n", x);
            } else {
                printf("%d - составное число\n", x);
            }
            break;
        }
        case 's': {
            char digits[16];
            int len, negative;
            st = to_hex_digits(x, digits, &len, &negative);
            if (st != STATUS_OK) {
                fprintf(stderr, "Ошибка\n");
                return 1;
            }
            if (negative) {
                printf("- ");
            }
            for (int i = 0; i < len; i++) {
                printf("%c ", digits[i]);
            }
            printf("\n");
            break;
        }
        case 'e': {
            long long table[10][10];
            st = power_table(x, table);
            if (st == STATUS_ERR_RANGE) {
                fprintf(stderr, "Ошибка: для -e x должно быть от 1 до 10\n");
                return 1;
            }
            for (int base = 1; base <= 10; base++) {
                for (int p = 1; p <= x; p++) {
                    printf("%12lld", table[base - 1][p - 1]);
                }
                printf("\n");
            }
            break;
        }
        case 'a': {
            unsigned long long sum;
            st = sum_to_x(x, &sum);
            if (st == STATUS_ERR_RANGE) {
                fprintf(stderr, "Ошибка: x должно быть натуральным числом\n");
                return 1;
            }
            printf("Сумма от 1 до %d = %llu\n", x, sum);
            break;
        }
        case 'f': {
            unsigned long long f;
            st = factorial(x, &f);
            if (st == STATUS_ERR_RANGE) {
                fprintf(stderr, "Ошибка: факториал отрицательного числа не определён\n");
                return 1;
            } else if (st == STATUS_ERR_OVERFLOW) {
                fprintf(stderr, "Ошибка: факториал слишком большой (переполнение)\n");
                return 1;
            }
            printf("%d! = %llu\n", x, f);
            break;
        }
        default:
            fprintf(stderr, "Ошибка: неизвестный флаг '%s'\n", flag);
            return 1;
    }
    return 0;
}
