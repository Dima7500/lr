#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef enum {
    STATUS_OK,
    STATUS_ERR_NULL_ARG,
    STATUS_ERR_BASE,
    STATUS_ERR_INVALID_NUMBER,
    STATUS_ERR_MEMORY
} status_t;

status_t char_to_digit(const char c, const int base, int *out) {
    if (out == NULL) return STATUS_ERR_NULL_ARG;

    int digit = -1;
    if (c >= '0' && c <= '9')
        digit = c - '0';
    else if (c >= 'A' && c <= 'Z')
        digit = c - 'A' + 10;
    else if (c >= 'a' && c <= 'z')
        digit = c - 'a' + 10;

    if (digit < 0 || digit >= base) return STATUS_ERR_INVALID_NUMBER;

    *out = digit;
    return STATUS_OK;
}

char digit_to_char(const int digit) {
    return (char)(digit < 10 ? '0' + digit : 'A' + (digit - 10));
}

status_t validate_number(const char *number, const int base) {
    if (number == NULL) return STATUS_ERR_NULL_ARG;
    if (*number == '\0') return STATUS_ERR_INVALID_NUMBER;

    for (const char *p = number; *p != '\0'; ++p) {
        int digit = 0;
        const status_t rc = char_to_digit(*p, base, &digit);
        if (rc != STATUS_OK) return rc;
    }
    return STATUS_OK;
}

const char *skip_leading_zeros(const char *number) {
    while (number[0] == '0' && number[1] != '\0')
        ++number;
    return number;
}

status_t add_in_base(const char *a, const char *b, const int base, char **out) {
    if (a == NULL || b == NULL || out == NULL) return STATUS_ERR_NULL_ARG;
    if (base < 2 || base > 36) return STATUS_ERR_BASE;

    status_t rc = validate_number(a, base);
    if (rc == STATUS_OK)
        rc = validate_number(b, base);
    if (rc != STATUS_OK) return rc;

    a = skip_leading_zeros(a);
    b = skip_leading_zeros(b);

    const size_t len_a = strlen(a);
    const size_t len_b = strlen(b);
    const size_t len = (len_a > len_b ? len_a : len_b) + 1;

    char *sum = (char *)malloc(len + 1);
    if (sum == NULL) return STATUS_ERR_MEMORY;
    sum[len] = '\0';

    int carry = 0;
    for (size_t i = 0; i < len; i++) {
        int digit_a = 0;
        int digit_b = 0;
        if (i < len_a)
            char_to_digit(a[len_a - 1 - i], base, &digit_a);
        if (i < len_b)
            char_to_digit(b[len_b - 1 - i], base, &digit_b);

        const int value = digit_a + digit_b + carry;
        sum[len - 1 - i] = digit_to_char(value % base);
        carry = value / base;
    }

    const char *start = skip_leading_zeros(sum);
    if (start != sum)
        memmove(sum, start, strlen(start) + 1);

    *out = sum;
    return STATUS_OK;
}

status_t sum_in_base(char **result, const int base, const size_t count, ...) {
    if (result == NULL) return STATUS_ERR_NULL_ARG;
    if (base < 2 || base > 36) return STATUS_ERR_BASE;

    char *sum = (char *)malloc(2);
    if (sum == NULL) return STATUS_ERR_MEMORY;
    sum[0] = '0';
    sum[1] = '\0';

    status_t rc = STATUS_OK;
    va_list args;
    va_start(args, count);

    for (size_t i = 0; i < count && rc == STATUS_OK; i++) {
        const char *number = va_arg(args, char *);
        char *next = NULL;

        rc = add_in_base(sum, number, base, &next);
        if (rc == STATUS_OK) {
            free(sum);
            sum = next;
        }
    }

    va_end(args);

    if (rc != STATUS_OK) {
        free(sum);
        return rc;
    }

    *result = sum;
    return STATUS_OK;
}

void print_status(const status_t rc) {
    switch (rc) {
        case STATUS_OK:
            printf("успешно\n");
            break;
        case STATUS_ERR_NULL_ARG:
            printf("ошибка: передан нулевой указатель\n");
            break;
        case STATUS_ERR_BASE:
            printf("ошибка: основание системы счисления должно быть в диапазоне [2..36]\n");
            break;
        case STATUS_ERR_INVALID_NUMBER:
            printf("ошибка: строка не является неотрицательным целым числом в заданной системе счисления\n");
            break;
        case STATUS_ERR_MEMORY:
            printf("ошибка: недостаточно памяти\n");
            break;
        default:
            printf("неизвестная ошибка\n");
            break;
    }
}

void report(const char *title, const status_t rc, char *result) {
    printf("%s -> ", title);
    if (rc == STATUS_OK)
        printf("%s\n", result);
    else
        print_status(rc);
    free(result);
}

int main(int argc, char *argv[]) {
    (void)argv;

    if (argc != 1) {
        fprintf(stderr, "Формат запуска программы: ./[программа] (без аргументов)\n");
        return 1;
    }

    char *result = NULL;
    status_t rc;

    printf("=== Сложение в столбик двух чисел (add_in_base) ===\n");
    rc = add_in_base("999", "1", 10, &result);
    report("(10) 999 + 1", rc, result);
    result = NULL;
    rc = add_in_base("1011", "111", 2, &result);
    report("(2) 1011 + 111", rc, result);
    result = NULL;
    rc = add_in_base("FF", "1", 16, &result);
    report("(16) FF + 1", rc, result);

    printf("\n=== Сумма произвольного количества чисел (sum_in_base) ===\n");
    result = NULL;
    rc = sum_in_base(&result, 10, 4, "123", "877", "1000", "0");
    report("(10) 123 + 877 + 1000 + 0", rc, result);
    result = NULL;
    rc = sum_in_base(&result, 2, 3, "1", "1", "1");
    report("(2) 1 + 1 + 1", rc, result);
    result = NULL;
    rc = sum_in_base(&result, 16, 3, "ffff", "FFFF", "2");
    report("(16) ffff + FFFF + 2 (регистр букв не важен)", rc, result);
    result = NULL;
    rc = sum_in_base(&result, 36, 2, "zzzz", "1");
    report("(36) zzzz + 1", rc, result);
    result = NULL;
    rc = sum_in_base(&result, 10, 3, "000", "0000123", "00");
    report("(10) 000 + 0000123 + 00 (ведущие нули)", rc, result);
    result = NULL;
    rc = sum_in_base(&result, 10, 2, "0", "000");
    report("(10) 0 + 000", rc, result);
    result = NULL;
    rc = sum_in_base(&result, 10, 0);
    report("(10) пустая сумма", rc, result);

    printf("\n=== Числа, не помещающиеся во встроенные типы ===\n");
    result = NULL;
    rc = sum_in_base(&result, 10, 2, "18446744073709551615", "18446744073709551615");
    report("(10) ULLONG_MAX + ULLONG_MAX", rc, result);
    result = NULL;
    rc = sum_in_base(&result, 10, 3, "99999999999999999999999999999999999999",
                     "1", "123456789012345678901234567890");
    report("(10) 10^38 - 1 + 1 + 123456789012345678901234567890", rc, result);
    result = NULL;
    rc = sum_in_base(&result, 2, 2, "1111111111111111111111111111111111111111111111111111111111111111111111", "1");
    report("(2) 2^70 - 1 + 1", rc, result);

    printf("\n=== Обработка ошибок ===\n");
    result = NULL;
    rc = sum_in_base(&result, 1, 1, "1");
    report("основание 1", rc, result);
    result = NULL;
    rc = sum_in_base(&result, 37, 1, "1");
    report("основание 37", rc, result);
    result = NULL;
    rc = sum_in_base(&result, 2, 2, "101", "102");
    report("(2) 101 + 102 (цифра 2 в двоичной)", rc, result);
    result = NULL;
    rc = sum_in_base(&result, 10, 2, "12", "-5");
    report("(10) 12 + -5 (отрицательное число)", rc, result);
    result = NULL;
    rc = sum_in_base(&result, 10, 2, "12", "");
    report("(10) 12 + \"\" (пустая строка)", rc, result);
    result = NULL;
    rc = sum_in_base(&result, 10, 2, "12", (char *)NULL);
    report("(10) 12 + NULL", rc, result);

    return 0;
}
