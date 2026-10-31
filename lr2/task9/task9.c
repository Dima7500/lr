#include <limits.h>
#include <math.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

typedef enum {
    STATUS_OK,
    STATUS_ERR_NULL_ARG,
    STATUS_ERR_INVALID,
    STATUS_ERR_OUT_OF_RANGE,
    STATUS_ERR_NO_FRACTION,
    STATUS_ERR_MEMORY
} status_t;

typedef struct {
    double value;
    status_t status;
    unsigned long long numerator;
    unsigned long long denominator;
    int is_finite;
} fraction_result_t;

unsigned long long gcd(unsigned long long a, unsigned long long b) {
    while (b != 0) {
        const unsigned long long r = a % b;
        a = b;
        b = r;
    }
    return a;
}

status_t to_decimal_fraction(const double x, const double eps,
                             unsigned long long *numerator, unsigned long long *denominator) {
    if (numerator == NULL || denominator == NULL) return STATUS_ERR_NULL_ARG;
    if (!isfinite(x) || !isfinite(eps) || eps <= 0.0) return STATUS_ERR_INVALID;

    const int MAX_DIGITS = 17;
    unsigned long long power = 1;

    for (int k = 1; k <= MAX_DIGITS; k++) {
        power *= 10;
        const double digits = round(x * (double)power);

        if (fabs(x - digits / (double)power) < eps) {
            const unsigned long long m = (unsigned long long)digits;
            const unsigned long long g = gcd(m, power);
            *numerator = m / g;
            *denominator = power / g;
            return STATUS_OK;
        }
    }

    return STATUS_ERR_NO_FRACTION;
}

int is_finite_in_base(unsigned long long denominator, const unsigned long long base) {
    unsigned long long g = gcd(denominator, base);
    while (g > 1) {
        while (denominator % g == 0)
            denominator /= g;
        g = gcd(denominator, base);
    }
    return denominator == 1;
}

status_t check_fractions(fraction_result_t **results, const double eps, const int base, const size_t count, ...) {
    if (results == NULL) return STATUS_ERR_NULL_ARG;
    if (!isfinite(eps) || eps <= 0.0 || eps >= 1.0 || base < 2 || count == 0) return STATUS_ERR_INVALID;

    fraction_result_t *res = (fraction_result_t *)calloc(count, sizeof(fraction_result_t));
    if (res == NULL) return STATUS_ERR_MEMORY;

    va_list args;
    va_start(args, count);

    for (size_t i = 0; i < count; i++) {
        const double x = va_arg(args, double);
        res[i].value = x;

        if (!isfinite(x) || x < eps || x > 1.0 - eps) {
            res[i].status = STATUS_ERR_OUT_OF_RANGE;
            continue;
        }

        res[i].status = to_decimal_fraction(x, eps, &res[i].numerator, &res[i].denominator);
        if (res[i].status == STATUS_OK)
            res[i].is_finite = is_finite_in_base(res[i].denominator, (unsigned long long)base);
    }

    va_end(args);

    *results = res;
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
        case STATUS_ERR_INVALID:
            printf("ошибка: некорректные аргументы (eps вне (0; 1), основание < 2, количество 0)\n");
            break;
        case STATUS_ERR_OUT_OF_RANGE:
            printf("ошибка: число не лежит в интервале (0; 1)\n");
            break;
        case STATUS_ERR_NO_FRACTION:
            printf("ошибка: число не является десятичной дробью (не более 17 знаков) с точностью eps\n");
            break;
        case STATUS_ERR_MEMORY:
            printf("ошибка: недостаточно памяти\n");
            break;
        default:
            printf("неизвестная ошибка\n");
            break;
    }
}

void print_results(const char *title, const int base, const status_t rc,
                   const fraction_result_t *results, const size_t count) {
    printf("=== %s (основание %d) ===\n", title, base);

    if (rc != STATUS_OK) {
        print_status(rc);
        printf("\n");
        return;
    }

    for (size_t i = 0; i < count; i++) {
        printf("  %-20.15g ", results[i].value);
        if (results[i].status != STATUS_OK) {
            print_status(results[i].status);
            continue;
        }
        printf("= %llu/%llu -> %s\n", results[i].numerator, results[i].denominator,
               results[i].is_finite ? "конечное представление" : "бесконечное (периодическое) представление");
    }
    printf("\n");
}

int main(int argc, char *argv[]) {
    (void)argv;

    if (argc != 1) {
        fprintf(stderr, "Формат запуска программы: ./[программа] (без аргументов)\n");
        return 1;
    }

    const double eps = 1e-12;
    fraction_result_t *results = NULL;
    status_t rc;

    rc = check_fractions(&results, eps, 10, 5, 0.5, 0.1, 0.123456789, 0.333, 1.0 / 3.0);
    print_results("Десятичная система: любая десятичная дробь конечна", 10, rc, results, 5);
    free(results);
    results = NULL;

    rc = check_fractions(&results, eps, 2, 6, 0.5, 0.75, 0.125, 0.0625, 0.1, 0.2);
    print_results("Двоичная система: конечны дроби со знаменателем 2^k", 2, rc, results, 6);
    free(results);
    results = NULL;

    rc = check_fractions(&results, eps, 5, 5, 0.2, 0.04, 0.008, 0.5, 0.1);
    print_results("Пятеричная система: конечны дроби со знаменателем 5^k", 5, rc, results, 5);
    free(results);
    results = NULL;

    rc = check_fractions(&results, eps, 6, 5, 0.5, 0.25, 0.375, 0.2, 0.15);
    print_results("Шестеричная система (6 = 2 * 3)", 6, rc, results, 5);
    free(results);
    results = NULL;

    rc = check_fractions(&results, eps, 3, 4, 0.5, 0.2, 0.1, 0.75);
    print_results("Троичная система: ни одна десятичная дробь не конечна", 3, rc, results, 4);
    free(results);
    results = NULL;

    rc = check_fractions(&results, eps, 20, 5, 0.05, 0.35, 0.0025, 0.125, 0.0001);
    print_results("Основание 20 (20 = 2^2 * 5)", 20, rc, results, 5);
    free(results);
    results = NULL;

    rc = check_fractions(&results, eps, 10, 5, 0.0, 1.0, -0.5, 1.5, NAN);
    print_results("Числа вне интервала (0; 1)", 10, rc, results, 5);
    free(results);
    results = NULL;

    rc = check_fractions(&results, eps, 1, 1, 0.5);
    print_results("Ошибка: основание 1", 1, rc, results, 1);

    rc = check_fractions(&results, 0.0, 10, 1, 0.5);
    print_results("Ошибка: eps = 0", 10, rc, results, 1);

    return 0;
}
