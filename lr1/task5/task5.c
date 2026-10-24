#include <math.h>
#include <stdio.h>
#include <stdlib.h>

typedef enum {
    STATUS_OK,
    STATUS_ERR_NULL_ARG,
    STATUS_ERR_FORMAT,
    STATUS_ERR_RANGE,
    STATUS_ERR_INVALID,
    STATUS_ERR_DIVERGES
} status_t;

typedef double (*func_t)(double);

status_t parse_double(const char *str, double *out) {
    if (str == NULL || out == NULL) return STATUS_ERR_NULL_ARG;
    if (*str == '\0') return STATUS_ERR_FORMAT;

    char *end = NULL;
    double value = strtod(str, &end);

    if (*end != '\0')
        return STATUS_ERR_FORMAT;
    if (!isfinite(value))
        return STATUS_ERR_RANGE;

    *out = value;
    return STATUS_OK;
}

status_t sum1_series(const double x, const double eps, double *out) {
    if (out == NULL) return STATUS_ERR_NULL_ARG;
    if (eps <= 0.0) return STATUS_ERR_INVALID;

    double term = 1.0;
    double sum  = term;
    int n = 0;
    const int LIMIT = 1000000;

    while (n < LIMIT) {
        ++n;
        term *= x / (double)n;
        sum  += term;

        if (fabs(term) < eps)
            break;
    }

    if (n >= LIMIT || !isfinite(sum))
        return STATUS_ERR_RANGE;

    *out = sum;
    return STATUS_OK;
}

status_t sum2_series(const double x, const double eps, double *out) {
    if (out == NULL) return STATUS_ERR_NULL_ARG;
    if (eps <= 0.0) return STATUS_ERR_INVALID;

    double term = 1.0;
    double sum  = term;
    int n = 0;
    const int LIMIT = 1000000;

    while (n < LIMIT) {
        term *= -x * x / ((2.0 * n + 1.0) * (2.0 * n + 2.0));
        ++n;
        sum += term;

        if (fabs(term) < eps)
            break;
    }

    if (n >= LIMIT || !isfinite(sum))
        return STATUS_ERR_RANGE;

    *out = sum;
    return STATUS_OK;
}

status_t sum3_series(const double x, const double eps, double *out) {
    if (out == NULL) return STATUS_ERR_NULL_ARG;
    if (eps <= 0.0) return STATUS_ERR_INVALID;
    if (fabs(x) >= 1.0) return STATUS_ERR_DIVERGES;

    double term = 1.0;
    double sum  = term;
    int n = 0;
    const int LIMIT = 1000000;

    while (n < LIMIT) {
        double np1 = (double)(n + 1);
        term *= 27.0 * np1 * np1 * np1 * x * x
              / ((3.0 * n + 1.0) * (3.0 * n + 2.0) * (3.0 * n + 3.0));
        ++n;
        sum += term;

        if (fabs(term) < eps)
            break;
    }

    if (n >= LIMIT || !isfinite(sum))
        return STATUS_ERR_RANGE;

    *out = sum;
    return STATUS_OK;
}

status_t sum4_series(const double x, const double eps, double *out) {
    if (out == NULL) return STATUS_ERR_NULL_ARG;
    if (eps <= 0.0) return STATUS_ERR_INVALID;
    if (fabs(x) >= 1.0) return STATUS_ERR_DIVERGES;

    double term = -x * x / 2.0;
    double sum  = term;
    int n = 1;
    const int LIMIT = 1000000;

    while (n < LIMIT) {
        term *= -(2.0 * n + 1.0) * x * x / (2.0 * n + 2.0);
        ++n;
        sum += term;

        if (fabs(term) < eps)
            break;
    }

    if (n >= LIMIT)
        return STATUS_ERR_RANGE;

    *out = sum;
    return STATUS_OK;
}

double f_1(const double x) {
    if (fabs(x) < 1e-15)
        return 1.0;
    return log(1.0 + x) / x;
}

double f_2(const double x) {
    return exp(-x * x / 2.0);
}

double f_3(const double x) {
    if (x >= 1.0)
        return 0.0;
    return log(1.0 / (1.0 - x));
}

double f_4(const double x) {
    if (fabs(x) < 1e-15)
        return 1.0;
    return pow(x, x);
}

status_t integrate(const func_t f, const double a, const double b, const double eps, double *out) {
    if (f == NULL || out == NULL) return STATUS_ERR_NULL_ARG;
    if (b < a || eps <= 0.0) return STATUS_ERR_INVALID;

    int n = 1;
    double prev = 0.0;
    double cur = 0.0;
    const int LIMIT = 1 << 28;

    while (n < LIMIT) {
        double h = (b - a) / (double)n;
        double sum = 0.0;

        for (int i = 0; i < n; i++)
            sum += f(a + (i + 0.5) * h);

        prev = cur;
        cur = h * sum;

        if (n > 1 && fabs(cur - prev) < eps)
            break;

        n *= 2;
    }

    if (n >= LIMIT || !isfinite(cur))
        return STATUS_ERR_RANGE;

    *out = cur;
    return STATUS_OK;
}

void print_result(const char *name, const status_t rc, const double value) {
    switch (rc) {
        case STATUS_OK:
            printf("%s = %.10f\n", name, value);
            break;
        case STATUS_ERR_DIVERGES:
            printf("%s: ряд расходится (нужно |x| < 1)\n", name);
            break;
        case STATUS_ERR_RANGE:
            printf("%s: не удалось достичь точности / переполнение\n", name);
            break;
        default:
            printf("%s: неверные аргументы\n", name);
            break;
    }
}

int main(int argc, char *argv[]) {
    if (argc != 3) {
        fprintf(stderr, "Формат запуска программы: ./[программа] [эпсилон] [x]\n");
        return 1;
    }

    double eps = 0.0;
    switch (parse_double(argv[1], &eps)) {
        case STATUS_OK:
            if (eps <= 0 || eps >= 1) {
                fprintf(stderr, "Эпсилон должен быть в (0, 1)\n");
                return 1;
            }
            break;
        case STATUS_ERR_FORMAT:
            fprintf(stderr, "Ошибка: '%s' не является числом\n", argv[1]);
            return 1;
        default:
            fprintf(stderr, "Эпсилон должен быть в (0, 1)\n");
            return 1;
    }

    double x = 0.0;
    if (parse_double(argv[2], &x) != STATUS_OK) {
        fprintf(stderr, "Переменная x неправильного формата\n");
        return 1;
    }

    double value = 0.0;
    status_t rc;

    rc = sum1_series(x, eps, &value);
    print_result("Σ(n=0..∞) xⁿ / n!", rc, value);
    rc = sum2_series(x, eps, &value);
    print_result("Σ(n=0..∞) (−1)ⁿ x²ⁿ / (2n)!", rc, value);
    rc = sum3_series(x, eps, &value);
    print_result("Σ(n=0..∞) 3³ⁿ (n!)³ x²ⁿ / (3n)!", rc, value);
    rc = sum4_series(x, eps, &value);
    print_result("Σ(n=1..∞) (−1)ⁿ (2n−1)!! x²ⁿ / (2n)!!", rc, value);

    rc = integrate(f_1, 0.0, 1.0, eps, &value);
    print_result("∫₀¹ ln(1+x)/x dx", rc, value);
    rc = integrate(f_2, 0.0, 1.0, eps, &value);
    print_result("∫₀¹ e^(−x²/2) dx", rc, value);
    rc = integrate(f_3, 0.0, 1.0, eps, &value);
    print_result("∫₀¹ ln(1/(1−x)) dx", rc, value);
    rc = integrate(f_4, 0.0, 1.0, eps, &value);
    print_result("∫₀¹ xˣ dx", rc, value);

    return 0;
}
