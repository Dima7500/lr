#include <stdio.h>
#include <stdlib.h>
#include <math.h>

typedef enum {
    STATUS_OK = 0,
    STATUS_ERR_INVALID,
    STATUS_ERR_PRECISION,
    STATUS_ERR_ALLOC
} status_t;

status_t calc_e_limit(const double eps, double *out)
{
    if (out == NULL) return STATUS_ERR_INVALID;

    long long n = 1;
    double prev = 2.0;
    while (n < 100000000) {
        n *= 2;
        double res = pow(1.0 + 1.0 / n, n);
        if (fabs(res - prev) < eps) {
            *out = res;
            return STATUS_OK;
        }
        prev = res;
    }
    return STATUS_ERR_PRECISION;
}

status_t calc_e_series(const double eps, double *out)
{
    if (out == NULL) return STATUS_ERR_INVALID;

    double res = 1.0;
    double term = 1.0;
    int n = 1;
    while (term >= eps) {
        term /= n;
        res += term;
        n++;
    }
    *out = res;
    return STATUS_OK;
}

status_t solve_e(const double eps, double *out)
{
    if (out == NULL) return STATUS_ERR_INVALID;

    double a = 2.0, b = 3.0;

    while (b - a > eps) {
        double mid = (a + b) / 2;
        if ((log(a) - 1) * (log(mid) - 1) <= 0) b = mid;
        else a = mid;
    }
    *out = (a + b) / 2;
    return STATUS_OK;
}

status_t calc_pi_limit(const double eps, double *out)
{
    if (out == NULL) return STATUS_ERR_INVALID;

    double res = 4.0, prev = 4.0;
    long long k = 1, n = 1;
    while (n < 10000000) {
        n *= 2;
        while (k < n) {
            res *= 4.0 * k * (k + 1) / ((2.0 * k + 1) * (2.0 * k + 1));
            k++;
        }
        if (fabs(res - prev) < eps) {
            *out = res;
            return STATUS_OK;
        }
        prev = res;
    }
    return STATUS_ERR_PRECISION;
}

status_t calc_pi_series(const double eps, double *out)
{
    if (out == NULL) return STATUS_ERR_INVALID;

    double res = 0.0, sign = 1.0;
    for (long long n = 1; n < 100000000; n++) {
        res += sign * 4.0 / (2 * n - 1);
        if (4.0 / (2 * n + 1) < eps) {
            *out = res;
            return STATUS_OK;
        }
        sign = -sign;
    }
    return STATUS_ERR_PRECISION;
}

status_t solve_pi(const double eps, double *out)
{
    if (out == NULL) return STATUS_ERR_INVALID;

    double a = 3.0, b = 4.0;

    while (b - a > eps) {
        double mid = (a + b) / 2;
        if (cos(a / 2) * cos(mid / 2) <= 0) b = mid;
        else a = mid;
    }
    *out = (a + b) / 2;
    return STATUS_OK;
}
status_t calc_ln2_limit(const double eps, double *out)
{
    if (out == NULL) return STATUS_ERR_INVALID;

    long long n = 1;
    double prev = 1.0;
    while (n < 10000000) {
        n *= 2;
        double res = n * (pow(2.0, 1.0 / n) - 1);
        if (fabs(res - prev) < eps) {
            *out = res;
            return STATUS_OK;
        }
        prev = res;
    }
    return STATUS_ERR_PRECISION;
}

status_t calc_ln2_series(const double eps, double *out)
{
    if (out == NULL) return STATUS_ERR_INVALID;

    double res = 0.0, sign = 1.0;
    for (long long n = 1; n < 100000000; n++) {
        res += sign / n;
        if (1.0 / (n + 1) < eps) {
            *out = res;
            return STATUS_OK;
        }
        sign = -sign;
    }
    return STATUS_ERR_PRECISION;
}

status_t solve_ln2(const double eps, double *out)
{
    if (out == NULL || eps <= 0) return STATUS_ERR_INVALID;

    double a = 0.0, b = 1.0;

    while (b - a > eps) {
        double mid = (a + b) / 2;
        if ((exp(a) - 2) * (exp(mid) - 2) <= 0) b = mid;
        else a = mid;
    }
    *out = (a + b) / 2;
    return STATUS_OK;
}

status_t calc_sq2_limit(const double eps, double *out)
{
    if (out == NULL) return STATUS_ERR_INVALID;

    double res = -0.5;
    for (int n = 0; n < 1000000; n++) {
        double next = res - res * res / 2 + 1;
        if (fabs(next - res) < eps) {
            *out = next;
            return STATUS_OK;
        }
        res = next;
    }
    return STATUS_ERR_PRECISION;
}

status_t calc_sq2_product(const double eps, double *out)
{
    if (out == NULL) return STATUS_ERR_INVALID;

    double res = 1.0;
    for (int k = 2; k < 1000; k++) {
        double next = res * pow(2.0, pow(2.0, -k));
        if (fabs(next - res) < eps) {
            *out = next;
            return STATUS_OK;
        }
        res = next;
    }
    *out = res;
    return STATUS_OK;
}

status_t solve_sq2(const double eps, double *out)
{
    if (out == NULL) return STATUS_ERR_INVALID;

    double a = 1.0, b = 2.0;

    while (b - a > eps) {
        double mid = (a + b) / 2;
        if ((a * a - 2) * (mid * mid - 2) <= 0) b = mid;
        else a = mid;
    }
    *out = (a + b) / 2;
    return STATUS_OK;
}

double gamma_sum(const int m)
{
    double sum = 0.0;
    double c = 1.0;
    double ln_fact = 0.0;
    double sign = -1.0;
    for (int k = 1; k <= m; k++) {
        c = c * (m - k + 1) / k;
        ln_fact += log(k);
        sum += sign * c / k * ln_fact;
        sign = -sign;
    }
    return sum;
}

status_t calc_y_limit(const double eps, double *out)
{
    if (out == NULL) return STATUS_ERR_INVALID;

    int m = 1;
    double prev = gamma_sum(1);
    while (m < 32) {
        m *= 2;
        double res = gamma_sum(m);
        if (fabs(res - prev) < eps) {
            *out = res;
            return STATUS_OK;
        }
        prev = res;
    }
    return STATUS_ERR_PRECISION;
}

status_t calc_y_series(const double eps, double *out)
{
    if (out == NULL) return STATUS_ERR_INVALID;

    const double pi = 3.14159265358979323846;
    double res = -pi * pi / 6;
    double prev = res;
    long long k = 2;
    long long check = 1;
    for (long long m = 1; m <= 32768; m++) {
        while (k < (m + 1) * (m + 1)) {
            res += 1.0 / (m * m) - 1.0 / k;
            k++;
        }
        if (m == check) {
            if (fabs(res - prev) < eps) {
                *out = res;
                return STATUS_OK;
            }
            prev = res;
            check *= 2;
        }
    }
    return STATUS_ERR_PRECISION;
}

status_t primes_product(const int t, double *out)
{
    if (out == NULL || t < 2) return STATUS_ERR_INVALID;

    char *composite = calloc(t + 1, sizeof(char));
    if (composite == NULL) return STATUS_ERR_ALLOC;

    double prod = 1.0;
    for (long long p = 2; p <= t; p++) {
        if (!composite[p]) {
            prod = prod * (p - 1) / p;
            for (long long j = p * p; j <= t; j += p) composite[j] = 1;
        }
    }
    free(composite);
    *out = log(t) * prod;
    return STATUS_OK;
}

status_t solve_y(const double eps, double *out)
{
    if (out == NULL || eps <= 0) return STATUS_ERR_INVALID;

    double L, prev;
    status_t st = primes_product(2, &prev);
    if (st != STATUS_OK) return st;

    int t = 2, good = 0;
    while (good < 2) {
        if (t >= 16000000) return STATUS_ERR_PRECISION;
        t *= 2;
        st = primes_product(t, &L);
        if (st != STATUS_OK) return st;
        if (fabs(L - prev) < eps / 4) good++;
        else good = 0;
        prev = L;
    }

    double a = 0.0, b = 1.0;

    while (b - a > eps / 2) {
        double mid = (a + b) / 2;
        if ((exp(-a) - L) * (exp(-mid) - L) <= 0) b = mid;
        else a = mid;
    }
    *out = (a + b) / 2;
    return STATUS_OK;
}

status_t parse_eps(const char *s, double *out)
{
    if (s == NULL || out == NULL) return STATUS_ERR_INVALID;

    char *end;
    double v = strtod(s, &end);
    if (end == s || *end != '\0') return STATUS_ERR_INVALID;
    if (isnan(v) || isinf(v)) return STATUS_ERR_INVALID;
    if (v < 1e-15 || v >= 1) return STATUS_ERR_INVALID;

    *out = v;
    return STATUS_OK;
}

void print_result(const char *name, const status_t st, const double value)
{
    switch (st) {
        case STATUS_OK:
            printf("  %s\t%.12f\n", name, value);
            break;
        case STATUS_ERR_INVALID:
            fprintf(stderr, "Ошибка: неверные аргументы\n");
            break;
        case STATUS_ERR_PRECISION:
            fprintf(stderr, "Ошибка: такую точность этим способом не получить\n");
            break;
        case STATUS_ERR_ALLOC:
            fprintf(stderr, "Ошибка: не хватило памяти\n");
            break;
        default:
            fprintf(stderr, "Неизвестная ошибка\n");
            break;
    }
}

int main(int argc, char *argv[])
{
    if (argc != 2) {
        fprintf(stderr, "Формат запуска: ./[программа] [эпсилон]\n");
        return 1;
    }

    double eps;
    if (parse_eps(argv[1], &eps) != STATUS_OK) {
        fprintf(stderr, "Ошибка: эпсилон должен быть числом от 1e-15 до 1\n");
        return 1;
    }

    double v = 0;
    status_t st;
    printf("Эпсилон: %g\n", eps);

    printf("e\n");
    st = calc_e_limit(eps, &v);     print_result("предел:", st, v);
    st = calc_e_series(eps, &v);    print_result("ряд:", st, v);
    st = solve_e(eps, &v);          print_result("уравнение:", st, v);

    printf("pi\n");
    st = calc_pi_limit(eps, &v);    print_result("предел:", st, v);
    st = calc_pi_series(eps, &v);   print_result("ряд:", st, v);
    st = solve_pi(eps, &v);         print_result("уравнение:", st, v);

    printf("ln 2\n");
    st = calc_ln2_limit(eps, &v);   print_result("предел:", st, v);
    st = calc_ln2_series(eps, &v);  print_result("ряд:", st, v);
    st = solve_ln2(eps, &v);        print_result("уравнение:", st, v);

    printf("sqrt 2\n");
    st = calc_sq2_limit(eps, &v);   print_result("предел:", st, v);
    st = calc_sq2_product(eps, &v); print_result("ряд:", st, v);
    st = solve_sq2(eps, &v);        print_result("уравнение:", st, v);

    printf("gamma\n");
    st = calc_y_limit(eps, &v);     print_result("предел:", st, v);
    st = calc_y_series(eps, &v);    print_result("ряд:", st, v);
    st = solve_y(eps, &v);          print_result("уравнение:", st, v);

    return 0;
}
