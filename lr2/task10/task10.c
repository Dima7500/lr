#include <math.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

typedef enum {
    STATUS_OK,
    STATUS_ERR_NULL_ARG,
    STATUS_ERR_INVALID,
    STATUS_ERR_RANGE,
    STATUS_ERR_MEMORY
} status_t;

status_t reexpand_polynomial(const double eps, const double a, double **result, const int degree, ...) {
    if (result == NULL) return STATUS_ERR_NULL_ARG;
    if (!isfinite(eps) || eps <= 0.0 || !isfinite(a) || degree < 0) return STATUS_ERR_INVALID;

    const size_t count = (size_t)degree + 1;
    double *g = (double *)malloc(count * sizeof(double));
    if (g == NULL) return STATUS_ERR_MEMORY;

    status_t rc = STATUS_OK;
    va_list args;
    va_start(args, degree);
    for (size_t i = 0; i < count; i++) {
        g[i] = va_arg(args, double);
        if (!isfinite(g[i]))
            rc = STATUS_ERR_INVALID;
    }
    va_end(args);

    for (size_t k = 0; k + 1 < count && rc == STATUS_OK; k++) {
        for (size_t j = count - 1; j > k; j--)
            g[j - 1] += a * g[j];
    }

    for (size_t i = 0; i < count && rc == STATUS_OK; i++) {
        if (!isfinite(g[i]))
            rc = STATUS_ERR_RANGE;
        else if (fabs(g[i]) < eps)
            g[i] = 0.0;
    }

    if (rc != STATUS_OK) {
        free(g);
        return rc;
    }

    *result = g;
    return STATUS_OK;
}

status_t evaluate(const double *coeffs, const int degree, const double x, double *out) {
    if (coeffs == NULL || out == NULL) return STATUS_ERR_NULL_ARG;
    if (degree < 0 || !isfinite(x)) return STATUS_ERR_INVALID;

    double value = 0.0;
    for (int i = degree; i >= 0; i--)
        value = value * x + coeffs[i];

    if (!isfinite(value)) return STATUS_ERR_RANGE;
    *out = value;
    return STATUS_OK;
}

status_t check_reexpansion(const double *f, const double *g, const int degree, const double a,
                           const double eps, double *max_diff, int *equal) {
    if (f == NULL || g == NULL || max_diff == NULL || equal == NULL) return STATUS_ERR_NULL_ARG;
    if (!isfinite(eps) || eps <= 0.0) return STATUS_ERR_INVALID;

    const double points[] = {-2.0, -1.0, -0.5, 0.0, 0.5, 1.0, 2.0, 3.0};
    const size_t count = sizeof(points) / sizeof(points[0]);
    double worst = 0.0;
    int ok = 1;

    for (size_t i = 0; i < count; i++) {
        double fx = 0.0;
        double gx = 0.0;
        status_t rc = evaluate(f, degree, points[i], &fx);
        if (rc == STATUS_OK)
            rc = evaluate(g, degree, points[i] - a, &gx);
        if (rc != STATUS_OK) return rc;

        const double diff = fabs(fx - gx);
        worst = fmax(worst, diff);
        if (diff > eps * fmax(1.0, fabs(fx)))
            ok = 0;
    }

    *max_diff = worst;
    *equal = ok;
    return STATUS_OK;
}

void print_polynomial(const double *coeffs, const int degree, const char *variable) {
    int printed = 0;

    for (int i = 0; i <= degree; i++) {
        if (fpclassify(coeffs[i]) == FP_ZERO)
            continue;

        printf(printed ? (coeffs[i] < 0.0 ? " - " : " + ") : (coeffs[i] < 0.0 ? "-" : ""));
        printf("%.10g", fabs(coeffs[i]));
        if (i == 1)
            printf("*%s", variable);
        else if (i > 1)
            printf("*%s^%d", variable, i);
        printed = 1;
    }

    if (!printed)
        printf("0");
    printf("\n");
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
            printf("ошибка: некорректный аргумент (eps <= 0, отрицательная степень, NaN/inf)\n");
            break;
        case STATUS_ERR_RANGE:
            printf("ошибка: коэффициент не помещается в double\n");
            break;
        case STATUS_ERR_MEMORY:
            printf("ошибка: недостаточно памяти\n");
            break;
        default:
            printf("неизвестная ошибка\n");
            break;
    }
}

void report(const char *title, const status_t rc, double *g, const double *f, const int degree,
            const double a, const double eps) {
    printf("=== %s ===\n", title);

    if (rc != STATUS_OK) {
        print_status(rc);
        printf("\n");
        return;
    }

    char variable[64];
    if (a < 0.0)
        snprintf(variable, sizeof(variable), "(x + %.10g)", -a);
    else
        snprintf(variable, sizeof(variable), "(x - %.10g)", a);

    printf("  f(x) = ");
    print_polynomial(f, degree, "x");
    printf("  g    = ");
    print_polynomial(g, degree, variable);

    printf("  коэффициенты g:");
    for (int i = 0; i <= degree; i++)
        printf(" %.10g", g[i]);
    printf("\n");

    double max_diff = 0.0;
    int equal = 0;
    const status_t check = check_reexpansion(f, g, degree, a, eps, &max_diff, &equal);
    if (check == STATUS_OK)
        printf("  проверка в 8 точках: max |f(x) - g(x)| = %g -> %s\n\n", max_diff,
               equal ? "многочлены совпадают" : "РАСХОЖДЕНИЕ");
    else
        print_status(check);

    free(g);
}

int main(int argc, char *argv[]) {
    (void)argv;

    if (argc != 1) {
        fprintf(stderr, "Формат запуска программы: ./[программа] (без аргументов)\n");
        return 1;
    }

    const double eps = 1e-9;
    double *g = NULL;
    status_t rc;

    const double f1[] = {0.0, 0.0, 1.0};
    rc = reexpand_polynomial(eps, 1.0, &g, 2, 0.0, 0.0, 1.0);
    report("x^2 по степеням (x - 1)", rc, g, f1, 2, 1.0, eps);

    const double f2[] = {-5.0, 2.0, -3.0, 1.0};
    g = NULL;
    rc = reexpand_polynomial(eps, 2.0, &g, 3, -5.0, 2.0, -3.0, 1.0);
    report("x^3 - 3x^2 + 2x - 5 по степеням (x - 2)", rc, g, f2, 3, 2.0, eps);

    const double f3[] = {1.0, 4.0, 6.0, 4.0, 1.0};
    g = NULL;
    rc = reexpand_polynomial(eps, -1.0, &g, 4, 1.0, 4.0, 6.0, 4.0, 1.0);
    report("(x + 1)^4 по степеням (x + 1): должно получиться (x + 1)^4", rc, g, f3, 4, -1.0, eps);

    const double f4[] = {3.0, -1.5, 0.0, 2.0, 0.0, -0.5};
    g = NULL;
    rc = reexpand_polynomial(eps, 0.5, &g, 5, 3.0, -1.5, 0.0, 2.0, 0.0, -0.5);
    report("-0.5x^5 + 2x^3 - 1.5x + 3 по степеням (x - 0.5)", rc, g, f4, 5, 0.5, eps);

    const double f5[] = {7.0};
    g = NULL;
    rc = reexpand_polynomial(eps, 10.0, &g, 0, 7.0);
    report("Константа 7 (степень 0)", rc, g, f5, 0, 10.0, eps);

    const double f6[] = {1.0, 2.0, 3.0};
    g = NULL;
    rc = reexpand_polynomial(eps, 0.0, &g, 2, 1.0, 2.0, 3.0);
    report("a = 0: коэффициенты не меняются", rc, g, f6, 2, 0.0, eps);

    printf("=== Обработка ошибок ===\n");
    printf("отрицательная степень   -> ");
    print_status(reexpand_polynomial(eps, 1.0, &g, -1, 1.0));
    printf("eps = 0                 -> ");
    print_status(reexpand_polynomial(0.0, 1.0, &g, 1, 1.0, 2.0));
    printf("коэффициент NaN         -> ");
    print_status(reexpand_polynomial(eps, 1.0, &g, 1, 1.0, NAN));
    printf("a = inf                 -> ");
    print_status(reexpand_polynomial(eps, INFINITY, &g, 1, 1.0, 2.0));
    printf("result = NULL           -> ");
    print_status(reexpand_polynomial(eps, 1.0, NULL, 1, 1.0, 2.0));
    printf("переполнение (a = 1e200) -> ");
    print_status(reexpand_polynomial(eps, 1e200, &g, 3, 1.0, 1.0, 1.0, 1.0));

    return 0;
}
