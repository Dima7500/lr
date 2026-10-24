#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <limits.h>
#include <math.h>

typedef enum {
    STATUS_OK = 0,
    STATUS_ERR_NULL_ARG,
    STATUS_ERR_INVALID,
    STATUS_ERR_FORMAT,
    STATUS_ERR_RANGE
} status_t;

typedef enum {
    EQ_INFINITE = -1,
    EQ_NONE     =  0,
    EQ_ONE      =  1,
    EQ_TWO      =  2
} eq_roots_count_t;

typedef struct {
    double a, b, c;
    double roots[2];
    eq_roots_count_t count;
} equation_t;

status_t parse_int(const char *s, int *out) {
    if (s == NULL || out == NULL)
        return STATUS_ERR_NULL_ARG;

    char *end = NULL;
    long v = strtol(s, &end, 10);

    if (end == s || *end != '\0')
        return STATUS_ERR_FORMAT;

    if (v < INT_MIN || v > INT_MAX)
        return STATUS_ERR_RANGE;

    *out = (int)v;
    return STATUS_OK;
}

status_t parse_double(const char *s, double *out) {
    if (s == NULL || out == NULL)
        return STATUS_ERR_NULL_ARG;

    char *end = NULL;
    double v = strtod(s, &end);

    if (end == s || *end != '\0')
        return STATUS_ERR_FORMAT;

    if (!isfinite(v))
        return STATUS_ERR_RANGE;

    *out = v;
    return STATUS_OK;
}

status_t parse_eps(const char *s, double *out) {
    if (s == NULL || out == NULL)
        return STATUS_ERR_NULL_ARG;

    status_t rc = parse_double(s, out);
    if (rc != STATUS_OK)
        return rc;

    if (*out <= 0.0 || *out >= 1.0)
        return STATUS_ERR_RANGE;

    return STATUS_OK;
}

status_t solve_quadratic(const double a, const double b, const double c,
                         const double eps, equation_t *eq) {
    if (eq == NULL)  return STATUS_ERR_NULL_ARG;
    if (eps <= 0.0)  return STATUS_ERR_INVALID;

    eq->a = a;
    eq->b = b;
    eq->c = c;
    eq->roots[0] = 0.0;
    eq->roots[1] = 0.0;
    eq->count = EQ_NONE;

    if (fabs(a) <= eps) {
        if (fabs(b) <= eps) {
            if (fabs(c) <= eps)
                eq->count = EQ_INFINITE;
            else
                eq->count = EQ_NONE;
            return STATUS_OK;
        }

        eq->roots[0] = -c / b;
        eq->count = EQ_ONE;
        return STATUS_OK;
    }

    double D = b * b - 4.0 * a * c;

    if (fabs(D) <= eps) {
        eq->roots[0] = -b / (2.0 * a);
        eq->count = EQ_ONE;
        return STATUS_OK;
    }

    if (D < 0.0) {
        eq->count = EQ_NONE;
        return STATUS_OK;
    }

    double sqrtD = sqrt(D);
    eq->roots[0] = (-b - sqrtD) / (2.0 * a);
    eq->roots[1] = (-b + sqrtD) / (2.0 * a);

    if (eq->roots[0] > eq->roots[1]) {
        double t = eq->roots[0];
        eq->roots[0] = eq->roots[1];
        eq->roots[1] = t;
    }

    eq->count = EQ_TWO;
    return STATUS_OK;
}

status_t solve_all_permutations(const double arr[3], const double eps,
                                equation_t *out, int *out_count) {
    if (arr == NULL || out == NULL || out_count == NULL)
        return STATUS_ERR_NULL_ARG;

    double coefs[6][3] = {
        { arr[0], arr[1], arr[2] },
        { arr[0], arr[2], arr[1] },
        { arr[1], arr[0], arr[2] },
        { arr[1], arr[2], arr[0] },
        { arr[2], arr[0], arr[1] },
        { arr[2], arr[1], arr[0] }
    };

    int written = 0;

    for (int i = 0; i < 6; ++i) {
        bool dup = false;
        for (int j = 0; j < written; ++j) {
            if (fabs(out[j].a - coefs[i][0]) <= eps &&
                fabs(out[j].b - coefs[i][1]) <= eps &&
                fabs(out[j].c - coefs[i][2]) <= eps) {
                dup = true;
                break;
            }
        }
        if (dup) continue;

        status_t rc = solve_quadratic(coefs[i][0], coefs[i][1], coefs[i][2],
                                      eps, &out[written]);
        if (rc != STATUS_OK)
            return rc;

        ++written;
    }

    *out_count = written;
    return STATUS_OK;
}

status_t is_multiple(const int a, const int b, bool *out) {
    if (out == NULL)
        return STATUS_ERR_NULL_ARG;

    if (a == 0 || b == 0)
        return STATUS_ERR_INVALID;

    if (b == -1) {
        *out = true;
        return STATUS_OK;
    }

    *out = (a % b == 0);
    return STATUS_OK;
}

status_t is_right_triangle(const double a, const double b, const double c,
                           const double eps, bool *out) {
    if (out == NULL)  return STATUS_ERR_NULL_ARG;
    if (eps <= 0.0)   return STATUS_ERR_INVALID;

    *out = false;

    if (a <= eps || b <= eps || c <= eps)
        return STATUS_OK;

    double x = a, y = b, z = c;
    double t;
    if (x > y) { t = x; x = y; y = t; }
    if (y > z) { t = y; y = z; z = t; }
    if (x > y) { t = x; x = y; y = t; }

    if (fabs(x * x + y * y - z * z) <= eps)
        *out = true;

    return STATUS_OK;
}


int main(int argc, char *argv[]) {
    if (argc < 2) {
        fprintf(stderr, "Формат запуска программы: ./[программа] [флаг] [параметры]\n");
        return 1;
    }

    char flag;
    if ((argv[1][0] == '-' || argv[1][0] == '/') &&
         argv[1][1] != '\0' && argv[1][2] == '\0') {
        flag = argv[1][1];
    } else {
        fprintf(stderr, "'%s' не флаг формата -X или /X\n", argv[1]);
        return 1;
    }

    switch (flag) {
        case 'q': {
            if (argc != 6) {
                fprintf(stderr, "Формат флага q: ./[программа] [-q] [эпсилон] [число1] [число2] [число3]\n");
                return 1;
            }

            double eps;
            switch (parse_eps(argv[2], &eps)) {
                case STATUS_OK:
                    break;
                case STATUS_ERR_FORMAT:
                    fprintf(stderr, "Ошибка: '%s' не является числом\n", argv[2]);
                    return 1;
                default:
                    fprintf(stderr, "Эпсилон должен быть в (0, 1)\n");
                    return 1;
            }

            double arr[3];
            for (int i = 0; i < 3; ++i) {
                if (parse_double(argv[3 + i], &arr[i]) != STATUS_OK) {
                    fprintf(stderr, "Ошибка: '%s' не является вещественным числом\n", argv[3 + i]);
                    return 1;
                }
            }

            equation_t eqs[6];
            int n = 0;

            switch (solve_all_permutations(arr, eps, eqs, &n)) {
                case STATUS_OK:
                    break;
                case STATUS_ERR_NULL_ARG:
                    fprintf(stderr, "В функцию был передан параметр NULL\n");
                    return 1;
                default:
                    fprintf(stderr, "Некорректные параметры функции\n");
                    return 1;
            }

            for (int i = 0; i < n; i++) {
                printf("Корни уравнения %.5fx² + %.5fx + %.5f = 0 : ",
                       eqs[i].a, eqs[i].b, eqs[i].c);
                switch (eqs[i].count) {
                    case EQ_NONE:
                        printf("отсутствуют\n");
                        break;
                    case EQ_INFINITE:
                        printf("все вещественные числа\n");
                        break;
                    case EQ_ONE:
                        printf("x₁ = %.5f\n", eqs[i].roots[0]);
                        break;
                    case EQ_TWO:
                        printf("x₁ = %.5f  x₂ = %.5f\n", eqs[i].roots[0], eqs[i].roots[1]);
                        break;
                }
            }
            break;
        }
        case 'm': {
            if (argc != 4) {
                fprintf(stderr, "Формат флага m: ./[программа] [-m] [число1] [число2]\n");
                return 1;
            }

            int a, b;
            if (parse_int(argv[2], &a) != STATUS_OK || parse_int(argv[3], &b) != STATUS_OK) {
                fprintf(stderr, "Флаг m принимает на вход целые ненулевые числа\n");
                return 1;
            }

            bool result;
            switch (is_multiple(a, b, &result)) {
                case STATUS_OK:
                    if (result)
                        printf("%d кратно %d\n", a, b);
                    else
                        printf("%d не кратно %d\n", a, b);
                    break;
                case STATUS_ERR_INVALID:
                    fprintf(stderr, "Одно из чисел — ноль\n");
                    return 1;
                default:
                    fprintf(stderr, "В функцию был передан параметр NULL\n");
                    return 1;
            }
            break;
        }
        case 't': {
            if (argc != 6) {
                fprintf(stderr, "Формат флага t: ./[программа] [-t] [эпсилон] [число1] [число2] [число3]\n");
                return 1;
            }

            double eps;
            switch (parse_eps(argv[2], &eps)) {
                case STATUS_OK:
                    break;
                case STATUS_ERR_FORMAT:
                    fprintf(stderr, "Ошибка: '%s' не является числом\n", argv[2]);
                    return 1;
                default:
                    fprintf(stderr, "Эпсилон должен быть в (0, 1)\n");
                    return 1;
            }

            double s[3];
            for (int i = 0; i < 3; ++i) {
                if (parse_double(argv[3 + i], &s[i]) != STATUS_OK) {
                    fprintf(stderr, "Ошибка: '%s' не является вещественным числом\n", argv[3 + i]);
                    return 1;
                }
            }

            bool result;
            switch (is_right_triangle(s[0], s[1], s[2], eps, &result)) {
                case STATUS_OK:
                    if (result)
                        printf("%.5f %.5f %.5f - стороны прямоугольного треугольника\n", s[0], s[1], s[2]);
                    else
                        printf("%.5f %.5f %.5f - не стороны прямоугольного треугольника\n", s[0], s[1], s[2]);
                    break;
                case STATUS_ERR_NULL_ARG:
                    fprintf(stderr, "В функцию был передан параметр NULL\n");
                    return 1;
                default:
                    fprintf(stderr, "Некорректные параметры функции\n");
                    return 1;
            }
            break;
        }
        default:
            fprintf(stderr, "Существующие флаги: q, m, t\n");
            return 1;
    }

    return 0;
}
