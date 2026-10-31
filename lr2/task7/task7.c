#include <math.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef enum {
    STATUS_OK,
    STATUS_ERR_NULL_ARG,
    STATUS_ERR_INVALID,
    STATUS_ERR_NOT_SYMMETRIC,
    STATUS_ERR_NOT_POSITIVE_DEFINITE,
    STATUS_ERR_RANGE,
    STATUS_ERR_MEMORY
} status_t;

typedef struct {
    const double *coords;
} vector_t;

typedef status_t (*norm_func_t)(const vector_t *x, const size_t n, const void *context, double *out);

typedef struct {
    double p;
    double eps;
} p_context_t;

typedef struct {
    const double *matrix;
    double eps;
} a_context_t;

typedef struct {
    double max_norm;
    size_t *indices;
    size_t count;
} longest_t;

status_t check_vector(const vector_t *x, const size_t n) {
    if (x == NULL || x->coords == NULL) return STATUS_ERR_NULL_ARG;
    if (n == 0) return STATUS_ERR_INVALID;

    for (size_t i = 0; i < n; i++) {
        if (!isfinite(x->coords[i]))
            return STATUS_ERR_INVALID;
    }
    return STATUS_OK;
}

status_t check_eps(const double eps) {
    return (isfinite(eps) && eps > 0.0) ? STATUS_OK : STATUS_ERR_INVALID;
}

status_t norm_max(const vector_t *x, const size_t n, const void *context, double *out) {
    (void)context;
    if (out == NULL) return STATUS_ERR_NULL_ARG;

    const status_t rc = check_vector(x, n);
    if (rc != STATUS_OK) return rc;

    double result = 0.0;
    for (size_t j = 0; j < n; j++)
        result = fmax(result, fabs(x->coords[j]));

    *out = result;
    return STATUS_OK;
}

status_t norm_p(const vector_t *x, const size_t n, const void *context, double *out) {
    if (context == NULL || out == NULL) return STATUS_ERR_NULL_ARG;

    const p_context_t *ctx = (const p_context_t *)context;
    if (check_eps(ctx->eps) != STATUS_OK || isnan(ctx->p)) return STATUS_ERR_INVALID;
    if (ctx->p < 1.0 - ctx->eps) return STATUS_ERR_INVALID;
    if (isinf(ctx->p)) return norm_max(x, n, NULL, out);

    double largest = 0.0;
    const status_t rc = norm_max(x, n, NULL, &largest);
    if (rc != STATUS_OK) return rc;

    if (fpclassify(largest) == FP_ZERO) {
        *out = 0.0;
        return STATUS_OK;
    }

    double sum = 0.0;
    for (size_t j = 0; j < n; j++)
        sum += pow(fabs(x->coords[j]) / largest, ctx->p);

    const double result = largest * pow(sum, 1.0 / ctx->p);
    if (!isfinite(result)) return STATUS_ERR_RANGE;

    *out = result;
    return STATUS_OK;
}

status_t check_positive_definite(const double *matrix, const size_t n, const double eps) {
    if (matrix == NULL) return STATUS_ERR_NULL_ARG;
    if (n == 0 || check_eps(eps) != STATUS_OK) return STATUS_ERR_INVALID;
    if (n > SIZE_MAX / n / sizeof(double)) return STATUS_ERR_MEMORY;

    for (size_t i = 0; i < n; i++) {
        for (size_t j = 0; j < n; j++) {
            if (!isfinite(matrix[i * n + j])) return STATUS_ERR_INVALID;
            if (fabs(matrix[i * n + j] - matrix[j * n + i]) > eps) return STATUS_ERR_NOT_SYMMETRIC;
        }
    }

    double *work = (double *)malloc(n * n * sizeof(double));
    if (work == NULL) return STATUS_ERR_MEMORY;
    memcpy(work, matrix, n * n * sizeof(double));

    status_t rc = STATUS_OK;
    for (size_t k = 0; k < n && rc == STATUS_OK; k++) {
        const double pivot = work[k * n + k];
        if (pivot <= eps) {
            rc = STATUS_ERR_NOT_POSITIVE_DEFINITE;
            continue;
        }

        for (size_t i = k + 1; i < n; i++) {
            const double factor = work[i * n + k] / pivot;
            for (size_t j = k; j < n; j++)
                work[i * n + j] -= factor * work[k * n + j];
        }
    }

    free(work);
    return rc;
}

status_t norm_a(const vector_t *x, const size_t n, const void *context, double *out) {
    if (context == NULL || out == NULL) return STATUS_ERR_NULL_ARG;

    const a_context_t *ctx = (const a_context_t *)context;
    status_t rc = check_vector(x, n);
    if (rc == STATUS_OK)
        rc = check_positive_definite(ctx->matrix, n, ctx->eps);
    if (rc != STATUS_OK) return rc;

    double dot = 0.0;
    for (size_t i = 0; i < n; i++) {
        double row = 0.0;
        for (size_t j = 0; j < n; j++)
            row += ctx->matrix[i * n + j] * x->coords[j];
        dot += row * x->coords[i];
    }

    const double result = sqrt(fmax(dot, 0.0));
    if (!isfinite(result)) return STATUS_ERR_RANGE;

    *out = result;
    return STATUS_OK;
}

void free_longest(longest_t *results, const size_t count) {
    if (results == NULL) return;

    for (size_t i = 0; i < count; i++)
        free(results[i].indices);
    free(results);
}

status_t select_longest(const norm_func_t norm, const void *context, const vector_t *vectors,
                        const size_t vectors_count, const size_t n, const double eps,
                        double *values, longest_t *out) {
    if (norm == NULL || vectors == NULL || values == NULL || out == NULL) return STATUS_ERR_NULL_ARG;
    if (vectors_count == 0) return STATUS_ERR_INVALID;

    double max = 0.0;
    for (size_t i = 0; i < vectors_count; i++) {
        const status_t rc = norm(&vectors[i], n, context, &values[i]);
        if (rc != STATUS_OK) return rc;
        max = (i == 0) ? values[i] : fmax(max, values[i]);
    }

    size_t count = 0;
    for (size_t i = 0; i < vectors_count; i++) {
        if (max - values[i] <= eps)
            ++count;
    }

    size_t *indices = (size_t *)malloc(count * sizeof(size_t));
    if (indices == NULL) return STATUS_ERR_MEMORY;

    size_t pos = 0;
    for (size_t i = 0; i < vectors_count; i++) {
        if (max - values[i] <= eps)
            indices[pos++] = i;
    }

    out->max_norm = max;
    out->indices = indices;
    out->count = count;
    return STATUS_OK;
}

status_t find_longest(longest_t **results, const double eps, const size_t n,
                      const size_t vectors_count, const size_t norms_count, ...) {
    if (results == NULL) return STATUS_ERR_NULL_ARG;
    if (check_eps(eps) != STATUS_OK || n == 0 || vectors_count == 0 || norms_count == 0)
        return STATUS_ERR_INVALID;
    if (vectors_count > SIZE_MAX / sizeof(vector_t)) return STATUS_ERR_MEMORY;

    vector_t *vectors = (vector_t *)malloc(vectors_count * sizeof(vector_t));
    double *values = (double *)malloc(vectors_count * sizeof(double));
    longest_t *res = (longest_t *)calloc(norms_count, sizeof(longest_t));
    status_t rc = (vectors == NULL || values == NULL || res == NULL) ? STATUS_ERR_MEMORY : STATUS_OK;

    if (rc == STATUS_OK) {
        va_list args;
        va_start(args, norms_count);

        for (size_t i = 0; i < vectors_count; i++) {
            vectors[i] = va_arg(args, vector_t);
            if (rc == STATUS_OK)
                rc = check_vector(&vectors[i], n);
        }

        for (size_t k = 0; k < norms_count; k++) {
            const norm_func_t norm = va_arg(args, norm_func_t);
            const void *context = va_arg(args, const void *);

            if (rc == STATUS_OK && norm == NULL)
                rc = STATUS_ERR_NULL_ARG;
            if (rc == STATUS_OK)
                rc = select_longest(norm, context, vectors, vectors_count, n, eps, values, &res[k]);
        }

        va_end(args);
    }

    free(vectors);
    free(values);

    if (rc != STATUS_OK) {
        free_longest(res, norms_count);
        return rc;
    }

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
            printf("ошибка: некорректный аргумент (eps <= 0, n = 0, p < 1, NaN/inf)\n");
            break;
        case STATUS_ERR_NOT_SYMMETRIC:
            printf("ошибка: матрица A не симметрична\n");
            break;
        case STATUS_ERR_NOT_POSITIVE_DEFINITE:
            printf("ошибка: матрица A не положительно определена\n");
            break;
        case STATUS_ERR_RANGE:
            printf("ошибка: значение нормы не помещается в double\n");
            break;
        case STATUS_ERR_MEMORY:
            printf("ошибка: недостаточно памяти\n");
            break;
        default:
            printf("неизвестная ошибка\n");
            break;
    }
}

void print_vector(const vector_t *v, const size_t n) {
    printf("(");
    for (size_t j = 0; j < n; j++)
        printf(j == 0 ? "%.15g" : ", %.15g", v->coords[j]);
    printf(")");
}

void print_results(const char *title, const status_t rc, const longest_t *results, const char *const *names,
                   const size_t norms_count, const vector_t *vectors, const size_t n) {
    printf("=== %s ===\n", title);

    if (rc != STATUS_OK) {
        print_status(rc);
        printf("\n");
        return;
    }

    for (size_t k = 0; k < norms_count; k++) {
        printf("  %s = %.6g:", names[k], results[k].max_norm);
        for (size_t i = 0; i < results[k].count; i++) {
            const size_t idx = results[k].indices[i];
            printf(" v%zu", idx + 1);
            print_vector(&vectors[idx], n);
        }
        printf("\n");
    }
    printf("\n");
}

void demo_plane(void) {
    const double eps = 1e-9;
    const double c1[] = {4.0, 0.0};
    const double c2[] = {3.0, 3.0};
    const double c3[] = {0.0, -4.0};
    const double c4[] = {-1.0, 3.5};
    const vector_t v[] = {{c1}, {c2}, {c3}, {c4}};

    const double a_diag[] = {1.0, 0.0,
                             0.0, 3.0};
    const double a_full[] = {5.0, 2.0,
                             2.0, 1.0};

    const p_context_t p1 = {1.0, eps};
    const p_context_t p2 = {2.0, eps};
    const a_context_t a1 = {a_diag, eps};
    const a_context_t a2 = {a_full, eps};
    const void *none = NULL;
    const void *ctx_p1 = &p1;
    const void *ctx_p2 = &p2;
    const void *ctx_a1 = &a1;
    const void *ctx_a2 = &a2;

    const char *const names[] = {"||x||_inf", "||x||_1", "||x||_2",
                                 "||x||_A, A = [[1, 0], [0, 3]]", "||x||_A, A = [[5, 2], [2, 1]]"};

    printf("R^2: v1 = (4, 0), v2 = (3, 3), v3 = (0, -4), v4 = (-1, 3.5)\n");

    longest_t *results = NULL;
    const status_t rc = find_longest(&results, eps, 2, 4, 5,
                                     v[0], v[1], v[2], v[3],
                                     norm_max, none,
                                     norm_p, ctx_p1,
                                     norm_p, ctx_p2,
                                     norm_a, ctx_a1,
                                     norm_a, ctx_a2);
    print_results("Самые длинные векторы для каждой нормы", rc, results, names, 5, v, 2);
    if (rc == STATUS_OK)
        free_longest(results, 5);
}

void demo_space(void) {
    const double eps = 1e-6;
    const double c1[] = {1.0, -2.0, 2.0};
    const double c2[] = {2.0, 2.0, -1.0};
    const double c3[] = {0.0, 0.0, 3.0000001};
    const double c4[] = {-2.0, 1.0, 2.0};
    const vector_t v[] = {{c1}, {c2}, {c3}, {c4}};

    const double a[] = {2.0, -1.0,  0.0,
                       -1.0,  2.0, -1.0,
                        0.0, -1.0,  2.0};

    const p_context_t p1 = {1.0, eps};
    const p_context_t p2 = {2.0, eps};
    const p_context_t p_inf = {INFINITY, eps};
    const a_context_t a_ctx = {a, eps};
    const void *ctx_p1 = &p1;
    const void *ctx_p2 = &p2;
    const void *ctx_inf = &p_inf;
    const void *ctx_a = &a_ctx;

    const char *const names[] = {"||x||_1", "||x||_2", "||x||_p, p = INFINITY",
                                 "||x||_A, A = трёхдиагональная [-1, 2, -1]"};

    printf("R^3, eps = 1e-6: v1 = (1, -2, 2), v2 = (2, 2, -1), v3 = (0, 0, 3.0000001), v4 = (-2, 1, 2)\n");

    longest_t *results = NULL;
    const status_t rc = find_longest(&results, eps, 3, 4, 4,
                                     v[0], v[1], v[2], v[3],
                                     norm_p, ctx_p1,
                                     norm_p, ctx_p2,
                                     norm_p, ctx_inf,
                                     norm_a, ctx_a);
    print_results("Ничьи с учётом eps", rc, results, names, 4, v, 3);
    if (rc == STATUS_OK)
        free_longest(results, 4);
}

void demo_errors(void) {
    const double eps = 1e-9;
    const double c[] = {1.0, 2.0};
    const double bad[] = {1.0, NAN};
    const vector_t v = {c};
    const vector_t v_nan = {bad};
    const vector_t v_null = {NULL};

    const double not_symmetric[] = {1.0, 2.0,
                                    0.0, 1.0};
    const double indefinite[] = {1.0, 2.0,
                                 2.0, 1.0};
    const p_context_t p_half = {0.5, eps};
    const a_context_t a_nonsym = {not_symmetric, eps};
    const a_context_t a_indef = {indefinite, eps};
    const void *none = NULL;
    const void *ctx_half = &p_half;
    const void *ctx_nonsym = &a_nonsym;
    const void *ctx_indef = &a_indef;
    const norm_func_t no_norm = NULL;

    longest_t *results = NULL;
    status_t rc;

    printf("=== Обработка ошибок ===\n");

    printf("p = 0.5                            -> ");
    rc = find_longest(&results, eps, 2, 1, 1, v, norm_p, ctx_half);
    print_status(rc);

    printf("A несимметрична                    -> ");
    rc = find_longest(&results, eps, 2, 1, 1, v, norm_a, ctx_nonsym);
    print_status(rc);

    printf("A не положительно определена       -> ");
    rc = find_longest(&results, eps, 2, 1, 1, v, norm_a, ctx_indef);
    print_status(rc);

    printf("указатель на функцию нормы NULL    -> ");
    rc = find_longest(&results, eps, 2, 1, 1, v, no_norm, none);
    print_status(rc);

    printf("координата NaN                     -> ");
    rc = find_longest(&results, eps, 2, 1, 1, v_nan, norm_max, none);
    print_status(rc);

    printf("координаты NULL                    -> ");
    rc = find_longest(&results, eps, 2, 1, 1, v_null, norm_max, none);
    print_status(rc);

    printf("eps = 0                            -> ");
    rc = find_longest(&results, 0.0, 2, 1, 1, v, norm_max, none);
    print_status(rc);

    printf("размерность n = 0                  -> ");
    rc = find_longest(&results, eps, 0, 1, 1, v, norm_max, none);
    print_status(rc);
}

int main(int argc, char *argv[]) {
    (void)argv;

    if (argc != 1) {
        fprintf(stderr, "Формат запуска программы: ./[программа] (без аргументов)\n");
        return 1;
    }

    demo_plane();
    demo_space();
    demo_errors();
    return 0;
}
