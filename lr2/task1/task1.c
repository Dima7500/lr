#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <limits.h>

typedef enum {
    STATUS_OK = 0,
    STATUS_ERR_NULL_ARG,
    STATUS_ERR_INVALID,
    STATUS_ERR_ALLOC,
    STATUS_ERR_OVERFLOW
} status_t;

int is_digit(const char c) {
    return c >= '0' && c <= '9';
}

int is_lower_latin(const char c) {
    return c >= 'a' && c <= 'z';
}

int is_latin(const char c) {
    return is_lower_latin(c) || (c >= 'A' && c <= 'Z');
}

char to_upper(const char c) {
    return is_lower_latin(c) ? (char)(c - 'a' + 'A') : c;
}

status_t string_length(const char *str, size_t *len) {
    if (str == NULL || len == NULL)
        return STATUS_ERR_NULL_ARG;

    size_t n = 0;
    while (str[n] != '\0')
        ++n;

    *len = n;
    return STATUS_OK;
}

status_t string_reverse(const char *str, char **result) {
    if (str == NULL || result == NULL)
        return STATUS_ERR_NULL_ARG;

    size_t len;
    status_t rc = string_length(str, &len);
    if (rc != STATUS_OK)
        return rc;

    char *out = malloc(len + 1);
    if (!out)
        return STATUS_ERR_ALLOC;

    for (size_t i = 0; i < len; ++i)
        out[i] = str[len - 1 - i];
    out[len] = '\0';

    *result = out;
    return STATUS_OK;
}

status_t string_upper_odd(const char *str, char **result) {
    if (str == NULL || result == NULL)
        return STATUS_ERR_NULL_ARG;

    size_t len;
    status_t rc = string_length(str, &len);
    if (rc != STATUS_OK)
        return rc;

    char *out = malloc(len + 1);
    if (!out)
        return STATUS_ERR_ALLOC;

    for (size_t i = 0; i < len; ++i) {
        out[i] = (i % 2 == 1) ? to_upper(str[i]) : str[i];
    }
    out[len] = '\0';

    *result = out;
    return STATUS_OK;
}

status_t string_group_chars(const char *str, char **result) {
    if (str == NULL || result == NULL)
        return STATUS_ERR_NULL_ARG;

    size_t len;
    status_t rc = string_length(str, &len);
    if (rc != STATUS_OK)
        return rc;

    char *out = malloc(len + 1);
    if (!out)
        return STATUS_ERR_ALLOC;

    size_t w = 0;
    for (size_t i = 0; i < len; ++i) {
        if (is_digit(str[i]))
            out[w++] = str[i];
    }
    for (size_t i = 0; i < len; ++i) {
        if (is_latin(str[i]))
            out[w++] = str[i];
    }
    for (size_t i = 0; i < len; ++i) {
        if (!is_digit(str[i]) && !is_latin(str[i]))
            out[w++] = str[i];
    }
    out[w] = '\0';

    *result = out;
    return STATUS_OK;
}

status_t string_concat_random(const char *const *strings, const size_t count,
                       const unsigned int seed, char **result) {
    if (strings == NULL || result == NULL)
        return STATUS_ERR_NULL_ARG;
    if (count == 0)
        return STATUS_ERR_INVALID;
    if (count > SIZE_MAX / sizeof(size_t))
        return STATUS_ERR_OVERFLOW;

    size_t total = 0;
    for (size_t i = 0; i < count; ++i) {
        if (strings[i] == NULL)
            return STATUS_ERR_NULL_ARG;
        size_t len;
        status_t rc = string_length(strings[i], &len);
        if (rc != STATUS_OK)
            return rc;
        if (len > SIZE_MAX - 1 - total)
            return STATUS_ERR_OVERFLOW;
        total += len;
    }

    size_t *order = malloc(count * sizeof(size_t));
    if (!order)
        return STATUS_ERR_ALLOC;

    char *out = malloc(total + 1);
    if (!out) {
        free(order);
        return STATUS_ERR_ALLOC;
    }

    srand(seed);
    for (size_t i = 0; i < count; ++i)
        order[i] = i;
    for (size_t i = count - 1; i > 0; --i) {
        const size_t j = (size_t)rand() % (i + 1);
        const size_t tmp = order[i];
        order[i] = order[j];
        order[j] = tmp;
    }

    size_t w = 0;
    for (size_t i = 0; i < count; ++i) {
        const char *s = strings[order[i]];
        for (size_t k = 0; s[k] != '\0'; ++k)
            out[w++] = s[k];
    }
    out[w] = '\0';

    free(order);
    *result = out;
    return STATUS_OK;
}

status_t parse_uint(const char *str, unsigned int *value) {
    if (str == NULL || value == NULL)
        return STATUS_ERR_NULL_ARG;
    if (str[0] == '\0')
        return STATUS_ERR_INVALID;

    unsigned int v = 0;
    for (size_t i = 0; str[i] != '\0'; ++i) {
        if (!is_digit(str[i]))
            return STATUS_ERR_INVALID;
        const unsigned int d = (unsigned int)(str[i] - '0');
        if (v > (UINT_MAX - d) / 10)
            return STATUS_ERR_OVERFLOW;
        v = v * 10 + d;
    }

    *value = v;
    return STATUS_OK;
}

status_t parse_flag(const char *arg, char *flag) {
    if (arg == NULL || flag == NULL)
        return STATUS_ERR_NULL_ARG;
    if (arg[0] != '-' || arg[1] == '\0' || arg[2] != '\0')
        return STATUS_ERR_INVALID;
    if (arg[1] != 'l' && arg[1] != 'r' && arg[1] != 'u' && arg[1] != 'n' && arg[1] != 'c')
        return STATUS_ERR_INVALID;

    *flag = arg[1];
    return STATUS_OK;
}

void print_usage(void) {
    fprintf(stderr,
            "Формат запуска:\n"
            "  ./task1 -l|-r|-u|-n <строка>\n"
            "  ./task1 -c <строка> <seed> [строка ...]\n");
}

int main(int argc, char *argv[]) {
    if (argc < 3) {
        print_usage();
        return 1;
    }

    char flag;
    status_t rc = parse_flag(argv[1], &flag);
    switch (rc) {
        case STATUS_OK:
            break;
        case STATUS_ERR_INVALID:
            fprintf(stderr, "Неизвестный флаг. Допустимые флаги: -l, -r, -u, -n, -c\n");
            print_usage();
            return 1;
        default:
            fprintf(stderr, "Ошибка разбора флага\n");
            return 1;
    }

    if (flag != 'c' && argc != 3) {
        fprintf(stderr, "Флаг -%c принимает ровно одну строку\n", flag);
        return 1;
    }
    if (flag == 'c' && argc < 4) {
        fprintf(stderr, "Флагу -c нужны строка и seed (целое число типа unsigned int)\n");
        return 1;
    }

    const char *str = argv[2];
    char *result = NULL;

    switch (flag) {
        case 'l': {
            size_t len;
            rc = string_length(str, &len);
            if (rc == STATUS_OK) {
                printf("Длина строки: %zu\n", len);
                return 0;
            }
            break;
        }
        case 'r':
            rc = string_reverse(str, &result);
            break;
        case 'u':
            rc = string_upper_odd(str, &result);
            break;
        case 'n':
            rc = string_group_chars(str, &result);
            break;
        case 'c': {
            unsigned int seed;
            rc = parse_uint(argv[3], &seed);
            if (rc == STATUS_ERR_INVALID) {
                fprintf(stderr, "seed должен быть целым неотрицательным числом: '%s'\n", argv[3]);
                return 1;
            }
            if (rc == STATUS_ERR_OVERFLOW) {
                fprintf(stderr, "seed не помещается в unsigned int (максимум %u)\n", UINT_MAX);
                return 1;
            }
            if (rc != STATUS_OK)
                break;

            const size_t count = 1 + (size_t)(argc - 4);
            const char **strings = malloc(count * sizeof(const char *));
            if (!strings) {
                rc = STATUS_ERR_ALLOC;
                break;
            }
            strings[0] = argv[2];
            for (size_t i = 1; i < count; ++i)
                strings[i] = argv[3 + i];

            rc = string_concat_random(strings, count, seed, &result);
            free(strings);
            break;
        }
        default:
            rc = STATUS_ERR_INVALID;
            break;
    }

    switch (rc) {
        case STATUS_OK:
            printf("Результат: %s\n", result);
            free(result);
            return 0;
        case STATUS_ERR_ALLOC:
            fprintf(stderr, "Ошибка выделения памяти\n");
            break;
        case STATUS_ERR_OVERFLOW:
            fprintf(stderr, "Слишком длинные строки\n");
            break;
        case STATUS_ERR_NULL_ARG:
            fprintf(stderr, "Передан NULL\n");
            break;
        default:
            fprintf(stderr, "Ошибка обработки\n");
            break;
    }
    free(result);
    return 1;
}
