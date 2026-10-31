#include <ctype.h>
#include <limits.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef enum {
    STATUS_OK,
    STATUS_EOF,
    STATUS_MATCH_FAIL,
    STATUS_ERR_NULL_ARG,
    STATUS_ERR_MEMORY,
    STATUS_ERR_FORMAT
} status_t;

typedef enum {
    SPEC_NONE,
    SPEC_ROMAN,
    SPEC_ZECKENDORF,
    SPEC_BASE_LOWER,
    SPEC_BASE_UPPER
} custom_spec_t;

typedef enum {
    LEN_NONE,
    LEN_HH,
    LEN_H,
    LEN_L,
    LEN_LL,
    LEN_J,
    LEN_Z,
    LEN_T,
    LEN_BIG_L
} length_t;

typedef struct {
    FILE *stream;
    const char *str;
    size_t pos;
    size_t consumed;
} reader_t;

int reader_get(reader_t *r) {
    if (r == NULL) return EOF;

    int c = EOF;
    if (r->stream != NULL)
        c = fgetc(r->stream);
    else if (r->str != NULL && r->str[r->pos] != '\0')
        c = (unsigned char)r->str[r->pos++];

    if (c != EOF)
        ++r->consumed;
    return c;
}

void reader_unget(reader_t *r, const int c) {
    if (r == NULL || c == EOF) return;

    if (r->stream != NULL)
        ungetc(c, r->stream);
    else
        --r->pos;
    --r->consumed;
}

void reader_skip_spaces(reader_t *r) {
    if (r == NULL) return;

    int c = reader_get(r);
    while (c != EOF && isspace(c))
        c = reader_get(r);
    reader_unget(r, c);
}

status_t reader_match(reader_t *r, const char expected) {
    if (r == NULL) return STATUS_ERR_NULL_ARG;

    const int c = reader_get(r);
    if (c == EOF) return STATUS_EOF;

    if (c != (unsigned char)expected) {
        reader_unget(r, c);
        return STATUS_MATCH_FAIL;
    }
    return STATUS_OK;
}

int roman_digit_value(const int c) {
    switch (c) {
        case 'I': return 1;
        case 'V': return 5;
        case 'X': return 10;
        case 'L': return 50;
        case 'C': return 100;
        case 'D': return 500;
        case 'M': return 1000;
        default:  return 0;
    }
}

status_t scan_roman(reader_t *r, int *out) {
    if (r == NULL || out == NULL) return STATUS_ERR_NULL_ARG;

    reader_skip_spaces(r);
    int c = reader_get(r);
    if (c == EOF) return STATUS_EOF;

    const int negative = (c == '-');
    if (c == '-' || c == '+')
        c = reader_get(r);

    if (c == 'N') {
        *out = 0;
        return STATUS_OK;
    }

    long long total = 0;
    int prev = 0;
    int digits = 0;
    int value = roman_digit_value(c);

    while (value > 0) {
        total += (value > prev) ? value - 2 * prev : value;
        if (total > INT_MAX)
            total = INT_MAX;

        prev = value;
        ++digits;
        c = reader_get(r);
        value = roman_digit_value(c);
    }
    reader_unget(r, c);

    if (digits == 0) return STATUS_MATCH_FAIL;

    *out = negative ? -(int)total : (int)total;
    return STATUS_OK;
}

status_t scan_zeckendorf(reader_t *r, unsigned int *out) {
    if (r == NULL || out == NULL) return STATUS_ERR_NULL_ARG;

    reader_skip_spaces(r);
    int c = reader_get(r);
    if (c == EOF) return STATUS_EOF;

    unsigned int value = 0;
    unsigned int last_added = 0;
    unsigned int fib = 1;
    unsigned int next_fib = 2;
    int prev_one = 0;
    int digits = 0;
    int terminated = 0;

    while (c == '0' || c == '1') {
        ++digits;
        if (c == '1' && prev_one) {
            terminated = 1;
            break;
        }

        if (c == '1') {
            value += fib;
            last_added = fib;
        }
        prev_one = (c == '1');

        const unsigned int sum = fib + next_fib;
        fib = next_fib;
        next_fib = sum;
        c = reader_get(r);
    }

    if (!terminated)
        reader_unget(r, c);

    if (digits == 0) return STATUS_MATCH_FAIL;
    if (!terminated) {
        if (!prev_one) return STATUS_MATCH_FAIL;
        value -= last_added;
    }

    *out = value;
    return STATUS_OK;
}

unsigned int normalize_base(const int base) {
    return (base < 2 || base > 36) ? 10u : (unsigned int)base;
}

status_t char_to_digit(const int c, const int uppercase, unsigned int *out) {
    if (out == NULL) return STATUS_ERR_NULL_ARG;

    if (c >= '0' && c <= '9')
        *out = (unsigned int)(c - '0');
    else if (uppercase && c >= 'A' && c <= 'Z')
        *out = (unsigned int)(c - 'A') + 10u;
    else if (!uppercase && c >= 'a' && c <= 'z')
        *out = (unsigned int)(c - 'a') + 10u;
    else
        return STATUS_ERR_FORMAT;

    return STATUS_OK;
}

status_t scan_int_in_base(reader_t *r, const int base, const int uppercase, int *out) {
    if (r == NULL || out == NULL) return STATUS_ERR_NULL_ARG;

    const unsigned int radix = normalize_base(base);

    reader_skip_spaces(r);
    int c = reader_get(r);
    if (c == EOF) return STATUS_EOF;

    const int negative = (c == '-');
    if (c == '-' || c == '+')
        c = reader_get(r);

    const long long limit = negative ? -(long long)INT_MIN : (long long)INT_MAX;
    long long acc = 0;
    int digits = 0;
    unsigned int digit = 0;

    while (char_to_digit(c, uppercase, &digit) == STATUS_OK && digit < radix) {
        if (acc <= limit)
            acc = acc * (long long)radix + (long long)digit;
        ++digits;
        c = reader_get(r);
    }
    reader_unget(r, c);

    if (digits == 0) return STATUS_MATCH_FAIL;

    if (acc > limit)
        acc = limit;
    *out = negative ? (int)(-acc) : (int)acc;
    return STATUS_OK;
}

custom_spec_t detect_custom_spec(const char *p) {
    if (p == NULL || p[0] == '\0') return SPEC_NONE;

    const char first = p[0];
    const char second = p[1];

    if (first == 'R' && second == 'o') return SPEC_ROMAN;
    if (first == 'Z' && second == 'r') return SPEC_ZECKENDORF;
    if (first == 'C' && second == 'v') return SPEC_BASE_LOWER;
    if (first == 'C' && second == 'V') return SPEC_BASE_UPPER;

    return SPEC_NONE;
}

status_t scan_custom(reader_t *r, const custom_spec_t spec, va_list *args) {
    if (r == NULL || args == NULL) return STATUS_ERR_NULL_ARG;

    switch (spec) {
        case SPEC_ROMAN: {
            int *dst = va_arg(*args, int *);
            return scan_roman(r, dst);
        }
        case SPEC_ZECKENDORF: {
            unsigned int *dst = va_arg(*args, unsigned int *);
            return scan_zeckendorf(r, dst);
        }
        case SPEC_BASE_LOWER:
        case SPEC_BASE_UPPER: {
            int *dst = va_arg(*args, int *);
            const int base = va_arg(*args, int);
            return scan_int_in_base(r, base, spec == SPEC_BASE_UPPER, dst);
        }
        default:
            return STATUS_ERR_FORMAT;
    }
}

int is_digit(const char c) {
    return c >= '0' && c <= '9';
}

int is_conversion(const char c) {
    return c != '\0' && strchr("diouxXaAeEfFgGcsp[n", c) != NULL;
}

int is_length_allowed(const char conversion, const length_t length) {
    switch (conversion) {
        case 'c':
        case 's':
        case '[':
        case 'p':
            return length == LEN_NONE;
        case 'a':
        case 'A':
        case 'e':
        case 'E':
        case 'f':
        case 'F':
        case 'g':
        case 'G':
            return length == LEN_NONE || length == LEN_L || length == LEN_BIG_L;
        default:
            return length != LEN_BIG_L;
    }
}

status_t parse_length(const char **cursor, length_t *out) {
    if (cursor == NULL || *cursor == NULL || out == NULL) return STATUS_ERR_NULL_ARG;

    const char *p = *cursor;
    length_t length = LEN_NONE;

    if (p[0] == 'h' && p[1] == 'h') {
        length = LEN_HH;
        p += 2;
    } else if (p[0] == 'l' && p[1] == 'l') {
        length = LEN_LL;
        p += 2;
    } else if (p[0] == 'h') {
        length = LEN_H;
        ++p;
    } else if (p[0] == 'l') {
        length = LEN_L;
        ++p;
    } else if (p[0] == 'j') {
        length = LEN_J;
        ++p;
    } else if (p[0] == 'z') {
        length = LEN_Z;
        ++p;
    } else if (p[0] == 't') {
        length = LEN_T;
        ++p;
    } else if (p[0] == 'L') {
        length = LEN_BIG_L;
        ++p;
    }

    *cursor = p;
    *out = length;
    return STATUS_OK;
}

status_t skip_scanset(const char **cursor) {
    if (cursor == NULL || *cursor == NULL) return STATUS_ERR_NULL_ARG;

    const char *p = *cursor + 1;
    if (*p == '^')
        ++p;
    if (*p == ']')
        ++p;
    while (*p != '\0' && *p != ']')
        ++p;

    if (*p == '\0') return STATUS_ERR_FORMAT;

    *cursor = p;
    return STATUS_OK;
}

status_t parse_standard_spec(const char **cursor, int *suppressed, length_t *length, char *conversion) {
    if (cursor == NULL || *cursor == NULL || suppressed == NULL || length == NULL || conversion == NULL)
        return STATUS_ERR_NULL_ARG;

    const char *p = *cursor + 1;

    *suppressed = (*p == '*');
    if (*suppressed)
        ++p;

    while (is_digit(*p))
        ++p;

    status_t rc = parse_length(&p, length);
    if (rc != STATUS_OK) return rc;

    *conversion = *p;
    if (!is_conversion(*conversion)) return STATUS_ERR_FORMAT;
    if (!is_length_allowed(*conversion, *length)) return STATUS_ERR_FORMAT;

    if (*conversion == '[') {
        rc = skip_scanset(&p);
        if (rc != STATUS_OK) return rc;
    }

    *cursor = p + 1;
    return STATUS_OK;
}

status_t build_delegate_spec(const char *start, const char *end, char **out) {
    if (start == NULL || end == NULL || out == NULL) return STATUS_ERR_NULL_ARG;
    if (end <= start) return STATUS_ERR_FORMAT;

    const size_t len = (size_t)(end - start);
    char *spec = (char *)malloc(len + 3);
    if (spec == NULL) return STATUS_ERR_MEMORY;

    memcpy(spec, start, len);
    spec[len] = '%';
    spec[len + 1] = 'n';
    spec[len + 2] = '\0';

    *out = spec;
    return STATUS_OK;
}

int delegate_scan(reader_t *r, const char *spec, ...) {
    if (r == NULL || spec == NULL) return EOF;

    va_list args;
    va_start(args, spec);

    int rc = EOF;
    if (r->stream != NULL)
        rc = vfscanf(r->stream, spec, args);
    else if (r->str != NULL)
        rc = vsscanf(r->str + r->pos, spec, args);

    va_end(args);
    return rc;
}

status_t finish_delegate(reader_t *r, const int rc, const int consumed) {
    if (r == NULL) return STATUS_ERR_NULL_ARG;
    if (consumed < 0) return (rc == EOF) ? STATUS_EOF : STATUS_MATCH_FAIL;

    if (r->stream == NULL)
        r->pos += (size_t)consumed;
    r->consumed += (size_t)consumed;
    return STATUS_OK;
}

status_t scan_signed(reader_t *r, const char *spec, const length_t length, va_list *args) {
    if (r == NULL || spec == NULL || args == NULL) return STATUS_ERR_NULL_ARG;

    int consumed = -1;
    int rc = EOF;

    switch (length) {
        case LEN_NONE: {
            int *dst = va_arg(*args, int *);
            if (dst == NULL) return STATUS_ERR_NULL_ARG;
            rc = delegate_scan(r, spec, dst, &consumed);
            break;
        }
        case LEN_HH: {
            signed char *dst = va_arg(*args, signed char *);
            if (dst == NULL) return STATUS_ERR_NULL_ARG;
            rc = delegate_scan(r, spec, dst, &consumed);
            break;
        }
        case LEN_H: {
            short *dst = va_arg(*args, short *);
            if (dst == NULL) return STATUS_ERR_NULL_ARG;
            rc = delegate_scan(r, spec, dst, &consumed);
            break;
        }
        case LEN_L: {
            long *dst = va_arg(*args, long *);
            if (dst == NULL) return STATUS_ERR_NULL_ARG;
            rc = delegate_scan(r, spec, dst, &consumed);
            break;
        }
        case LEN_LL: {
            long long *dst = va_arg(*args, long long *);
            if (dst == NULL) return STATUS_ERR_NULL_ARG;
            rc = delegate_scan(r, spec, dst, &consumed);
            break;
        }
        case LEN_J: {
            intmax_t *dst = va_arg(*args, intmax_t *);
            if (dst == NULL) return STATUS_ERR_NULL_ARG;
            rc = delegate_scan(r, spec, dst, &consumed);
            break;
        }
        case LEN_Z: {
            size_t *dst = va_arg(*args, size_t *);
            if (dst == NULL) return STATUS_ERR_NULL_ARG;
            rc = delegate_scan(r, spec, dst, &consumed);
            break;
        }
        case LEN_T: {
            ptrdiff_t *dst = va_arg(*args, ptrdiff_t *);
            if (dst == NULL) return STATUS_ERR_NULL_ARG;
            rc = delegate_scan(r, spec, dst, &consumed);
            break;
        }
        default:
            return STATUS_ERR_FORMAT;
    }

    return finish_delegate(r, rc, consumed);
}

status_t scan_unsigned(reader_t *r, const char *spec, const length_t length, va_list *args) {
    if (r == NULL || spec == NULL || args == NULL) return STATUS_ERR_NULL_ARG;

    int consumed = -1;
    int rc = EOF;

    switch (length) {
        case LEN_NONE: {
            unsigned int *dst = va_arg(*args, unsigned int *);
            if (dst == NULL) return STATUS_ERR_NULL_ARG;
            rc = delegate_scan(r, spec, dst, &consumed);
            break;
        }
        case LEN_HH: {
            unsigned char *dst = va_arg(*args, unsigned char *);
            if (dst == NULL) return STATUS_ERR_NULL_ARG;
            rc = delegate_scan(r, spec, dst, &consumed);
            break;
        }
        case LEN_H: {
            unsigned short *dst = va_arg(*args, unsigned short *);
            if (dst == NULL) return STATUS_ERR_NULL_ARG;
            rc = delegate_scan(r, spec, dst, &consumed);
            break;
        }
        case LEN_L: {
            unsigned long *dst = va_arg(*args, unsigned long *);
            if (dst == NULL) return STATUS_ERR_NULL_ARG;
            rc = delegate_scan(r, spec, dst, &consumed);
            break;
        }
        case LEN_LL: {
            unsigned long long *dst = va_arg(*args, unsigned long long *);
            if (dst == NULL) return STATUS_ERR_NULL_ARG;
            rc = delegate_scan(r, spec, dst, &consumed);
            break;
        }
        case LEN_J: {
            uintmax_t *dst = va_arg(*args, uintmax_t *);
            if (dst == NULL) return STATUS_ERR_NULL_ARG;
            rc = delegate_scan(r, spec, dst, &consumed);
            break;
        }
        case LEN_Z: {
            size_t *dst = va_arg(*args, size_t *);
            if (dst == NULL) return STATUS_ERR_NULL_ARG;
            rc = delegate_scan(r, spec, dst, &consumed);
            break;
        }
        case LEN_T: {
            ptrdiff_t *dst = va_arg(*args, ptrdiff_t *);
            if (dst == NULL) return STATUS_ERR_NULL_ARG;
            rc = delegate_scan(r, spec, dst, &consumed);
            break;
        }
        default:
            return STATUS_ERR_FORMAT;
    }

    return finish_delegate(r, rc, consumed);
}

status_t scan_floating(reader_t *r, const char *spec, const length_t length, va_list *args) {
    if (r == NULL || spec == NULL || args == NULL) return STATUS_ERR_NULL_ARG;

    int consumed = -1;
    int rc = EOF;

    switch (length) {
        case LEN_NONE: {
            float *dst = va_arg(*args, float *);
            if (dst == NULL) return STATUS_ERR_NULL_ARG;
            rc = delegate_scan(r, spec, dst, &consumed);
            break;
        }
        case LEN_L: {
            double *dst = va_arg(*args, double *);
            if (dst == NULL) return STATUS_ERR_NULL_ARG;
            rc = delegate_scan(r, spec, dst, &consumed);
            break;
        }
        case LEN_BIG_L: {
            long double *dst = va_arg(*args, long double *);
            if (dst == NULL) return STATUS_ERR_NULL_ARG;
            rc = delegate_scan(r, spec, dst, &consumed);
            break;
        }
        default:
            return STATUS_ERR_FORMAT;
    }

    return finish_delegate(r, rc, consumed);
}

status_t scan_chars(reader_t *r, const char *spec, va_list *args) {
    if (r == NULL || spec == NULL || args == NULL) return STATUS_ERR_NULL_ARG;

    char *dst = va_arg(*args, char *);
    if (dst == NULL) return STATUS_ERR_NULL_ARG;

    int consumed = -1;
    const int rc = delegate_scan(r, spec, dst, &consumed);
    return finish_delegate(r, rc, consumed);
}

status_t scan_pointer(reader_t *r, const char *spec, va_list *args) {
    if (r == NULL || spec == NULL || args == NULL) return STATUS_ERR_NULL_ARG;

    void **dst = va_arg(*args, void **);
    if (dst == NULL) return STATUS_ERR_NULL_ARG;

    int consumed = -1;
    const int rc = delegate_scan(r, spec, dst, &consumed);
    return finish_delegate(r, rc, consumed);
}

status_t scan_suppressed(reader_t *r, const char *spec) {
    if (r == NULL || spec == NULL) return STATUS_ERR_NULL_ARG;

    int consumed = -1;
    const int rc = delegate_scan(r, spec, &consumed);
    return finish_delegate(r, rc, consumed);
}

status_t store_count(const size_t count, const length_t length, va_list *args) {
    if (args == NULL) return STATUS_ERR_NULL_ARG;

    switch (length) {
        case LEN_NONE: {
            int *dst = va_arg(*args, int *);
            if (dst == NULL) return STATUS_ERR_NULL_ARG;
            *dst = (int)count;
            return STATUS_OK;
        }
        case LEN_HH: {
            signed char *dst = va_arg(*args, signed char *);
            if (dst == NULL) return STATUS_ERR_NULL_ARG;
            *dst = (signed char)count;
            return STATUS_OK;
        }
        case LEN_H: {
            short *dst = va_arg(*args, short *);
            if (dst == NULL) return STATUS_ERR_NULL_ARG;
            *dst = (short)count;
            return STATUS_OK;
        }
        case LEN_L: {
            long *dst = va_arg(*args, long *);
            if (dst == NULL) return STATUS_ERR_NULL_ARG;
            *dst = (long)count;
            return STATUS_OK;
        }
        case LEN_LL: {
            long long *dst = va_arg(*args, long long *);
            if (dst == NULL) return STATUS_ERR_NULL_ARG;
            *dst = (long long)count;
            return STATUS_OK;
        }
        case LEN_J: {
            intmax_t *dst = va_arg(*args, intmax_t *);
            if (dst == NULL) return STATUS_ERR_NULL_ARG;
            *dst = (intmax_t)count;
            return STATUS_OK;
        }
        case LEN_Z: {
            size_t *dst = va_arg(*args, size_t *);
            if (dst == NULL) return STATUS_ERR_NULL_ARG;
            *dst = count;
            return STATUS_OK;
        }
        case LEN_T: {
            ptrdiff_t *dst = va_arg(*args, ptrdiff_t *);
            if (dst == NULL) return STATUS_ERR_NULL_ARG;
            *dst = (ptrdiff_t)count;
            return STATUS_OK;
        }
        default:
            return STATUS_ERR_FORMAT;
    }
}

status_t scan_value(reader_t *r, const char *spec, const char conversion, const length_t length, va_list *args) {
    if (r == NULL || spec == NULL || args == NULL) return STATUS_ERR_NULL_ARG;

    switch (conversion) {
        case 'd':
        case 'i':
            return scan_signed(r, spec, length, args);
        case 'o':
        case 'u':
        case 'x':
        case 'X':
            return scan_unsigned(r, spec, length, args);
        case 'a':
        case 'A':
        case 'e':
        case 'E':
        case 'f':
        case 'F':
        case 'g':
        case 'G':
            return scan_floating(r, spec, length, args);
        case 'c':
        case 's':
        case '[':
            return scan_chars(r, spec, args);
        case 'p':
            return scan_pointer(r, spec, args);
        default:
            return STATUS_ERR_FORMAT;
    }
}

status_t scan_standard(reader_t *r, const char **cursor, va_list *args, int *stored, int *converted) {
    if (r == NULL || cursor == NULL || *cursor == NULL || args == NULL || stored == NULL || converted == NULL)
        return STATUS_ERR_NULL_ARG;

    const char *start = *cursor;
    int suppressed = 0;
    length_t length = LEN_NONE;
    char conversion = '\0';

    *stored = 0;
    *converted = 0;

    status_t rc = parse_standard_spec(cursor, &suppressed, &length, &conversion);
    if (rc != STATUS_OK) return rc;

    if (conversion == 'n') {
        if (suppressed) return STATUS_ERR_FORMAT;
        return store_count(r->consumed, length, args);
    }

    char *spec = NULL;
    rc = build_delegate_spec(start, *cursor, &spec);
    if (rc != STATUS_OK) return rc;

    rc = suppressed ? scan_suppressed(r, spec) : scan_value(r, spec, conversion, length, args);
    free(spec);

    if (rc == STATUS_OK) {
        *stored = !suppressed;
        *converted = 1;
    }
    return rc;
}

status_t over_vscan(reader_t *r, const char *format, va_list *args, int *assigned, int *converted) {
    if (r == NULL || format == NULL || args == NULL || assigned == NULL || converted == NULL)
        return STATUS_ERR_NULL_ARG;

    *assigned = 0;
    *converted = 0;
    const char *p = format;
    status_t rc = STATUS_OK;

    while (*p != '\0' && rc == STATUS_OK) {
        if (isspace((unsigned char)*p)) {
            reader_skip_spaces(r);
            ++p;
        } else if (*p != '%') {
            rc = reader_match(r, *p);
            ++p;
        } else if (p[1] == '%') {
            reader_skip_spaces(r);
            rc = reader_match(r, '%');
            p += 2;
        } else {
            const custom_spec_t custom = detect_custom_spec(p + 1);
            int stored = 0;
            int done = 0;

            if (custom != SPEC_NONE) {
                rc = scan_custom(r, custom, args);
                stored = done = (rc == STATUS_OK);
                p += 3;
            } else {
                rc = scan_standard(r, &p, args, &stored, &done);
            }

            *assigned += stored;
            *converted += done;
        }
    }

    return rc;
}

int scan_result(const status_t rc, const int assigned, const int converted) {
    switch (rc) {
        case STATUS_OK:
        case STATUS_MATCH_FAIL:
            return assigned;
        case STATUS_EOF:
            return (converted == 0) ? EOF : assigned;
        default:
            return -(int)rc;
    }
}

int overfscanf(FILE *stream, const char *format, ...) {
    if (stream == NULL || format == NULL) return -(int)STATUS_ERR_NULL_ARG;

    reader_t reader = {stream, NULL, 0, 0};
    int assigned = 0;
    int converted = 0;

    va_list args;
    va_start(args, format);
    const status_t rc = over_vscan(&reader, format, &args, &assigned, &converted);
    va_end(args);

    return scan_result(rc, assigned, converted);
}

int oversscanf(const char *str, const char *format, ...) {
    if (str == NULL || format == NULL) return -(int)STATUS_ERR_NULL_ARG;

    reader_t reader = {NULL, str, 0, 0};
    int assigned = 0;
    int converted = 0;

    va_list args;
    va_start(args, format);
    const status_t rc = over_vscan(&reader, format, &args, &assigned, &converted);
    va_end(args);

    return scan_result(rc, assigned, converted);
}

void print_status(const status_t rc) {
    switch (rc) {
        case STATUS_OK:
            printf("успешно\n");
            break;
        case STATUS_EOF:
            printf("достигнут конец ввода\n");
            break;
        case STATUS_MATCH_FAIL:
            printf("входные данные не соответствуют формату\n");
            break;
        case STATUS_ERR_NULL_ARG:
            printf("ошибка: передан нулевой указатель\n");
            break;
        case STATUS_ERR_MEMORY:
            printf("ошибка: не удалось выделить память\n");
            break;
        case STATUS_ERR_FORMAT:
            printf("ошибка: некорректная строка формата\n");
            break;
        default:
            printf("неизвестная ошибка\n");
            break;
    }
}

void print_scan_result(const int rc) {
    if (rc == EOF)
        printf("EOF (конец ввода до первого преобразования)\n");
    else if (rc < 0)
        print_status((status_t)(-rc));
    else
        printf("считано значений: %d\n", rc);
}

typedef struct {
    int rc;
    int d;
    int hex;
    int oct;
    double f;
    char word[16];
    char rest[16];
    char set[16];
    char c;
    unsigned char hhu;
    long long ll;
    int n;
} sample_t;

int samples_equal(const sample_t *a, const sample_t *b, const double eps) {
    if (a == NULL || b == NULL) return 0;

    const double diff = (a->f > b->f) ? a->f - b->f : b->f - a->f;
    return a->rc == b->rc && a->d == b->d && a->hex == b->hex && a->oct == b->oct && diff < eps
        && strcmp(a->word, b->word) == 0 && strcmp(a->rest, b->rest) == 0 && strcmp(a->set, b->set) == 0
        && a->c == b->c && a->hhu == b->hhu && a->ll == b->ll && a->n == b->n;
}

void print_sample(const char *name, const sample_t *s) {
    if (name == NULL || s == NULL) return;

    printf("%-10s: rc=%d d=%d hex=%d oct=%d f=%g word=\"%s\" rest=\"%s\" set=\"%s\" c='%c' hhu=%u ll=%lld n=%d\n",
           name, s->rc, s->d, s->hex, s->oct, s->f, s->word, s->rest, s->set, s->c, (unsigned int)s->hhu, s->ll, s->n);
}

void demo_standard(void) {
    printf("=== Стандартные спецификаторы: sscanf и oversscanf ===\n");

    const char *input = "  -42 0x1F 017 3.5e2 helloworld skipped abc:def Z 255 -9000000000 tail";
    sample_t expected = {0};
    sample_t actual = {0};

    printf("Вход: \"%s\"\n", input);
    expected.rc = sscanf(input, "%d %i %i %lf %5s%15s %*s %15[a-z:] %c %hhu %lld%n",
                         &expected.d, &expected.hex, &expected.oct, &expected.f, expected.word, expected.rest,
                         expected.set, &expected.c, &expected.hhu, &expected.ll, &expected.n);
    actual.rc = oversscanf(input, "%d %i %i %lf %5s%15s %*s %15[a-z:] %c %hhu %lld%n",
                           &actual.d, &actual.hex, &actual.oct, &actual.f, actual.word, actual.rest,
                           actual.set, &actual.c, &actual.hhu, &actual.ll, &actual.n);

    print_sample("sscanf", &expected);
    print_sample("oversscanf", &actual);
    printf("Результаты %s\n\n", samples_equal(&expected, &actual, 1e-12) ? "совпадают" : "НЕ совпадают");
}

void demo_return_values(void) {
    printf("=== Возвращаемое значение: sscanf и oversscanf ===\n");

    const char *const inputs[] = {"", "   ", "12 abc", "abc", "7 8", "x5", "y5", "-"};
    const char *const formats[] = {"%d", " %d", "%d %d", "%d", "%*d %d", "y%d", "y%d", "%d"};
    const size_t count = sizeof(inputs) / sizeof(inputs[0]);

    for (size_t i = 0; i < count; i++) {
        int a1 = 0, b1 = 0, a2 = 0, b2 = 0;
        const int expected = sscanf(inputs[i], formats[i], &a1, &b1);
        const int actual = oversscanf(inputs[i], formats[i], &a2, &b2);
        printf("вход \"%s\", формат \"%s\": sscanf = %d, oversscanf = %d\n", inputs[i], formats[i], expected, actual);
    }
    printf("\n");
}

void demo_roman(void) {
    printf("=== %%Ro: римская запись -> int ===\n");

    const char *input = "MCMXCIV IV -LVIII N MMMCMXCIX XLII";
    int v[6] = {0};
    const int rc = oversscanf(input, "%Ro %Ro %Ro %Ro %Ro %Ro", &v[0], &v[1], &v[2], &v[3], &v[4], &v[5]);

    printf("Вход: \"%s\"\n", input);
    print_scan_result(rc);
    for (int i = 0; i < rc && i < 6; i++)
        printf("  %d\n", v[i]);
    printf("\n");
}

void demo_zeckendorf(void) {
    printf("=== %%Zr: цекендорфово представление -> unsigned int ===\n");

    const char *input = "11 011 1011 010011 01 00101000011";
    unsigned int v[6] = {0};
    int rc = oversscanf(input, "%Zr %Zr %Zr %Zr %Zr %Zr", &v[0], &v[1], &v[2], &v[3], &v[4], &v[5]);

    printf("Вход: \"%s\"\n", input);
    print_scan_result(rc);
    for (int i = 0; i < rc && i < 6; i++)
        printf("  %u\n", v[i]);

    const char *joined = "01111011";
    rc = oversscanf(joined, "%Zr%Zr%Zr", &v[0], &v[1], &v[2]);
    printf("Записи без разделителей \"%s\" -> ", joined);
    if (rc == 3)
        printf("%u, %u, %u\n\n", v[0], v[1], v[2]);
    else
        print_scan_result(rc);
}

void demo_base(void) {
    printf("=== %%Cv / %%CV: число в системе счисления с основанием [2..36] -> int ===\n");

    const char *input = "ff -11111111 zik0zj 7FFFFFFF 123";
    int v[5] = {0};
    int rc = oversscanf(input, "%Cv %Cv %Cv %CV %Cv", &v[0], 16, &v[1], 2, &v[2], 36, &v[3], 16, &v[4], 1);

    printf("Вход: \"%s\", основания 16, 2, 36, 16 (%%CV), 1 (-> 10)\n", input);
    print_scan_result(rc);
    for (int i = 0; i < rc && i < 5; i++)
        printf("  %d\n", v[i]);

    int x = 0;
    rc = oversscanf("FF", "%Cv", &x, 16);
    printf("%%Cv на \"FF\" (прописные буквы) -> ");
    print_scan_result(rc);

    rc = oversscanf("ff", "%CV", &x, 16);
    printf("%%CV на \"ff\" (строчные буквы)  -> ");
    print_scan_result(rc);
    printf("\n");
}

void demo_mixed(void) {
    printf("=== Смешанная строка формата ===\n");

    const char *input = "Год MMXXVI, код 0x2A, двоичное 101010, Цекендорф 010011, остаток 99%";
    int year = 0;
    unsigned int code = 0;
    int bin = 0;
    unsigned int zeck = 0;
    int rest = 0;

    const int rc = oversscanf(input, "Год %Ro, код %x, двоичное %Cv, Цекендорф %Zr, остаток %d%%",
                              &year, &code, &bin, 2, &zeck, &rest);

    printf("Вход: \"%s\"\n", input);
    print_scan_result(rc);
    printf("  год = %d, код = %u, двоичное = %d, Цекендорф = %u, остаток = %d\n\n", year, code, bin, zeck, rest);
}

void demo_file(FILE *file) {
    if (file == NULL) return;

    printf("=== overfscanf: чтение записей из файла (\"%%63s %%Ro %%Zr %%Cv\", основание 16) ===\n");

    char name[64];
    int roman = 0;
    unsigned int zeck = 0;
    int hex = 0;
    int records = 0;
    int rc = 0;

    while ((rc = overfscanf(file, "%63s %Ro %Zr %Cv", name, &roman, &zeck, &hex, 16)) == 4) {
        ++records;
        printf("  %-8s римское = %5d, Цекендорф = %5u, hex = %d\n", name, roman, zeck, hex);
    }

    if (rc == EOF) {
        printf("Файл прочитан полностью, записей: %d\n", records);
    } else {
        printf("Чтение прервано на записи %d: ", records + 1);
        print_scan_result(rc);
    }
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Формат запуска программы: ./[программа] [входной файл]\n");
        return 1;
    }

    FILE *file = fopen(argv[1], "r");
    if (file == NULL) {
        fprintf(stderr, "Ошибка: не удалось открыть файл '%s'\n", argv[1]);
        return 1;
    }

    demo_standard();
    demo_return_values();
    demo_roman();
    demo_zeckendorf();
    demo_base();
    demo_mixed();
    demo_file(file);

    fclose(file);
    return 0;
}
