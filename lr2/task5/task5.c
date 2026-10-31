#include <limits.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef enum {
    STATUS_OK,
    STATUS_ERR_NULL_ARG,
    STATUS_ERR_MEMORY,
    STATUS_ERR_FORMAT,
    STATUS_ERR_RANGE,
    STATUS_ERR_INVALID,
    STATUS_ERR_IO
} status_t;

typedef enum {
    SPEC_NONE,
    SPEC_ROMAN,
    SPEC_ZECKENDORF,
    SPEC_BASE_LOWER,
    SPEC_BASE_UPPER,
    SPEC_TO_DEC_LOWER,
    SPEC_TO_DEC_UPPER,
    SPEC_DUMP_INT,
    SPEC_DUMP_UINT,
    SPEC_DUMP_DOUBLE,
    SPEC_DUMP_FLOAT
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
    char *data;
    size_t len;
    size_t cap;
} buffer_t;

typedef struct {
    char data[64];
    size_t len;
} spec_t;

status_t buffer_init(buffer_t *buf) {
    if (buf == NULL) return STATUS_ERR_NULL_ARG;

    buf->len = 0;
    buf->cap = 16;
    buf->data = (char *)malloc(buf->cap);
    if (buf->data == NULL) return STATUS_ERR_MEMORY;

    buf->data[0] = '\0';
    return STATUS_OK;
}

void buffer_free(buffer_t *buf) {
    if (buf == NULL) return;

    free(buf->data);
    buf->data = NULL;
    buf->len = 0;
    buf->cap = 0;
}

status_t buffer_reserve(buffer_t *buf, const size_t extra) {
    if (buf == NULL || buf->data == NULL) return STATUS_ERR_NULL_ARG;
    if (extra >= SIZE_MAX - buf->len) return STATUS_ERR_RANGE;

    const size_t need = buf->len + extra + 1;
    if (need <= buf->cap) return STATUS_OK;

    size_t new_cap = buf->cap;
    while (new_cap < need)
        new_cap = (new_cap > SIZE_MAX / 2) ? need : new_cap * 2;

    char *tmp = (char *)realloc(buf->data, new_cap);
    if (tmp == NULL) return STATUS_ERR_MEMORY;

    buf->data = tmp;
    buf->cap = new_cap;
    return STATUS_OK;
}

status_t buffer_append_char(buffer_t *buf, const char c) {
    status_t rc = buffer_reserve(buf, 1);
    if (rc != STATUS_OK) return rc;

    buf->data[buf->len++] = c;
    buf->data[buf->len] = '\0';
    return STATUS_OK;
}

status_t buffer_append_str(buffer_t *buf, const char *str) {
    if (buf == NULL || str == NULL) return STATUS_ERR_NULL_ARG;

    status_t rc = STATUS_OK;
    for (size_t i = 0; str[i] != '\0' && rc == STATUS_OK; i++)
        rc = buffer_append_char(buf, str[i]);

    return rc;
}

status_t buffer_append_formatted(buffer_t *buf, const char *spec, ...) {
    if (buf == NULL || spec == NULL) return STATUS_ERR_NULL_ARG;

    va_list args;
    va_list copy;
    va_start(args, spec);
    va_copy(copy, args);
    const int needed = vsnprintf(NULL, 0, spec, copy);
    va_end(copy);

    status_t rc = (needed < 0) ? STATUS_ERR_FORMAT : buffer_reserve(buf, (size_t)needed);
    if (rc == STATUS_OK) {
        vsnprintf(buf->data + buf->len, (size_t)needed + 1, spec, args);
        buf->len += (size_t)needed;
    }

    va_end(args);
    return rc;
}

status_t append_roman(buffer_t *buf, const int value) {
    if (buf == NULL) return STATUS_ERR_NULL_ARG;
    if (value < -3999 || value > 3999) return STATUS_ERR_RANGE;
    if (value == 0) return buffer_append_char(buf, 'N');

    const int values[] = {1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1};
    const char *const symbols[] = {"M", "CM", "D", "CD", "C", "XC", "L", "XL", "X", "IX", "V", "IV", "I"};
    const size_t count = sizeof(values) / sizeof(values[0]);

    status_t rc = (value < 0) ? buffer_append_char(buf, '-') : STATUS_OK;
    int rest = (value < 0) ? -value : value;

    for (size_t i = 0; i < count && rc == STATUS_OK; i++) {
        while (rest >= values[i] && rc == STATUS_OK) {
            rc = buffer_append_str(buf, symbols[i]);
            rest -= values[i];
        }
    }

    return rc;
}

status_t append_zeckendorf(buffer_t *buf, const unsigned int value) {
    if (buf == NULL) return STATUS_ERR_NULL_ARG;

    unsigned int fib[sizeof(unsigned int) * CHAR_BIT * 2];
    char digits[sizeof(unsigned int) * CHAR_BIT * 2];
    size_t count = 0;

    unsigned int prev = 1;
    unsigned int cur = 2;
    fib[count++] = prev;

    while (cur <= value) {
        fib[count++] = cur;
        if (prev > UINT_MAX - cur)
            break;

        const unsigned int next = prev + cur;
        prev = cur;
        cur = next;
    }

    unsigned int rest = value;
    for (size_t i = count; i > 0; i--) {
        if (fib[i - 1] <= rest) {
            digits[i - 1] = '1';
            rest -= fib[i - 1];
        } else {
            digits[i - 1] = '0';
        }
    }

    status_t rc = STATUS_OK;
    for (size_t i = 0; i < count && rc == STATUS_OK; i++)
        rc = buffer_append_char(buf, digits[i]);

    if (rc == STATUS_OK)
        rc = buffer_append_char(buf, '1');

    return rc;
}

unsigned int normalize_base(const int base) {
    return (base < 2 || base > 36) ? 10u : (unsigned int)base;
}

char digit_to_char(const unsigned int digit, const int uppercase) {
    if (digit < 10)
        return (char)('0' + digit);
    return (char)((uppercase ? 'A' : 'a') + (digit - 10));
}

status_t char_to_digit(const char c, const int uppercase, unsigned int *out) {
    if (out == NULL) return STATUS_ERR_NULL_ARG;

    if (c >= '0' && c <= '9')
        *out = (unsigned int)(c - '0');
    else if (uppercase && c >= 'A' && c <= 'Z')
        *out = (unsigned int)(c - 'A') + 10u;
    else if (!uppercase && c >= 'a' && c <= 'z')
        *out = (unsigned int)(c - 'a') + 10u;
    else
        return STATUS_ERR_INVALID;

    return STATUS_OK;
}

status_t append_int_in_base(buffer_t *buf, const int value, const int base, const int uppercase) {
    if (buf == NULL) return STATUS_ERR_NULL_ARG;

    const unsigned int radix = normalize_base(base);
    unsigned int rest = (value < 0) ? 0u - (unsigned int)value : (unsigned int)value;
    char digits[sizeof(int) * CHAR_BIT];
    size_t count = 0;

    do {
        digits[count++] = digit_to_char(rest % radix, uppercase);
        rest /= radix;
    } while (rest != 0);

    status_t rc = (value < 0) ? buffer_append_char(buf, '-') : STATUS_OK;
    while (count > 0 && rc == STATUS_OK)
        rc = buffer_append_char(buf, digits[--count]);

    return rc;
}

status_t parse_int_in_base(const char *str, const int base, const int uppercase, int *out) {
    if (str == NULL || out == NULL) return STATUS_ERR_NULL_ARG;

    const unsigned int radix = normalize_base(base);
    const char *p = str;
    const int negative = (*p == '-');

    if (*p == '-' || *p == '+')
        ++p;
    if (*p == '\0')
        return STATUS_ERR_INVALID;

    const long long limit = negative ? -(long long)INT_MIN : (long long)INT_MAX;
    long long acc = 0;

    for (; *p != '\0'; ++p) {
        unsigned int digit = 0;
        const status_t rc = char_to_digit(*p, uppercase, &digit);
        if (rc != STATUS_OK) return rc;
        if (digit >= radix) return STATUS_ERR_INVALID;

        acc = acc * (long long)radix + (long long)digit;
        if (acc > limit) return STATUS_ERR_RANGE;
    }

    *out = negative ? (int)(-acc) : (int)acc;
    return STATUS_OK;
}

status_t append_memory_dump(buffer_t *buf, const void *data, const size_t size) {
    if (buf == NULL || data == NULL) return STATUS_ERR_NULL_ARG;

    const unsigned char *bytes = (const unsigned char *)data;
    status_t rc = STATUS_OK;

    for (size_t i = 0; i < size && rc == STATUS_OK; i++) {
        if (i > 0)
            rc = buffer_append_char(buf, ' ');

        for (int bit = CHAR_BIT - 1; bit >= 0 && rc == STATUS_OK; bit--)
            rc = buffer_append_char(buf, ((bytes[i] >> bit) & 1u) ? '1' : '0');
    }

    return rc;
}

custom_spec_t detect_custom_spec(const char *p) {
    if (p == NULL || p[0] == '\0') return SPEC_NONE;

    const char first = p[0];
    const char second = p[1];

    if (first == 'R' && second == 'o') return SPEC_ROMAN;
    if (first == 'Z' && second == 'r') return SPEC_ZECKENDORF;
    if (first == 'C' && second == 'v') return SPEC_BASE_LOWER;
    if (first == 'C' && second == 'V') return SPEC_BASE_UPPER;
    if (first == 't' && second == 'o') return SPEC_TO_DEC_LOWER;
    if (first == 'T' && second == 'O') return SPEC_TO_DEC_UPPER;
    if (first == 'm' && second == 'i') return SPEC_DUMP_INT;
    if (first == 'm' && second == 'u') return SPEC_DUMP_UINT;
    if (first == 'm' && second == 'd') return SPEC_DUMP_DOUBLE;
    if (first == 'm' && second == 'f') return SPEC_DUMP_FLOAT;

    return SPEC_NONE;
}

status_t format_custom(buffer_t *buf, const custom_spec_t spec, va_list *args) {
    if (buf == NULL || args == NULL) return STATUS_ERR_NULL_ARG;

    switch (spec) {
        case SPEC_ROMAN: {
            const int value = va_arg(*args, int);
            return append_roman(buf, value);
        }
        case SPEC_ZECKENDORF: {
            const unsigned int value = va_arg(*args, unsigned int);
            return append_zeckendorf(buf, value);
        }
        case SPEC_BASE_LOWER:
        case SPEC_BASE_UPPER: {
            const int value = va_arg(*args, int);
            const int base = va_arg(*args, int);
            return append_int_in_base(buf, value, base, spec == SPEC_BASE_UPPER);
        }
        case SPEC_TO_DEC_LOWER:
        case SPEC_TO_DEC_UPPER: {
            const char *str = va_arg(*args, char *);
            const int base = va_arg(*args, int);
            int value = 0;
            const status_t rc = parse_int_in_base(str, base, spec == SPEC_TO_DEC_UPPER, &value);
            if (rc != STATUS_OK) return rc;
            return buffer_append_formatted(buf, "%d", value);
        }
        case SPEC_DUMP_INT: {
            const int32_t value = (int32_t)va_arg(*args, int);
            return append_memory_dump(buf, &value, sizeof(value));
        }
        case SPEC_DUMP_UINT: {
            const uint32_t value = (uint32_t)va_arg(*args, unsigned int);
            return append_memory_dump(buf, &value, sizeof(value));
        }
        case SPEC_DUMP_DOUBLE: {
            const double value = va_arg(*args, double);
            return append_memory_dump(buf, &value, sizeof(value));
        }
        case SPEC_DUMP_FLOAT: {
            const float value = (float)va_arg(*args, double);
            return append_memory_dump(buf, &value, sizeof(value));
        }
        default:
            return STATUS_ERR_FORMAT;
    }
}

int is_flag(const char c) {
    return c == '-' || c == '+' || c == ' ' || c == '#' || c == '0';
}

int is_digit(const char c) {
    return c >= '0' && c <= '9';
}

status_t spec_push(spec_t *spec, const char c) {
    if (spec == NULL) return STATUS_ERR_NULL_ARG;
    if (spec->len + 1 >= sizeof(spec->data)) return STATUS_ERR_FORMAT;

    spec->data[spec->len++] = c;
    spec->data[spec->len] = '\0';
    return STATUS_OK;
}

status_t spec_push_int(spec_t *spec, const int value) {
    if (spec == NULL) return STATUS_ERR_NULL_ARG;

    char digits[32];
    const int n = snprintf(digits, sizeof(digits), "%d", value);
    if (n < 0) return STATUS_ERR_FORMAT;

    status_t rc = STATUS_OK;
    for (int i = 0; i < n && rc == STATUS_OK; i++)
        rc = spec_push(spec, digits[i]);

    return rc;
}

status_t spec_push_digits(const char **cursor, spec_t *spec) {
    if (cursor == NULL || *cursor == NULL || spec == NULL) return STATUS_ERR_NULL_ARG;

    const char *p = *cursor;
    status_t rc = STATUS_OK;

    while (is_digit(*p) && rc == STATUS_OK) {
        rc = spec_push(spec, *p);
        ++p;
    }

    *cursor = p;
    return rc;
}

status_t parse_flags(const char **cursor, spec_t *spec) {
    if (cursor == NULL || *cursor == NULL || spec == NULL) return STATUS_ERR_NULL_ARG;

    const char *p = *cursor;
    status_t rc = STATUS_OK;

    while (is_flag(*p) && rc == STATUS_OK) {
        rc = spec_push(spec, *p);
        ++p;
    }

    *cursor = p;
    return rc;
}

status_t parse_width(const char **cursor, spec_t *spec, va_list *args) {
    if (cursor == NULL || *cursor == NULL || spec == NULL || args == NULL) return STATUS_ERR_NULL_ARG;

    if (**cursor != '*')
        return spec_push_digits(cursor, spec);

    ++(*cursor);
    const int width = va_arg(*args, int);
    return spec_push_int(spec, width);
}

status_t parse_precision(const char **cursor, spec_t *spec, va_list *args) {
    if (cursor == NULL || *cursor == NULL || spec == NULL || args == NULL) return STATUS_ERR_NULL_ARG;
    if (**cursor != '.') return STATUS_OK;

    ++(*cursor);

    if (**cursor != '*') {
        const status_t rc = spec_push(spec, '.');
        if (rc != STATUS_OK) return rc;
        return spec_push_digits(cursor, spec);
    }

    ++(*cursor);
    const int precision = va_arg(*args, int);
    if (precision < 0)
        return STATUS_OK;

    const status_t rc = spec_push(spec, '.');
    if (rc != STATUS_OK) return rc;
    return spec_push_int(spec, precision);
}

status_t parse_length(const char **cursor, spec_t *spec, length_t *out) {
    if (cursor == NULL || *cursor == NULL || spec == NULL || out == NULL) return STATUS_ERR_NULL_ARG;

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

    status_t rc = STATUS_OK;
    for (const char *c = *cursor; c < p && rc == STATUS_OK; c++)
        rc = spec_push(spec, *c);

    *cursor = p;
    *out = length;
    return rc;
}

status_t append_signed(buffer_t *buf, const char *spec, const length_t length, va_list *args) {
    if (buf == NULL || spec == NULL || args == NULL) return STATUS_ERR_NULL_ARG;

    switch (length) {
        case LEN_NONE:
        case LEN_HH:
        case LEN_H:
            return buffer_append_formatted(buf, spec, va_arg(*args, int));
        case LEN_L:
            return buffer_append_formatted(buf, spec, va_arg(*args, long));
        case LEN_LL:
            return buffer_append_formatted(buf, spec, va_arg(*args, long long));
        case LEN_J:
            return buffer_append_formatted(buf, spec, va_arg(*args, intmax_t));
        case LEN_Z:
            return buffer_append_formatted(buf, spec, va_arg(*args, size_t));
        case LEN_T:
            return buffer_append_formatted(buf, spec, va_arg(*args, ptrdiff_t));
        default:
            return STATUS_ERR_FORMAT;
    }
}

status_t append_unsigned(buffer_t *buf, const char *spec, const length_t length, va_list *args) {
    if (buf == NULL || spec == NULL || args == NULL) return STATUS_ERR_NULL_ARG;

    switch (length) {
        case LEN_NONE:
        case LEN_HH:
        case LEN_H:
            return buffer_append_formatted(buf, spec, va_arg(*args, unsigned int));
        case LEN_L:
            return buffer_append_formatted(buf, spec, va_arg(*args, unsigned long));
        case LEN_LL:
            return buffer_append_formatted(buf, spec, va_arg(*args, unsigned long long));
        case LEN_J:
            return buffer_append_formatted(buf, spec, va_arg(*args, uintmax_t));
        case LEN_Z:
            return buffer_append_formatted(buf, spec, va_arg(*args, size_t));
        case LEN_T:
            return buffer_append_formatted(buf, spec, va_arg(*args, ptrdiff_t));
        default:
            return STATUS_ERR_FORMAT;
    }
}

status_t append_floating(buffer_t *buf, const char *spec, const length_t length, va_list *args) {
    if (buf == NULL || spec == NULL || args == NULL) return STATUS_ERR_NULL_ARG;

    switch (length) {
        case LEN_NONE:
        case LEN_L:
            return buffer_append_formatted(buf, spec, va_arg(*args, double));
        case LEN_BIG_L:
            return buffer_append_formatted(buf, spec, va_arg(*args, long double));
        default:
            return STATUS_ERR_FORMAT;
    }
}

status_t append_string(buffer_t *buf, const char *spec, const length_t length, va_list *args) {
    if (buf == NULL || spec == NULL || args == NULL) return STATUS_ERR_NULL_ARG;
    if (length != LEN_NONE) return STATUS_ERR_FORMAT;

    const char *str = va_arg(*args, char *);
    if (str == NULL) return STATUS_ERR_NULL_ARG;

    return buffer_append_formatted(buf, spec, str);
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

status_t format_standard_value(buffer_t *buf, const char *spec, const char conversion,
                               const length_t length, va_list *args) {
    if (buf == NULL || spec == NULL || args == NULL) return STATUS_ERR_NULL_ARG;

    switch (conversion) {
        case 'd':
        case 'i':
            return append_signed(buf, spec, length, args);
        case 'o':
        case 'u':
        case 'x':
        case 'X':
            return append_unsigned(buf, spec, length, args);
        case 'f':
        case 'F':
        case 'e':
        case 'E':
        case 'g':
        case 'G':
        case 'a':
        case 'A':
            return append_floating(buf, spec, length, args);
        case 'c':
            if (length != LEN_NONE) return STATUS_ERR_FORMAT;
            return buffer_append_formatted(buf, spec, va_arg(*args, int));
        case 's':
            return append_string(buf, spec, length, args);
        case 'p':
            if (length != LEN_NONE) return STATUS_ERR_FORMAT;
            return buffer_append_formatted(buf, spec, va_arg(*args, void *));
        case 'n':
            return store_count(buf->len, length, args);
        default:
            return STATUS_ERR_FORMAT;
    }
}

status_t format_standard(buffer_t *buf, const char **cursor, va_list *args) {
    if (buf == NULL || cursor == NULL || *cursor == NULL || args == NULL) return STATUS_ERR_NULL_ARG;

    spec_t spec = {{0}, 0};
    length_t length = LEN_NONE;

    status_t rc = spec_push(&spec, '%');
    if (rc == STATUS_OK) rc = parse_flags(cursor, &spec);
    if (rc == STATUS_OK) rc = parse_width(cursor, &spec, args);
    if (rc == STATUS_OK) rc = parse_precision(cursor, &spec, args);
    if (rc == STATUS_OK) rc = parse_length(cursor, &spec, &length);
    if (rc != STATUS_OK) return rc;

    const char conversion = **cursor;
    if (conversion == '\0') return STATUS_ERR_FORMAT;

    rc = spec_push(&spec, conversion);
    if (rc != STATUS_OK) return rc;
    ++(*cursor);

    return format_standard_value(buf, spec.data, conversion, length, args);
}

status_t over_vformat(buffer_t *buf, const char *format, va_list *args) {
    if (buf == NULL || format == NULL || args == NULL) return STATUS_ERR_NULL_ARG;

    const char *p = format;
    status_t rc = STATUS_OK;

    while (*p != '\0' && rc == STATUS_OK) {
        if (*p != '%') {
            rc = buffer_append_char(buf, *p);
            ++p;
        } else if (p[1] == '%') {
            rc = buffer_append_char(buf, '%');
            p += 2;
        } else {
            ++p;
            const custom_spec_t custom = detect_custom_spec(p);
            if (custom != SPEC_NONE) {
                rc = format_custom(buf, custom, args);
                p += 2;
            } else {
                rc = format_standard(buf, &p, args);
            }
        }
    }

    return rc;
}

int overfprintf(FILE *stream, const char *format, ...) {
    if (stream == NULL || format == NULL) return -(int)STATUS_ERR_NULL_ARG;

    buffer_t buf;
    status_t rc = buffer_init(&buf);

    if (rc == STATUS_OK) {
        va_list args;
        va_start(args, format);
        rc = over_vformat(&buf, format, &args);
        va_end(args);
    }

    if (rc == STATUS_OK && buf.len > (size_t)INT_MAX)
        rc = STATUS_ERR_RANGE;
    if (rc == STATUS_OK && fwrite(buf.data, 1, buf.len, stream) != buf.len)
        rc = STATUS_ERR_IO;

    const int result = (rc == STATUS_OK) ? (int)buf.len : -(int)rc;
    buffer_free(&buf);
    return result;
}

int oversprintf(char *str, const char *format, ...) {
    if (str == NULL || format == NULL) return -(int)STATUS_ERR_NULL_ARG;

    buffer_t buf;
    status_t rc = buffer_init(&buf);

    if (rc == STATUS_OK) {
        va_list args;
        va_start(args, format);
        rc = over_vformat(&buf, format, &args);
        va_end(args);
    }

    if (rc == STATUS_OK && buf.len > (size_t)INT_MAX)
        rc = STATUS_ERR_RANGE;
    if (rc == STATUS_OK)
        memcpy(str, buf.data, buf.len + 1);

    const int result = (rc == STATUS_OK) ? (int)buf.len : -(int)rc;
    buffer_free(&buf);
    return result;
}

void print_status(const status_t rc) {
    switch (rc) {
        case STATUS_OK:
            printf("успешно\n");
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
        case STATUS_ERR_RANGE:
            printf("ошибка: значение вне допустимого диапазона\n");
            break;
        case STATUS_ERR_INVALID:
            printf("ошибка: некорректное значение аргумента\n");
            break;
        case STATUS_ERR_IO:
            printf("ошибка: не удалось записать данные в поток\n");
            break;
        default:
            printf("неизвестная ошибка\n");
            break;
    }
}

void print_call_result(const int rc) {
    if (rc < 0)
        print_status((status_t)(-rc));
}

void demo_standard(void) {
    printf("=== Стандартные спецификаторы: sprintf и oversprintf ===\n");

    char expected[512];
    char actual[512];
    int count_expected = 0;
    int count_actual = 0;

    const int len_expected = sprintf(expected,
        "[%d] [%5i] [%-6u] [%+.3f] [%e] [%#x] [%o] [%s] [%c] [%%] [%*d] [%.*f] [%lld] [%zu] [%10.4s] [%G]%n",
        -42, 7, 100u, 3.14159, 12345.678, 255u, 8u, "text", 'A', 6, 42, 2, 2.71828,
        -9000000000LL, sizeof(double), "abcdefgh", 0.000012345, &count_expected);
    const int len_actual = oversprintf(actual,
        "[%d] [%5i] [%-6u] [%+.3f] [%e] [%#x] [%o] [%s] [%c] [%%] [%*d] [%.*f] [%lld] [%zu] [%10.4s] [%G]%n",
        -42, 7, 100u, 3.14159, 12345.678, 255u, 8u, "text", 'A', 6, 42, 2, 2.71828,
        -9000000000LL, sizeof(double), "abcdefgh", 0.000012345, &count_actual);

    printf("sprintf     (%d): %s\n", len_expected, expected);
    if (len_actual < 0) {
        printf("oversprintf: ");
        print_call_result(len_actual);
        return;
    }
    printf("oversprintf (%d): %s\n", len_actual, actual);
    printf("%%n: sprintf = %d, oversprintf = %d\n", count_expected, count_actual);
    printf("Результаты %s\n\n",
           (len_expected == len_actual && strcmp(expected, actual) == 0 && count_expected == count_actual)
               ? "совпадают" : "НЕ совпадают");
}

void demo_roman(void) {
    printf("%%Ro: римская запись числа типа int\n");

    const int values[] = {1, 4, 9, 14, 40, 90, 400, 1994, 2026, 3999, -58, 0};
    const size_t count = sizeof(values) / sizeof(values[0]);

    for (size_t i = 0; i < count; i++)
        print_call_result(overfprintf(stdout, "%5d -> %Ro\n", values[i], values[i]));
    printf("\n");
}

void demo_zeckendorf(void) {
    printf("%%Zr: цекендорфово представление числа типа unsigned int\n");

    const unsigned int values[] = {0u, 1u, 2u, 3u, 4u, 10u, 100u, UINT_MAX};
    const size_t count = sizeof(values) / sizeof(values[0]);

    for (size_t i = 0; i < count; i++)
        print_call_result(overfprintf(stdout, "%10u -> %Zr\n", values[i], values[i]));
    printf("\n");
}

void demo_base(void) {
    printf("%%Cv / %%CV: число типа int в системе счисления с основанием [2..36]\n");

    const int values[] = {255, 255, -255, 12345, INT_MAX, INT_MIN, 100, 100};
    const int bases[] = {16, 2, 16, 8, 36, 36, 1, 40};
    const size_t count = sizeof(values) / sizeof(values[0]);

    for (size_t i = 0; i < count; i++)
        print_call_result(overfprintf(stdout, "%11d, основание %2d -> %%Cv: %Cv, %%CV: %CV\n",
                                      values[i], bases[i], values[i], bases[i], values[i], bases[i]));
    printf("\n");
}

void demo_to_decimal(void) {
    printf("%%to / %%TO: перевод строки из системы счисления с основанием [2..36] в десятичную\n");

    const char *const strings[] = {"ff", "FF", "-zik0zk", "ZIK0ZJ", "777", "101010", "123"};
    const int bases[] = {16, 16, 36, 36, 8, 2, 99};
    const int uppercase[] = {0, 1, 0, 1, 0, 0, 0};
    const size_t count = sizeof(bases) / sizeof(bases[0]);

    for (size_t i = 0; i < count; i++) {
        if (uppercase[i])
            print_call_result(overfprintf(stdout, "%%TO(\"%s\", %d) = %TO\n",
                                          strings[i], bases[i], strings[i], bases[i]));
        else
            print_call_result(overfprintf(stdout, "%%to(\"%s\", %d) = %to\n",
                                          strings[i], bases[i], strings[i], bases[i]));
    }
    printf("\n");
}

void demo_memory_dump(void) {
    printf("%%mi, %%mu, %%md, %%mf: дамп памяти\n");

    print_call_result(overfprintf(stdout, "%%mi(%d) = %mi\n", 1, 1));
    print_call_result(overfprintf(stdout, "%%mi(%d) = %mi\n", -1, -1));
    print_call_result(overfprintf(stdout, "%%mi(%d) = %mi\n", INT_MIN, INT_MIN));
    print_call_result(overfprintf(stdout, "%%mu(%u) = %mu\n", 0xDEADBEEFu, 0xDEADBEEFu));
    print_call_result(overfprintf(stdout, "%%md(%g) = %md\n", 1.0, 1.0));
    print_call_result(overfprintf(stdout, "%%md(%g) = %md\n", -2.5, -2.5));
    print_call_result(overfprintf(stdout, "%%mf(%g) = %mf\n", 1.0f, 1.0f));
    print_call_result(overfprintf(stdout, "%%mf(%g) = %mf\n", 0.1f, 0.1f));
    printf("\n");
}

void demo_sprintf(void) {
    printf("oversprintf: смешанная строка формата\n");

    char out[256];
    const int rc = oversprintf(out, "%d год = %Ro, %u = %Zr, %d = 0x%CV, \"%s\"(16) = %to",
                               2026, 2026, 10u, 10u, 48879, 48879, 16, "beef", "beef", 16);
    if (rc < 0) {
        print_call_result(rc);
        return;
    }

    printf("Строка: %s\n", out);
    printf("Длина:  %d\n\n", rc);
}

int main(int argc, char *argv[]) {
    (void)argv;

    if (argc != 1) {
        fprintf(stderr, "Формат запуска программы: ./[программа] (без аргументов)\n");
        return 1;
    }

    demo_standard();
    demo_roman();
    demo_zeckendorf();
    demo_base();
    demo_to_decimal();
    demo_memory_dump();
    demo_sprintf();

    return 0;
}
