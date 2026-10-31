#include <ctype.h>
#include <float.h>
#include <limits.h>
#include <math.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef enum {
    STATUS_OK,
    STATUS_EOF,
    STATUS_ERR_NULL_ARG,
    STATUS_ERR_MEMORY,
    STATUS_ERR_IO,
    STATUS_ERR_FLAG,
    STATUS_ERR_FIELD_COUNT,
    STATUS_ERR_ID,
    STATUS_ERR_NAME,
    STATUS_ERR_SURNAME,
    STATUS_ERR_SALARY
} status_t;

typedef enum {
    ORDER_ASC,
    ORDER_DESC
} sort_order_t;

typedef struct {
    unsigned int id;
    char *name;
    char *surname;
    double salary;
} Employee;

typedef struct {
    Employee *items;
    size_t size;
    size_t capacity;
} employee_array_t;

status_t parse_flag(const char *flag, sort_order_t *out) {
    if (flag == NULL || out == NULL) return STATUS_ERR_NULL_ARG;
    if (flag[0] != '-' && flag[0] != '/') return STATUS_ERR_FLAG;
    if (flag[1] == '\0' || flag[2] != '\0') return STATUS_ERR_FLAG;

    if (flag[1] == 'a')
        *out = ORDER_ASC;
    else if (flag[1] == 'd')
        *out = ORDER_DESC;
    else
        return STATUS_ERR_FLAG;

    return STATUS_OK;
}

int is_latin_letter(const char c) {
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
}

int is_digit(const char c) {
    return c >= '0' && c <= '9';
}

int is_valid_name(const char *str) {
    if (str == NULL || *str == '\0') return 0;

    for (const char *p = str; *p != '\0'; ++p)
        if (!is_latin_letter(*p))
            return 0;

    return 1;
}

int is_blank_line(const char *line) {
    if (line == NULL) return 1;

    for (const char *p = line; *p != '\0'; ++p)
        if (!isspace((unsigned char)*p))
            return 0;

    return 1;
}

status_t parse_id(const char *str, unsigned int *out) {
    if (str == NULL || out == NULL) return STATUS_ERR_NULL_ARG;
    if (*str == '\0') return STATUS_ERR_ID;

    unsigned int value = 0;
    for (const char *p = str; *p != '\0'; ++p) {
        if (!is_digit(*p)) return STATUS_ERR_ID;

        const unsigned int digit = (unsigned int)(*p - '0');
        if (value > (UINT_MAX - digit) / 10u) return STATUS_ERR_ID;
        value = value * 10u + digit;
    }

    *out = value;
    return STATUS_OK;
}

status_t parse_salary(const char *str, double *out) {
    if (str == NULL || out == NULL) return STATUS_ERR_NULL_ARG;

    size_t digits = 0;
    size_t dots = 0;
    for (const char *p = str; *p != '\0'; ++p) {
        if (is_digit(*p))
            ++digits;
        else if (*p == '.')
            ++dots;
        else
            return STATUS_ERR_SALARY;
    }

    if (digits == 0 || dots > 1) return STATUS_ERR_SALARY;

    char *end = NULL;
    const double value = strtod(str, &end);
    if (*end != '\0' || !isfinite(value)) return STATUS_ERR_SALARY;

    *out = value;
    return STATUS_OK;
}

status_t duplicate_string(const char *src, char **out) {
    if (src == NULL || out == NULL) return STATUS_ERR_NULL_ARG;

    const size_t len = strlen(src);
    char *copy = (char *)malloc(len + 1);
    if (copy == NULL) return STATUS_ERR_MEMORY;

    memcpy(copy, src, len + 1);
    *out = copy;
    return STATUS_OK;
}

status_t split_line(char *line, char **tokens, const size_t max_tokens, size_t *count) {
    if (line == NULL || tokens == NULL || count == NULL) return STATUS_ERR_NULL_ARG;

    size_t n = 0;
    char *p = line;

    while (*p != '\0') {
        while (*p != '\0' && isspace((unsigned char)*p))
            ++p;
        if (*p == '\0')
            break;

        if (n == max_tokens) return STATUS_ERR_FIELD_COUNT;
        tokens[n++] = p;

        while (*p != '\0' && !isspace((unsigned char)*p))
            ++p;
        if (*p != '\0')
            *p++ = '\0';
    }

    *count = n;
    return STATUS_OK;
}

void free_employee(Employee *employee) {
    if (employee == NULL) return;

    free(employee->name);
    free(employee->surname);
    employee->name = NULL;
    employee->surname = NULL;
}

status_t parse_employee(char *line, Employee *out) {
    if (line == NULL || out == NULL) return STATUS_ERR_NULL_ARG;

    char *tokens[4];
    const size_t fields = sizeof(tokens) / sizeof(tokens[0]);
    size_t count = 0;

    status_t rc = split_line(line, tokens, fields, &count);
    if (rc != STATUS_OK) return rc;
    if (count != fields) return STATUS_ERR_FIELD_COUNT;

    unsigned int id = 0;
    double salary = 0.0;

    rc = parse_id(tokens[0], &id);
    if (rc != STATUS_OK) return rc;
    if (!is_valid_name(tokens[1])) return STATUS_ERR_NAME;
    if (!is_valid_name(tokens[2])) return STATUS_ERR_SURNAME;
    rc = parse_salary(tokens[3], &salary);
    if (rc != STATUS_OK) return rc;

    char *name = NULL;
    char *surname = NULL;

    rc = duplicate_string(tokens[1], &name);
    if (rc != STATUS_OK) return rc;

    rc = duplicate_string(tokens[2], &surname);
    if (rc != STATUS_OK) {
        free(name);
        return rc;
    }

    out->id = id;
    out->name = name;
    out->surname = surname;
    out->salary = salary;
    return STATUS_OK;
}

void employee_array_free(employee_array_t *arr) {
    if (arr == NULL) return;

    for (size_t i = 0; i < arr->size; i++)
        free_employee(&arr->items[i]);

    free(arr->items);
    arr->items = NULL;
    arr->size = 0;
    arr->capacity = 0;
}

status_t employee_array_push(employee_array_t *arr, const Employee *employee) {
    if (arr == NULL || employee == NULL) return STATUS_ERR_NULL_ARG;

    if (arr->size == arr->capacity) {
        if (arr->capacity > SIZE_MAX / 2 / sizeof(Employee)) return STATUS_ERR_MEMORY;

        const size_t new_capacity = (arr->capacity == 0) ? 16 : arr->capacity * 2;
        Employee *tmp = (Employee *)realloc(arr->items, new_capacity * sizeof(Employee));
        if (tmp == NULL) return STATUS_ERR_MEMORY;

        arr->items = tmp;
        arr->capacity = new_capacity;
    }

    arr->items[arr->size++] = *employee;
    return STATUS_OK;
}

status_t ensure_capacity(char **buffer, size_t *capacity, const size_t needed) {
    if (buffer == NULL || capacity == NULL) return STATUS_ERR_NULL_ARG;
    if (needed <= *capacity) return STATUS_OK;

    size_t new_capacity = (*capacity == 0) ? 64 : *capacity;
    while (new_capacity < needed) {
        if (new_capacity > SIZE_MAX / 2) return STATUS_ERR_MEMORY;
        new_capacity *= 2;
    }

    char *tmp = (char *)realloc(*buffer, new_capacity);
    if (tmp == NULL) return STATUS_ERR_MEMORY;

    *buffer = tmp;
    *capacity = new_capacity;
    return STATUS_OK;
}

status_t read_line(FILE *file, char **buffer, size_t *capacity) {
    if (file == NULL || buffer == NULL || capacity == NULL) return STATUS_ERR_NULL_ARG;

    int c = fgetc(file);
    if (c == EOF) return ferror(file) ? STATUS_ERR_IO : STATUS_EOF;

    size_t len = 0;
    status_t rc = STATUS_OK;

    while (c != EOF && c != '\n' && rc == STATUS_OK) {
        rc = ensure_capacity(buffer, capacity, len + 2);
        if (rc == STATUS_OK) {
            (*buffer)[len++] = (char)c;
            c = fgetc(file);
        }
    }

    if (rc == STATUS_OK && ferror(file))
        rc = STATUS_ERR_IO;
    if (rc == STATUS_OK)
        rc = ensure_capacity(buffer, capacity, len + 1);
    if (rc == STATUS_OK)
        (*buffer)[len] = '\0';

    return rc;
}

status_t read_employees(FILE *file, employee_array_t *arr, size_t *line_number) {
    if (file == NULL || arr == NULL || line_number == NULL) return STATUS_ERR_NULL_ARG;

    char *line = NULL;
    size_t capacity = 0;
    status_t rc = STATUS_OK;
    *line_number = 0;

    while ((rc = read_line(file, &line, &capacity)) == STATUS_OK) {
        ++(*line_number);
        if (is_blank_line(line))
            continue;

        Employee employee;
        rc = parse_employee(line, &employee);
        if (rc == STATUS_OK) {
            rc = employee_array_push(arr, &employee);
            if (rc != STATUS_OK)
                free_employee(&employee);
        }

        if (rc != STATUS_OK)
            break;
    }

    free(line);
    return (rc == STATUS_EOF) ? STATUS_OK : rc;
}

int compare_double(const double a, const double b, const double eps) {
    const double diff = a - b;
    if (diff > -eps && diff < eps)
        return 0;
    return (diff < 0.0) ? -1 : 1;
}

int compare_unsigned(const unsigned int a, const unsigned int b) {
    return (a > b) - (a < b);
}

int compare_employees_asc(const void *lhs, const void *rhs) {
    const Employee *a = (const Employee *)lhs;
    const Employee *b = (const Employee *)rhs;
    const double eps = 1e-9;

    int rc = compare_double(a->salary, b->salary, eps);
    if (rc != 0) return rc;

    rc = strcmp(a->surname, b->surname);
    if (rc != 0) return rc;

    rc = strcmp(a->name, b->name);
    if (rc != 0) return rc;

    return compare_unsigned(a->id, b->id);
}

int compare_employees_desc(const void *lhs, const void *rhs) {
    return compare_employees_asc(rhs, lhs);
}

status_t sort_employees(employee_array_t *arr, const sort_order_t order) {
    if (arr == NULL) return STATUS_ERR_NULL_ARG;
    if (arr->size < 2) return STATUS_OK;

    qsort(arr->items, arr->size, sizeof(Employee),
          (order == ORDER_ASC) ? compare_employees_asc : compare_employees_desc);
    return STATUS_OK;
}

status_t write_employees(FILE *file, const employee_array_t *arr) {
    if (file == NULL || arr == NULL) return STATUS_ERR_NULL_ARG;

    for (size_t i = 0; i < arr->size; i++) {
        const Employee *e = &arr->items[i];
        if (fprintf(file, "%u %s %s %.*g\n", e->id, e->name, e->surname, DBL_DIG, e->salary) < 0)
            return STATUS_ERR_IO;
    }

    return STATUS_OK;
}

void print_error(const status_t rc, const size_t line_number) {
    switch (rc) {
        case STATUS_ERR_FIELD_COUNT:
            fprintf(stderr, "Ошибка в строке %zu: ожидается 4 поля (id имя фамилия зарплата)\n", line_number);
            break;
        case STATUS_ERR_ID:
            fprintf(stderr, "Ошибка в строке %zu: id должен быть целым неотрицательным числом (не больше %u)\n",
                    line_number, UINT_MAX);
            break;
        case STATUS_ERR_NAME:
            fprintf(stderr, "Ошибка в строке %zu: имя должно быть непустой строкой из латинских букв\n", line_number);
            break;
        case STATUS_ERR_SURNAME:
            fprintf(stderr, "Ошибка в строке %zu: фамилия должна быть непустой строкой из латинских букв\n", line_number);
            break;
        case STATUS_ERR_SALARY:
            fprintf(stderr, "Ошибка в строке %zu: зарплата должна быть неотрицательным вещественным числом\n", line_number);
            break;
        case STATUS_ERR_MEMORY:
            fprintf(stderr, "Ошибка: не удалось выделить память\n");
            break;
        case STATUS_ERR_IO:
            fprintf(stderr, "Ошибка ввода-вывода при работе с файлом\n");
            break;
        case STATUS_ERR_FLAG:
            fprintf(stderr, "Ошибка: флаг должен быть одним из -a, /a, -d, /d\n");
            break;
        default:
            fprintf(stderr, "Ошибка: неверные аргументы\n");
            break;
    }
}

int main(int argc, char *argv[]) {
    if (argc != 4) {
        fprintf(stderr, "Формат запуска программы: ./[программа] [входной файл] [-a|/a|-d|/d] [выходной файл]\n");
        return 1;
    }

    sort_order_t order = ORDER_ASC;
    status_t rc = parse_flag(argv[2], &order);
    if (rc != STATUS_OK) {
        print_error(rc, 0);
        return 1;
    }

    if (strcmp(argv[1], argv[3]) == 0) {
        fprintf(stderr, "Ошибка: входной и выходной файлы должны быть разными\n");
        return 1;
    }

    FILE *input = fopen(argv[1], "r");
    if (input == NULL) {
        fprintf(stderr, "Ошибка: не удалось открыть входной файл '%s'\n", argv[1]);
        return 1;
    }

    employee_array_t employees = {NULL, 0, 0};
    size_t line_number = 0;
    rc = read_employees(input, &employees, &line_number);
    fclose(input);

    if (rc == STATUS_OK)
        rc = sort_employees(&employees, order);
    if (rc != STATUS_OK) {
        print_error(rc, line_number);
        employee_array_free(&employees);
        return 1;
    }

    FILE *output = fopen(argv[3], "w");
    if (output == NULL) {
        fprintf(stderr, "Ошибка: не удалось открыть выходной файл '%s'\n", argv[3]);
        employee_array_free(&employees);
        return 1;
    }

    rc = write_employees(output, &employees);
    if (fclose(output) != 0 && rc == STATUS_OK)
        rc = STATUS_ERR_IO;

    const size_t count = employees.size;
    employee_array_free(&employees);

    if (rc != STATUS_OK) {
        print_error(rc, 0);
        return 1;
    }

    printf("Отсортировано записей: %zu, результат записан в '%s'\n", count, argv[3]);
    return 0;
}
