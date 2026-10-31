#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef enum {
    STATUS_OK,
    STATUS_ERR_NULL_ARG,
    STATUS_ERR_INVALID,
    STATUS_ERR_MEMORY,
    STATUS_ERR_FILE_OPEN,
    STATUS_ERR_IO
} status_t;

typedef struct {
    size_t line;
    size_t column;
} position_t;

typedef struct {
    const char *path;
    status_t status;
    position_t *matches;
    size_t count;
    size_t cap;
} file_matches_t;

typedef struct {
    file_matches_t *files;
    size_t count;
} search_result_t;

typedef struct {
    const char *text;
    size_t length;
    size_t *prefix;
} pattern_t;

status_t pattern_init(pattern_t *pattern, const char *text) {
    if (pattern == NULL || text == NULL) return STATUS_ERR_NULL_ARG;

    const size_t length = strlen(text);
    if (length == 0) return STATUS_ERR_INVALID;

    size_t *prefix = (size_t *)calloc(length, sizeof(size_t));
    if (prefix == NULL) return STATUS_ERR_MEMORY;

    size_t k = 0;
    for (size_t i = 1; i < length; i++) {
        while (k > 0 && text[i] != text[k])
            k = prefix[k - 1];
        if (text[i] == text[k])
            ++k;
        prefix[i] = k;
    }

    pattern->text = text;
    pattern->length = length;
    pattern->prefix = prefix;
    return STATUS_OK;
}

void pattern_free(pattern_t *pattern) {
    if (pattern == NULL) return;

    free(pattern->prefix);
    pattern->prefix = NULL;
    pattern->length = 0;
}

status_t matches_push(file_matches_t *file, const position_t position) {
    if (file == NULL) return STATUS_ERR_NULL_ARG;

    if (file->count == file->cap) {
        if (file->cap > SIZE_MAX / 2 / sizeof(position_t)) return STATUS_ERR_MEMORY;

        const size_t new_cap = (file->cap == 0) ? 16 : file->cap * 2;
        position_t *tmp = (position_t *)realloc(file->matches, new_cap * sizeof(position_t));
        if (tmp == NULL) return STATUS_ERR_MEMORY;

        file->matches = tmp;
        file->cap = new_cap;
    }

    file->matches[file->count++] = position;
    return STATUS_OK;
}

status_t search_stream(FILE *stream, const pattern_t *pattern, file_matches_t *out) {
    if (stream == NULL || pattern == NULL || pattern->prefix == NULL || out == NULL) return STATUS_ERR_NULL_ARG;

    const size_t m = pattern->length;
    position_t *window = (position_t *)calloc(m, sizeof(position_t));
    if (window == NULL) return STATUS_ERR_MEMORY;

    position_t current = {1, 1};
    size_t state = 0;
    size_t index = 0;
    status_t rc = STATUS_OK;
    int c = fgetc(stream);

    while (c != EOF && rc == STATUS_OK) {
        window[index % m] = current;

        while (state > 0 && (unsigned char)pattern->text[state] != c)
            state = pattern->prefix[state - 1];
        if ((unsigned char)pattern->text[state] == c)
            ++state;

        if (state == m) {
            rc = matches_push(out, window[(index + 1 - m) % m]);
            state = pattern->prefix[m - 1];
        }

        if (c == '\n') {
            ++current.line;
            current.column = 1;
        } else {
            ++current.column;
        }

        ++index;
        c = fgetc(stream);
    }

    free(window);

    if (rc == STATUS_OK && ferror(stream))
        rc = STATUS_ERR_IO;
    return rc;
}

status_t search_path(const char *path, const pattern_t *pattern, file_matches_t *out) {
    if (path == NULL || pattern == NULL || out == NULL) return STATUS_ERR_NULL_ARG;

    FILE *file = fopen(path, "r");
    if (file == NULL) return STATUS_ERR_FILE_OPEN;

    const status_t rc = search_stream(file, pattern, out);
    fclose(file);
    return rc;
}

void free_search_result(search_result_t *result) {
    if (result == NULL) return;

    if (result->files != NULL) {
        for (size_t i = 0; i < result->count; i++)
            free(result->files[i].matches);
        free(result->files);
    }

    result->files = NULL;
    result->count = 0;
}

status_t find_in_files(search_result_t *result, const char *substring, const size_t count, ...) {
    if (result == NULL || substring == NULL) return STATUS_ERR_NULL_ARG;
    if (substring[0] == '\0' || count == 0) return STATUS_ERR_INVALID;

    search_result_t res = {NULL, count};
    res.files = (file_matches_t *)calloc(count, sizeof(file_matches_t));
    if (res.files == NULL) return STATUS_ERR_MEMORY;

    status_t rc = STATUS_OK;
    va_list args;
    va_start(args, count);
    for (size_t i = 0; i < count; i++) {
        res.files[i].path = va_arg(args, char *);
        if (res.files[i].path == NULL)
            rc = STATUS_ERR_NULL_ARG;
    }
    va_end(args);

    pattern_t pattern = {NULL, 0, NULL};
    if (rc == STATUS_OK)
        rc = pattern_init(&pattern, substring);

    for (size_t i = 0; i < count && rc == STATUS_OK; i++) {
        res.files[i].status = search_path(res.files[i].path, &pattern, &res.files[i]);
        if (res.files[i].status == STATUS_ERR_MEMORY)
            rc = STATUS_ERR_MEMORY;
    }

    pattern_free(&pattern);

    if (rc != STATUS_OK) {
        free_search_result(&res);
        return rc;
    }

    *result = res;
    return STATUS_OK;
}

void print_escaped(const char *str) {
    if (str == NULL) {
        printf("(null)");
        return;
    }

    putchar('"');
    for (const char *p = str; *p != '\0'; ++p) {
        switch (*p) {
            case '\n':
                printf("\\n");
                break;
            case '\t':
                printf("\\t");
                break;
            case '\r':
                printf("\\r");
                break;
            case '"':
                printf("\\\"");
                break;
            case '\\':
                printf("\\\\");
                break;
            default:
                if ((unsigned char)*p < 0x20)
                    printf("\\x%02X", (unsigned int)(unsigned char)*p);
                else
                    putchar(*p);
                break;
        }
    }
    putchar('"');
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
            printf("ошибка: пустая подстрока или нулевое количество файлов\n");
            break;
        case STATUS_ERR_MEMORY:
            printf("ошибка: недостаточно памяти\n");
            break;
        case STATUS_ERR_FILE_OPEN:
            printf("ошибка: не удалось открыть файл\n");
            break;
        case STATUS_ERR_IO:
            printf("ошибка: не удалось прочитать файл\n");
            break;
        default:
            printf("неизвестная ошибка\n");
            break;
    }
}

void print_search_result(const search_result_t *result) {
    if (result == NULL || result->files == NULL) return;

    for (size_t i = 0; i < result->count; i++) {
        const file_matches_t *file = &result->files[i];
        printf("  Файл '%s': ", file->path);

        if (file->status != STATUS_OK) {
            print_status(file->status);
            continue;
        }
        if (file->count == 0) {
            printf("вхождений нет\n");
            continue;
        }

        printf("вхождений: %zu\n", file->count);
        for (size_t j = 0; j < file->count; j++)
            printf("    строка %zu, символ %zu\n", file->matches[j].line, file->matches[j].column);
    }
}

void report_search(const char *title, const char *substring, const status_t rc, search_result_t *result) {
    printf("=== %s ===\nПодстрока: ", title);
    print_escaped(substring);
    printf("\n");

    if (rc == STATUS_OK)
        print_search_result(result);
    else
        print_status(rc);

    printf("\n");
    free_search_result(result);
}

int main(int argc, char *argv[]) {
    if (argc != 3) {
        fprintf(stderr, "Формат запуска программы: ./[программа] [файл 1] [файл 2]\n");
        return 1;
    }

    const char *first = argv[1];
    const char *second = argv[2];
    const char *missing = "task3_missing_file.txt";
    search_result_t result = {NULL, 0};
    status_t rc;

    rc = find_in_files(&result, "cat", 3, first, second, missing);
    report_search("Обычное слово, три файла (третий не существует)", "cat", rc, &result);

    rc = find_in_files(&result, "aa", 1, first);
    report_search("Перекрывающиеся вхождения", "aa", rc, &result);

    rc = find_in_files(&result, "\t", 2, first, second);
    report_search("Символ табуляции", "\t", rc, &result);

    rc = find_in_files(&result, "of line\nstart", 2, first, second);
    report_search("Подстрока с переносом строки", "of line\nstart", rc, &result);

    rc = find_in_files(&result, "\n\n", 2, first, second);
    report_search("Два переноса подряд (пустая строка)", "\n\n", rc, &result);

    rc = find_in_files(&result, " \t", 2, first, second);
    report_search("Пробел и табуляция", " \t", rc, &result);

    rc = find_in_files(&result, "", 1, first);
    report_search("Ошибка: пустая подстрока", "", rc, &result);

    rc = find_in_files(&result, "cat", 0);
    report_search("Ошибка: количество файлов равно 0", "cat", rc, &result);

    rc = find_in_files(&result, NULL, 1, first);
    report_search("Ошибка: подстрока NULL", NULL, rc, &result);

    rc = find_in_files(&result, "cat", 2, first, (char *)NULL);
    report_search("Ошибка: путь к файлу NULL", "cat", rc, &result);

    return 0;
}
