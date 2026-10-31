#include <ctype.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef enum {
    STATUS_OK,
    STATUS_ERR_NULL_ARG,
    STATUS_ERR_FORMAT,
    STATUS_ERR_FIELDS,
    STATUS_ERR_RANGE,
    STATUS_ERR_INVALID,
    STATUS_ERR_DUPLICATE,
    STATUS_ERR_EOF,
    STATUS_ERR_IO,
    STATUS_ERR_MEMORY,
    STATUS_ERR_OVERFLOW,
    STATUS_ERR_TOO_BIG
} status_t;

typedef struct {
    char *name;
    unsigned long long size;
    unsigned long long importance;
} file_info_t;

typedef struct {
    file_info_t *files;
    size_t count;
    unsigned long long total_size;
    unsigned long long total_importance;
} backup_t;

status_t parse_ull(const char *str, unsigned long long *out) {
    if (str == NULL || out == NULL) return STATUS_ERR_NULL_ARG;
    if (*str == '\0') return STATUS_ERR_FORMAT;

    unsigned long long value = 0;
    for (const char *p = str; *p != '\0'; p++) {
        if (*p < '0' || *p > '9')
            return STATUS_ERR_FORMAT;

        const unsigned digit = (unsigned)(*p - '0');
        if (value > (ULLONG_MAX - digit) / 10)
            return STATUS_ERR_RANGE;
        value = value * 10 + digit;
    }

    *out = value;
    return STATUS_OK;
}

status_t copy_string(const char *src, char **out) {
    if (src == NULL || out == NULL) return STATUS_ERR_NULL_ARG;

    const size_t length = strlen(src);
    char *copy = malloc(length + 1);
    if (copy == NULL)
        return STATUS_ERR_MEMORY;

    memcpy(copy, src, length + 1);
    *out = copy;
    return STATUS_OK;
}

status_t read_line(FILE *stream, char **out) {
    if (stream == NULL || out == NULL) return STATUS_ERR_NULL_ARG;

    size_t allocated = 16;
    size_t length = 0;
    char *buffer = malloc(allocated);
    if (buffer == NULL)
        return STATUS_ERR_MEMORY;

    int ch = 0;
    while ((ch = fgetc(stream)) != EOF && ch != '\n') {
        if (length + 1 == allocated) {
            char *tmp = (allocated <= SIZE_MAX / 2) ? realloc(buffer, allocated * 2) : NULL;
            if (tmp == NULL) {
                free(buffer);
                return STATUS_ERR_MEMORY;
            }
            buffer = tmp;
            allocated *= 2;
        }
        buffer[length++] = (char)ch;
    }

    if (ferror(stream)) {
        free(buffer);
        return STATUS_ERR_IO;
    }
    if (ch == EOF && length == 0) {
        free(buffer);
        return STATUS_ERR_EOF;
    }

    buffer[length] = '\0';
    *out = buffer;
    return STATUS_OK;
}

status_t split_line(char *line, char *tokens[], const size_t max_tokens, size_t *count) {
    if (line == NULL || tokens == NULL || count == NULL) return STATUS_ERR_NULL_ARG;

    size_t n = 0;
    char *p = line;
    while (*p != '\0') {
        while (*p != '\0' && isspace((unsigned char)*p))
            *p++ = '\0';
        if (*p == '\0')
            break;

        if (n < max_tokens)
            tokens[n] = p;
        ++n;

        while (*p != '\0' && !isspace((unsigned char)*p))
            p++;
    }

    *count = n;
    return STATUS_OK;
}

status_t parse_header(char *line, size_t *count, unsigned long long *capacity) {
    if (line == NULL || count == NULL || capacity == NULL) return STATUS_ERR_NULL_ARG;

    char *tokens[2];
    size_t n_tokens = 0;
    status_t rc = split_line(line, tokens, 2, &n_tokens);
    if (rc != STATUS_OK)
        return rc;
    if (n_tokens != 2)
        return STATUS_ERR_FIELDS;

    unsigned long long n = 0;
    unsigned long long cap = 0;
    rc = parse_ull(tokens[0], &n);
    if (rc == STATUS_OK)
        rc = parse_ull(tokens[1], &cap);
    if (rc != STATUS_OK)
        return rc;

    if (n == 0)
        return STATUS_ERR_INVALID;
    if ((unsigned long long)(size_t)n != n)
        return STATUS_ERR_RANGE;

    *count = (size_t)n;
    *capacity = cap;
    return STATUS_OK;
}

status_t parse_file_line(char *line, file_info_t *out) {
    if (line == NULL || out == NULL) return STATUS_ERR_NULL_ARG;

    char *tokens[3];
    size_t n_tokens = 0;
    status_t rc = split_line(line, tokens, 3, &n_tokens);
    if (rc != STATUS_OK)
        return rc;
    if (n_tokens != 3)
        return STATUS_ERR_FIELDS;

    unsigned long long size = 0;
    unsigned long long importance = 0;
    rc = parse_ull(tokens[1], &size);
    if (rc == STATUS_OK)
        rc = parse_ull(tokens[2], &importance);
    if (rc != STATUS_OK)
        return rc;

    if (size == 0)
        return STATUS_ERR_INVALID;

    char *name = NULL;
    rc = copy_string(tokens[0], &name);
    if (rc != STATUS_OK)
        return rc;

    out->name = name;
    out->size = size;
    out->importance = importance;
    return STATUS_OK;
}

void free_files(file_info_t *files, const size_t count) {
    if (files == NULL) return;

    for (size_t i = 0; i < count; i++)
        free(files[i].name);
    free(files);
}

status_t grow_files(file_info_t **files, size_t *allocated) {
    if (files == NULL || allocated == NULL) return STATUS_ERR_NULL_ARG;

    const size_t new_allocated = (*allocated == 0) ? 16 : *allocated * 2;
    if (new_allocated < *allocated || new_allocated > SIZE_MAX / sizeof(**files))
        return STATUS_ERR_MEMORY;

    file_info_t *tmp = realloc(*files, new_allocated * sizeof(**files));
    if (tmp == NULL)
        return STATUS_ERR_MEMORY;

    *files = tmp;
    *allocated = new_allocated;
    return STATUS_OK;
}

int compare_names(const void *a, const void *b) {
    const file_info_t *fa = *(const file_info_t *const *)a;
    const file_info_t *fb = *(const file_info_t *const *)b;
    return strcmp(fa->name, fb->name);
}

status_t check_unique_names(const file_info_t *files, const size_t count, size_t *dup_index) {
    if (dup_index == NULL) return STATUS_ERR_NULL_ARG;
    if (files == NULL && count > 0) return STATUS_ERR_NULL_ARG;
    if (count < 2) return STATUS_OK;

    const file_info_t **sorted = malloc(count * sizeof(*sorted));
    if (sorted == NULL)
        return STATUS_ERR_MEMORY;

    for (size_t i = 0; i < count; i++)
        sorted[i] = &files[i];
    qsort(sorted, count, sizeof(*sorted), compare_names);

    status_t rc = STATUS_OK;
    for (size_t i = 1; i < count && rc == STATUS_OK; i++) {
        if (strcmp(sorted[i - 1]->name, sorted[i]->name) == 0) {
            const size_t first  = (size_t)(sorted[i - 1] - files);
            const size_t second = (size_t)(sorted[i] - files);
            *dup_index = (first > second) ? first : second;
            rc = STATUS_ERR_DUPLICATE;
        }
    }

    free(sorted);
    return rc;
}

status_t read_input(FILE *stream, file_info_t **files_out, size_t *count_out,
                    unsigned long long *capacity_out, size_t *line_no) {
    if (stream == NULL || files_out == NULL || count_out == NULL) return STATUS_ERR_NULL_ARG;
    if (capacity_out == NULL || line_no == NULL) return STATUS_ERR_NULL_ARG;

    char *line = NULL;
    size_t expected = 0;
    unsigned long long capacity = 0;

    *line_no = 1;
    status_t rc = read_line(stream, &line);
    if (rc != STATUS_OK)
        return rc;

    rc = parse_header(line, &expected, &capacity);
    free(line);
    if (rc != STATUS_OK)
        return rc;

    file_info_t *files = NULL;
    size_t count = 0;
    size_t allocated = 0;

    while (rc == STATUS_OK && count < expected) {
        ++(*line_no);
        rc = read_line(stream, &line);
        if (rc != STATUS_OK)
            break;

        if (count == allocated)
            rc = grow_files(&files, &allocated);
        if (rc == STATUS_OK)
            rc = parse_file_line(line, &files[count]);
        if (rc == STATUS_OK)
            ++count;
        free(line);
    }

    if (rc == STATUS_OK) {
        size_t dup_index = 0;
        rc = check_unique_names(files, count, &dup_index);
        if (rc == STATUS_ERR_DUPLICATE)
            *line_no = dup_index + 2;
    }

    if (rc != STATUS_OK) {
        free_files(files, count);
        return rc;
    }

    *files_out = files;
    *count_out = count;
    *capacity_out = capacity;
    return STATUS_OK;
}

status_t knapsack(const file_info_t *files, const size_t count, const size_t capacity,
                  unsigned char *chosen) {
    if (files == NULL || chosen == NULL) return STATUS_ERR_NULL_ARG;
    for (size_t i = 0; i < count; i++)
        if (files[i].size == 0) return STATUS_ERR_INVALID;

    const size_t row = capacity + 1;
    unsigned long long *best = calloc(row, sizeof(*best));
    unsigned char *taken = calloc(count * row, sizeof(*taken));
    if (best == NULL || taken == NULL) {
        free(best);
        free(taken);
        return STATUS_ERR_MEMORY;
    }

    for (size_t i = 0; i < count; i++) {
        if (files[i].size > capacity)
            continue;

        const size_t size = (size_t)files[i].size;
        for (size_t c = capacity; c >= size; c--) {
            if (best[c - size] + files[i].importance > best[c]) {
                best[c] = best[c - size] + files[i].importance;
                taken[i * row + c] = 1;
            }
        }
    }

    size_t c = capacity;
    for (size_t k = 0; k < count; k++) {
        const size_t i = count - 1 - k;
        if (taken[i * row + c]) {
            chosen[i] = 1;
            c -= (size_t)files[i].size;
        }
    }

    free(best);
    free(taken);
    return STATUS_OK;
}

status_t select_files(const file_info_t *files, const size_t count,
                      const unsigned long long capacity, backup_t *out) {
    if (files == NULL || out == NULL) return STATUS_ERR_NULL_ARG;
    if (count == 0) return STATUS_ERR_INVALID;

    const unsigned long long WIDTH_LIMIT = 1ULL << 24;
    const unsigned long long CELLS_LIMIT = 1ULL << 27;

    int all_fit = 1;
    unsigned long long size_sum = 0;
    unsigned long long importance_sum = 0;

    for (size_t i = 0; i < count; i++) {
        if (files[i].name == NULL || files[i].size == 0)
            return STATUS_ERR_INVALID;
        if (files[i].size > capacity)
            continue;

        if (files[i].importance > ULLONG_MAX - importance_sum)
            return STATUS_ERR_OVERFLOW;
        importance_sum += files[i].importance;

        if (all_fit && files[i].size <= capacity - size_sum)
            size_sum += files[i].size;
        else
            all_fit = 0;
    }

    if (!all_fit && (capacity >= WIDTH_LIMIT || count > CELLS_LIMIT / (capacity + 1)))
        return STATUS_ERR_TOO_BIG;

    unsigned char *chosen = calloc(count, sizeof(*chosen));
    if (chosen == NULL)
        return STATUS_ERR_MEMORY;

    status_t rc = STATUS_OK;
    if (all_fit) {
        for (size_t i = 0; i < count; i++)
            chosen[i] = (files[i].size <= capacity);
    } else {
        rc = knapsack(files, count, (size_t)capacity, chosen);
    }

    backup_t result = {NULL, 0, 0, 0};
    if (rc == STATUS_OK) {
        result.files = malloc(count * sizeof(*result.files));
        if (result.files == NULL)
            rc = STATUS_ERR_MEMORY;
    }

    if (rc == STATUS_OK) {
        for (size_t i = 0; i < count; i++) {
            if (!chosen[i])
                continue;
            result.files[result.count++] = files[i];
            result.total_size += files[i].size;
            result.total_importance += files[i].importance;
        }
        *out = result;
    }

    free(chosen);
    return rc;
}

status_t print_backup(FILE *stream, const backup_t *backup) {
    if (stream == NULL || backup == NULL) return STATUS_ERR_NULL_ARG;
    if (backup->files == NULL && backup->count > 0) return STATUS_ERR_NULL_ARG;

    fprintf(stream, "TOTAL_SIZE %llu\n", backup->total_size);
    fprintf(stream, "TOTAL_IMPORTANCE %llu\n", backup->total_importance);
    fprintf(stream, "FILES %zu\n", backup->count);
    for (size_t i = 0; i < backup->count; i++)
        fprintf(stream, "%s\n", backup->files[i].name);

    return STATUS_OK;
}

void print_input_error(const status_t rc, const size_t line_no) {
    switch (rc) {
        case STATUS_ERR_EOF:
            fprintf(stderr, "Ошибка: ввод закончился, ожидалась строка %zu\n", line_no);
            break;
        case STATUS_ERR_FIELDS:
            if (line_no == 1)
                fprintf(stderr, "Ошибка в строке 1: ожидалось два числа: n capacity\n");
            else
                fprintf(stderr, "Ошибка в строке %zu: ожидалось три поля: filename size importance\n",
                        line_no);
            break;
        case STATUS_ERR_FORMAT:
            fprintf(stderr, "Ошибка в строке %zu: ожидалось целое неотрицательное число\n", line_no);
            break;
        case STATUS_ERR_RANGE:
            fprintf(stderr, "Ошибка в строке %zu: число слишком большое\n", line_no);
            break;
        case STATUS_ERR_INVALID:
            if (line_no == 1)
                fprintf(stderr, "Ошибка в строке 1: количество файлов должно быть положительным\n");
            else
                fprintf(stderr, "Ошибка в строке %zu: размер файла должен быть положительным\n", line_no);
            break;
        case STATUS_ERR_DUPLICATE:
            fprintf(stderr, "Ошибка в строке %zu: файл с таким именем уже был\n", line_no);
            break;
        case STATUS_ERR_MEMORY:
            fprintf(stderr, "Ошибка: не удалось выделить память\n");
            break;
        case STATUS_ERR_IO:
            fprintf(stderr, "Ошибка чтения из стандартного потока ввода\n");
            break;
        default:
            fprintf(stderr, "Ошибка: неверные аргументы\n");
            break;
    }
}

void print_select_error(const status_t rc) {
    switch (rc) {
        case STATUS_ERR_OVERFLOW:
            fprintf(stderr, "Ошибка: суммарная важность файлов слишком большая\n");
            break;
        case STATUS_ERR_TOO_BIG:
            fprintf(stderr, "Ошибка: слишком большая вместимость накопителя\n");
            break;
        case STATUS_ERR_MEMORY:
            fprintf(stderr, "Ошибка: не удалось выделить память\n");
            break;
        case STATUS_ERR_INVALID:
            fprintf(stderr, "Ошибка: некорректное описание файла\n");
            break;
        default:
            fprintf(stderr, "Ошибка: неверные аргументы\n");
            break;
    }
}

int main(int argc, char *argv[]) {
    (void)argv;
    if (argc != 1) {
        fprintf(stderr, "Формат запуска программы: ./[программа] < [файл с входными данными]\n");
        return 1;
    }

    file_info_t *files = NULL;
    size_t count = 0;
    unsigned long long capacity = 0;
    size_t line_no = 0;

    status_t rc = read_input(stdin, &files, &count, &capacity, &line_no);
    if (rc != STATUS_OK) {
        print_input_error(rc, line_no);
        return 1;
    }

    backup_t backup = {NULL, 0, 0, 0};
    rc = select_files(files, count, capacity, &backup);
    if (rc == STATUS_OK)
        rc = print_backup(stdout, &backup);
    if (rc != STATUS_OK)
        print_select_error(rc);

    free(backup.files);
    free_files(files, count);
    return (rc == STATUS_OK) ? 0 : 1;
}
