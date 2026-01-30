/**
 * poscal-rs Runtime Library
 * 
 * This C library implements the runtime functions for the poscal-rs compiler.
 * It provides implementations for all external functions declared in the
 * standard library units (System, SysUtils, Classes, Math).
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <time.h>
#include <ctype.h>
#include <sys/stat.h>
#include <unistd.h>
#include <dirent.h>
#include <setjmp.h>

// Add setjmp/longjmp based exception handling
jmp_buf pas_exception_buf;

void pas_raise(int code) {
    longjmp(pas_exception_buf, code);
}

// At the top, after includes
extern int pas_argc;
extern char** pas_argv;

/* ============================================================================
 * String Type Definition
 * ============================================================================ */

typedef struct {
    int length;
    int capacity;
    char* data;
} PascalString;

/* ============================================================================
 * I/O Functions
 * ============================================================================ */

void pas_write(const char* s) {
    printf("%s", s);
    fflush(stdout);
}

void pas_writeln(const char* s) {
    printf("%s\n", s);
}

char* pas_readln() {
    char* line = NULL;
    size_t len = 0;
    getline(&line, &len, stdin);
    return line;
}

void pas_read(char* c) {
    *c = getchar();
}

void pas_readln(char* buffer, int maxlen) {
    if (fgets(buffer, maxlen, stdin) != NULL) {
        size_t len = strlen(buffer);
        if (len > 0 && buffer[len-1] == '\n') {
            buffer[len-1] = '\0';
        }
    }
}

/* ============================================================================
 * String Functions
 * ============================================================================ */

int pas_length(const char* s) {
    return s ? strlen(s) : 0;
}

void pas_copy(const char* s, int index, int count, char* result, int maxlen) {
    if (!s || index < 1 || count < 0) {
        result[0] = '\0';
        return;
    }
    
    int len = strlen(s);
    int start = index - 1; // Pascal is 1-indexed
    
    if (start >= len) {
        result[0] = '\0';
        return;
    }
    
    int actual_count = (start + count > len) ? (len - start) : count;
    if (actual_count > maxlen - 1) {
        actual_count = maxlen - 1;
    }
    
    strncpy(result, s + start, actual_count);
    result[actual_count] = '\0';
}

void pas_concat(const char* s1, const char* s2, char* result, int maxlen) {
    snprintf(result, maxlen, "%s%s", s1 ? s1 : "", s2 ? s2 : "");
}

int pas_pos(const char* substr, const char* s) {
    if (!substr || !s) return 0;
    
    const char* found = strstr(s, substr);
    return found ? (int)(found - s) + 1 : 0; // Pascal is 1-indexed
}

char pas_upcase(char c) {
    return toupper(c);
}

void pas_lowercase(const char* s, char* result, int maxlen) {
    if (!s) {
        result[0] = '\0';
        return;
    }
    
    int len = strlen(s);
    if (len >= maxlen) len = maxlen - 1;
    
    for (int i = 0; i < len; i++) {
        result[i] = tolower(s[i]);
    }
    result[len] = '\0';
}

void pas_uppercase(const char* s, char* result, int maxlen) {
    if (!s) {
        result[0] = '\0';
        return;
    }
    
    int len = strlen(s);
    if (len >= maxlen) len = maxlen - 1;
    
    for (int i = 0; i < len; i++) {
        result[i] = toupper(s[i]);
    }
    result[len] = '\0';
}

/* ============================================================================
 * Type Conversion Functions
 * ============================================================================ */

void pas_inttostr(int i, char* result, int maxlen) {
    snprintf(result, maxlen, "%d", i);
}

int pas_strtoint(const char* s) {
    return s ? atoi(s) : 0;
}

void pas_floattostr(double f, char* result, int maxlen) {
    snprintf(result, maxlen, "%.6f", f);
}

double pas_strtofloat(const char* s) {
    return s ? atof(s) : 0.0;
}

char pas_chr(int i) {
    return (char)i;
}

int pas_ord(char c) {
    return (int)c;
}

/* ============================================================================
 * Math Functions
 * ============================================================================ */

int pas_abs_int(int x) {
    return abs(x);
}

double pas_abs_real(double x) {
    return fabs(x);
}

int pas_sqr_int(int x) {
    return x * x;
}

double pas_sqr_real(double x) {
    return x * x;
}

double pas_sqrt(double x) {
    return sqrt(x);
}

double pas_sin(double x) {
    return sin(x);
}

double pas_cos(double x) {
    return cos(x);
}

double pas_tan(double x) {
    return tan(x);
}

double pas_arctan(double x) {
    return atan(x);
}

double pas_ln(double x) {
    return log(x);
}

double pas_exp(double x) {
    return exp(x);
}

int pas_round(double x) {
    return (int)round(x);
}

int pas_trunc(double x) {
    return (int)trunc(x);
}

double pas_frac(double x) {
    return x - trunc(x);
}

double pas_int(double x) {
    return trunc(x);
}

/* ============================================================================
 * Memory Management Functions
 * ============================================================================ */

void pas_new(void** p, size_t size) {
    *p = malloc(size);
}

void pas_dispose(void** p) {
    if (p && *p) {
        free(*p);
        *p = NULL;
    }
}

void pas_getmem(void** p, size_t size) {
    *p = malloc(size);
}

void pas_freemem(void** p) {
    if (p && *p) {
        free(*p);
        *p = NULL;
    }
}

/* ============================================================================
 * Program Control Functions
 * ============================================================================ */

void pas_halt(void) {
    exit(0);
}

void pas_halt_code(int exitCode) {
    exit(exitCode);
}

/* ============================================================================
 * Global Variables for argc/argv
 * ============================================================================ */

void pas_init_runtime(int argc, char** argv) {
    pas_argc = argc;
    pas_argv = argv;
}

int pas_paramcount(void) {
    return pas_argc - 1;
}

void pas_paramstr(int index, char* result, int maxlen) {
    if (index >= 0 && index < pas_argc) {
        snprintf(result, maxlen, "%s", pas_argv[index]);
    } else {
        result[0] = '\0';
    }
}

int pas_paramcount() {
    return pas_argc - 1;
}

char* pas_paramstr(int i) {
    if (i >= 0 && i < pas_argc) {
        return pas_argv[i];
    }
    return "";
}

void pas_halt() {
    exit(0);
}

/* ============================================================================
 * File Operations
 * ============================================================================ */

typedef struct {
    FILE* handle;
    char filename[256];
    int mode; // 0=closed, 1=read, 2=write
} PascalFile;

void pas_assign(PascalFile* f, const char* name) {
    strncpy(f->filename, name, sizeof(f->filename) - 1);
    f->filename[sizeof(f->filename) - 1] = '\0';
    f->handle = NULL;
    f->mode = 0;
}

void pas_reset(PascalFile* f) {
    f->handle = fopen(f->filename, "r");
    f->mode = f->handle ? 1 : 0;
}

void pas_rewrite(PascalFile* f) {
    f->handle = fopen(f->filename, "w");
    f->mode = f->handle ? 2 : 0;
}

void pas_close(PascalFile* f) {
    if (f->handle) {
        fclose(f->handle);
        f->handle = NULL;
        f->mode = 0;
    }
}

int pas_eof(PascalFile* f) {
    return f->handle ? feof(f->handle) : 1;
}

int pas_eoln(PascalFile* f) {
    if (!f->handle) return 1;
    int c = fgetc(f->handle);
    if (c == EOF) return 1;
    ungetc(c, f->handle);
    return c == '\n';
}

/* ============================================================================
 * Date/Time Functions
 * ============================================================================ */

double pas_now(void) {
    time_t t = time(NULL);
    return (double)t / 86400.0 + 25569.0; // Days since 1899-12-30
}

double pas_date(void) {
    return trunc(pas_now());
}

double pas_time(void) {
    return pas_frac(pas_now());
}

void pas_datetostr(double d, char* result, int maxlen) {
    time_t t = (time_t)((d - 25569.0) * 86400.0);
    struct tm* tm_info = localtime(&t);
    strftime(result, maxlen, "%Y-%m-%d", tm_info);
}

void pas_timetostr(double t, char* result, int maxlen) {
    int seconds = (int)(t * 86400.0);
    int hours = seconds / 3600;
    int minutes = (seconds % 3600) / 60;
    int secs = seconds % 60;
    snprintf(result, maxlen, "%02d:%02d:%02d", hours, minutes, secs);
}

void pas_datetimetostr(double dt, char* result, int maxlen) {
    char date[32], time[32];
    pas_datetostr(dt, date, sizeof(date));
    pas_timetostr(pas_frac(dt), time, sizeof(time));
    snprintf(result, maxlen, "%s %s", date, time);
}

/* ============================================================================
 * Boolean Functions
 * ============================================================================ */

int pas_odd(int x) {
    return x % 2 != 0;
}

int pas_succ_int(int x) {
    return x + 1;
}

char pas_succ_char(char c) {
    return c + 1;
}

int pas_pred_int(int x) {
    return x - 1;
}

char pas_pred_char(char c) {
    return c - 1;
}

/* ============================================================================
 * Min/Max Functions
 * ============================================================================ */

int pas_min_int(int a, int b) {
    return a < b ? a : b;
}

double pas_min_real(double a, double b) {
    return a < b ? a : b;
}

int pas_max_int(int a, int b) {
    return a > b ? a : b;
}

double pas_max_real(double a, double b) {
    return a > b ? a : b;
}

/* ============================================================================
 * File System Functions (SysUtils)
 * ============================================================================ */

int pas_fileexists(const char* filename) {
    struct stat st;
    return stat(filename, &st) == 0 && S_ISREG(st.st_mode);
}

int pas_directoryexists(const char* dirname) {
    struct stat st;
    return stat(dirname, &st) == 0 && S_ISDIR(st.st_mode);
}

int pas_deletefile(const char* filename) {
    return unlink(filename) == 0;
}

int pas_renamefile(const char* oldname, const char* newname) {
    return rename(oldname, newname) == 0;
}

void pas_getcurrentdir(char* result, int maxlen) {
    if (getcwd(result, maxlen) == NULL) {
        result[0] = '\0';
    }
}

int pas_setcurrentdir(const char* dir) {
    return chdir(dir) == 0;
}

int pas_createdir(const char* dir) {
    return mkdir(dir, 0755) == 0;
}

int pas_removedir(const char* dir) {
    return rmdir(dir) == 0;
}

/* ============================================================================
 * Random Functions
 * ============================================================================ */

static int random_initialized = 0;

double pas_random(void) {
    if (!random_initialized) {
        srand(time(NULL));
        random_initialized = 1;
    }
    return (double)rand() / RAND_MAX;
}

int pas_random_range(int range) {
    if (!random_initialized) {
        srand(time(NULL));
        random_initialized = 1;
    }
    return rand() % range;
}

void pas_randomize(void) {
    srand(time(NULL));
    random_initialized = 1;
}

/* ============================================================================
 * Utility Functions
 * ============================================================================ */

void pas_sleep(int milliseconds) {
    usleep(milliseconds * 1000);
}

unsigned int pas_gettickcount(void) {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (unsigned int)(ts.tv_sec * 1000 + ts.tv_nsec / 1000000);
}

/* ============================================================================
 * Memory Utility Functions
 * ============================================================================ */

void pas_fillchar(void* x, int count, unsigned char value) {
    memset(x, value, count);
}

void pas_move(const void* source, void* dest, int count) {
    memmove(dest, source, count);
}

int pas_comparemem(const void* buf1, const void* buf2, int count) {
    return memcmp(buf1, buf2, count) == 0;
}

/* ============================================================================
 * Advanced Math Functions
 * ============================================================================ */

double pas_power(double base, double exponent) {
    return pow(base, exponent);
}

double pas_log10(double x) {
    return log10(x);
}

double pas_log2(double x) {
    return log2(x);
}

int pas_ceil(double x) {
    return (int)ceil(x);
}

int pas_floor(double x) {
    return (int)floor(x);
}

double pas_arcsin(double x) {
    return asin(x);
}

double pas_arccos(double x) {
    return acos(x);
}

double pas_arctan2(double y, double x) {
    return atan2(y, x);
}

/* ============================================================================
 * Global Variables for argc/argv
 * ============================================================================ */

int pas_strlen(const char* s) { return strlen(s); }

char* pas_strcopy(const char* s, int index, int count) {
    char* result = malloc(count + 1);
    strncpy(result, s + index - 1, count);
    result[count] = '\0';
    return result;
}

int pas_strpos(const char* sub, const char* s) {
    char* p = strstr(s, sub);
    return p ? (p - s) + 1 : 0;
}
