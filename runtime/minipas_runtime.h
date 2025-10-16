/**
 * MiniPAS Runtime Library Header
 * 
 * External declarations for the MiniPAS runtime library functions.
 */

#ifndef MINIPAS_RUNTIME_H
#define MINIPAS_RUNTIME_H

#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * I/O Functions
 * ============================================================================ */

void pas_write(const char* s);
void pas_writeln(const char* s);
void pas_read(char* c);
void pas_readln(char* buffer, int maxlen);

/* ============================================================================
 * String Functions
 * ============================================================================ */

int pas_length(const char* s);
void pas_copy(const char* s, int index, int count, char* result, int maxlen);
void pas_concat(const char* s1, const char* s2, char* result, int maxlen);
int pas_pos(const char* substr, const char* s);
char pas_upcase(char c);
void pas_lowercase(const char* s, char* result, int maxlen);
void pas_uppercase(const char* s, char* result, int maxlen);

/* ============================================================================
 * Type Conversion Functions
 * ============================================================================ */

void pas_inttostr(int i, char* result, int maxlen);
int pas_strtoint(const char* s);
void pas_floattostr(double f, char* result, int maxlen);
double pas_strtofloat(const char* s);
char pas_chr(int i);
int pas_ord(char c);

/* ============================================================================
 * Math Functions
 * ============================================================================ */

int pas_abs_int(int x);
double pas_abs_real(double x);
int pas_sqr_int(int x);
double pas_sqr_real(double x);
double pas_sqrt(double x);
double pas_sin(double x);
double pas_cos(double x);
double pas_tan(double x);
double pas_arctan(double x);
double pas_arcsin(double x);
double pas_arccos(double x);
double pas_arctan2(double y, double x);
double pas_ln(double x);
double pas_exp(double x);
int pas_round(double x);
int pas_trunc(double x);
double pas_frac(double x);
double pas_int(double x);

/* ============================================================================
 * Memory Management Functions
 * ============================================================================ */

void pas_new(void** p, size_t size);
void pas_dispose(void** p);
void pas_getmem(void** p, size_t size);
void pas_freemem(void** p);

/* ============================================================================
 * Program Control Functions
 * ============================================================================ */

void pas_halt(void);
void pas_halt_code(int exitCode);
int pas_paramcount(void);
void pas_paramstr(int index, char* result, int maxlen);

/* ============================================================================
 * File Operations
 * ============================================================================ */

typedef struct {
    void* handle;
    char filename[256];
    int mode;
} PascalFile;

void pas_assign(PascalFile* f, const char* name);
void pas_reset(PascalFile* f);
void pas_rewrite(PascalFile* f);
void pas_close(PascalFile* f);
int pas_eof(PascalFile* f);
int pas_eoln(PascalFile* f);

/* ============================================================================
 * Date/Time Functions
 * ============================================================================ */

double pas_now(void);
double pas_date(void);
double pas_time(void);
void pas_datetostr(double d, char* result, int maxlen);
void pas_timetostr(double t, char* result, int maxlen);
void pas_datetimetostr(double dt, char* result, int maxlen);

/* ============================================================================
 * Boolean Functions
 * ============================================================================ */

int pas_odd(int x);
int pas_succ_int(int x);
char pas_succ_char(char c);
int pas_pred_int(int x);
char pas_pred_char(char c);

/* ============================================================================
 * Min/Max Functions
 * ============================================================================ */

int pas_min_int(int a, int b);
double pas_min_real(double a, double b);
int pas_max_int(int a, int b);
double pas_max_real(double a, double b);

/* ============================================================================
 * File System Functions
 * ============================================================================ */

int pas_fileexists(const char* filename);
int pas_directoryexists(const char* dirname);
int pas_deletefile(const char* filename);
int pas_renamefile(const char* oldname, const char* newname);
void pas_getcurrentdir(char* result, int maxlen);
int pas_setcurrentdir(const char* dir);
int pas_createdir(const char* dir);
int pas_removedir(const char* dir);

/* ============================================================================
 * Random Functions
 * ============================================================================ */

double pas_random(void);
int pas_random_range(int range);
void pas_randomize(void);

/* ============================================================================
 * Utility Functions
 * ============================================================================ */

void pas_sleep(int milliseconds);
unsigned int pas_gettickcount(void);

/* ============================================================================
 * Memory Utility Functions
 * ============================================================================ */

void pas_fillchar(void* x, int count, unsigned char value);
void pas_move(const void* source, void* dest, int count);
int pas_comparemem(const void* buf1, const void* buf2, int count);

/* ============================================================================
 * Advanced Math Functions
 * ============================================================================ */

double pas_power(double base, double exponent);
double pas_log10(double x);
double pas_log2(double x);
int pas_ceil(double x);
int pas_floor(double x);

/* ============================================================================
 * Runtime Initialization
 * ============================================================================ */

void pas_init_runtime(int argc, char** argv);

#ifdef __cplusplus
}
#endif

#endif /* MINIPAS_RUNTIME_H */
