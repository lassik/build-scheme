/// = Desert Island Scheme
/// :toc: macro
/// :toc-title:
///
/// toc::[]
///
/// == Introduction
///
/// *Desert Island Scheme* is a self-contained implementation of a
/// useful version of the Scheme programming language.
///
/// The language is a subset of R7RS extended with parts of many
/// SRFIs. Instead of inventing custom procedures just for this
/// interpreter, we stick to SRFIs so your code can run with few
/// modifications in other Scheme implementations.
///
/// The interpreter is self-contained in one C file which compiles
/// instantly. It's a derivative of the venerable Mini-Scheme via
/// TinyScheme.
///
/// The implementation is also self-documenting. Filtering the source
/// file for lines starting with `///` produces a user's manual in
/// AsciiDoc format.
///
/// === Lineage
///
/// - Mini-Scheme by Atsushi Moriwaki / kurims.kyoto-u.ac.jp (11/5/1989)
/// - Mini-Scheme modified by R.C. Secrist
/// - Mini-Scheme modified by Akira Kida (version 0.85k4, 1994-05-15)
/// - TinyScheme 1.41 by Dimitrios Souflis, Kevin Cozens, Jonathan S. Shapiro
/// - Desert Island Scheme by Lassi Kortela
///

#ifndef SCHEME_VERSION
#define SCHEME_VERSION ""
#endif

#ifndef SCHEME_BUILD_DATE
#define SCHEME_BUILD_DATE ""
#endif

#ifdef __unix__
#define SCHEME_UNIX
#endif

#ifdef __APPLE__
#ifdef __MACH__
#define SCHEME_UNIX
#define st_mtim st_mtimespec
#endif
#endif

#ifdef _WIN32
#define SCHEME_WINDOWS
#endif

//

#ifdef SCHEME_WINDOWS
#define UNICODE
#define _UNICODE
#endif

#ifdef SCHEME_UNIX
#include <sys/types.h>
#endif

#ifdef SCHEME_UNIX
#include <sys/stat.h>
#endif

#ifdef SCHEME_UNIX
#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <pwd.h>
#include <unistd.h>
#endif

#ifdef SCHEME_WINDOWS
#include <fcntl.h>
#include <io.h>
#include <windows.h>
#endif

#include <ctype.h>
#include <float.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#ifdef SCHEME_WINDOWS
#define snprintf _snprintf
#endif

#ifdef __BORLANDC__
#define _setmode setmode
#endif

#ifdef SCHEME_UNIX
extern char **environ;
#endif

//

/*
 * Leave it defined if you want continuations, and also for the Sharp Zaurus.
 * Undefine it if you only care about faster speed and not strict Scheme
 * compatibility.
 */
#define USE_SCHEME_STACK

#ifndef USE_TRACING
#define USE_TRACING 1
#endif

/* To force system errors through user-defined error handling (see
 * *error-hook*) */
#ifndef USE_ERROR_HOOK
#define USE_ERROR_HOOK 1
#endif

#ifndef SHOW_ERROR_LINE /* Show error line in file */
#define SHOW_ERROR_LINE 1
#endif

typedef struct scheme scheme;
typedef struct cell *pointer;

typedef void *(*func_alloc)(size_t);
typedef void (*func_dealloc)(void *);

/* num, for generic arithmetic */
typedef struct num {
    char is_fixnum;
    union {
        long ivalue;
        double rvalue;
    } value;
} num;

int scheme_init(scheme *sc);
int scheme_init_custom_alloc(scheme *sc, func_alloc, func_dealloc);
void scheme_deinit(scheme *sc);
void scheme_set_input_port_file(scheme *sc, FILE *fin);
void scheme_set_input_port_string(
    scheme *sc, char *start, char *past_the_end);
void scheme_set_output_port_file(scheme *sc, FILE *fin);
void scheme_set_output_port_string(
    scheme *sc, char *start, char *past_the_end);
void scheme_define(scheme *sc, pointer env, pointer symbol, pointer value);

pointer _cons(scheme *sc, pointer a, pointer b, int immutable);
pointer mk_integer(scheme *sc, long num);
pointer mk_real(scheme *sc, double num);
pointer mk_symbol(scheme *sc, const char *name);
pointer mk_string(scheme *sc, const char *str);
pointer mk_counted_string(scheme *sc, const char *str, int len);
pointer mk_empty_string(scheme *sc, int len, char fill);
pointer mk_character(scheme *sc, int c);
void putstr(scheme *sc, const char *s);
int list_length(scheme *sc, pointer a);
int eqv(pointer a, pointer b);
static void os_close_directory_list(pointer obj);

enum scheme_port_kind {
    port_free = 0,
    port_file = 1,
    port_string = 2,
    port_srfi6 = 4,
    port_input = 16,
    port_output = 32,
    port_saw_EOF = 64
};

typedef struct port {
    unsigned char kind;
    union {
        struct {
            FILE *file;
            int closeit;
#if SHOW_ERROR_LINE
            int curr_line;
            char *filename;
#endif
        } stdio;
        struct {
            char *start;
            char *past_the_end;
            char *curr;
        } string;
    } rep;
} port;

struct user_info {
    long uid;
    long gid;
    pointer name;
    pointer home_dir;
    pointer shell;
    pointer full_name;
    pointer parsed_full_name;
};

/* cell structure */
struct cell {
    unsigned int _flag;
    union {
        struct {
            char *_svalue;
            int _length;
        } _string;
        num _number;
        port *_port;
        struct {
            struct cell *_car;
            struct cell *_cdr;
        } _cons;
        struct {
            long sec;
            long nsec;
        } _timespec;
        struct {
            void *_p;
        } _opaque;
    } _object;
};

struct scheme {
    /* arrays for segments */
    func_alloc malloc;
    func_dealloc free;

    /* return code */
    int retcode;
    int tracing;

#define CELL_SEGSIZE 5000 /* # of cells in one segment */
#define CELL_NSEGMENT 10 /* # of segments for cells */
    char *alloc_seg[CELL_NSEGMENT];
    pointer cell_seg[CELL_NSEGMENT];
    int last_cell_seg;

    /* We use 4 registers. */
    pointer args; /* register for arguments of function */
    pointer envir; /* stack register for current environment */
    pointer code; /* register for current code */
    pointer dump; /* stack register for next evaluation */

    int interactive_repl; /* are we in an interactive REPL? */

    struct cell _sink;
    pointer sink; /* when mem. alloc. fails */
    struct cell _NIL;
    pointer NIL; /* special cell representing empty cell */
    struct cell _HASHT;
    pointer T; /* special cell representing #t */
    struct cell _HASHF;
    pointer F; /* special cell representing #f */
    struct cell _EOF_OBJ;
    pointer EOF_OBJ; /* special cell representing end-of-file object */
    pointer oblist; /* pointer to symbol table */
    pointer global_env; /* pointer to global environment */
    pointer c_nest; /* stack for nested calls from C */

    /* global pointers to special symbols */
    pointer LAMBDA; /* pointer to syntax lambda */
    pointer QUOTE; /* pointer to syntax quote */

    pointer QQUOTE; /* pointer to symbol quasiquote */
    pointer UNQUOTE; /* pointer to symbol unquote */
    pointer UNQUOTESP; /* pointer to symbol unquote-splicing */
    pointer FEED_TO; /* => */
    pointer ERROR_HOOK; /* *error-hook* */
    pointer COMPILE_HOOK; /* *compile-hook* */

    pointer free_cell; /* pointer to top of free cells */
    long fcells; /* # of free cells */

    pointer inport;
    pointer outport;
    pointer save_inport;
    pointer loadport;

#define MAXFIL 64
    port load_stack[MAXFIL]; /* Stack of open files for port -1 (LOADing) */
    int nesting_stack[MAXFIL];
    int file_i;
    int nesting;

    char gc_verbose; /* if gc_verbose is not zero, print gc status */
    char no_memory; /* Whether mem. alloc. has failed */

#define LINESIZE 1024
    char linebuff[LINESIZE];
#define STRBUFFSIZE 256
    char strbuff[STRBUFFSIZE];

    FILE *tmpfp;
    int tok;
    int print_flag;
    pointer value;
    int op;

    long gensym_cnt;

    struct scheme_interface *vptr;
    void *dump_base; /* pointer to base of allocated dump stack */
    int dump_size; /* number of frames allocated for dump stack */
};

static scheme *sc;
static const char *prompt = "discheme> ";
static const char *features_as_strings[] = { "discheme", 0 };
static pointer g_features;
static pointer g_command_line;

/* operator code */
enum scheme_opcodes {
#define _OP_DEF(A, B, C, D, E, OP) OP,
#include "opdefines.h"
    OP_MAXDEFINED
};

#define cons(sc, a, b) _cons(sc, a, b, 0)
#define immutable_cons(sc, a, b) _cons(sc, a, b, 1)

int is_string(pointer p);
char *string_value(pointer p);
int is_number(pointer p);
num nvalue(pointer p);
long ivalue(pointer p);
double rvalue(pointer p);
int is_integer(pointer p);
int is_real(pointer p);
int is_character(pointer p);
long charvalue(pointer p);
int is_vector(pointer p);

int is_port(pointer p);

int is_pair(pointer p);
pointer pair_car(pointer p);
pointer pair_cdr(pointer p);
pointer set_car(pointer p, pointer q);
pointer set_cdr(pointer p, pointer q);

int is_symbol(pointer p);
char *symname(pointer p);
int hasprop(pointer p);

int is_syntax(pointer p);
int is_proc(pointer p);
char *syntaxname(pointer p);
int is_closure(pointer p);
#ifdef USE_MACRO
int is_macro(pointer p);
#endif
pointer closure_code(pointer p);
pointer closure_env(pointer p);

int is_continuation(pointer p);
int is_promise(pointer p);
int is_environment(pointer p);
int is_immutable(pointer p);
void setimmutable(pointer p);

#define TOK_EOF (-1)
#define TOK_LPAREN 0
#define TOK_RPAREN 1
#define TOK_DOT 2
#define TOK_ATOM 3
#define TOK_QUOTE 4
#define TOK_COMMENT 5
#define TOK_DQUOTE 6
#define TOK_BQUOTE 7
#define TOK_COMMA 8
#define TOK_ATMARK 9
#define TOK_SHARP 10
#define TOK_SHARP_CONST 11
#define TOK_VEC 12

#define BACKQUOTE '`'
#define DELIMITERS "()\";\f\t\v\n\r "

static int our_stricmp(const char *s1, const char *s2)
{
    unsigned char c1, c2;
    do {
        c1 = tolower(*s1);
        c2 = tolower(*s2);
        if (c1 < c2)
            return -1;
        else if (c1 > c2)
            return 1;
        s1++, s2++;
    } while (c1 != 0);
    return 0;
}

static const char *our_strlwr(char *s)
{
    const char *p = s;
    while (*s) {
        *s = tolower(*s);
        s++;
    }
    return p;
}

#ifndef FIRST_CELLSEGS
#define FIRST_CELLSEGS 3
#endif

enum scheme_types {
    T_STRING = 1,
    T_NUMBER = 2,
    T_SYMBOL = 3,
    T_PROC = 4,
    T_PAIR = 5,
    T_CLOSURE = 6,
    T_CONTINUATION = 7,
    T_CHARACTER = 8,
    T_PORT = 9,
    T_VECTOR = 10,
    T_MACRO = 11,
    T_PROMISE = 12,
    T_ENVIRONMENT = 13,
    T_FILE_INFO = 14,
    T_USER_INFO = 15,
    T_TIMESPEC = 16,
    T_DIRECTORY_LIST = 17,
    T_LAST_SYSTEM_TYPE = 17
};

/* ADJ is enough slack to align cells in a TYPE_BITS-bit boundary */
#define ADJ 32
#define TYPE_BITS 5
#define T_MASKTYPE 31 /* 0000000000011111 */
#define T_SYNTAX 4096 /* 0001000000000000 */
#define T_IMMUTABLE 8192 /* 0010000000000000 */
#define T_ATOM 16384 /* 0100000000000000 */ /* only for gc */
#define CLRATOM 49151 /* 1011111111111111 */ /* only for gc */
#define MARK 32768 /* 1000000000000000 */
#define UNMARK 32767 /* 0111111111111111 */

static num num_add(num a, num b);
static num num_mul(num a, num b);
static num num_div(num a, num b);
static num num_intdiv(num a, num b);
static num num_sub(num a, num b);
static num num_rem(num a, num b);
static num num_mod(num a, num b);
static int num_eq(num a, num b);
static int num_gt(num a, num b);
static int num_ge(num a, num b);
static int num_lt(num a, num b);
static int num_le(num a, num b);

static void die(const char *msg)
{
    fprintf(stderr, "%s\n", msg);
    exit(2);
}

static int is_zero_double(double x);
static int num_is_integer(pointer p)
{
    return ((p)->_object._number.is_fixnum);
}

static num num_zero;
static num num_one;

/* macros for cell operations */
#define typeflag(p) ((p)->_flag)
#define type(p) (typeflag(p) & T_MASKTYPE)

static int is_boolean(pointer p) { return (p == sc->F) || (p == sc->T); }

int is_string(pointer p) { return (type(p) == T_STRING); }
#define strvalue(p) ((p)->_object._string._svalue)
#define strlength(p) ((p)->_object._string._length)

static int is_list(pointer p);
int is_vector(pointer p) { return (type(p) == T_VECTOR); }
static void fill_vector(pointer vec, pointer obj);
static pointer vector_elem(pointer vec, int ielem);
static pointer set_vector_elem(pointer vec, int ielem, pointer a);
int is_number(pointer p) { return (type(p) == T_NUMBER); }
int is_integer(pointer p)
{
    if (!is_number(p))
        return 0;
    if (num_is_integer(p) || (double)ivalue(p) == rvalue(p))
        return 1;
    return 0;
}

int is_real(pointer p)
{
    return is_number(p) && (!(p)->_object._number.is_fixnum);
}

int is_character(pointer p) { return (type(p) == T_CHARACTER); }
char *string_value(pointer p) { return strvalue(p); }
num nvalue(pointer p) { return ((p)->_object._number); }
long ivalue(pointer p)
{
    return (num_is_integer(p) ? (p)->_object._number.value.ivalue
                              : (long)(p)->_object._number.value.rvalue);
}
double rvalue(pointer p)
{
    return (!num_is_integer(p) ? (p)->_object._number.value.rvalue
                               : (double)(p)->_object._number.value.ivalue);
}
#define ivalue_unchecked(p) ((p)->_object._number.value.ivalue)
#define rvalue_unchecked(p) ((p)->_object._number.value.rvalue)
#define set_num_integer(p) (p)->_object._number.is_fixnum = 1;
#define set_num_real(p) (p)->_object._number.is_fixnum = 0;
long charvalue(pointer p) { return ivalue_unchecked(p); }

int is_port(pointer p) { return (type(p) == T_PORT); }
int is_inport(pointer p)
{
    return is_port(p) && p->_object._port->kind & port_input;
}
int is_outport(pointer p)
{
    return is_port(p) && p->_object._port->kind & port_output;
}

int is_file_info(pointer p) { return type(p) == T_FILE_INFO; }
int is_user_info(pointer p) { return type(p) == T_USER_INFO; }
int is_directory_list(pointer p) { return type(p) == T_DIRECTORY_LIST; }

int is_timespec(pointer p) { return type(p) == T_TIMESPEC; }
long timespec_sec(pointer p) { return p->_object._timespec.sec; }
long timespec_nsec(pointer p) { return p->_object._timespec.nsec; }

int is_pair(pointer p) { return (type(p) == T_PAIR); }
#define car(p) ((p)->_object._cons._car)
#define cdr(p) ((p)->_object._cons._cdr)
pointer pair_car(pointer p) { return car(p); }
pointer pair_cdr(pointer p) { return cdr(p); }
pointer set_car(pointer p, pointer q) { return car(p) = q; }
pointer set_cdr(pointer p, pointer q) { return cdr(p) = q; }

int is_symbol(pointer p) { return (type(p) == T_SYMBOL); }
char *symname(pointer p) { return strvalue(car(p)); }

int is_syntax(pointer p) { return (typeflag(p) & T_SYNTAX); }
int is_proc(pointer p) { return (type(p) == T_PROC); }
char *syntaxname(pointer p) { return strvalue(car(p)); }
#define procnum(p) ivalue(p)
static const char *procname(pointer x);

int is_closure(pointer p) { return (type(p) == T_CLOSURE); }
int is_macro(pointer p) { return (type(p) == T_MACRO); }
pointer closure_code(pointer p) { return car(p); }
pointer closure_env(pointer p) { return cdr(p); }

int is_continuation(pointer p) { return (type(p) == T_CONTINUATION); }
#define cont_dump(p) cdr(p)

/* To do: promise should be forced ONCE only */
int is_promise(pointer p) { return (type(p) == T_PROMISE); }

static int is_callable(pointer p)
{
    return is_proc(p) || is_closure(p) || is_continuation(p);
}

int is_environment(pointer p) { return (type(p) == T_ENVIRONMENT); }
#define setenvironment(p) typeflag(p) = T_ENVIRONMENT

#define is_atom(p) (typeflag(p) & T_ATOM)
#define setatom(p) typeflag(p) |= T_ATOM
#define clratom(p) typeflag(p) &= CLRATOM

#define is_mark(p) (typeflag(p) & MARK)
#define setmark(p) typeflag(p) |= MARK
#define clrmark(p) typeflag(p) &= UNMARK

int is_immutable(pointer p) { return (typeflag(p) & T_IMMUTABLE); }
/*#define setimmutable(p)  typeflag(p) |= T_IMMUTABLE*/
void setimmutable(pointer p) { typeflag(p) |= T_IMMUTABLE; }

#define caar(p) car(car(p))
#define cadr(p) car(cdr(p))
#define cdar(p) cdr(car(p))
#define cddr(p) cdr(cdr(p))
#define cadar(p) car(cdr(car(p)))
#define caddr(p) car(cdr(cdr(p)))
#define cdaar(p) cdr(car(car(p)))
#define cadaar(p) car(cdr(car(car(p))))
#define cadddr(p) car(cdr(cdr(cdr(p))))
#define cddddr(p) cdr(cdr(cdr(cdr(p))))

static int file_push(scheme *sc, const char *fname);
static void file_pop(scheme *sc);
static int file_interactive(scheme *sc);
static int is_one_of(char *s, int c);
static int alloc_cellseg(scheme *sc, int n);
static long binary_decode(const char *s);
static pointer get_cell(scheme *sc, pointer a, pointer b);
static pointer _get_cell(scheme *sc, pointer a, pointer b);
static pointer get_consecutive_cells(scheme *sc, int n);
static pointer find_consecutive_cells(scheme *sc, int n);
static void finalize_cell(scheme *sc, pointer a);
static int count_consecutive_cells(pointer x, int needed);
static pointer find_slot_in_env(
    scheme *sc, pointer env, pointer sym, int all);
static pointer mk_number(scheme *sc, num n);
static char *store_string(scheme *sc, int len, const char *str, char fill);
static pointer mk_vector(scheme *sc, int len);
static pointer mk_port(scheme *sc, port *p);
static pointer port_from_filename(scheme *sc, const char *fn, int prop);
static pointer port_from_file(scheme *sc, FILE *, int prop);
static pointer port_from_string(
    scheme *sc, char *start, char *past_the_end, int prop);
static port *port_rep_from_filename(scheme *sc, const char *fn, int prop);
static port *port_rep_from_file(scheme *sc, FILE *, int prop);
static port *port_rep_from_string(
    scheme *sc, char *start, char *past_the_end, int prop);
static void port_close(scheme *sc, pointer p, int flag);
static void mark(pointer a);
static void gc(scheme *sc, pointer a, pointer b);
static int basic_inchar(port *pt);
static int inchar(scheme *sc);
static void backchar(scheme *sc, int c);
static char *readstr_upto(scheme *sc, char *delim);
static pointer readstrexp(scheme *sc);
static int skipspace(scheme *sc);
static int token(scheme *sc);
static void printslashstring(scheme *sc, char *s, int len);
static void atom2str(scheme *sc, pointer l, int f, char **pp, int *plen);
static void printatom(scheme *sc, pointer l, int f);
static pointer mk_proc(scheme *sc, enum scheme_opcodes op);
static pointer mk_closure(scheme *sc, pointer c, pointer e);
static pointer mk_continuation(scheme *sc, pointer d);
static pointer reverse_in_place(scheme *sc, pointer term, pointer list);
static pointer revappend(scheme *sc, pointer a, pointer b);
static void dump_stack_mark(scheme *);
static pointer opexe_0(scheme *sc, enum scheme_opcodes op);
static pointer opexe_1(scheme *sc, enum scheme_opcodes op);
static pointer opexe_2(scheme *sc, enum scheme_opcodes op);
static pointer opexe_4(scheme *sc, enum scheme_opcodes op);
static pointer opexe_5(scheme *sc, enum scheme_opcodes op);
static pointer opexe_6(scheme *sc, enum scheme_opcodes op);
static void Eval_Cycle(scheme *sc, enum scheme_opcodes op);
static void assign_syntax(scheme *sc, char *name);
static int syntaxnum(pointer p);
static void assign_proc(scheme *sc, enum scheme_opcodes, const char *name);

#define num_ivalue(n)                                                        \
    (n.is_fixnum ? (n).value.ivalue : (long)(n).value.rvalue)
#define num_rvalue(n)                                                        \
    (!n.is_fixnum ? (n).value.rvalue : (double)(n).value.ivalue)

static num num_add(num a, num b)
{
    num ret;
    ret.is_fixnum = a.is_fixnum && b.is_fixnum;
    if (ret.is_fixnum) {
        ret.value.ivalue = a.value.ivalue + b.value.ivalue;
    } else {
        ret.value.rvalue = num_rvalue(a) + num_rvalue(b);
    }
    return ret;
}

static num num_mul(num a, num b)
{
    num ret;
    ret.is_fixnum = a.is_fixnum && b.is_fixnum;
    if (ret.is_fixnum) {
        ret.value.ivalue = a.value.ivalue * b.value.ivalue;
    } else {
        ret.value.rvalue = num_rvalue(a) * num_rvalue(b);
    }
    return ret;
}

static num num_div(num a, num b)
{
    num ret;
    ret.is_fixnum
        = a.is_fixnum && b.is_fixnum && a.value.ivalue % b.value.ivalue == 0;
    if (ret.is_fixnum) {
        ret.value.ivalue = a.value.ivalue / b.value.ivalue;
    } else {
        ret.value.rvalue = num_rvalue(a) / num_rvalue(b);
    }
    return ret;
}

static num num_intdiv(num a, num b)
{
    num ret;
    ret.is_fixnum = a.is_fixnum && b.is_fixnum;
    if (ret.is_fixnum) {
        ret.value.ivalue = a.value.ivalue / b.value.ivalue;
    } else {
        ret.value.rvalue = num_rvalue(a) / num_rvalue(b);
    }
    return ret;
}

static num num_sub(num a, num b)
{
    num ret;
    ret.is_fixnum = a.is_fixnum && b.is_fixnum;
    if (ret.is_fixnum) {
        ret.value.ivalue = a.value.ivalue - b.value.ivalue;
    } else {
        ret.value.rvalue = num_rvalue(a) - num_rvalue(b);
    }
    return ret;
}

static num num_rem(num a, num b)
{
    num ret;
    long e1, e2, res;
    ret.is_fixnum = a.is_fixnum && b.is_fixnum;
    e1 = num_ivalue(a);
    e2 = num_ivalue(b);
    res = e1 % e2;
    /* remainder should have same sign as second operand */
    if (res > 0) {
        if (e1 < 0) {
            res -= labs(e2);
        }
    } else if (res < 0) {
        if (e1 > 0) {
            res += labs(e2);
        }
    }
    ret.value.ivalue = res;
    return ret;
}

static num num_mod(num a, num b)
{
    num ret;
    long e1, e2, res;
    ret.is_fixnum = a.is_fixnum && b.is_fixnum;
    e1 = num_ivalue(a);
    e2 = num_ivalue(b);
    res = e1 % e2;
    /* modulo should have same sign as second operand */
    if (res * e2 < 0) {
        res += e2;
    }
    ret.value.ivalue = res;
    return ret;
}

static int num_eq(num a, num b)
{
    int ret;
    int is_fixnum = a.is_fixnum && b.is_fixnum;
    if (is_fixnum) {
        ret = a.value.ivalue == b.value.ivalue;
    } else {
        ret = num_rvalue(a) == num_rvalue(b);
    }
    return ret;
}

static int num_gt(num a, num b)
{
    int ret;
    int is_fixnum = a.is_fixnum && b.is_fixnum;
    if (is_fixnum) {
        ret = a.value.ivalue > b.value.ivalue;
    } else {
        ret = num_rvalue(a) > num_rvalue(b);
    }
    return ret;
}

static int num_ge(num a, num b) { return !num_lt(a, b); }

static int num_lt(num a, num b)
{
    int ret;
    int is_fixnum = a.is_fixnum && b.is_fixnum;
    if (is_fixnum) {
        ret = a.value.ivalue < b.value.ivalue;
    } else {
        ret = num_rvalue(a) < num_rvalue(b);
    }
    return ret;
}

static int num_le(num a, num b) { return !num_gt(a, b); }

static int is_zero_double(double x) { return x < DBL_MIN && x > -DBL_MIN; }

static long binary_decode(const char *s)
{
    long x = 0;

    while (*s != 0 && (*s == '1' || *s == '0')) {
        x <<= 1;
        x += *s - '0';
        s++;
    }

    return x;
}

/* allocate new cell segment */
static int alloc_cellseg(scheme *sc, int n)
{
    pointer newp;
    pointer last;
    pointer p;
    void *cp;
    long i;
    int k;
    size_t adj = ADJ;

    if (adj < sizeof(struct cell)) {
        adj = sizeof(struct cell);
    }

    for (k = 0; k < n; k++) {
        if (sc->last_cell_seg >= CELL_NSEGMENT - 1)
            return k;
        cp = (char *)sc->malloc(CELL_SEGSIZE * sizeof(struct cell) + adj);
        if (cp == 0)
            return k;
        i = ++sc->last_cell_seg;
        sc->alloc_seg[i] = cp;
        /* adjust in TYPE_BITS-bit boundary */
        if (((unsigned long)cp) % adj != 0) {
            cp = (char *)(adj * ((unsigned long)cp / adj + 1));
        }
        /* insert new segment in address order */
        newp = (pointer)cp;
        sc->cell_seg[i] = newp;
        while (i > 0 && sc->cell_seg[i - 1] > sc->cell_seg[i]) {
            p = sc->cell_seg[i];
            sc->cell_seg[i] = sc->cell_seg[i - 1];
            sc->cell_seg[--i] = p;
        }
        sc->fcells += CELL_SEGSIZE;
        last = newp + CELL_SEGSIZE - 1;
        for (p = newp; p <= last; p++) {
            typeflag(p) = 0;
            cdr(p) = p + 1;
            car(p) = sc->NIL;
        }
        /* insert new cells in address order on free list */
        if (sc->free_cell == sc->NIL || p < sc->free_cell) {
            cdr(last) = sc->free_cell;
            sc->free_cell = newp;
        } else {
            p = sc->free_cell;
            while (cdr(p) != sc->NIL && newp > cdr(p))
                p = cdr(p);
            cdr(last) = cdr(p);
            cdr(p) = newp;
        }
    }
    return n;
}

static pointer get_cell_x(scheme *sc, pointer a, pointer b)
{
    if (sc->free_cell != sc->NIL) {
        pointer x = sc->free_cell;
        sc->free_cell = cdr(x);
        --sc->fcells;
        return (x);
    }
    return _get_cell(sc, a, b);
}

/* get new cell.  parameter a, b is marked by gc. */
static pointer _get_cell(scheme *sc, pointer a, pointer b)
{
    pointer x;

    if (sc->no_memory) {
        return sc->sink;
    }

    if (sc->free_cell == sc->NIL) {
        const int min_to_be_recovered = sc->last_cell_seg * 8;
        gc(sc, a, b);
        if (sc->fcells < min_to_be_recovered || sc->free_cell == sc->NIL) {
            /* if only a few recovered, get more to avoid fruitless gc's */
            if (!alloc_cellseg(sc, 1) && sc->free_cell == sc->NIL) {
                sc->no_memory = 1;
                return sc->sink;
            }
        }
    }
    x = sc->free_cell;
    sc->free_cell = cdr(x);
    --sc->fcells;
    return (x);
}

static pointer get_consecutive_cells(scheme *sc, int n)
{
    pointer x;

    if (sc->no_memory) {
        return sc->sink;
    }

    /* Are there any cells available? */
    x = find_consecutive_cells(sc, n);
    if (x != sc->NIL) {
        return x;
    }

    /* If not, try gc'ing some */
    gc(sc, sc->NIL, sc->NIL);
    x = find_consecutive_cells(sc, n);
    if (x != sc->NIL) {
        return x;
    }

    /* If there still aren't, try getting more heap */
    if (!alloc_cellseg(sc, 1)) {
        sc->no_memory = 1;
        return sc->sink;
    }

    x = find_consecutive_cells(sc, n);
    if (x != sc->NIL) {
        return x;
    }

    /* If all fail, report failure */
    sc->no_memory = 1;
    return sc->sink;
}

static int count_consecutive_cells(pointer x, int needed)
{
    int n = 1;
    while (cdr(x) == x + 1) {
        x = cdr(x);
        n++;
        if (n > needed)
            return n;
    }
    return n;
}

static pointer find_consecutive_cells(scheme *sc, int n)
{
    pointer *pp;
    int cnt;

    pp = &sc->free_cell;
    while (*pp != sc->NIL) {
        cnt = count_consecutive_cells(*pp, n);
        if (cnt >= n) {
            pointer x = *pp;
            *pp = cdr(*pp + n - 1);
            sc->fcells -= n;
            return x;
        }
        pp = &cdr(*pp + cnt - 1);
    }
    return sc->NIL;
}

/* To retain recent allocs before interpreter knows about them -
   Tehom */

static void push_recent_alloc(scheme *sc, pointer recent, pointer extra)
{
    pointer holder = get_cell_x(sc, recent, extra);
    typeflag(holder) = T_PAIR | T_IMMUTABLE;
    car(holder) = recent;
    cdr(holder) = car(sc->sink);
    car(sc->sink) = holder;
}

static pointer get_cell(scheme *sc, pointer a, pointer b)
{
    pointer cell = get_cell_x(sc, a, b);
    /* For right now, include "a" and "b" in "cell" so that gc doesn't
       think they are garbage. */
    /* Tentatively record it as a pair so gc understands it. */
    typeflag(cell) = T_PAIR;
    car(cell) = a;
    cdr(cell) = b;
    push_recent_alloc(sc, cell, sc->NIL);
    return cell;
}

static pointer get_vector_object(scheme *sc, int len, pointer init)
{
    pointer cells = get_consecutive_cells(sc, len / 2 + len % 2 + 1);
    if (sc->no_memory) {
        return sc->sink;
    }
    /* Record it as a vector so that gc understands it. */
    typeflag(cells) = (T_VECTOR | T_ATOM);
    ivalue_unchecked(cells) = len;
    set_num_integer(cells);
    fill_vector(cells, init);
    push_recent_alloc(sc, cells, sc->NIL);
    return cells;
}

static void ok_to_freely_gc(scheme *sc) { car(sc->sink) = sc->NIL; }

#ifdef TSGRIND
static void check_cell_alloced(pointer p, int expect_alloced)
{
    /* Can't use putstr(sc,str) because callers have no access to
       sc.  */
    if (typeflag(p) & !expect_alloced) {
        fprintf(stderr, "Cell is already allocated!\n");
    }
    if (!(typeflag(p)) & expect_alloced) {
        fprintf(stderr, "Cell is not allocated!\n");
    }
}
#endif

#ifdef TSGRIND
static void check_range_alloced(pointer p, int n, int expect_alloced)
{
    int i;
    for (i = 0; i < n; i++) {
        (void)check_cell_alloced(p + i, expect_alloced);
    }
}
#endif

/* Medium level cell allocation */

/* get new cons cell */
pointer _cons(scheme *sc, pointer a, pointer b, int immutable)
{
    pointer x = get_cell(sc, a, b);

    typeflag(x) = T_PAIR;
    if (immutable) {
        setimmutable(x);
    }
    car(x) = a;
    cdr(x) = b;
    return (x);
}

static int hash_fn(const char *key, int table_size);

static pointer oblist_initial_value(scheme *sc)
{
    return mk_vector(sc, 461); /* probably should be bigger */
}

/* returns the new symbol */
static pointer oblist_add_by_name(scheme *sc, const char *name)
{
    pointer x;
    int location;

    x = immutable_cons(sc, mk_string(sc, name), sc->NIL);
    typeflag(x) = T_SYMBOL;
    setimmutable(car(x));

    location = hash_fn(name, ivalue_unchecked(sc->oblist));
    set_vector_elem(sc->oblist, location,
        immutable_cons(sc, x, vector_elem(sc->oblist, location)));
    return x;
}

static pointer oblist_find_by_name(scheme *sc, const char *name)
{
    int location;
    pointer x;
    char *s;

    location = hash_fn(name, ivalue_unchecked(sc->oblist));
    for (x = vector_elem(sc->oblist, location); x != sc->NIL; x = cdr(x)) {
        s = symname(car(x));
        /* case-insensitive, per R5RS section 2. */
        if (our_stricmp(name, s) == 0) {
            return car(x);
        }
    }
    return sc->NIL;
}

static pointer oblist_all_symbols(scheme *sc)
{
    int i;
    pointer x;
    pointer ob_list = sc->NIL;

    for (i = 0; i < ivalue_unchecked(sc->oblist); i++) {
        for (x = vector_elem(sc->oblist, i); x != sc->NIL; x = cdr(x)) {
            ob_list = cons(sc, x, ob_list);
        }
    }
    return ob_list;
}

static pointer mk_port(scheme *sc, port *p)
{
    pointer x = get_cell(sc, sc->NIL, sc->NIL);

    typeflag(x) = T_PORT | T_ATOM;
    x->_object._port = p;
    return (x);
}

static pointer mk_opaque_type(enum scheme_types type_, void *p)
{
    pointer x;

    x = get_cell(sc, sc->NIL, sc->NIL);
    typeflag(x) = T_ATOM | type_;
    x->_object._opaque._p = p;
    return x;
}

pointer mk_character(scheme *sc, int c)
{
    pointer x = get_cell(sc, sc->NIL, sc->NIL);

    typeflag(x) = (T_CHARACTER | T_ATOM);
    ivalue_unchecked(x) = c;
    set_num_integer(x);
    return (x);
}

/* get number atom (integer) */
pointer mk_integer(scheme *sc, long num)
{
    pointer x = get_cell(sc, sc->NIL, sc->NIL);

    typeflag(x) = (T_NUMBER | T_ATOM);
    ivalue_unchecked(x) = num;
    set_num_integer(x);
    return (x);
}

pointer mk_real(scheme *sc, double n)
{
    pointer x = get_cell(sc, sc->NIL, sc->NIL);

    typeflag(x) = (T_NUMBER | T_ATOM);
    rvalue_unchecked(x) = n;
    set_num_real(x);
    return (x);
}

static pointer mk_number(scheme *sc, num n)
{
    if (n.is_fixnum) {
        return mk_integer(sc, n.value.ivalue);
    } else {
        return mk_real(sc, n.value.rvalue);
    }
}

/* allocate name to string area */
static char *store_string(scheme *sc, int len_str, const char *str, char fill)
{
    char *q;

    q = (char *)sc->malloc(len_str + 1);
    if (q == 0) {
        sc->no_memory = 1;
        return sc->strbuff;
    }
    if (str != 0) {
        snprintf(q, len_str + 1, "%s", str);
    } else {
        memset(q, fill, len_str);
        q[len_str] = 0;
    }
    return (q);
}

/* get new string */
pointer mk_string(scheme *sc, const char *str)
{
    return mk_counted_string(sc, str, strlen(str));
}

pointer mk_counted_string(scheme *sc, const char *str, int len)
{
    pointer x = get_cell(sc, sc->NIL, sc->NIL);
    typeflag(x) = (T_STRING | T_ATOM);
    strvalue(x) = store_string(sc, len, str, 0);
    strlength(x) = len;
    return (x);
}

pointer mk_empty_string(scheme *sc, int len, char fill)
{
    pointer x = get_cell(sc, sc->NIL, sc->NIL);
    typeflag(x) = (T_STRING | T_ATOM);
    strvalue(x) = store_string(sc, len, 0, fill);
    strlength(x) = len;
    return (x);
}

static pointer mk_vector(scheme *sc, int len)
{
    return get_vector_object(sc, len, sc->NIL);
}

static void fill_vector(pointer vec, pointer obj)
{
    int i;
    int num = ivalue(vec) / 2 + ivalue(vec) % 2;
    for (i = 0; i < num; i++) {
        typeflag(vec + 1 + i) = T_PAIR;
        setimmutable(vec + 1 + i);
        car(vec + 1 + i) = obj;
        cdr(vec + 1 + i) = obj;
    }
}

static pointer vector_elem(pointer vec, int ielem)
{
    int n = ielem / 2;
    if (ielem % 2 == 0) {
        return car(vec + 1 + n);
    } else {
        return cdr(vec + 1 + n);
    }
}

static pointer set_vector_elem(pointer vec, int ielem, pointer a)
{
    int n = ielem / 2;
    if (ielem % 2 == 0) {
        return car(vec + 1 + n) = a;
    } else {
        return cdr(vec + 1 + n) = a;
    }
}

/* get new symbol */
pointer mk_symbol(scheme *sc, const char *name)
{
    pointer x;

    /* first check oblist */
    x = oblist_find_by_name(sc, name);
    if (x != sc->NIL) {
        return (x);
    } else {
        x = oblist_add_by_name(sc, name);
        return (x);
    }
}

pointer mk_timespec(scheme *sc, long sec, long nsec)
{
    pointer x;

    x = get_cell(sc, sc->NIL, sc->NIL);
    typeflag(x) = T_ATOM | T_TIMESPEC;
    x->_object._timespec.sec = sec;
    x->_object._timespec.nsec = nsec;
    return x;
}

/* make symbol or number atom from string */
static pointer mk_atom(scheme *sc, char *q)
{
    char c, *p;
    int has_dec_point = 0;
    int has_fp_exp = 0;

    p = q;
    c = *p++;
    if ((c == '+') || (c == '-')) {
        c = *p++;
        if (c == '.') {
            has_dec_point = 1;
            c = *p++;
        }
        if (!isdigit(c)) {
            return (mk_symbol(sc, our_strlwr(q)));
        }
    } else if (c == '.') {
        has_dec_point = 1;
        c = *p++;
        if (!isdigit(c)) {
            return (mk_symbol(sc, our_strlwr(q)));
        }
    } else if (!isdigit(c)) {
        return (mk_symbol(sc, our_strlwr(q)));
    }

    for (; (c = *p) != 0; ++p) {
        if (!isdigit(c)) {
            if (c == '.') {
                if (!has_dec_point) {
                    has_dec_point = 1;
                    continue;
                }
            } else if ((c == 'e') || (c == 'E')) {
                if (!has_fp_exp) {
                    has_dec_point = 1; /* decimal point illegal
                                          from now on */
                    p++;
                    if ((*p == '-') || (*p == '+') || isdigit(*p)) {
                        continue;
                    }
                }
            }
            return (mk_symbol(sc, our_strlwr(q)));
        }
    }
    if (has_dec_point) {
        return mk_real(sc, atof(q));
    }
    return (mk_integer(sc, atol(q)));
}

/* make constant */
static pointer mk_sharp_const(scheme *sc, const char *name)
{
    long x;
    char tmp[STRBUFFSIZE];

    if (!strcmp(name, "t"))
        return (sc->T);
    else if (!strcmp(name, "f"))
        return (sc->F);
    else if (*name == 'o') { /* #o (octal) */
        snprintf(tmp, STRBUFFSIZE, "0%s", name + 1);
        sscanf(tmp, "%lo", (long unsigned *)&x);
        return (mk_integer(sc, x));
    } else if (*name == 'd') { /* #d (decimal) */
        sscanf(name + 1, "%ld", (long int *)&x);
        return (mk_integer(sc, x));
    } else if (*name == 'x') { /* #x (hex) */
        snprintf(tmp, STRBUFFSIZE, "0x%s", name + 1);
        sscanf(tmp, "%lx", (long unsigned *)&x);
        return (mk_integer(sc, x));
    } else if (*name == 'b') { /* #b (binary) */
        x = binary_decode(name + 1);
        return (mk_integer(sc, x));
    } else if (*name == '\\') { /* #\w (character) */
        int c;
        if (our_stricmp(name + 1, "space") == 0) {
            c = ' ';
        } else if (our_stricmp(name + 1, "newline") == 0) {
            c = '\n';
        } else if (our_stricmp(name + 1, "return") == 0) {
            c = '\r';
        } else if (our_stricmp(name + 1, "tab") == 0) {
            c = '\t';
        } else if (name[1] == 'x' && name[2] != 0) {
            int c1 = 0;
            if (sscanf(name + 2, "%x", (unsigned int *)&c1) == 1
                && c1 < UCHAR_MAX) {
                c = c1;
            } else {
                return sc->NIL;
            }
        } else if (name[2] == 0) {
            c = name[1];
        } else {
            return sc->NIL;
        }
        return mk_character(sc, c);
    } else
        return (sc->NIL);
}

/* ========== garbage collector ========== */

/*--
 *  We use algorithm E (Knuth, The Art of Computer Programming Vol.1,
 *  sec. 2.3.5), the Schorr-Deutsch-Waite link-inversion algorithm,
 *  for marking.
 */
static void mark(pointer a)
{
    pointer t, q, p;

    t = (pointer)0;
    p = a;
E2:
    setmark(p);
    if (is_vector(p)) {
        int i;
        int num = ivalue_unchecked(p) / 2 + ivalue_unchecked(p) % 2;
        for (i = 0; i < num; i++) {
            /* Vector cells will be treated like ordinary cells */
            mark(p + 1 + i);
        }
    }
    if (is_atom(p))
        goto E6;
    /* E4: down car */
    q = car(p);
    if (q && !is_mark(q)) {
        setatom(p); /* a note that we have moved car */
        car(p) = t;
        t = p;
        p = q;
        goto E2;
    }
E5:
    q = cdr(p); /* down cdr */
    if (q && !is_mark(q)) {
        cdr(p) = t;
        t = p;
        p = q;
        goto E2;
    }
E6: /* up.  Undo the link switching from steps E4 and E5. */
    if (!t)
        return;
    q = t;
    if (is_atom(q)) {
        clratom(q);
        t = car(q);
        car(q) = p;
        p = q;
        goto E5;
    } else {
        t = cdr(q);
        cdr(q) = p;
        p = q;
        goto E6;
    }
}

/* garbage collection. parameter a, b is marked. */
static void gc(scheme *sc, pointer a, pointer b)
{
    pointer p;
    int i;

    if (sc->gc_verbose) {
        putstr(sc, "gc...");
    }

    /* mark system globals */
    mark(sc->oblist);
    mark(sc->global_env);
    mark(g_features);
    mark(g_command_line);

    /* mark current registers */
    mark(sc->args);
    mark(sc->envir);
    mark(sc->code);
    dump_stack_mark(sc);
    mark(sc->value);
    mark(sc->inport);
    mark(sc->save_inport);
    mark(sc->outport);
    mark(sc->loadport);

    /* Mark recent objects the interpreter doesn't know about yet. */
    mark(car(sc->sink));
    /* Mark any older stuff above nested C calls */
    mark(sc->c_nest);

    /* mark variables a, b */
    mark(a);
    mark(b);

    /* garbage collect */
    clrmark(sc->NIL);
    sc->fcells = 0;
    sc->free_cell = sc->NIL;
    /* free-list is kept sorted by address so as to maintain consecutive
       ranges, if possible, for use with vectors. Here we scan the cells
       (which are also kept sorted by address) downwards to build the
       free-list in sorted order.
    */
    for (i = sc->last_cell_seg; i >= 0; i--) {
        p = sc->cell_seg[i] + CELL_SEGSIZE;
        while (--p >= sc->cell_seg[i]) {
            if (is_mark(p)) {
                clrmark(p);
            } else {
                /* reclaim cell */
                if (typeflag(p) != 0) {
                    finalize_cell(sc, p);
                    typeflag(p) = 0;
                    car(p) = sc->NIL;
                }
                ++sc->fcells;
                cdr(p) = sc->free_cell;
                sc->free_cell = p;
            }
        }
    }

    if (sc->gc_verbose) {
        char msg[80];
        snprintf(msg, 80, "done: %ld cells were recovered.\n", sc->fcells);
        putstr(sc, msg);
    }
}

static void finalize_cell(scheme *sc, pointer a)
{
    if (is_string(a)) {
        sc->free(strvalue(a));
    } else if (is_port(a)) {
        if (a->_object._port->kind & port_file
            && a->_object._port->rep.stdio.closeit) {
            port_close(sc, a, port_input | port_output);
        }
        sc->free(a->_object._port);
    } else if (is_file_info(a)) {
        sc->free(a->_object._opaque._p);
    } else if (is_user_info(a)) {
        struct user_info *info = a->_object._opaque._p;
        finalize_cell(sc, info->name);
        finalize_cell(sc, info->home_dir);
        finalize_cell(sc, info->shell);
        finalize_cell(sc, info->full_name);
        finalize_cell(
            sc, info->parsed_full_name); // TODO: this is wrong, it's a list
        sc->free(info);
    } else if (is_directory_list(a)) {
        os_close_directory_list(a);
    }
}

/* ========== Routines for Reading ========== */

static int file_push(scheme *sc, const char *fname)
{
    FILE *fin;

    if (sc->file_i == MAXFIL - 1)
        return 0;
    fin = fopen(fname, "r");
    if (fin != 0) {
        sc->file_i++;
        sc->load_stack[sc->file_i].kind = port_file | port_input;
        sc->load_stack[sc->file_i].rep.stdio.file = fin;
        sc->load_stack[sc->file_i].rep.stdio.closeit = 1;
        sc->nesting_stack[sc->file_i] = 0;
        sc->loadport->_object._port = sc->load_stack + sc->file_i;

#if SHOW_ERROR_LINE
        sc->load_stack[sc->file_i].rep.stdio.curr_line = 0;
        if (fname)
            sc->load_stack[sc->file_i].rep.stdio.filename
                = store_string(sc, strlen(fname), fname, 0);
#endif
    }
    return fin != 0;
}

static void file_pop(scheme *sc)
{
    if (sc->file_i != 0) {
        sc->nesting = sc->nesting_stack[sc->file_i];
        port_close(sc, sc->loadport, port_input);
        sc->file_i--;
        sc->loadport->_object._port = sc->load_stack + sc->file_i;
    }
}

static int file_interactive(scheme *sc)
{
    return sc->file_i == 0 && sc->load_stack[0].rep.stdio.file == stdin
        && sc->inport->_object._port->kind & port_file;
}

static port *port_rep_from_filename(scheme *sc, const char *fn, int prop)
{
    FILE *f;
    char *rw;
    port *pt;
    if (prop == (port_input | port_output)) {
        rw = "a+";
    } else if (prop == port_output) {
        rw = "w";
    } else {
        rw = "r";
    }
    f = fopen(fn, rw);
    if (f == 0) {
        return 0;
    }
    pt = port_rep_from_file(sc, f, prop);
    pt->rep.stdio.closeit = 1;

#if SHOW_ERROR_LINE
    if (fn)
        pt->rep.stdio.filename = store_string(sc, strlen(fn), fn, 0);

    pt->rep.stdio.curr_line = 0;
#endif
    return pt;
}

static pointer port_from_filename(scheme *sc, const char *fn, int prop)
{
    port *pt;
    pt = port_rep_from_filename(sc, fn, prop);
    if (pt == 0) {
        return sc->NIL;
    }
    return mk_port(sc, pt);
}

static port *port_rep_from_file(scheme *sc, FILE *f, int prop)
{
    port *pt;

    pt = (port *)sc->malloc(sizeof *pt);
    if (pt == NULL) {
        return NULL;
    }
    pt->kind = port_file | prop;
    pt->rep.stdio.file = f;
    pt->rep.stdio.closeit = 0;
    return pt;
}

static pointer port_from_file(scheme *sc, FILE *f, int prop)
{
    port *pt;
    pt = port_rep_from_file(sc, f, prop);
    if (pt == 0) {
        return sc->NIL;
    }
    return mk_port(sc, pt);
}

static port *port_rep_from_string(
    scheme *sc, char *start, char *past_the_end, int prop)
{
    port *pt;
    pt = (port *)sc->malloc(sizeof(port));
    if (pt == 0) {
        return 0;
    }
    pt->kind = port_string | prop;
    pt->rep.string.start = start;
    pt->rep.string.curr = start;
    pt->rep.string.past_the_end = past_the_end;
    return pt;
}

static pointer port_from_string(
    scheme *sc, char *start, char *past_the_end, int prop)
{
    port *pt;
    pt = port_rep_from_string(sc, start, past_the_end, prop);
    if (pt == 0) {
        return sc->NIL;
    }
    return mk_port(sc, pt);
}

#define BLOCK_SIZE 256

static port *port_rep_from_scratch(scheme *sc)
{
    port *pt;
    char *start;
    pt = (port *)sc->malloc(sizeof(port));
    if (pt == 0) {
        return 0;
    }
    start = sc->malloc(BLOCK_SIZE);
    if (start == 0) {
        return 0;
    }
    memset(start, ' ', BLOCK_SIZE - 1);
    start[BLOCK_SIZE - 1] = '\0';
    pt->kind = port_string | port_output | port_srfi6;
    pt->rep.string.start = start;
    pt->rep.string.curr = start;
    pt->rep.string.past_the_end = start + BLOCK_SIZE - 1;
    return pt;
}

static pointer port_from_scratch(scheme *sc)
{
    port *pt;
    pt = port_rep_from_scratch(sc);
    if (pt == 0) {
        return sc->NIL;
    }
    return mk_port(sc, pt);
}

static void port_close(scheme *sc, pointer p, int flag)
{
    port *pt = p->_object._port;
    pt->kind &= ~flag;
    if ((pt->kind & (port_input | port_output)) == 0) {
        if (pt->kind & port_file) {

#if SHOW_ERROR_LINE
            /* Cleanup is here so (close-*-port) functions could work too */
            pt->rep.stdio.curr_line = 0;

            if (pt->rep.stdio.filename)
                sc->free(pt->rep.stdio.filename);
#endif

            fclose(pt->rep.stdio.file);
        }
        pt->kind = port_free;
    }
}

/* get new character from input file */
static int inchar(scheme *sc)
{
    int c;
    port *pt;

    pt = sc->inport->_object._port;
    if (pt->kind & port_saw_EOF) {
        return EOF;
    }
    c = basic_inchar(pt);
    if (c == EOF && sc->inport == sc->loadport) {
        /* Instead, set port_saw_EOF */
        pt->kind |= port_saw_EOF;

        /* file_pop(sc); */
        return EOF;
        /* NOTREACHED */
    }
    return c;
}

static int basic_inchar(port *pt)
{
    if (pt->kind & port_file) {
        return fgetc(pt->rep.stdio.file);
    } else {
        if (*pt->rep.string.curr == 0
            || pt->rep.string.curr == pt->rep.string.past_the_end) {
            return EOF;
        } else {
            return *pt->rep.string.curr++;
        }
    }
}

/* back character to input buffer */
static void backchar(scheme *sc, int c)
{
    port *pt;
    if (c == EOF)
        return;
    pt = sc->inport->_object._port;
    if (pt->kind & port_file) {
        ungetc(c, pt->rep.stdio.file);
    } else {
        if (pt->rep.string.curr != pt->rep.string.start) {
            --pt->rep.string.curr;
        }
    }
}

static int realloc_port_string(scheme *sc, port *p)
{
    char *start = p->rep.string.start;
    size_t new_size = p->rep.string.past_the_end - start + 1 + BLOCK_SIZE;
    char *str = sc->malloc(new_size);
    if (str) {
        memset(str, ' ', new_size - 1);
        str[new_size - 1] = '\0';
        strcpy(str, start);
        p->rep.string.start = str;
        p->rep.string.past_the_end = str + new_size - 1;
        p->rep.string.curr -= start - str;
        sc->free(start);
        return 1;
    } else {
        return 0;
    }
}

void putstr(scheme *sc, const char *s)
{
    port *pt = sc->outport->_object._port;
    if (pt->kind & port_file) {
        fputs(s, pt->rep.stdio.file);
    } else {
        for (; *s; s++) {
            if (pt->rep.string.curr != pt->rep.string.past_the_end) {
                *pt->rep.string.curr++ = *s;
            } else if (pt->kind & port_srfi6 && realloc_port_string(sc, pt)) {
                *pt->rep.string.curr++ = *s;
            }
        }
    }
}

static void putchars(scheme *sc, const char *s, int len)
{
    port *pt = sc->outport->_object._port;
    if (pt->kind & port_file) {
        fwrite(s, 1, len, pt->rep.stdio.file);
    } else {
        for (; len; len--) {
            if (pt->rep.string.curr != pt->rep.string.past_the_end) {
                *pt->rep.string.curr++ = *s++;
            } else if (pt->kind & port_srfi6 && realloc_port_string(sc, pt)) {
                *pt->rep.string.curr++ = *s++;
            }
        }
    }
}

void putcharacter(scheme *sc, int c)
{
    port *pt = sc->outport->_object._port;
    if (pt->kind & port_file) {
        fputc(c, pt->rep.stdio.file);
    } else {
        if (pt->rep.string.curr != pt->rep.string.past_the_end) {
            *pt->rep.string.curr++ = c;
        } else if (pt->kind & port_srfi6 && realloc_port_string(sc, pt)) {
            *pt->rep.string.curr++ = c;
        }
    }
}

/* read characters up to delimiter, but cater to character constants */
static char *readstr_upto(scheme *sc, char *delim)
{
    char *p = sc->strbuff;

    while ((p - sc->strbuff < sizeof(sc->strbuff))
        && !is_one_of(delim, (*p++ = inchar(sc))))
        ;

    if (p == sc->strbuff + 2 && p[-2] == '\\') {
        *p = 0;
    } else {
        backchar(sc, p[-1]);
        *--p = '\0';
    }
    return sc->strbuff;
}

/* read string expression "xxx...xxx" */
static pointer readstrexp(scheme *sc)
{
    char *p = sc->strbuff;
    int c;
    int c1 = 0;
    enum { st_ok, st_bsl, st_x1, st_x2, st_oct1, st_oct2 } state = st_ok;

    for (;;) {
        c = inchar(sc);
        if (c == EOF || p - sc->strbuff > sizeof(sc->strbuff) - 1) {
            return sc->F;
        }
        switch (state) {
        case st_ok:
            switch (c) {
            case '\\':
                state = st_bsl;
                break;
            case '"':
                *p = 0;
                return mk_counted_string(sc, sc->strbuff, p - sc->strbuff);
            default:
                *p++ = c;
                break;
            }
            break;
        case st_bsl:
            switch (c) {
            case '0':
            case '1':
            case '2':
            case '3':
            case '4':
            case '5':
            case '6':
            case '7':
                state = st_oct1;
                c1 = c - '0';
                break;
            case 'x':
            case 'X':
                state = st_x1;
                c1 = 0;
                break;
            case 'n':
                *p++ = '\n';
                state = st_ok;
                break;
            case 't':
                *p++ = '\t';
                state = st_ok;
                break;
            case 'r':
                *p++ = '\r';
                state = st_ok;
                break;
            case '"':
                *p++ = '"';
                state = st_ok;
                break;
            default:
                *p++ = c;
                state = st_ok;
                break;
            }
            break;
        case st_x1:
        case st_x2:
            c = toupper(c);
            if (c >= '0' && c <= 'F') {
                if (c <= '9') {
                    c1 = (c1 << 4) + c - '0';
                } else {
                    c1 = (c1 << 4) + c - 'A' + 10;
                }
                if (state == st_x1) {
                    state = st_x2;
                } else {
                    *p++ = c1;
                    state = st_ok;
                }
            } else {
                return sc->F;
            }
            break;
        case st_oct1:
        case st_oct2:
            if (c < '0' || c > '7') {
                *p++ = c1;
                backchar(sc, c);
                state = st_ok;
            } else {
                if (state == st_oct2 && c1 >= 32)
                    return sc->F;

                c1 = (c1 << 3) + (c - '0');

                if (state == st_oct1)
                    state = st_oct2;
                else {
                    *p++ = c1;
                    state = st_ok;
                }
            }
            break;
        }
    }
}

/* check c is in chars */
static int is_one_of(char *s, int c)
{
    if (c == EOF)
        return 1;
    while (*s)
        if (*s++ == c)
            return (1);
    return (0);
}

/* skip white characters */
static int skipspace(scheme *sc)
{
    int c, curr_line;

    curr_line = 0;
    do {
        c = inchar(sc);
#if SHOW_ERROR_LINE
        if (c == '\n')
            curr_line++;
#endif
    } while (isspace(c));

/* record it */
#if SHOW_ERROR_LINE
    if (sc->load_stack[sc->file_i].kind & port_file)
        sc->load_stack[sc->file_i].rep.stdio.curr_line += curr_line;
#endif

    if (c != EOF) {
        backchar(sc, c);
        return 1;
    } else {
        return EOF;
    }
}

/* get token */
static int token(scheme *sc)
{
    int c;
    c = skipspace(sc);
    if (c == EOF) {
        return (TOK_EOF);
    }
    switch (c = inchar(sc)) {
    case EOF:
        return (TOK_EOF);
    case '(':
        return (TOK_LPAREN);
    case ')':
        return (TOK_RPAREN);
    case '.':
        c = inchar(sc);
        if (is_one_of(" \n\t", c)) {
            return (TOK_DOT);
        } else {
            backchar(sc, c);
            backchar(sc, '.');
            return TOK_ATOM;
        }
    case '\'':
        return (TOK_QUOTE);
    case ';':
        while ((c = inchar(sc)) != '\n' && c != EOF)
            ;

#if SHOW_ERROR_LINE
        if (c == '\n' && sc->load_stack[sc->file_i].kind & port_file)
            sc->load_stack[sc->file_i].rep.stdio.curr_line++;
#endif

        if (c == EOF) {
            return (TOK_EOF);
        } else {
            return (token(sc));
        }
    case '"':
        return (TOK_DQUOTE);
    case BACKQUOTE:
        return (TOK_BQUOTE);
    case ',':
        if ((c = inchar(sc)) == '@') {
            return (TOK_ATMARK);
        } else {
            backchar(sc, c);
            return (TOK_COMMA);
        }
    case '#':
        c = inchar(sc);
        if (c == '(') {
            return (TOK_VEC);
        } else if (c == '!') {
            while ((c = inchar(sc)) != '\n' && c != EOF)
                ;

#if SHOW_ERROR_LINE
            if (c == '\n' && sc->load_stack[sc->file_i].kind & port_file)
                sc->load_stack[sc->file_i].rep.stdio.curr_line++;
#endif

            if (c == EOF) {
                return (TOK_EOF);
            } else {
                return (token(sc));
            }
        } else {
            backchar(sc, c);
            if (is_one_of(" tfodxb\\", c)) {
                return TOK_SHARP_CONST;
            } else {
                return (TOK_SHARP);
            }
        }
    default:
        backchar(sc, c);
        return (TOK_ATOM);
    }
}

/* ========== Routines for Printing ========== */
#define ok_abbrev(x) (is_pair(x) && cdr(x) == sc->NIL)

static void printslashstring(scheme *sc, char *p, int len)
{
    int i;
    unsigned char *s = (unsigned char *)p;
    putcharacter(sc, '"');
    for (i = 0; i < len; i++) {
        if (*s == 0xff || *s == '"' || *s < ' ' || *s == '\\') {
            putcharacter(sc, '\\');
            switch (*s) {
            case '"':
                putcharacter(sc, '"');
                break;
            case '\n':
                putcharacter(sc, 'n');
                break;
            case '\t':
                putcharacter(sc, 't');
                break;
            case '\r':
                putcharacter(sc, 'r');
                break;
            case '\\':
                putcharacter(sc, '\\');
                break;
            default: {
                int d = *s / 16;
                putcharacter(sc, 'x');
                if (d < 10) {
                    putcharacter(sc, d + '0');
                } else {
                    putcharacter(sc, d - 10 + 'A');
                }
                d = *s % 16;
                if (d < 10) {
                    putcharacter(sc, d + '0');
                } else {
                    putcharacter(sc, d - 10 + 'A');
                }
            }
            }
        } else {
            putcharacter(sc, *s);
        }
        s++;
    }
    putcharacter(sc, '"');
}

/* print atoms */
static void printatom(scheme *sc, pointer l, int f)
{
    char *p;
    int len;
    atom2str(sc, l, f, &p, &len);
    putchars(sc, p, len);
}

/* Uses internal buffer unless string pointer is already available */
static void atom2str(scheme *sc, pointer l, int f, char **pp, int *plen)
{
    char *p;

    if (l == sc->NIL) {
        p = "()";
    } else if (l == sc->T) {
        p = "#t";
    } else if (l == sc->F) {
        p = "#f";
    } else if (l == sc->EOF_OBJ) {
        p = "#<EOF>";
    } else if (is_port(l)) {
        p = sc->strbuff;
        snprintf(p, STRBUFFSIZE, "#<PORT>");
    } else if (is_file_info(l)) {
        p = "#<FILE-INFO>";
    } else if (is_user_info(l)) {
        p = "#<USER-INFO>";
    } else if (is_directory_list(l)) {
        p = "#<directory-list>";
    } else if (is_timespec(l)) {
        p = sc->strbuff;
        snprintf(p, STRBUFFSIZE, "#<timespec %ld>", timespec_sec(l));
    } else if (is_number(l)) {
        p = sc->strbuff;
        if (f <= 1 || f == 10) /* f is the base for numbers if > 1 */ {
            if (num_is_integer(l)) {
                snprintf(p, STRBUFFSIZE, "%ld", ivalue_unchecked(l));
            } else {
                snprintf(p, STRBUFFSIZE, "%.10g", rvalue_unchecked(l));
                /* r5rs says there must be a '.' (unless 'e'?) */
                f = strcspn(p, ".e");
                if (p[f] == 0) {
                    p[f] = '.'; /* not found, so add '.0' at the end */
                    p[f + 1] = '0';
                    p[f + 2] = 0;
                }
            }
        } else {
            long v = ivalue(l);
            if (f == 16) {
                if (v >= 0)
                    snprintf(p, STRBUFFSIZE, "%lx", v);
                else
                    snprintf(p, STRBUFFSIZE, "-%lx", -v);
            } else if (f == 8) {
                if (v >= 0)
                    snprintf(p, STRBUFFSIZE, "%lo", v);
                else
                    snprintf(p, STRBUFFSIZE, "-%lo", -v);
            } else if (f == 2) {
                unsigned long b = (v < 0) ? -v : v;
                p = &p[STRBUFFSIZE - 1];
                *p = 0;
                do {
                    *--p = (b & 1) ? '1' : '0';
                    b >>= 1;
                } while (b != 0);
                if (v < 0)
                    *--p = '-';
            }
        }
    } else if (is_string(l)) {
        if (!f) {
            p = strvalue(l);
        } else { /* Hack, uses the fact that printing is needed */
            *pp = sc->strbuff;
            *plen = 0;
            printslashstring(sc, strvalue(l), strlength(l));
            return;
        }
    } else if (is_character(l)) {
        int c = charvalue(l);
        p = sc->strbuff;
        if (!f) {
            p[0] = c;
            p[1] = 0;
        } else {
            switch (c) {
            case ' ':
                snprintf(p, STRBUFFSIZE, "#\\space");
                break;
            case '\n':
                snprintf(p, STRBUFFSIZE, "#\\newline");
                break;
            case '\r':
                snprintf(p, STRBUFFSIZE, "#\\return");
                break;
            case '\t':
                snprintf(p, STRBUFFSIZE, "#\\tab");
                break;
            default:
                if (c < 32) {
                    snprintf(p, STRBUFFSIZE, "#\\x%x", c);
                    break;
                }
                snprintf(p, STRBUFFSIZE, "#\\%c", c);
                break;
            }
        }
    } else if (is_symbol(l)) {
        p = symname(l);
    } else if (is_proc(l)) {
        p = sc->strbuff;
        snprintf(p, STRBUFFSIZE, "#<built-in procedure %s>", procname(l));
    } else if (is_macro(l)) {
        p = "#<macro>";
    } else if (is_closure(l)) {
        p = "#<procedure>";
    } else if (is_promise(l)) {
        p = "#<promise>";
    } else if (is_continuation(l)) {
        p = "#<continuation>";
    } else {
        p = "#<error>";
    }
    *pp = p;
    *plen = strlen(p);
}
/* ========== Routines for Evaluation Cycle ========== */

/* make closure. c is code. e is environment */
static pointer mk_closure(scheme *sc, pointer c, pointer e)
{
    pointer x = get_cell(sc, c, e);

    typeflag(x) = T_CLOSURE;
    car(x) = c;
    cdr(x) = e;
    return (x);
}

/* make continuation. */
static pointer mk_continuation(scheme *sc, pointer d)
{
    pointer x = get_cell(sc, sc->NIL, d);

    typeflag(x) = T_CONTINUATION;
    cont_dump(x) = d;
    return (x);
}

static pointer cons_star(scheme *sc, pointer d)
{
    pointer p, q;
    if (cdr(d) == sc->NIL) {
        return car(d);
    }
    p = cons(sc, car(d), cdr(d));
    q = p;
    while (cdr(cdr(p)) != sc->NIL) {
        d = cons(sc, car(p), cdr(p));
        if (cdr(cdr(p)) != sc->NIL) {
            p = cdr(d);
        }
    }
    cdr(p) = car(cdr(p));
    return q;
}

/* reverse list --- in-place */
static pointer reverse_in_place(scheme *sc, pointer term, pointer list)
{
    pointer p = list, result = term, q;

    while (p != sc->NIL) {
        q = cdr(p);
        cdr(p) = result;
        result = p;
        p = q;
    }
    return (result);
}

/* append list -- produce new list (in reverse order) */
static pointer revappend(scheme *sc, pointer a, pointer b)
{
    pointer result = a;
    pointer p = b;

    while (is_pair(p)) {
        result = cons(sc, car(p), result);
        p = cdr(p);
    }

    if (p == sc->NIL) {
        return result;
    }

    return sc->F; /* signal an error */
}

static pointer mk_string_list(const char **sv, int want_symbols)
{
    pointer x, xs;
    const char *s;

    xs = sc->NIL;
    for (; (s = *sv); sv++) {
        x = want_symbols ? mk_symbol(sc, s) : mk_string(sc, s);
        xs = cons(sc, x, xs);
    }
    return reverse_in_place(sc, sc->NIL, xs);
}

/* equivalence of atoms */
int eqv(pointer a, pointer b)
{
    if (is_string(a)) {
        if (is_string(b))
            return (strvalue(a) == strvalue(b));
        else
            return (0);
    } else if (is_number(a)) {
        if (is_number(b)) {
            if (num_is_integer(a) == num_is_integer(b))
                return num_eq(nvalue(a), nvalue(b));
        }
        return (0);
    } else if (is_character(a)) {
        if (is_character(b))
            return charvalue(a) == charvalue(b);
        else
            return (0);
    } else if (is_port(a)) {
        if (is_port(b))
            return a == b;
        else
            return (0);
    } else if (is_proc(a)) {
        if (is_proc(b))
            return procnum(a) == procnum(b);
        else
            return (0);
    } else {
        return (a == b);
    }
}

/* true or false value macro */
/* () is #t in R5RS */
#define is_true(p) ((p) != sc->F)
#define is_false(p) ((p) == sc->F)

/* ========== Environment implementation  ========== */

#if !defined(USE_ALIST_ENV) || !defined(USE_OBJECT_LIST)

static int hash_fn(const char *key, int table_size)
{
    unsigned int hashed = 0;
    const char *c;
    int bits_per_int = sizeof(unsigned int) * 8;

    for (c = key; *c; c++) {
        /* letters have about 5 bits in them */
        hashed = (hashed << 5) | (hashed >> (bits_per_int - 5));
        hashed ^= *c;
    }
    return hashed % table_size;
}
#endif

#ifndef USE_ALIST_ENV

/*
 * In this implementation, each frame of the environment may be
 * a hash table: a vector of alists hashed by variable name.
 * In practice, we use a vector only for the initial frame;
 * subsequent frames are too small and transient for the lookup
 * speed to out-weigh the cost of making a new vector.
 */

static void new_frame_in_env(scheme *sc, pointer old_env)
{
    pointer new_frame;

    /* The interaction-environment has about 300 variables in it. */
    if (old_env == sc->NIL) {
        new_frame = mk_vector(sc, 461);
    } else {
        new_frame = sc->NIL;
    }

    sc->envir = immutable_cons(sc, new_frame, old_env);
    setenvironment(sc->envir);
}

static void new_slot_spec_in_env(
    scheme *sc, pointer env, pointer variable, pointer value)
{
    pointer slot = immutable_cons(sc, variable, value);

    if (is_vector(car(env))) {
        int location = hash_fn(symname(variable), ivalue_unchecked(car(env)));

        set_vector_elem(car(env), location,
            immutable_cons(sc, slot, vector_elem(car(env), location)));
    } else {
        car(env) = immutable_cons(sc, slot, car(env));
    }
}

static pointer find_slot_in_env(scheme *sc, pointer env, pointer hdl, int all)
{
    pointer x, y;
    int location;

    for (x = env; x != sc->NIL; x = cdr(x)) {
        if (is_vector(car(x))) {
            location = hash_fn(symname(hdl), ivalue_unchecked(car(x)));
            y = vector_elem(car(x), location);
        } else {
            y = car(x);
        }
        for (; y != sc->NIL; y = cdr(y)) {
            if (caar(y) == hdl) {
                break;
            }
        }
        if (y != sc->NIL) {
            break;
        }
        if (!all) {
            return sc->NIL;
        }
    }
    if (x != sc->NIL) {
        return car(y);
    }
    return sc->NIL;
}

#else /* USE_ALIST_ENV */

static void new_frame_in_env(scheme *sc, pointer old_env)
{
    sc->envir = immutable_cons(sc, sc->NIL, old_env);
    setenvironment(sc->envir);
}

static void new_slot_spec_in_env(
    scheme *sc, pointer env, pointer variable, pointer value)
{
    car(env)
        = immutable_cons(sc, immutable_cons(sc, variable, value), car(env));
}

static pointer find_slot_in_env(scheme *sc, pointer env, pointer hdl, int all)
{
    pointer x, y;
    for (x = env; x != sc->NIL; x = cdr(x)) {
        for (y = car(x); y != sc->NIL; y = cdr(y)) {
            if (caar(y) == hdl) {
                break;
            }
        }
        if (y != sc->NIL) {
            break;
        }
        if (!all) {
            return sc->NIL;
        }
    }
    if (x != sc->NIL) {
        return car(y);
    }
    return sc->NIL;
}

#endif /* USE_ALIST_ENV else */

static void new_slot_in_env(scheme *sc, pointer variable, pointer value)
{
    new_slot_spec_in_env(sc, sc->envir, variable, value);
}

static void set_slot_in_env(scheme *sc, pointer slot, pointer value)
{
    (void)sc;
    cdr(slot) = value;
}

static pointer slot_value_in_env(pointer slot) { return cdr(slot); }

/* ========== Evaluation Cycle ========== */

static pointer _Error_1(scheme *sc, const char *s, pointer a)
{
    const char *str = s;
#if USE_ERROR_HOOK
    pointer x;
    pointer hdl = sc->ERROR_HOOK;
#endif

#if SHOW_ERROR_LINE
    char sbuf[STRBUFFSIZE];

    /* make sure error is not in REPL */
    if (sc->load_stack[sc->file_i].kind & port_file
        && sc->load_stack[sc->file_i].rep.stdio.file != stdin) {
        int ln = sc->load_stack[sc->file_i].rep.stdio.curr_line;
        const char *fname = sc->load_stack[sc->file_i].rep.stdio.filename;

        /* should never happen */
        if (!fname)
            fname = "<unknown>";

        /* we started from 0 */
        ln++;
        snprintf(sbuf, STRBUFFSIZE, "(%s : %i) %s", fname, ln, s);

        str = (const char *)sbuf;
    }
#endif

#if USE_ERROR_HOOK
    x = find_slot_in_env(sc, sc->envir, hdl, 1);
    if (x != sc->NIL) {
        if (a != 0) {
            sc->code = cons(
                sc, cons(sc, sc->QUOTE, cons(sc, (a), sc->NIL)), sc->NIL);
        } else {
            sc->code = sc->NIL;
        }
        sc->code = cons(sc, mk_string(sc, str), sc->code);
        setimmutable(car(sc->code));
        sc->code = cons(sc, slot_value_in_env(x), sc->code);
        sc->op = (int)OP_EVAL;
        return sc->T;
    }
#endif

    if (a != 0) {
        sc->args = cons(sc, (a), sc->NIL);
    } else {
        sc->args = sc->NIL;
    }
    sc->args = cons(sc, mk_string(sc, str), sc->args);
    setimmutable(car(sc->args));
    sc->op = (int)OP_ERR0;
    return sc->T;
}
#define Error_1(sc, s, a) return _Error_1(sc, s, a)
#define Error_0(sc, s) return _Error_1(sc, s, 0)

/* Too small to turn into function */
#define BEGIN do {
#define END                                                                  \
    }                                                                        \
    while (0)
#define s_goto(sc, a)                                                        \
    BEGIN                                                                    \
    sc->op = (int)(a);                                                       \
    return sc->T;                                                            \
    END

#define s_return(sc, a) return _s_return(sc, a)

#ifndef USE_SCHEME_STACK

/* this structure holds all the interpreter's registers */
struct dump_stack_frame {
    enum scheme_opcodes op;
    pointer args;
    pointer envir;
    pointer code;
};

#define STACK_GROWTH 3

static void s_save(
    scheme *sc, enum scheme_opcodes op, pointer args, pointer code)
{
    int nframes = (int)sc->dump;
    struct dump_stack_frame *next_frame;

    /* enough room for the next frame? */
    if (nframes >= sc->dump_size) {
        sc->dump_size += STACK_GROWTH;
        /* alas there is no sc->realloc */
        sc->dump_base = realloc(
            sc->dump_base, sizeof(struct dump_stack_frame) * sc->dump_size);
    }
    next_frame = (struct dump_stack_frame *)sc->dump_base + nframes;
    next_frame->op = op;
    next_frame->args = args;
    next_frame->envir = sc->envir;
    next_frame->code = code;
    sc->dump = (pointer)(nframes + 1);
}

static pointer _s_return(scheme *sc, pointer a)
{
    int nframes = (int)sc->dump;
    struct dump_stack_frame *frame;

    sc->value = (a);
    if (nframes <= 0) {
        return sc->NIL;
    }
    nframes--;
    frame = (struct dump_stack_frame *)sc->dump_base + nframes;
    sc->op = frame->op;
    sc->args = frame->args;
    sc->envir = frame->envir;
    sc->code = frame->code;
    sc->dump = (pointer)nframes;
    return sc->T;
}

static void dump_stack_reset(scheme *sc)
{
    /* in this implementation, sc->dump is the number of frames on the stack
     */
    sc->dump = (pointer)0;
}

static void dump_stack_initialize(scheme *sc)
{
    sc->dump_size = 0;
    sc->dump_base = NULL;
    dump_stack_reset(sc);
}

static void dump_stack_free(scheme *sc)
{
    free(sc->dump_base);
    sc->dump_base = NULL;
    sc->dump = (pointer)0;
    sc->dump_size = 0;
}

static void dump_stack_mark(scheme *sc)
{
    int nframes = (int)sc->dump;
    int i;
    for (i = 0; i < nframes; i++) {
        struct dump_stack_frame *frame;
        frame = (struct dump_stack_frame *)sc->dump_base + i;
        mark(frame->args);
        mark(frame->envir);
        mark(frame->code);
    }
}

#else

static void dump_stack_reset(scheme *sc) { sc->dump = sc->NIL; }

static void dump_stack_initialize(scheme *sc) { dump_stack_reset(sc); }

static void dump_stack_free(scheme *sc) { sc->dump = sc->NIL; }

static pointer _s_return(scheme *sc, pointer a)
{
    sc->value = (a);
    if (sc->dump == sc->NIL)
        return sc->NIL;
    sc->op = ivalue(car(sc->dump));
    sc->args = cadr(sc->dump);
    sc->envir = caddr(sc->dump);
    sc->code = cadddr(sc->dump);
    sc->dump = cddddr(sc->dump);
    return sc->T;
}

static void s_save(
    scheme *sc, enum scheme_opcodes op, pointer args, pointer code)
{
    sc->dump = cons(sc, sc->envir, cons(sc, (code), sc->dump));
    sc->dump = cons(sc, (args), sc->dump);
    sc->dump = cons(sc, mk_integer(sc, (long)(op)), sc->dump);
}

static void dump_stack_mark(scheme *sc) { mark(sc->dump); }
#endif

#define s_retbool(tf) s_return(sc, (tf) ? sc->T : sc->F)

static pointer opexe_0(scheme *sc, enum scheme_opcodes op)
{
    pointer x, y;

    switch (op) {
    case OP_LOAD: /* load */
        if (file_interactive(sc)) {
            fprintf(sc->outport->_object._port->rep.stdio.file,
                "Loading %s\n", strvalue(car(sc->args)));
        }
        if (!file_push(sc, strvalue(car(sc->args)))) {
            Error_1(sc, "unable to open", car(sc->args));
        } else {
            sc->args = mk_integer(sc, sc->file_i);
            s_goto(sc, OP_T0LVL);
        }

    case OP_T0LVL: /* top level */
        /* If we reached the end of file, this loop is done. */
        if (sc->loadport->_object._port->kind & port_saw_EOF) {
            if (sc->file_i == 0) {
                sc->args = sc->NIL;
                return sc->NIL;
            } else {
                file_pop(sc);
                s_return(sc, sc->value);
            }
            /* NOTREACHED */
        }

        /* If interactive, be nice to user. */
        if (file_interactive(sc)) {
            sc->envir = sc->global_env;
            dump_stack_reset(sc);
            putstr(sc, "\n");
            putstr(sc, prompt);
        }

        /* Set up another iteration of REPL */
        sc->nesting = 0;
        sc->save_inport = sc->inport;
        sc->inport = sc->loadport;
        s_save(sc, OP_T0LVL, sc->NIL, sc->NIL);
        s_save(sc, OP_VALUEPRINT, sc->NIL, sc->NIL);
        s_save(sc, OP_T1LVL, sc->NIL, sc->NIL);
        s_goto(sc, OP_READ_INTERNAL);

    case OP_T1LVL: /* top level */
        sc->code = sc->value;
        sc->inport = sc->save_inport;
        s_goto(sc, OP_EVAL);

    case OP_READ_INTERNAL: /* internal read */
        sc->tok = token(sc);
        if (sc->tok == TOK_EOF) {
            s_return(sc, sc->EOF_OBJ);
        }
        s_goto(sc, OP_RDSEXPR);

    case OP_VALUEPRINT: /* print evaluation result */
        /* OP_VALUEPRINT is always pushed, because when changing from
           non-interactive to interactive mode, it needs to be
           already on the stack */
        if (sc->tracing) {
            putstr(sc, "\nGives: ");
        }
        if (!file_interactive(sc)) {
            s_return(sc, sc->value);
        } else if (sc->loadport->_object._port->kind & port_saw_EOF) {
            putstr(sc, "\n");
            s_return(sc, sc->value);
        } else {
            sc->print_flag = 1;
            sc->args = sc->value;
            s_goto(sc, OP_P0LIST);
        }

    case OP_EVAL: /* main part of evaluation */
#if USE_TRACING
        if (sc->tracing) {
            /*s_save(sc,OP_VALUEPRINT,sc->NIL,sc->NIL);*/
            s_save(sc, OP_REAL_EVAL, sc->args, sc->code);
            sc->args = sc->code;
            putstr(sc, "\nEval: ");
            s_goto(sc, OP_P0LIST);
        }
        /* fall through */
    case OP_REAL_EVAL:
#endif
        if (is_symbol(sc->code)) { /* symbol */
            x = find_slot_in_env(sc, sc->envir, sc->code, 1);
            if (x != sc->NIL) {
                s_return(sc, slot_value_in_env(x));
            } else {
                Error_1(sc, "eval: unbound variable:", sc->code);
            }
        } else if (is_pair(sc->code)) {
            if (is_syntax(x = car(sc->code))) { /* SYNTAX */
                sc->code = cdr(sc->code);
                s_goto(sc, syntaxnum(x));
            } else { /* first, eval top element and eval arguments */
                s_save(sc, OP_E0ARGS, sc->NIL, sc->code);
                /* If no macros => s_save(sc,OP_E1ARGS, sc->NIL,
                 * cdr(sc->code));*/
                sc->code = car(sc->code);
                s_goto(sc, OP_EVAL);
            }
        } else {
            s_return(sc, sc->code);
        }

    case OP_E0ARGS: /* eval arguments */
        if (is_macro(sc->value)) { /* macro expansion */
            s_save(sc, OP_DOMACRO, sc->NIL, sc->NIL);
            sc->args = cons(sc, sc->code, sc->NIL);
            sc->code = sc->value;
            s_goto(sc, OP_APPLY);
        } else {
            sc->code = cdr(sc->code);
            s_goto(sc, OP_E1ARGS);
        }

    case OP_E1ARGS: /* eval arguments */
        sc->args = cons(sc, sc->value, sc->args);
        if (is_pair(sc->code)) { /* continue */
            s_save(sc, OP_E1ARGS, sc->args, cdr(sc->code));
            sc->code = car(sc->code);
            sc->args = sc->NIL;
            s_goto(sc, OP_EVAL);
        } else { /* end */
            sc->args = reverse_in_place(sc, sc->NIL, sc->args);
            sc->code = car(sc->args);
            sc->args = cdr(sc->args);
            s_goto(sc, OP_APPLY);
        }

#if USE_TRACING
    case OP_TRACING: {
        int tr = sc->tracing;
        sc->tracing = ivalue(car(sc->args));
        s_return(sc, mk_integer(sc, tr));
    }
#endif

    case OP_APPLY: /* apply 'code' to 'args' */
#if USE_TRACING
        if (sc->tracing) {
            s_save(sc, OP_REAL_APPLY, sc->args, sc->code);
            sc->print_flag = 1;
            /*  sc->args=cons(sc,sc->code,sc->args);*/
            putstr(sc, "\nApply to: ");
            s_goto(sc, OP_P0LIST);
        }
        /* fall through */
    case OP_REAL_APPLY:
#endif
        if (is_proc(sc->code)) {
            s_goto(sc, procnum(sc->code)); /* PROCEDURE */
        } else if (is_closure(sc->code) || is_macro(sc->code)
            || is_promise(sc->code)) { /* CLOSURE */
            /* Should not accept promise */
            /* make environment */
            new_frame_in_env(sc, closure_env(sc->code));
            for (x = car(closure_code(sc->code)), y = sc->args; is_pair(x);
                 x = cdr(x), y = cdr(y)) {
                if (y == sc->NIL) {
                    Error_0(sc, "not enough arguments");
                } else {
                    new_slot_in_env(sc, car(x), car(y));
                }
            }
            if (x == sc->NIL) {
                /*--
                 * if (y != sc->NIL) {
                 *   Error_0(sc,"too many arguments");
                 * }
                 */
            } else if (is_symbol(x))
                new_slot_in_env(sc, x, y);
            else {
                Error_1(sc, "syntax error in closure: not a symbol:", x);
            }
            sc->code = cdr(closure_code(sc->code));
            sc->args = sc->NIL;
            s_goto(sc, OP_BEGIN);
        } else if (is_continuation(sc->code)) { /* CONTINUATION */
            sc->dump = cont_dump(sc->code);
            s_return(sc, sc->args != sc->NIL ? car(sc->args) : sc->NIL);
        } else {
            Error_0(sc, "illegal function");
        }

    case OP_DOMACRO: /* do macro */
        sc->code = sc->value;
        s_goto(sc, OP_EVAL);

#if 1
    case OP_LAMBDA: /* lambda */
        /* If the hook is defined, apply it to sc->code, otherwise
           set sc->value fall thru */
        {
            pointer f = find_slot_in_env(sc, sc->envir, sc->COMPILE_HOOK, 1);
            if (f == sc->NIL) {
                sc->value = sc->code;
                /* Fallthru */
            } else {
                s_save(sc, OP_LAMBDA1, sc->args, sc->code);
                sc->args = cons(sc, sc->code, sc->NIL);
                sc->code = slot_value_in_env(f);
                s_goto(sc, OP_APPLY);
            }
        }

    case OP_LAMBDA1:
        s_return(sc, mk_closure(sc, sc->value, sc->envir));

#else
    case OP_LAMBDA: /* lambda */
        s_return(sc, mk_closure(sc, sc->code, sc->envir));

#endif

    case OP_MKCLOSURE: /* make-closure */
        x = car(sc->args);
        if (car(x) == sc->LAMBDA) {
            x = cdr(x);
        }
        if (cdr(sc->args) == sc->NIL) {
            y = sc->envir;
        } else {
            y = cadr(sc->args);
        }
        s_return(sc, mk_closure(sc, x, y));

    case OP_QUOTE: /* quote */
        s_return(sc, car(sc->code));

    case OP_DEF0: /* define */
        if (is_immutable(car(sc->code)))
            Error_1(sc, "define: unable to alter immutable", car(sc->code));

        if (is_pair(car(sc->code))) {
            x = caar(sc->code);
            sc->code = cons(
                sc, sc->LAMBDA, cons(sc, cdar(sc->code), cdr(sc->code)));
        } else {
            x = car(sc->code);
            sc->code = cadr(sc->code);
        }
        if (!is_symbol(x)) {
            Error_0(sc, "variable is not a symbol");
        }
        s_save(sc, OP_DEF1, sc->NIL, x);
        s_goto(sc, OP_EVAL);

    case OP_DEF1: /* define */
        x = find_slot_in_env(sc, sc->envir, sc->code, 0);
        if (x != sc->NIL) {
            set_slot_in_env(sc, x, sc->value);
        } else {
            new_slot_in_env(sc, sc->code, sc->value);
        }
        s_return(sc, sc->code);

    case OP_DEFP: /* defined? */
        x = sc->envir;
        if (cdr(sc->args) != sc->NIL) {
            x = cadr(sc->args);
        }
        s_retbool(find_slot_in_env(sc, x, car(sc->args), 1) != sc->NIL);

    case OP_SET0: /* set! */
        if (is_immutable(car(sc->code)))
            Error_1(sc, "set!: unable to alter immutable variable",
                car(sc->code));
        s_save(sc, OP_SET1, sc->NIL, car(sc->code));
        sc->code = cadr(sc->code);
        s_goto(sc, OP_EVAL);

    case OP_SET1: /* set! */
        y = find_slot_in_env(sc, sc->envir, sc->code, 1);
        if (y != sc->NIL) {
            set_slot_in_env(sc, y, sc->value);
            s_return(sc, sc->value);
        } else {
            Error_1(sc, "set!: unbound variable:", sc->code);
        }

    case OP_BEGIN: /* begin */
        if (!is_pair(sc->code)) {
            s_return(sc, sc->code);
        }
        if (cdr(sc->code) != sc->NIL) {
            s_save(sc, OP_BEGIN, sc->NIL, cdr(sc->code));
        }
        sc->code = car(sc->code);
        s_goto(sc, OP_EVAL);

    case OP_IF0: /* if */
        s_save(sc, OP_IF1, sc->NIL, cdr(sc->code));
        sc->code = car(sc->code);
        s_goto(sc, OP_EVAL);

    case OP_IF1: /* if */
        if (is_true(sc->value))
            sc->code = car(sc->code);
        else
            sc->code = cadr(sc->code); /* (if #f 1) ==> () because
                                        * car(sc->NIL) = sc->NIL */
        s_goto(sc, OP_EVAL);

    case OP_LET0: /* let */
        sc->args = sc->NIL;
        sc->value = sc->code;
        sc->code = is_symbol(car(sc->code)) ? cadr(sc->code) : car(sc->code);
        s_goto(sc, OP_LET1);

    case OP_LET1: /* let (calculate parameters) */
        sc->args = cons(sc, sc->value, sc->args);
        if (is_pair(sc->code)) { /* continue */
            if (!is_pair(car(sc->code)) || !is_pair(cdar(sc->code))) {
                Error_1(
                    sc, "Bad syntax of binding spec in let :", car(sc->code));
            }
            s_save(sc, OP_LET1, sc->args, cdr(sc->code));
            sc->code = cadar(sc->code);
            sc->args = sc->NIL;
            s_goto(sc, OP_EVAL);
        } else { /* end */
            sc->args = reverse_in_place(sc, sc->NIL, sc->args);
            sc->code = car(sc->args);
            sc->args = cdr(sc->args);
            s_goto(sc, OP_LET2);
        }

    case OP_LET2: /* let */
        new_frame_in_env(sc, sc->envir);
        for (x = is_symbol(car(sc->code)) ? cadr(sc->code) : car(sc->code),
            y = sc->args;
             y != sc->NIL; x = cdr(x), y = cdr(y)) {
            new_slot_in_env(sc, caar(x), car(y));
        }
        if (is_symbol(car(sc->code))) { /* named let */
            for (x = cadr(sc->code), sc->args = sc->NIL; x != sc->NIL;
                 x = cdr(x)) {
                if (!is_pair(x))
                    Error_1(sc, "Bad syntax of binding in let :", x);
                if (!is_list(car(x)))
                    Error_1(sc, "Bad syntax of binding in let :", car(x));
                sc->args = cons(sc, caar(x), sc->args);
            }
            x = mk_closure(sc,
                cons(sc, reverse_in_place(sc, sc->NIL, sc->args),
                    cddr(sc->code)),
                sc->envir);
            new_slot_in_env(sc, car(sc->code), x);
            sc->code = cddr(sc->code);
            sc->args = sc->NIL;
        } else {
            sc->code = cdr(sc->code);
            sc->args = sc->NIL;
        }
        s_goto(sc, OP_BEGIN);

    case OP_LET0AST: /* let* */
        if (car(sc->code) == sc->NIL) {
            new_frame_in_env(sc, sc->envir);
            sc->code = cdr(sc->code);
            s_goto(sc, OP_BEGIN);
        }
        if (!is_pair(car(sc->code)) || !is_pair(caar(sc->code))
            || !is_pair(cdaar(sc->code))) {
            Error_1(
                sc, "Bad syntax of binding spec in let* :", car(sc->code));
        }
        s_save(sc, OP_LET1AST, cdr(sc->code), car(sc->code));
        sc->code = cadaar(sc->code);
        s_goto(sc, OP_EVAL);

    case OP_LET1AST: /* let* (make new frame) */
        new_frame_in_env(sc, sc->envir);
        s_goto(sc, OP_LET2AST);

    case OP_LET2AST: /* let* (calculate parameters) */
        new_slot_in_env(sc, caar(sc->code), sc->value);
        sc->code = cdr(sc->code);
        if (is_pair(sc->code)) { /* continue */
            s_save(sc, OP_LET2AST, sc->args, sc->code);
            sc->code = cadar(sc->code);
            sc->args = sc->NIL;
            s_goto(sc, OP_EVAL);
        } else { /* end */
            sc->code = sc->args;
            sc->args = sc->NIL;
            s_goto(sc, OP_BEGIN);
        }
    default:
        snprintf(sc->strbuff, STRBUFFSIZE, "%d: illegal operator", sc->op);
        Error_0(sc, sc->strbuff);
    }
    return sc->T;
}

static pointer opexe_1(scheme *sc, enum scheme_opcodes op)
{
    pointer x, y;

    switch (op) {
    case OP_LET0REC: /* letrec */
        new_frame_in_env(sc, sc->envir);
        sc->args = sc->NIL;
        sc->value = sc->code;
        sc->code = car(sc->code);
        s_goto(sc, OP_LET1REC);

    case OP_LET1REC: /* letrec (calculate parameters) */
        sc->args = cons(sc, sc->value, sc->args);
        if (is_pair(sc->code)) { /* continue */
            if (!is_pair(car(sc->code)) || !is_pair(cdar(sc->code))) {
                Error_1(sc,
                    "Bad syntax of binding spec in letrec :", car(sc->code));
            }
            s_save(sc, OP_LET1REC, sc->args, cdr(sc->code));
            sc->code = cadar(sc->code);
            sc->args = sc->NIL;
            s_goto(sc, OP_EVAL);
        } else { /* end */
            sc->args = reverse_in_place(sc, sc->NIL, sc->args);
            sc->code = car(sc->args);
            sc->args = cdr(sc->args);
            s_goto(sc, OP_LET2REC);
        }

    case OP_LET2REC: /* letrec */
        for (x = car(sc->code), y = sc->args; y != sc->NIL;
             x = cdr(x), y = cdr(y)) {
            new_slot_in_env(sc, caar(x), car(y));
        }
        sc->code = cdr(sc->code);
        sc->args = sc->NIL;
        s_goto(sc, OP_BEGIN);

    case OP_COND0: /* cond */
        if (!is_pair(sc->code)) {
            Error_0(sc, "syntax error in cond");
        }
        s_save(sc, OP_COND1, sc->NIL, sc->code);
        sc->code = caar(sc->code);
        s_goto(sc, OP_EVAL);

    case OP_COND1: /* cond */
        if (is_true(sc->value)) {
            if ((sc->code = cdar(sc->code)) == sc->NIL) {
                s_return(sc, sc->value);
            }
            if (car(sc->code) == sc->FEED_TO) {
                if (!is_pair(cdr(sc->code))) {
                    Error_0(sc, "syntax error in cond");
                }
                x = cons(sc, sc->QUOTE, cons(sc, sc->value, sc->NIL));
                sc->code = cons(sc, cadr(sc->code), cons(sc, x, sc->NIL));
                s_goto(sc, OP_EVAL);
            }
            s_goto(sc, OP_BEGIN);
        } else {
            if ((sc->code = cdr(sc->code)) == sc->NIL) {
                s_return(sc, sc->NIL);
            } else {
                s_save(sc, OP_COND1, sc->NIL, sc->code);
                sc->code = caar(sc->code);
                s_goto(sc, OP_EVAL);
            }
        }

    case OP_DELAY: /* delay */
        x = mk_closure(sc, cons(sc, sc->NIL, sc->code), sc->envir);
        typeflag(x) = T_PROMISE;
        s_return(sc, x);

    case OP_AND0: /* and */
        if (sc->code == sc->NIL) {
            s_return(sc, sc->T);
        }
        s_save(sc, OP_AND1, sc->NIL, cdr(sc->code));
        sc->code = car(sc->code);
        s_goto(sc, OP_EVAL);

    case OP_AND1: /* and */
        if (is_false(sc->value)) {
            s_return(sc, sc->value);
        } else if (sc->code == sc->NIL) {
            s_return(sc, sc->value);
        } else {
            s_save(sc, OP_AND1, sc->NIL, cdr(sc->code));
            sc->code = car(sc->code);
            s_goto(sc, OP_EVAL);
        }

    case OP_OR0: /* or */
        if (sc->code == sc->NIL) {
            s_return(sc, sc->F);
        }
        s_save(sc, OP_OR1, sc->NIL, cdr(sc->code));
        sc->code = car(sc->code);
        s_goto(sc, OP_EVAL);

    case OP_OR1: /* or */
        if (is_true(sc->value)) {
            s_return(sc, sc->value);
        } else if (sc->code == sc->NIL) {
            s_return(sc, sc->value);
        } else {
            s_save(sc, OP_OR1, sc->NIL, cdr(sc->code));
            sc->code = car(sc->code);
            s_goto(sc, OP_EVAL);
        }

    case OP_C0STREAM: /* cons-stream */
        s_save(sc, OP_C1STREAM, sc->NIL, cdr(sc->code));
        sc->code = car(sc->code);
        s_goto(sc, OP_EVAL);

    case OP_C1STREAM: /* cons-stream */
        sc->args = sc->value; /* save sc->value to register sc->args for gc */
        x = mk_closure(sc, cons(sc, sc->NIL, sc->code), sc->envir);
        typeflag(x) = T_PROMISE;
        s_return(sc, cons(sc, sc->args, x));

    case OP_MACRO0: /* macro */
        if (is_pair(car(sc->code))) {
            x = caar(sc->code);
            sc->code = cons(
                sc, sc->LAMBDA, cons(sc, cdar(sc->code), cdr(sc->code)));
        } else {
            x = car(sc->code);
            sc->code = cadr(sc->code);
        }
        if (!is_symbol(x)) {
            Error_0(sc, "variable is not a symbol");
        }
        s_save(sc, OP_MACRO1, sc->NIL, x);
        s_goto(sc, OP_EVAL);

    case OP_MACRO1: /* macro */
        typeflag(sc->value) = T_MACRO;
        x = find_slot_in_env(sc, sc->envir, sc->code, 0);
        if (x != sc->NIL) {
            set_slot_in_env(sc, x, sc->value);
        } else {
            new_slot_in_env(sc, sc->code, sc->value);
        }
        s_return(sc, sc->code);

    case OP_CASE0: /* case */
        s_save(sc, OP_CASE1, sc->NIL, cdr(sc->code));
        sc->code = car(sc->code);
        s_goto(sc, OP_EVAL);

    case OP_CASE1: /* case */
        for (x = sc->code; x != sc->NIL; x = cdr(x)) {
            if (!is_pair(y = caar(x))) {
                break;
            }
            for (; y != sc->NIL; y = cdr(y)) {
                if (eqv(car(y), sc->value)) {
                    break;
                }
            }
            if (y != sc->NIL) {
                break;
            }
        }
        if (x != sc->NIL) {
            if (is_pair(caar(x))) {
                sc->code = cdar(x);
                s_goto(sc, OP_BEGIN);
            } else { /* else */
                s_save(sc, OP_CASE2, sc->NIL, cdar(x));
                sc->code = caar(x);
                s_goto(sc, OP_EVAL);
            }
        } else {
            s_return(sc, sc->NIL);
        }

    case OP_CASE2: /* case */
        if (is_true(sc->value)) {
            s_goto(sc, OP_BEGIN);
        } else {
            s_return(sc, sc->NIL);
        }

    case OP_PAPPLY: /* apply */
        sc->code = car(sc->args);
        sc->args = cons_star(sc, cdr(sc->args));
        /*sc->args = cadr(sc->args);*/
        s_goto(sc, OP_APPLY);

    case OP_PEVAL: /* eval */
        if (cdr(sc->args) != sc->NIL) {
            sc->envir = cadr(sc->args);
        }
        sc->code = car(sc->args);
        s_goto(sc, OP_EVAL);

    case OP_CONTINUATION: /* call-with-current-continuation */
        sc->code = car(sc->args);
        sc->args = cons(sc, mk_continuation(sc, sc->dump), sc->NIL);
        s_goto(sc, OP_APPLY);

    default:
        snprintf(sc->strbuff, STRBUFFSIZE, "%d: illegal operator", sc->op);
        Error_0(sc, sc->strbuff);
    }
    return sc->T;
}

static pointer opexe_2(scheme *sc, enum scheme_opcodes op)
{
    pointer x;
    num v;

    switch (op) {

    case OP_ADD: /* + */
        v = num_zero;
        for (x = sc->args; x != sc->NIL; x = cdr(x)) {
            v = num_add(v, nvalue(car(x)));
        }
        s_return(sc, mk_number(sc, v));

    case OP_MUL: /* * */
        v = num_one;
        for (x = sc->args; x != sc->NIL; x = cdr(x)) {
            v = num_mul(v, nvalue(car(x)));
        }
        s_return(sc, mk_number(sc, v));

    case OP_SUB: /* - */
        if (cdr(sc->args) == sc->NIL) {
            x = sc->args;
            v = num_zero;
        } else {
            x = cdr(sc->args);
            v = nvalue(car(sc->args));
        }
        for (; x != sc->NIL; x = cdr(x)) {
            v = num_sub(v, nvalue(car(x)));
        }
        s_return(sc, mk_number(sc, v));

    case OP_DIV: /* / */
        if (cdr(sc->args) == sc->NIL) {
            x = sc->args;
            v = num_one;
        } else {
            x = cdr(sc->args);
            v = nvalue(car(sc->args));
        }
        for (; x != sc->NIL; x = cdr(x)) {
            if (!is_zero_double(rvalue(car(x))))
                v = num_div(v, nvalue(car(x)));
            else {
                Error_0(sc, "/: division by zero");
            }
        }
        s_return(sc, mk_number(sc, v));

    case OP_INTDIV: /* quotient */
        if (cdr(sc->args) == sc->NIL) {
            x = sc->args;
            v = num_one;
        } else {
            x = cdr(sc->args);
            v = nvalue(car(sc->args));
        }
        for (; x != sc->NIL; x = cdr(x)) {
            if (ivalue(car(x)) != 0)
                v = num_intdiv(v, nvalue(car(x)));
            else {
                Error_0(sc, "quotient: division by zero");
            }
        }
        s_return(sc, mk_number(sc, v));

    case OP_REM: /* remainder */
        v = nvalue(car(sc->args));
        if (ivalue(cadr(sc->args)) != 0)
            v = num_rem(v, nvalue(cadr(sc->args)));
        else {
            Error_0(sc, "remainder: division by zero");
        }
        s_return(sc, mk_number(sc, v));

    case OP_MOD: /* modulo */
        v = nvalue(car(sc->args));
        if (ivalue(cadr(sc->args)) != 0)
            v = num_mod(v, nvalue(cadr(sc->args)));
        else {
            Error_0(sc, "modulo: division by zero");
        }
        s_return(sc, mk_number(sc, v));

    case OP_CHAR2INT: { /* char->integer */
        char c;
        c = (char)ivalue(car(sc->args));
        s_return(sc, mk_integer(sc, (unsigned char)c));
    }

    case OP_INT2CHAR: { /* integer->char */
        unsigned char c;
        c = (unsigned char)ivalue(car(sc->args));
        s_return(sc, mk_character(sc, (char)c));
    }

    case OP_CHARUPCASE: {
        unsigned char c;
        c = (unsigned char)ivalue(car(sc->args));
        c = toupper(c);
        s_return(sc, mk_character(sc, (char)c));
    }

    case OP_CHARDNCASE: {
        unsigned char c;
        c = (unsigned char)ivalue(car(sc->args));
        c = tolower(c);
        s_return(sc, mk_character(sc, (char)c));
    }

    case OP_VECTOR: { /* vector */
        int i;
        pointer vec;
        int len = list_length(sc, sc->args);
        if (len < 0) {
            Error_1(sc, "vector: not a proper list:", sc->args);
        }
        vec = mk_vector(sc, len);
        if (sc->no_memory) {
            s_return(sc, sc->sink);
        }
        for (x = sc->args, i = 0; is_pair(x); x = cdr(x), i++) {
            set_vector_elem(vec, i, car(x));
        }
        s_return(sc, vec);
    }

    case OP_MKVECTOR: { /* make-vector */
        pointer fill = sc->NIL;
        int len;
        pointer vec;

        len = ivalue(car(sc->args));

        if (cdr(sc->args) != sc->NIL) {
            fill = cadr(sc->args);
        }
        vec = mk_vector(sc, len);
        if (sc->no_memory) {
            s_return(sc, sc->sink);
        }
        if (fill != sc->NIL) {
            fill_vector(vec, fill);
        }
        s_return(sc, vec);
    }

    case OP_VECLEN: /* vector-length */
        s_return(sc, mk_integer(sc, ivalue(car(sc->args))));

    case OP_VECREF: { /* vector-ref */
        int index;

        index = ivalue(cadr(sc->args));

        if (index >= ivalue(car(sc->args))) {
            Error_1(sc, "vector-ref: out of bounds:", cadr(sc->args));
        }

        s_return(sc, vector_elem(car(sc->args), index));
    }

    case OP_VECSET: { /* vector-set! */
        int index;

        if (is_immutable(car(sc->args))) {
            Error_1(sc, "vector-set!: unable to alter immutable vector:",
                car(sc->args));
        }

        index = ivalue(cadr(sc->args));
        if (index >= ivalue(car(sc->args))) {
            Error_1(sc, "vector-set!: out of bounds:", cadr(sc->args));
        }

        set_vector_elem(car(sc->args), index, caddr(sc->args));
        s_return(sc, car(sc->args));
    }

    default:
        snprintf(sc->strbuff, STRBUFFSIZE, "%d: illegal operator", sc->op);
        Error_0(sc, sc->strbuff);
    }
    return sc->T;
}

static int is_list(pointer a) { return list_length(sc, a) >= 0; }

/* Result is:
   proper list: length
   circular list: -1
   not even a pair: -2
   dotted list: -2 minus length before dot
*/
int list_length(scheme *sc, pointer a)
{
    int i = 0;
    pointer slow, fast;

    slow = fast = a;
    while (1) {
        if (fast == sc->NIL)
            return i;
        if (!is_pair(fast))
            return -2 - i;
        fast = cdr(fast);
        ++i;
        if (fast == sc->NIL)
            return i;
        if (!is_pair(fast))
            return -2 - i;
        ++i;
        fast = cdr(fast);

        /* Safe because we would have already returned if `fast'
           encountered a non-pair. */
        slow = cdr(slow);
        if (fast == slow) {
            /* the fast pointer has looped back around and caught up
               with the slow pointer, hence the structure is circular,
               not of finite length, and therefore not a list */
            return -1;
        }
    }
}

static pointer env_cons(const char *s, pointer list)
{
    const char *eq;
    pointer name, value;

    if ((eq = strchr(s, '='))) {
        name = mk_counted_string(sc, s, eq - s);
        value = mk_string(sc, eq + 1);
        list = cons(sc, cons(sc, name, value), list);
    }
    return list;
}

#ifdef SCHEME_WINDOWS
static pointer wstring_to_utf8(const wchar_t *wstring, char **out_utf8)
{
    char *utf8;
    int nbyte; /* including null terminator */

    *out_utf8 = 0;
    if (!(nbyte = WideCharToMultiByte(CP_UTF8, 0, wstring, -1, 0, 0, 0, 0))) {
        return _Error_1(sc, "cannot convert WinAPI string to UTF-8", 0);
    }
    if (!(utf8 = calloc(nbyte, 1))) {
        return _Error_1(sc, "out of memory", 0);
    }
    if (WideCharToMultiByte(CP_UTF8, 0, wstring, -1, utf8, nbyte, 0, 0)
        != nbyte) {
        free(utf8);
        return _Error_1(sc, "cannot convert WinAPI string to UTF-8", 0);
    }
    *out_utf8 = utf8;
    return 0;
}
#endif

#ifdef SCHEME_WINDOWS
static pointer utf8_to_wstring(const char *utf8, wchar_t **out_wstring)
{
    wchar_t *wstring;
    int nchar; /* including null terminator */

    *out_wstring = 0;
    if (!(nchar = MultiByteToWideChar(CP_UTF8, 0, utf8, -1, 0, 0))) {
        return _Error_1(sc, "cannot convert UTF-8 to WinAPI string", 0);
    }
    if (!(wstring = calloc(nchar, sizeof(*wstring)))) {
        return _Error_1(sc, "out of memory", 0);
    }
    if (MultiByteToWideChar(CP_UTF8, 0, utf8, -1, wstring, nchar) != nchar) {
        free(wstring);
        return _Error_1(sc, "cannot convert UTF-8 to WinAPI string", 0);
    }
    *out_wstring = wstring;
    return 0;
}
#endif

#ifdef SCHEME_UNIX
static pointer os_error(const char *syscall)
{
    (void)syscall;
    return _Error_1(sc, strerror(errno), 0);
}
#endif

#ifdef SCHEME_WINDOWS
static pointer os_error(const char *syscall)
{
    pointer err;
    wchar_t *wstring;
    char *utf8;

    (void)syscall;
    FormatMessageW(
        FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM, 0,
        GetLastError(), 0, (wchar_t *)&wstring, 0, 0);
    if ((err = wstring_to_utf8(wstring, &utf8))) {
        return err;
    }
    free(wstring);
    return _Error_1(sc, utf8, 0);
}
#endif

#ifdef SCHEME_UNIX
static pointer os_get_environment_variables(void)
{
    pointer list;
    char **sp;

    list = sc->NIL;
    for (sp = environ; *sp; sp++) {
        list = env_cons(*sp, list);
    }
    return list;
}
#endif

#ifdef SCHEME_WINDOWS
static pointer os_get_environment_variables(void)
{
    pointer list, err;
    const wchar_t *environment;
    const wchar_t *w;
    char *utf8;

    list = sc->NIL;
    if (!(w = environment = GetEnvironmentStringsW())) {
        return os_error("GetEnvironmentStrings");
    }
    while (*w) {
        if ((err = wstring_to_utf8(w, &utf8))) {
            return err;
        }
        list = env_cons(utf8, list);
        free(utf8);
        for (; *w; w++)
            ;
        w++;
    }
    FreeEnvironmentStringsW(environment);
    return list;
}
#endif

#ifdef SCHEME_UNIX
static pointer os_set_environment_variable(
    const char *name, const char *value)
{
    if (value) {
        if (setenv(name, value, 1) == -1) {
            return os_error("setenv");
        }
    } else {
        if (unsetenv(name) == -1) {
            return os_error("unsetenv");
        }
    }
    return _s_return(sc, sc->T);
}
#endif

#ifdef SCHEME_WINDOWS
static pointer os_set_environment_variable(
    const char *name, const char *value)
{
    pointer err;
    wchar_t *wname;
    wchar_t *wvalue;
    BOOL ok;

    if ((err = utf8_to_wstring(name, &wname))) {
        return err;
    }
    wvalue = 0;
    if (value) {
        if ((err = utf8_to_wstring(name, &wvalue))) {
            free(wname);
            return err;
        }
    }
    ok = SetEnvironmentVariableW(wname, wvalue);
    free(wvalue);
    free(wname);
    return ok ? _s_return(sc, sc->T) : os_error("SetEnvironmentVariable");
}
#endif

#ifdef SCHEME_UNIX
static pointer os_user_supplementary_gids(void)
{
    pointer list;
    gid_t effective, gid;
    gid_t *buf;
    gid_t *newbuf;
    size_t cap;
    int n;

    buf = 0;
    cap = 16 * sizeof(*buf);
    for (;;) {
        newbuf = realloc(buf, cap);
        if (!newbuf) {
            free(buf);
            return sc->NIL;
        }
        buf = newbuf;
        n = getgroups(cap / sizeof(*buf), buf);
        if (n != -1) {
            break;
        }
        if (errno != EINVAL) {
            return os_error("getgroups");
        }
        cap *= 2;
    }
    effective = getegid();
    list = sc->NIL;
    while (n > 0) {
        gid = buf[--n];
        if (gid != effective) {
            list = cons(sc, mk_integer(sc, gid), list);
        }
    }
    free(buf);
    return _s_return(sc, list);
}
#endif

#ifdef SCHEME_UNIX
static pointer os_open_directory_list(const char *path)
{
    void *p;

    if (!(p = opendir(path))) {
        return os_error("opendir");
    }
    return _s_return(sc, mk_opaque_type(T_DIRECTORY_LIST, p));
}
#endif

#ifdef SCHEME_UNIX
static pointer os_read_directory(pointer obj)
{
    DIR *p;
    struct dirent *d;
    const char *name;

    p = obj->_object._opaque._p;
    if (p) {
        for (;;) {
            errno = 0;
            d = readdir(p);
            if (!d && errno) {
                return os_error("readdir");
            }
            if (!d) {
                return _s_return(sc, sc->EOF_OBJ);
            }
            name = d->d_name;
            if (strcmp(name, ".") && strcmp(name, "..")) {
                return _s_return(sc, mk_string(sc, name));
            }
        }
    }
    return _s_return(sc, sc->EOF_OBJ);
}
#endif

#ifdef SCHEME_UNIX
static void os_close_directory_list(pointer obj)
{
    if (obj->_object._opaque._p) {
        closedir(obj->_object._opaque._p);
        obj->_object._opaque._p = 0;
    }
}
#endif

#ifdef SCHEME_UNIX
static pointer os_create_directory(const char *path, long mode)
{
    if (mkdir(path, mode) == -1) {
        return os_error("mkdir");
    }
    return _s_return(sc, sc->T);
}
#endif

#ifdef SCHEME_WINDOWS
static pointer os_create_directory(const char *path, long mode)
{
    pointer err;
    wchar_t *wpath;

    (void)mode; // TODO
    if ((err = utf8_to_wstring(path, &wpath))) {
        return err;
    }
    if (!CreateDirectoryW(wpath, 0)) {
        return os_error("CreateDirectory");
    }
    return _s_return(sc, sc->T);
}
#endif

#ifdef SCHEME_UNIX
static pointer os_delete_directory(const char *path)
{
    if (rmdir(path) == -1) {
        return os_error("rmdir");
    }
    return _s_return(sc, sc->T);
}
#endif

#ifdef SCHEME_WINDOWS
static pointer os_delete_directory(const char *path)
{
    pointer err;
    wchar_t *wpath;

    if ((err = utf8_to_wstring(path, &wpath))) {
        return err;
    }
    if (!RemoveDirectoryW(wpath)) {
        return os_error("RemoveDirectory");
    }
    return _s_return(sc, sc->T);
}
#endif

#ifdef SCHEME_UNIX
static pointer os_delete_file(const char *path)
{
    if (unlink(path) == -1) {
        return os_error("unlink");
    }
    return _s_return(sc, sc->T);
}
#endif

#ifdef SCHEME_WINDOWS
static pointer os_delete_file(const char *path)
{
    pointer err;
    wchar_t *wpath;

    if ((err = utf8_to_wstring(path, &wpath))) {
        return err;
    }
    if (!DeleteFileW(wpath)) {
        return os_error("DeleteFile");
    }
    return _s_return(sc, sc->T);
}
#endif

#ifdef SCHEME_UNIX
static pointer os_working_directory(void)
{
    pointer string;
    char *newbuf;
    char *buf;
    size_t cap;

    buf = 0;
    cap = 128;
    for (;;) {
        newbuf = realloc(buf, cap);
        if (!newbuf) {
            free(buf);
            return sc->NIL;
        }
        buf = newbuf;
        if (getcwd(buf, cap)) {
            break;
        }
        if (errno != ERANGE) {
            return os_error("getcwd");
        }
        cap *= 2;
    }
    string = mk_string(sc, buf);
    free(buf);
    return _s_return(sc, string);
}
#endif

#ifdef SCHEME_WINDOWS
static pointer os_working_directory(void)
{
    pointer err;
    wchar_t *wstring;
    char *utf8;
    DWORD cap, len;

    if (!(cap = GetCurrentDirectoryW(0, 0))) {
        return os_error("GetCurrentDirectory");
    }
    if (!(wstring = calloc(cap, sizeof(*wstring)))) {
        return _Error_1(sc, "out of memory", 0);
    }
    if (!(len = GetCurrentDirectoryW(cap, wstring))) {
        return os_error("GetCurrentDirectory");
    }
    if (len != cap - 1) {
        return _Error_1(sc, "GetCurrentDirectory length mismatch", 0);
    }
    err = wstring_to_utf8(wstring, &utf8);
    free(wstring);
    if (err) {
        return err;
    }
    return mk_string(sc, utf8);
}
#endif

#ifdef SCHEME_UNIX
static pointer os_real_path_or_false(const char *path)
{
    pointer ans;
    char *real;

    if (!(real = calloc(1, PATH_MAX + 1))) {
        die("out of memory");
    }
    if (realpath(path, real)) {
        ans = mk_string(sc, real);
    } else {
        ans = sc->F;
    }
    free(real);
    return ans;
}
#endif

#ifdef SCHEME_UNIX
static pointer os_real_path(const char *path)
{
    pointer ans;

    ans = os_real_path_or_false(path);
    return (ans == sc->F) ? os_error("stat") : _s_return(sc, ans);
}
#endif

#ifdef SCHEME_UNIX
static pointer os_file_exists_p(const char *path)
{
    struct stat st;

    if (stat(path, &st) == 0) {
        return _s_return(sc, sc->T);
    }
    if ((errno == ENOENT) || (errno == ENOTDIR)) {
        return _s_return(sc, sc->F);
    }
    return os_error("stat");
}
#endif

#ifdef SCHEME_UNIX
static pointer os_file_info(const char *path, int follow)
{
    struct stat *st;
    int ans;

    if (!(st = sc->malloc(sizeof(*st)))) {
        return sc->NIL;
    }
    if (follow) {
        ans = stat(path, st);
    } else {
        ans = lstat(path, st);
    }
    if (ans == -1) {
        sc->free(st);
        return os_error("stat");
    }
    return _s_return(sc, mk_opaque_type(T_FILE_INFO, st));
}
#endif

#ifdef SCHEME_UNIX
static long os_file_info_gid(struct stat *st) { return st->st_gid; }
#endif

#ifdef SCHEME_UNIX
static long os_file_info_uid(struct stat *st) { return st->st_uid; }
#endif

#ifdef SCHEME_UNIX
static long os_file_info_mode(struct stat *st) { return st->st_mode; }
#endif

#ifdef SCHEME_UNIX
static void os_file_info_mtime(struct stat *st, long *out_sec, long *out_nsec)
{
    *out_sec = st->st_mtim.tv_sec;
    *out_nsec = st->st_mtim.tv_nsec;
}
#endif

#ifdef SCHEME_UNIX
static long os_file_info_size(struct stat *st) { return st->st_size; }
#endif

#ifdef SCHEME_WINDOWS
static pointer os_file_info(const char *path, int follow)
{
    WIN32_FIND_DATAW buf;
    void *p;
    HANDLE handle;
    pointer err;
    wchar_t *wpath;

    (void)follow;
    if ((err = utf8_to_wstring(path, &wpath))) {
        return err;
    }
    handle = FindFirstFileW(wpath, &buf);
    if (handle == INVALID_HANDLE_VALUE) {
        return os_error("FindFirstFile");
    }
    if (!FindClose(handle)) {
        return os_error("FindClose");
    }
    if (!(p = sc->malloc(sizeof(buf)))) {
        return sc->NIL;
    }
    memcpy(p, &buf, sizeof(buf));
    return _s_return(sc, mk_opaque_type(T_FILE_INFO, p));
}
#endif

#ifdef SCHEME_WINDOWS
static long os_file_info_gid(WIN32_FIND_DATA *data) { return 0; }
#endif

#ifdef SCHEME_WINDOWS
static long os_file_info_uid(WIN32_FIND_DATA *data) { return 0; }
#endif

#ifdef SCHEME_WINDOWS
static long os_file_info_mode(WIN32_FIND_DATA *data) { return 0; }
#endif

#ifdef SCHEME_WINDOWS
static long os_file_info_size(WIN32_FIND_DATA *data)
{
    return data->nFileSizeLow + (data->nFileSizeHigh * (MAXDWORD + 1));
}
#endif

#ifdef SCHEME_UNIX
static pointer os_set_file_mode(const char *path, long mode)
{
    if (chmod(path, mode) == -1) {
        return os_error("chmod");
    }
    return _s_return(sc, sc->T);
}
#endif

#ifdef SCHEME_WINDOWS
static pointer os_set_file_mode(const char *path, long mode)
{
    return _Error_1(sc, "chmod not available on Windows", 0);
}
#endif

#ifdef SCHEME_UNIX
static pointer os_set_working_directory(const char *path)
{
    if (chdir(path) == -1) {
        return os_error("chdir");
    }
    return _s_return(sc, sc->T);
}
#endif

#ifdef SCHEME_WINDOWS
static pointer os_set_working_directory(const char *path)
{
    pointer err;
    wchar_t *wpath;

    if ((err = utf8_to_wstring(path, &wpath))) {
        return err;
    }
    if (!SetCurrentDirectory(wpath)) {
        return os_error("SetCurrentDirectory");
    }
    return _s_return(sc, sc->T);
}
#endif

#ifdef SCHEME_UNIX
static pointer os_user_info(const char *name, long uid)
{
    struct user_info *info;
    struct passwd *pw;

    if (name) {
        pw = getpwnam(name);
    } else {
        pw = getpwuid(uid);
    }
    if (!pw) {
        return os_error("getpw*");
    }
    if (!(info = calloc(1, sizeof(*info)))) {
        return _Error_1(sc, "out of memory", 0);
    }
    info->uid = pw->pw_uid;
    info->gid = pw->pw_gid;
    info->name = mk_string(sc, pw->pw_name);
    info->home_dir = mk_string(sc, pw->pw_dir);
    info->shell = mk_string(sc, pw->pw_shell);
    info->full_name = mk_string(sc, pw->pw_gecos);
    info->parsed_full_name = cons(sc, info->full_name, sc->NIL);
    return _s_return(sc, mk_opaque_type(T_USER_INFO, info));
}
#endif

#ifdef SCHEME_WINDOWS
static pointer os_user_info(const char *name, long uid) {}
#endif

static pointer opexe_4(scheme *sc, enum scheme_opcodes op)
{
    pointer x;

    switch (op) {
    case OP_FORCE: /* force */
        sc->code = car(sc->args);
        if (is_promise(sc->code)) {
            /* Should change type to closure here */
            s_save(sc, OP_SAVE_FORCED, sc->NIL, sc->code);
            sc->args = sc->NIL;
            s_goto(sc, OP_APPLY);
        } else {
            s_return(sc, sc->code);
        }

    case OP_SAVE_FORCED: /* Save forced value replacing promise */
        memcpy(sc->code, sc->value, sizeof(struct cell));
        s_return(sc, sc->value);

    case OP_WRITE: /* write */
    case OP_DISPLAY: /* display */
    case OP_WRITE_CHAR: /* write-char */
        if (is_pair(cdr(sc->args))) {
            if (cadr(sc->args) != sc->outport) {
                x = cons(sc, sc->outport, sc->NIL);
                s_save(sc, OP_SET_OUTPORT, x, sc->NIL);
                sc->outport = cadr(sc->args);
            }
        }
        sc->args = car(sc->args);
        if (op == OP_WRITE) {
            sc->print_flag = 1;
        } else {
            sc->print_flag = 0;
        }
        s_goto(sc, OP_P0LIST);

    case OP_NEWLINE: /* newline */
        if (is_pair(sc->args)) {
            if (car(sc->args) != sc->outport) {
                x = cons(sc, sc->outport, sc->NIL);
                s_save(sc, OP_SET_OUTPORT, x, sc->NIL);
                sc->outport = car(sc->args);
            }
        }
        putstr(sc, "\n");
        s_return(sc, sc->T);

    case OP_ERR0: /* error */
        sc->retcode = -1;
        if (!is_string(car(sc->args))) {
            sc->args = cons(sc, mk_string(sc, " -- "), sc->args);
            setimmutable(car(sc->args));
        }
        putstr(sc, "Error: ");
        putstr(sc, strvalue(car(sc->args)));
        sc->args = cdr(sc->args);
        s_goto(sc, OP_ERR1);

    case OP_ERR1: /* error */
        putstr(sc, " ");
        if (sc->args != sc->NIL) {
            s_save(sc, OP_ERR1, cdr(sc->args), sc->NIL);
            sc->args = car(sc->args);
            sc->print_flag = 1;
            s_goto(sc, OP_P0LIST);
        } else {
            putstr(sc, "\n");
            if (sc->interactive_repl) {
                s_goto(sc, OP_T0LVL);
            } else {
                return sc->NIL;
            }
        }

    case OP_OPEN_INFILE: /* open-input-file */
    case OP_OPEN_OUTFILE: /* open-output-file */
    case OP_OPEN_INOUTFILE: /* open-input-output-file */ {
        int prop = 0;
        pointer p;
        switch (op) {
        case OP_OPEN_INFILE:
            prop = port_input;
            break;
        case OP_OPEN_OUTFILE:
            prop = port_output;
            break;
        case OP_OPEN_INOUTFILE:
            prop = port_input | port_output;
            break;
        }
        p = port_from_filename(sc, strvalue(car(sc->args)), prop);
        if (p == sc->NIL) {
            s_return(sc, sc->F);
        }
        s_return(sc, p);
    }

    case OP_CLOSE_INPORT: /* close-input-port */
        port_close(sc, car(sc->args), port_input);
        s_return(sc, sc->T);

    case OP_CLOSE_OUTPORT: /* close-output-port */
        port_close(sc, car(sc->args), port_output);
        s_return(sc, sc->T);

    case OP_INT_ENV: /* interaction-environment */
        s_return(sc, sc->global_env);

    case OP_CURR_ENV: /* current-environment */
        s_return(sc, sc->envir);
    }
    return sc->T;
}

static pointer opexe_5(scheme *sc, enum scheme_opcodes op)
{
    pointer x;

    if (sc->nesting != 0) {
        int n = sc->nesting;
        sc->nesting = 0;
        sc->retcode = -1;
        Error_1(sc, "unmatched parentheses:", mk_integer(sc, n));
    }

    switch (op) {
    /* ========== reading part ========== */
    case OP_READ:
        if (!is_pair(sc->args)) {
            s_goto(sc, OP_READ_INTERNAL);
        }
        if (!is_inport(car(sc->args))) {
            Error_1(sc, "read: not an input port:", car(sc->args));
        }
        if (car(sc->args) == sc->inport) {
            s_goto(sc, OP_READ_INTERNAL);
        }
        x = sc->inport;
        sc->inport = car(sc->args);
        x = cons(sc, x, sc->NIL);
        s_save(sc, OP_SET_INPORT, x, sc->NIL);
        s_goto(sc, OP_READ_INTERNAL);

    case OP_READ_CHAR: /* read-char */
    case OP_PEEK_CHAR: /* peek-char */ {
        int c;
        if (is_pair(sc->args)) {
            if (car(sc->args) != sc->inport) {
                x = sc->inport;
                x = cons(sc, x, sc->NIL);
                s_save(sc, OP_SET_INPORT, x, sc->NIL);
                sc->inport = car(sc->args);
            }
        }
        c = inchar(sc);
        if (c == EOF) {
            s_return(sc, sc->EOF_OBJ);
        }
        if (sc->op == OP_PEEK_CHAR) {
            backchar(sc, c);
        }
        s_return(sc, mk_character(sc, c));
    }

    case OP_CHAR_READY: /* char-ready? */ {
        pointer p = sc->inport;
        int res;
        if (is_pair(sc->args)) {
            p = car(sc->args);
        }
        res = p->_object._port->kind & port_string;
        s_retbool(res);
    }

    case OP_SET_INPORT: /* set-input-port */
        sc->inport = car(sc->args);
        s_return(sc, sc->value);

    case OP_SET_OUTPORT: /* set-output-port */
        sc->outport = car(sc->args);
        s_return(sc, sc->value);

    case OP_RDSEXPR:
        switch (sc->tok) {
        case TOK_EOF:
            s_return(sc, sc->EOF_OBJ);
            /* NOTREACHED */
            /*
             * Commented out because we now skip comments in the scanner
             *
                      case TOK_COMMENT: {
                           int c;
                           while ((c=inchar(sc)) != '\n' && c!=EOF)
                                ;
                           sc->tok = token(sc);
                           s_goto(sc,OP_RDSEXPR);
                      }
            */
        case TOK_VEC:
            s_save(sc, OP_RDVEC, sc->NIL, sc->NIL);
            /* fall through */
        case TOK_LPAREN:
            sc->tok = token(sc);
            if (sc->tok == TOK_RPAREN) {
                s_return(sc, sc->NIL);
            } else if (sc->tok == TOK_DOT) {
                Error_0(sc, "syntax error: illegal dot expression");
            } else {
                sc->nesting_stack[sc->file_i]++;
                s_save(sc, OP_RDLIST, sc->NIL, sc->NIL);
                s_goto(sc, OP_RDSEXPR);
            }
        case TOK_QUOTE:
            s_save(sc, OP_RDQUOTE, sc->NIL, sc->NIL);
            sc->tok = token(sc);
            s_goto(sc, OP_RDSEXPR);
        case TOK_BQUOTE:
            sc->tok = token(sc);
            if (sc->tok == TOK_VEC) {
                s_save(sc, OP_RDQQUOTEVEC, sc->NIL, sc->NIL);
                sc->tok = TOK_LPAREN;
                s_goto(sc, OP_RDSEXPR);
            } else {
                s_save(sc, OP_RDQQUOTE, sc->NIL, sc->NIL);
            }
            s_goto(sc, OP_RDSEXPR);
        case TOK_COMMA:
            s_save(sc, OP_RDUNQUOTE, sc->NIL, sc->NIL);
            sc->tok = token(sc);
            s_goto(sc, OP_RDSEXPR);
        case TOK_ATMARK:
            s_save(sc, OP_RDUQTSP, sc->NIL, sc->NIL);
            sc->tok = token(sc);
            s_goto(sc, OP_RDSEXPR);
        case TOK_ATOM:
            s_return(sc, mk_atom(sc, readstr_upto(sc, DELIMITERS)));
        case TOK_DQUOTE:
            x = readstrexp(sc);
            if (x == sc->F) {
                Error_0(sc, "Error reading string");
            }
            setimmutable(x);
            s_return(sc, x);
        case TOK_SHARP: {
            Error_0(sc, "undefined sharp expression");
        }
        case TOK_SHARP_CONST:
            if ((x = mk_sharp_const(sc, readstr_upto(sc, DELIMITERS)))
                == sc->NIL) {
                Error_0(sc, "undefined sharp expression");
            } else {
                s_return(sc, x);
            }
        default:
            Error_0(sc, "syntax error: illegal token");
        }
        break;

    case OP_RDLIST: {
        sc->args = cons(sc, sc->value, sc->args);
        sc->tok = token(sc);
        /* We now skip comments in the scanner
                  while (sc->tok == TOK_COMMENT) {
                       int c;
                       while ((c=inchar(sc)) != '\n' && c!=EOF)
                            ;
                       sc->tok = token(sc);
                  }
        */
        if (sc->tok == TOK_EOF) {
            s_return(sc, sc->EOF_OBJ);
        } else if (sc->tok == TOK_RPAREN) {
            int c = inchar(sc);
            if (c != '\n')
                backchar(sc, c);
#if SHOW_ERROR_LINE
            else if (sc->load_stack[sc->file_i].kind & port_file)
                sc->load_stack[sc->file_i].rep.stdio.curr_line++;
#endif
            sc->nesting_stack[sc->file_i]--;
            s_return(sc, reverse_in_place(sc, sc->NIL, sc->args));
        } else if (sc->tok == TOK_DOT) {
            s_save(sc, OP_RDDOT, sc->args, sc->NIL);
            sc->tok = token(sc);
            s_goto(sc, OP_RDSEXPR);
        } else {
            s_save(sc, OP_RDLIST, sc->args, sc->NIL);
            ;
            s_goto(sc, OP_RDSEXPR);
        }
    }

    case OP_RDDOT:
        if (token(sc) != TOK_RPAREN) {
            Error_0(sc, "syntax error: illegal dot expression");
        } else {
            sc->nesting_stack[sc->file_i]--;
            s_return(sc, reverse_in_place(sc, sc->value, sc->args));
        }

    case OP_RDQUOTE:
        s_return(sc, cons(sc, sc->QUOTE, cons(sc, sc->value, sc->NIL)));

    case OP_RDQQUOTE:
        s_return(sc, cons(sc, sc->QQUOTE, cons(sc, sc->value, sc->NIL)));

    case OP_RDQQUOTEVEC:
        s_return(sc,
            cons(sc, mk_symbol(sc, "apply"),
                cons(sc, mk_symbol(sc, "vector"),
                    cons(sc,
                        cons(sc, sc->QQUOTE, cons(sc, sc->value, sc->NIL)),
                        sc->NIL))));

    case OP_RDUNQUOTE:
        s_return(sc, cons(sc, sc->UNQUOTE, cons(sc, sc->value, sc->NIL)));

    case OP_RDUQTSP:
        s_return(sc, cons(sc, sc->UNQUOTESP, cons(sc, sc->value, sc->NIL)));

    case OP_RDVEC:
        /*sc->code=cons(sc,mk_proc(sc,OP_VECTOR),sc->value);
        s_goto(sc,OP_EVAL); Cannot be quoted*/
        /*x=cons(sc,mk_proc(sc,OP_VECTOR),sc->value);
        s_return(sc,x); Cannot be part of pairs*/
        /*sc->code=mk_proc(sc,OP_VECTOR);
        sc->args=sc->value;
        s_goto(sc,OP_APPLY);*/
        sc->args = sc->value;
        s_goto(sc, OP_VECTOR);

    /* ========== printing part ========== */
    case OP_P0LIST:
        if (is_vector(sc->args)) {
            putstr(sc, "#(");
            sc->args = cons(sc, sc->args, mk_integer(sc, 0));
            s_goto(sc, OP_PVECFROM);
        } else if (is_environment(sc->args)) {
            putstr(sc, "#<ENVIRONMENT>");
            s_return(sc, sc->T);
        } else if (!is_pair(sc->args)) {
            printatom(sc, sc->args, sc->print_flag);
            s_return(sc, sc->T);
        } else if (car(sc->args) == sc->QUOTE && ok_abbrev(cdr(sc->args))) {
            putstr(sc, "'");
            sc->args = cadr(sc->args);
            s_goto(sc, OP_P0LIST);
        } else if (car(sc->args) == sc->QQUOTE && ok_abbrev(cdr(sc->args))) {
            putstr(sc, "`");
            sc->args = cadr(sc->args);
            s_goto(sc, OP_P0LIST);
        } else if (car(sc->args) == sc->UNQUOTE && ok_abbrev(cdr(sc->args))) {
            putstr(sc, ",");
            sc->args = cadr(sc->args);
            s_goto(sc, OP_P0LIST);
        } else if (car(sc->args) == sc->UNQUOTESP
            && ok_abbrev(cdr(sc->args))) {
            putstr(sc, ",@");
            sc->args = cadr(sc->args);
            s_goto(sc, OP_P0LIST);
        } else {
            putstr(sc, "(");
            s_save(sc, OP_P1LIST, cdr(sc->args), sc->NIL);
            sc->args = car(sc->args);
            s_goto(sc, OP_P0LIST);
        }

    case OP_P1LIST:
        if (is_pair(sc->args)) {
            s_save(sc, OP_P1LIST, cdr(sc->args), sc->NIL);
            putstr(sc, " ");
            sc->args = car(sc->args);
            s_goto(sc, OP_P0LIST);
        } else if (is_vector(sc->args)) {
            s_save(sc, OP_P1LIST, sc->NIL, sc->NIL);
            putstr(sc, " . ");
            s_goto(sc, OP_P0LIST);
        } else {
            if (sc->args != sc->NIL) {
                putstr(sc, " . ");
                printatom(sc, sc->args, sc->print_flag);
            }
            putstr(sc, ")");
            s_return(sc, sc->T);
        }
    case OP_PVECFROM: {
        int i = ivalue_unchecked(cdr(sc->args));
        pointer vec = car(sc->args);
        int len = ivalue_unchecked(vec);
        if (i == len) {
            putstr(sc, ")");
            s_return(sc, sc->T);
        } else {
            pointer elem = vector_elem(vec, i);
            ivalue_unchecked(cdr(sc->args)) = i + 1;
            s_save(sc, OP_PVECFROM, sc->args, sc->NIL);
            sc->args = elem;
            if (i > 0)
                putstr(sc, " ");
            s_goto(sc, OP_P0LIST);
        }
    }

    default:
        snprintf(sc->strbuff, STRBUFFSIZE, "%d: illegal operator", sc->op);
        Error_0(sc, sc->strbuff);
    }
    return sc->T;
}

static pointer opexe_6(scheme *sc, enum scheme_opcodes op)
{
    pointer x, y;

    switch (op) {

    case OP_ASSQ: /* assq */ /* a.k */
        x = car(sc->args);
        for (y = cadr(sc->args); is_pair(y); y = cdr(y)) {
            if (!is_pair(car(y))) {
                Error_0(sc, "unable to handle non pair element");
            }
            if (x == caar(y))
                break;
        }
        if (is_pair(y)) {
            s_return(sc, car(y));
        } else {
            s_return(sc, sc->F);
        }

    case OP_GET_CLOSURE: /* get-closure-code */ /* a.k */
        sc->args = car(sc->args);
        if (sc->args == sc->NIL) {
            s_return(sc, sc->F);
        } else if (is_closure(sc->args)) {
            s_return(sc, cons(sc, sc->LAMBDA, closure_code(sc->value)));
        } else if (is_macro(sc->args)) {
            s_return(sc, cons(sc, sc->LAMBDA, closure_code(sc->value)));
        } else {
            s_return(sc, sc->F);
        }
    default:
        snprintf(sc->strbuff, STRBUFFSIZE, "%d: illegal operator", sc->op);
        Error_0(sc, sc->strbuff);
    }
    return sc->T; /* NOTREACHED */
}

typedef pointer (*dispatch_func)(scheme *, enum scheme_opcodes);

typedef int (*test_predicate)(pointer);
static int is_any(pointer p)
{
    (void)p;
    return 1;
}

static int is_nonneg(pointer p) { return ivalue(p) >= 0 && is_integer(p); }

/* Correspond carefully with following defines! */
static struct {
    test_predicate fct;
    const char *kind;
} tests[] = { { 0, 0 }, /* unused */
    { is_any, 0 }, { is_string, "string" }, { is_symbol, "symbol" },
    { is_port, "port" }, { is_inport, "input port" },
    { is_outport, "output port" }, { is_environment, "environment" },
    { is_pair, "pair" }, { 0, "pair or '()" }, { is_character, "character" },
    { is_vector, "vector" }, { is_number, "number" },
    { is_integer, "integer" }, { is_nonneg, "non-negative integer" } };

#define TST_NONE 0
#define TST_ANY "\001"
#define TST_STRING "\002"
#define TST_SYMBOL "\003"
#define TST_PORT "\004"
#define TST_INPORT "\005"
#define TST_OUTPORT "\006"
#define TST_ENVIRONMENT "\007"
#define TST_PAIR "\010"
#define TST_LIST "\011"
#define TST_CHAR "\012"
#define TST_VECTOR "\013"
#define TST_NUMBER "\014"
#define TST_INTEGER "\015"
#define TST_NATURAL "\016"

typedef struct {
    dispatch_func func;
    char *name;
    int min_arity;
    int max_arity;
    char *arg_tests_encoding;
} op_code_info;

#define INF_ARG 0xffff

static op_code_info dispatch_table[] = {
#define _OP_DEF(A, B, C, D, E, OP) { A, B, C, D, E },
#include "opdefines.h"
    { 0 }
};

static const size_t ndispatch
    = (sizeof(dispatch_table) / sizeof(dispatch_table[0])) - 1;

struct primitive {
    const char *name;
    pointer (*func)(void);
};

#define ARG_ERR (sc->EOF_OBJ) // TODO

static const char *prim_name;
static const char *prim_arg_err;
static pointer prim_args;
static int prim_args_done;

static int arg_left(void) { return prim_args != sc->NIL; }

static int arg_set_err(const char *msg)
{
    if (!prim_arg_err) {
        prim_arg_err = msg;
    }
    return 0;
}

static int arg_obj(pointer *out)
{
    *out = NULL;
    if (prim_args == sc->NIL) {
        return arg_set_err("too few args");
    }
    if (!is_pair(prim_args)) {
        return arg_set_err("arglist is dotted");
    }
    *out = car(prim_args);
    prim_args = cdr(prim_args);
    return 1;
}

static int arg_obj_type(
    pointer *out, int predicate(pointer), const char *typename)
{
    pointer arg;

    (void)typename; // TODO: use this in error message
    *out = 0;
    if (!arg_obj(&arg)) {
        return 0;
    }
    if (!predicate(arg)) {
        return arg_set_err("arg is not of the desired type");
    }
    *out = arg;
    return 1;
}

static int arg_err(void)
{
    int ok;

    prim_args_done = 1;
    if (arg_left()) {
        ok = arg_set_err("too many args");
    } else {
        ok = !prim_arg_err;
    }
    return !ok;
}

static int arg_pair(pointer *out)
{
    pointer arg;

    if (!arg_obj(&arg)) {
        return 0;
    }
    if (!is_pair(arg)) {
        return arg_set_err("arg is not a pair");
    }
    *out = arg;
    return 1;
}

static int arg_symbol(const char **out)
{
    pointer arg;

    if (!arg_obj(&arg)) {
        return 0;
    }
    if (!is_symbol(arg)) {
        return arg_set_err("arg is not a symbol");
    }
    *out = symname(arg);
    return 1;
}

static int arg_string(const char **out)
{
    pointer arg;

    if (!arg_obj(&arg)) {
        return 0;
    }
    if (!is_string(arg)) {
        return arg_set_err("arg is not a string");
    }
    *out = strvalue(arg);
    return 1;
}

static int arg_char(long *out)
{
    pointer arg;

    if (!arg_obj(&arg)) {
        return 0;
    }
    if (!is_character(arg)) {
        return arg_set_err("arg is not a character");
    }
    *out = charvalue(arg);
    return 1;
}

static int arg_boolean(int *out)
{
    pointer arg;

    *out = 0;
    if (!arg_obj(&arg)) {
        return 0;
    }
    if (arg == sc->T) {
        *out = 1;
        return 1;
    }
    return arg == sc->F;
}

static int arg_long(long *out, long min, long max)
{
    pointer arg;
    long val;

    if (!arg_obj(&arg)) {
        return 0;
    }
    if (!is_integer(arg)) {
        return arg_set_err("arg is not an integer");
    }
    val = ivalue(arg);
    if (val < min) {
        return arg_set_err("arg is too small");
    }
    if (val > max) {
        return arg_set_err("arg is too large");
    }
    *out = val;
    return 1;
}

static int arg_string_or_long(
    const char **out_string, long *out_long, long min, long max)
{
    pointer arg;
    long val;

    *out_string = 0;
    *out_long = 0;
    if (!arg_obj(&arg)) {
        return 0;
    }
    if (is_string(arg)) {
        *out_string = strvalue(arg);
        return 1;
    }
    if (!is_integer(arg)) {
        return arg_set_err("arg is not an integer or a string");
    }
    val = ivalue(arg);
    if (val < min) {
        return arg_set_err("arg is too small");
    }
    if (val > max) {
        return arg_set_err("arg is too large");
    }
    *out_long = val;
    return 1;
}

static int arg_num(num *out)
{
    pointer arg;

    if (!arg_obj(&arg)) {
        return 0;
    }
    if (!is_number(arg)) {
        return arg_set_err("arg is not a number");
    }
    *out = arg->_object._number;
    return 1;
}

static int arg_stat(void **out)
{
    pointer arg;
    void *st;

    *out = 0;
    if (!arg_obj(&arg)) {
        return 0;
    }
    if (!is_file_info(arg)) {
        return arg_set_err("arg is not a file-info object");
    }
    st = arg->_object._opaque._p;
    *out = st;
    return 1;
}

static int arg_user_info(struct user_info **out)
{
    pointer arg;
    struct user_info *info;

    *out = 0;
    if (!arg_obj(&arg)) {
        return 0;
    }
    if (!is_user_info(arg)) {
        return arg_set_err("arg is not a user-info object");
    }
    info = arg->_object._opaque._p;
    *out = info;
    return 1;
}

static int arg_timespec(long *out_sec, long *out_nsec)
{
    pointer arg;

    *out_sec = *out_nsec = 0;
    if (!arg_obj(&arg)) {
        return 0;
    }
    if (!is_timespec(arg)) {
        return arg_set_err("arg is not a timespec object");
    }
    *out_sec = timespec_sec(arg);
    *out_nsec = timespec_nsec(arg);
    return 1;
}

static pointer obj_predicate(int predicate(pointer))
{
    pointer x;

    arg_obj(&x);
    return arg_err() ? ARG_ERR : _s_return(sc, predicate(x) ? sc->T : sc->F);
}

static pointer cmp_primitive(int cmp(num, num))
{
    num a, b;
    int ok = 1;

    if (!arg_num(&a)) {
        goto fail;
    }
    do {
        if (!arg_num(&b)) {
            goto fail;
        }
        if (ok) {
            if (!cmp(a, b)) {
                ok = 0;
            }
        }
        a = b;
    } while (arg_left());
    if (arg_err()) {
        return ARG_ERR;
    }
    return _s_return(sc, ok ? sc->T : sc->F);
fail:
    arg_err();
    return ARG_ERR;
}

/// == Primitives
///

/// === Equivalence predicates

/// *Procedure* (*eq?* _a_ _b_)
///
/// From R7RS
///
/// Return `#t` if _a_ and _b_ are the same object in memory. Else
/// return `#f`. Characters and integers are the same if they have the
/// same integer value. Strings and lists are the same if they are the
/// same reference. Symbols are the same if their *symbol\->string*
/// representations are *equal?*.
///
static pointer prim_eq_p(void)
{
    pointer a, b;

    arg_obj(&a);
    arg_obj(&b);
    if (arg_err()) {
        return ARG_ERR;
    }
    s_retbool(a == b);
}

/// *Procedure* (*eqv?* _a_ _b_)
///
/// From R7RS
///
/// Return `#t` if the objects _a_ and _b_ have an equivalent value.
/// Else return `#f`. Integers are considered equivalent if they have
/// the same numeric value. Characters are considered equivalent if
/// they have the same integer codepoint. Other objects are considered
/// equivalent if and only if they are EQ?.
///
static pointer prim_eqv_p(void)
{
    pointer a, b;

    arg_obj(&a);
    arg_obj(&b);
    if (arg_err()) {
        return ARG_ERR;
    }
    s_retbool(eqv(a, b));
}

/// === Booleans

/// *Procedure* (*not* _obj_)
///
/// From R7RS
///
/// If _obj_ is `#f`, returns `#t`. Else returns `#f`.
///
static pointer prim_not(void)
{
    pointer x;

    arg_obj(&x);
    if (arg_err()) {
        return ARG_ERR;
    }
    s_retbool(is_false(x));
}

static pointer prim_boolean_p(void) { return obj_predicate(is_boolean); }

/// === Pairs and lists

/// *Procedure* (*pair?* _obj_)
///
/// From R7RS
///
static pointer prim_pair_p(void) { return obj_predicate(is_pair); }

/// *Procedure* (*cons* _obj1_ _obj2_)
///
/// From R7RS
///
static pointer prim_cons(void)
{
    pointer a, b;

    arg_obj(&a);
    arg_obj(&b);
    return arg_err() ? ARG_ERR : _s_return(sc, cons(sc, a, b));
}

/// *Procedure* (*car* _pair_)
///
/// From R7RS
///
/// Return the car ("head") of _pair_.
///
static pointer prim_car(void)
{
    pointer pair;

    arg_pair(&pair);
    return arg_err() ? ARG_ERR : _s_return(sc, car(pair));
}

/// *Procedure* (*cdr* _pair_)
///
/// From R7RS
///
/// Return the cdr ("tail") of _pair_.
///
static pointer prim_cdr(void)
{
    pointer pair;

    arg_pair(&pair);
    return arg_err() ? ARG_ERR : _s_return(sc, cdr(pair));
}

/// *Procedure* (*null?* _obj_)
///
/// From R7RS
///
/// If _obj_ is `()`, returns `#t`. Else returns `#f`.
///
static pointer prim_null_p(void)
{
    pointer x;

    arg_obj(&x);
    if (arg_err()) {
        return ARG_ERR;
    }
    s_retbool(x == sc->NIL);
}

/// *Procedure* (*list?* _obj_)
///
/// From R7RS
///
/// Return `#t` is _obj_ is a proper list. Else return `#f`. A proper
/// list is either the empty list `()`, or a pair whose cdr is a
/// proper list. A "dotted list" is an improper list. This procedure
/// does not detect circular lists; they will cause an infinite loop.
///
static pointer prim_list_p(void)
{
    pointer arg;

    arg_obj(&arg);
    if (arg_err()) {
        return ARG_ERR;
    }
    s_retbool(list_length(sc, arg) >= 0);
}

/// *Procedure* (*make-list* _n_ [_elem_])
///
/// From R7RS
///
/// Return a fresh _n_-element list containing _elem_ repeated _n_
/// times. The default _elem_ is undefined in R7RS; in Desert Island
/// Scheme it is `#f`.
///
static pointer prim_make_list(void)
{
    long n;
    pointer fill, list;

    arg_long(&n, 0, INT_MAX);
    fill = sc->F;
    if (arg_left()) {
        arg_obj(&fill);
    }
    if (arg_err()) {
        return ARG_ERR;
    }
    list = sc->NIL;
    for (; n; n--) {
        list = cons(sc, fill, list);
    }
    return _s_return(sc, list);
}

/// *Procedure* (*list* _elem_...)
///
/// From R7RS
///
/// Return a fresh list with the given elements. Note that though the
/// list itself is fresh, the elements are simply references to the
/// existing objects, not copies of them.
///

/// *Procedure* (*cons** [_elem_...] _tail_)
///
/// From SRFI 1
///
/// Like *list*, but with an extra _tail_ argument that becomes the
/// cdr of the last pair in the list. This lets you make a dotted list
/// or append a previously-made list to the end of the new one. When
/// only _tail_ is given, just returns it. The returned list is fresh,
/// but neither the _elem_... nor _tail_ are ever copied. If _tail_ is
/// a list, the resulting list is part new, part old. Compare with
/// *append*.
///
/// This procedure has the more obvious name `list*` in Common Lisp,
/// but the name *cons** is compatible with SRFI 1.
///
static pointer prim_cons_star(void)
{
    pointer list, last;

    list = sc->NIL;
    if (!arg_obj(&last)) {
        return sc->NIL;
    }
    while (arg_left()) {
        list = cons(sc, last, list);
        if (!arg_obj(&last)) {
            return sc->NIL;
        }
    }
    if (arg_err()) {
        return ARG_ERR;
    }
    return _s_return(sc, reverse_in_place(sc, last, list));
}

/// *Procedure* (*iota* _n_)
///
/// From SRFI 1
///
/// Return a list of integers from 0 (inclusive) to _n_ (exclusive).
///
static pointer prim_iota(void)
{
    pointer list;
    long n;

    arg_long(&n, 0, INT_MAX);
    if (arg_err()) {
        return ARG_ERR;
    }
    list = sc->NIL;
    while (n) {
        list = cons(sc, mk_integer(sc, --n), list);
    }
    return _s_return(sc, list);
}

/// *Procedure* (*length* _list_)
///
/// From R7RS
///
/// Return the number of elements in _list_, which must be a proper
/// list. An error is signaled on a circular or improper list.
///
static pointer prim_length(void)
{
    pointer arg;
    int n;

    arg_obj(&arg);
    if (arg_err()) {
        return ARG_ERR;
    }
    n = list_length(sc, arg);
    if (n < 0) {
        Error_1(sc, "length: not a list:", arg);
    }
    return _s_return(sc, mk_integer(sc, n));
}

/// *Procedure* (*bounded-length* _max_ _list_)
///
/// Custom in Desert Island Scheme
///
/// Return the number of elements in _list_. But if _list_ is longer
/// than _max_ elements, stop counting and just return _max_. If a
/// dotted pair is encountered within the first _max_ pairs, `#f` is
/// returned. The rest of the list is not checked for dotted pairs.
/// Circular lists are not checked for.
///
static pointer prim_bounded_length(void)
{
    long max, len;
    pointer list;

    arg_long(&max, 0, LONG_MAX);
    arg_obj(&list);
    if (arg_err()) {
        return ARG_ERR;
    }
    for (len = 0; (len < max) && (list != sc->NIL); len++) {
        if (!is_pair(list)) {
            return _s_return(sc, sc->F);
        }
        list = cdr(list);
    }
    return _s_return(sc, mk_integer(sc, len));
}

// (append 'a)           => a
// (append '() 'a)       => a
// (append '(1) 'a)      => (1 . a)
// (append '(1) '(2) 'a) => (1 2 . a)
static pointer prim_append(void)
{
    pointer newlist, tail;

    newlist = tail = sc->NIL;
    while (arg_left()) {
        newlist = revappend(sc, newlist, tail);
        if (!arg_obj(&tail)) {
            return sc->NIL;
        }
        // Error_0(sc, "non-list argument to append");
    }
    if (arg_err()) {
        return ARG_ERR;
    }
    return _s_return(sc, reverse_in_place(sc, tail, newlist));
}

static pointer prim_reverse(void)
{
    pointer a; /* TODO: must be checked by gc */
    pointer p;

    arg_obj(&a);
    if (arg_err()) {
        return ARG_ERR;
    }
    p = sc->NIL;
    for (; is_pair(a); a = cdr(a)) {
        p = cons(sc, car(a), p);
    }
    return _s_return(sc, p);
}

// list-tail

// list-ref

// member obj list [compare]

// assoc obj alist [compare]

// list-copy

/// === Symbols

/// *Procedure* (*symbol->string* _symbol_) => _string_
///
/// From R7RS
///
/// Return the name of _symbol_ as a string, but without adding escapes.
///
static pointer prim_symbol_to_string(void)
{
    const char *sym;
    pointer str;

    arg_symbol(&sym);
    if (arg_err()) {
        return ARG_ERR;
    }
    str = mk_string(sc, sym);
    setimmutable(str);
    return _s_return(sc, str);
}

/// *Procedure* (*string->symbol* _string_)
///
/// From R7RS
///
/// Return the symbol whose name is _string_. _string_ can contain special
/// characters that would require escaping in a literal symbol.
///
static pointer prim_string_to_symbol(void)
{
    const char *s;

    arg_string(&s);
    return arg_err() ? ARG_ERR : _s_return(sc, mk_symbol(sc, s));
}

/// *Procedure* (*gensym*)
///
/// From Common Lisp
///
/// Return a unique symbol that is unequal to any existing symbol.
///
static pointer prim_gensym(void)
{
    pointer x;
    char name[40];

    if (arg_err()) {
        return ARG_ERR;
    }
    for (; sc->gensym_cnt < LONG_MAX; sc->gensym_cnt++) {
        snprintf(name, 40, "gensym-%ld", sc->gensym_cnt);

        /* first check oblist */
        x = oblist_find_by_name(sc, name);

        if (x != sc->NIL) {
            continue;
        } else {
            x = oblist_add_by_name(sc, name);
            return (x);
        }
    }
    return sc->NIL;
}

/// === Characters

/// === Strings

/// *Procedure* (*substring* _string_ _start_ _end_)
///
/// From R7RS
///
/// Return a fresh string formed from the characters of _string_
/// beginning with index _start_ and ending with index _end_. This is
/// equivalent to calling *string-copy* with the same arguments.
///
static pointer prim_substring(void)
{
    pointer string, x;
    char *str;
    long index0, index1, len;

    arg_obj_type(&string, is_string, "string");
    arg_long(&index0, 0, LONG_MAX);
    arg_long(&index1, 0, LONG_MAX);
    if (arg_err()) {
        return ARG_ERR;
    }
    str = strvalue(string);
    if (index0 > strlength(string)) {
        return _Error_1(sc, "substring: start out of bounds", 0);
    }
    if (index1 > strlength(string)) {
        return _Error_1(sc, "substring: end out of bounds", 0);
    }
    if (index1 < index0) {
        return _Error_1(sc, "substring: end less than start", 0);
    }
    len = index1 - index0;
    x = mk_empty_string(sc, len, ' ');
    memcpy(strvalue(x), str + index0, len);
    str = strvalue(x);
    str[len] = 0;
    return _s_return(sc, x);
}

/// *Procedure* (*string-append* _string_...)
///
/// From R7RS
///
/// Returns a fresh string whose characters are the concatenation of
/// the characters in the given strings.
///
static pointer prim_string_append(void)
{
    char *buf;
    char *newbuf;
    const char *sub;
    size_t sublen, len, cap;
    pointer string;

    buf = 0;
    len = 0;
    cap = 8;
    while (arg_left()) {
        if (!arg_string(&sub)) {
            free(buf);
            return sc->NIL;
        }
        sublen = strlen(sub);
        while (cap <= len + sublen) {
            cap *= 2;
        }
        newbuf = realloc(buf, cap);
        if (!newbuf) {
            free(buf);
            return sc->NIL;
        }
        buf = newbuf;
        memcpy(buf + len, sub, sublen + 1);
        len += sublen;
    }
    if (arg_err()) {
        free(buf);
        return ARG_ERR;
    }
    string = mk_string(sc, buf ? buf : "");
    free(buf);
    return _s_return(sc, string);
}

/// *Procedure* (*string->list* _string_)
///
/// From R7RS
///
/// Return a fresh list whose elements are the characters of _string_.
/// I.e. each element of the list is one character object.
///
static pointer prim_string_to_list(void)
{
    pointer list;
    const char *string;
    const char *p;

    arg_string(&string);
    if (arg_err()) {
        return ARG_ERR;
    }
    list = sc->NIL;
    p = strchr(string, 0);
    while (p > string) {
        list = cons(sc, mk_character(sc, *--p), list);
    }
    return _s_return(sc, list);
}

/// *Procedure* (*atom->string* _atom_)
///
/// From TinyScheme
///
static pointer prim_atom_to_string(void)
{
    long base = -1;
    pointer x;
    char *buf;
    int len;

    arg_obj(&x);
    if (arg_left()) {
        arg_long(&base, 2, 16);
    }
    if (arg_err()) {
        return ARG_ERR;
    }
    if (!is_number(x) && !is_character(x) && !is_string(x) && !is_symbol(x)) {
        return _Error_1(sc, "atom->string: not an atom:", x);
    }
    if (is_number(x) && (base != -1) && (base != 2) && (base != 8)
        && (base != 10) && (base != 16)) {
        return _Error_1(sc, "atom->string: bad base", 0);
    }
    atom2str(sc, x, (int)base, &buf, &len);
    return _s_return(sc, mk_counted_string(sc, buf, len));
}

/// *Procedure* (*string->atom* _string_)
///
/// From TinyScheme
///
static pointer prim_string_to_atom(void)
{
    long base = 0;
    long longval;
    char *limit;
    const char *s;

    arg_string(&s);
    if (arg_left()) {
        arg_long(&base, 2, 16);
    }
    if (arg_err()) {
        return ARG_ERR;
    }
    if (!base || (base == 10)) {
        return _s_return(sc, mk_atom(sc, strdup(s)));
    }
    if ((base != 2) && (base != 8) && (base != 16)) {
        return _Error_1(sc, "atom->string: bad base", 0);
    }
    if (*s == '#') { // no use of base!
        return _s_return(sc, mk_sharp_const(sc, s + 1));
    }
    longval = strtol(s, &limit, (int)base);
    return _s_return(sc, *limit ? sc->F : mk_integer(sc, longval));
}

/// === Numbers
///

/// *Procedure* (*<* _a_ _b_) +
/// *Procedure* (*\<=* _a_ _b_) +
/// *Procedure* (*=* _a_ _b_) +
/// *Procedure* (*>* _a_ _b_) +
/// *Procedure* (*>=* _a_ _b_) +
///
/// From R7RS
///
/// Return `#t` if _a_ and _b_ are the same object in memory. Else
///
static pointer prim_num_lt(void) { return cmp_primitive(num_lt); }

static pointer prim_num_le(void) { return cmp_primitive(num_le); }

static pointer prim_num_eq(void) { return cmp_primitive(num_eq); }

static pointer prim_num_gt(void) { return cmp_primitive(num_gt); }

static pointer prim_num_ge(void) { return cmp_primitive(num_ge); }

static pointer prim_char_p(void) { return obj_predicate(is_character); }

/// === Input and output

static pointer open_string_primitive(int prop)
{
    pointer string, p;

    arg_obj_type(&string, is_string, "string");
    if (arg_err()) {
        return ARG_ERR;
    }
    p = port_from_string(
        sc, strvalue(string), strvalue(string) + strlength(string), prop);
    return _s_return(sc, ((p == sc->NIL) ? sc->F : p));
}

/// *Procedure* (*open-input-string* _string_)
///
/// From SRFI 170
///
static pointer prim_open_input_string(void)
{
    return open_string_primitive(port_input);
}

/// *Procedure* (*open-input-output-string* _string_)
///
/// From TinyScheme
///
static pointer prim_open_input_output_string(void)
{
    return open_string_primitive(port_input | port_output);
}

/// *Procedure* (*open-output-string*)
///
/// From SRFI 170
///
static pointer prim_open_output_string(void)
{
    pointer p;

    if (arg_err()) {
        return ARG_ERR;
    }
    p = port_from_scratch(sc);
    return _s_return(sc, ((p == sc->NIL) ? sc->F : p));
}

/// *Procedure* (*get-output-string* _port_)
///
/// From SRFI 170
///
static pointer prim_get_output_string(void)
{
    size_t len;
    pointer portobj, s;
    port *port;
    char *str;

    arg_obj_type(&portobj, is_outport, "output port");
    if (arg_err()) {
        return ARG_ERR;
    }
    port = portobj->_object._port;
    if (!(port->kind & port_string)) {
        return _Error_1(sc, "arg is not a string port", portobj);
    }
    len = port->rep.string.curr - port->rep.string.start;
    str = sc->malloc(len + 1);
    if (!str) {
        return _s_return(sc, sc->F);
    }
    memcpy(str, port->rep.string.start, len);
    str[len] = 0;
    s = mk_string(sc, str);
    sc->free(str);
    return _s_return(sc, s);
}

// Note, macro object is also a closure.
// Therefore, (closure? <#MACRO>) ==> #t
static pointer prim_closure_p(void) { return obj_predicate(is_closure); }

/// === System interface

/// *Procedure* (*posix-time*) -> _timespec_ [SRFI 170]
///
/// Return the current time in UTC since the Unix epoch (1970-01-01
/// 00:00:00) as a _timespec_ object.
///
static pointer prim_posix_time(void)
{
    struct timespec tv;

    if (arg_err()) {
        return ARG_ERR;
    }
    if (clock_gettime(CLOCK_REALTIME, &tv) == -1) {
        return os_error("clock_gettime");
    }
    return _s_return(sc, mk_timespec(sc, tv.tv_sec, tv.tv_nsec));
}

/// *Procedure* (*timespec* _seconds_ _nanoseconds_) -> _timespec_
///
/// From SRFI 174
///
/// Return a _timespec_ object made of the given _seconds_ and
/// _nanoseconds_ components. Both are integers. _seconds_ may be
/// negative; _nanoseconds_ must be in the range 0..999999999.
///
static pointer prim_timespec(void)
{
    long sec, nsec;

    arg_long(&sec, LONG_MIN, LONG_MAX);
    arg_long(&nsec, 0, 999999999L);
    return arg_err() ? ARG_ERR : _s_return(sc, mk_timespec(sc, sec, nsec));
}

/// *Procedure* (*timespec-seconds*) -> _integer_
///
/// From SRFI 174
///
/// Return the seconds component of a _timespec_ object.
///
static pointer prim_timespec_seconds(void)
{
    long sec, nsec;

    arg_timespec(&sec, &nsec);
    return arg_err() ? ARG_ERR : _s_return(sc, mk_integer(sc, sec));
}

/// *Procedure* (*timespec-nanoseconds* _timespec_) -> _natural_
///
/// From SRFI 174
///
/// Return the nanoseconds component of a _timespec_ object.
///
static pointer prim_timespec_nanoseconds(void)
{
    long sec, nsec;

    arg_timespec(&sec, &nsec);
    return arg_err() ? ARG_ERR : _s_return(sc, mk_integer(sc, nsec));
}

/// *Procedure* (*timespec=?* _timespec1_ _timespec2_)
///
/// From SRFI 174
///
/// Return `#t` if _timespec1_ and _timespec2_ represent the same
/// instant in time. Else return `#f`.
///
static pointer prim_timespec_eq_p(void)
{
    long sec1, nsec1;
    long sec2, nsec2;

    arg_timespec(&sec1, &nsec1);
    arg_timespec(&sec2, &nsec2);
    return arg_err()
        ? ARG_ERR
        : _s_return(sc, ((sec1 == sec2) && (nsec1 == nsec2)) ? sc->T : sc->F);
}

/// *Procedure* (*timespec<?* _timespec1_ _timespec2_)
///
/// From SRFI 174
///
/// Return `#t` if _timespec1_ represents an earlier instant in time
/// than _timespec2_. Else return `#f`.
///
static pointer prim_timespec_lt_p(void)
{
    long sec1, nsec1;
    long sec2, nsec2;

    arg_timespec(&sec1, &nsec1);
    arg_timespec(&sec2, &nsec2);
    return arg_err()
        ? ARG_ERR
        : _s_return(sc,
            ((sec1 < sec2) || ((sec1 == sec2) && (nsec1 < nsec2))) ? sc->T
                                                                   : sc->F);
}

/// *Procedure* (*random-integer* _n_)
///
/// From SRFI 27
///
/// Return a random non-negative integer less than _n_. Expect the
/// random number generator to be of poor quality.
///
static pointer prim_random_integer(void)
{
    long n, r;

    arg_long(&n, 1, LONG_MAX);
    if (arg_err()) {
        return ARG_ERR;
    }
    r = rand();
    if (n < RAND_MAX) {
        r %= n;
    }
    return _s_return(sc, mk_integer(sc, r));
}

/// === File system
///

/// *Procedure* (*open-directory-list* _path_) => _directory-list_
///
/// From SRFI 170
///
/// Return an object for listing the directory entries in the
/// directory _path_. Use *read-directory* to read them.
///
static pointer prim_open_directory_list(void)
{
    const char *path;

    arg_string(&path);
    if (arg_err()) {
        return ARG_ERR;
    }
    return os_open_directory_list(path);
}

/// *Procedure* (*read-directory* _path_) => _directory-list_
///
/// From SRFI 170
///
/// Return an object for listing the directory entries in the
/// directory _path_. Use *read-directory* to read them.
///
static pointer prim_read_directory(void)
{
    pointer obj;

    if (!arg_obj(&obj)) {
        return ARG_ERR;
    }
    if (!is_directory_list(obj)) {
        arg_set_err("arg is not a directory-list object");
    }
    if (arg_err()) {
        return ARG_ERR;
    }
    return os_read_directory(obj);
}

/// *Procedure* (*close-directory-list* _directory-list_)
///
/// From SRFI 170
///
/// Close _directory-list_ when you are done reading it.
///
static pointer prim_close_directory_list(void)
{
    pointer obj;

    if (!arg_obj(&obj)) {
        return ARG_ERR;
    }
    if (!is_directory_list(obj)) {
        arg_set_err("arg is not a directory-list object");
    }
    if (arg_err()) {
        return ARG_ERR;
    }
    os_close_directory_list(obj);
    return _s_return(sc, sc->F);
}

/// *Procedure* (*create-directory* _path_ [_mode_])
///
/// From R7RS
///
/// Make a directory named _path_ in the file system. Raise an error
/// if that fails.
///
static pointer prim_create_directory(void)
{
    const char *path;
    long mode = 0555;

    arg_string(&path);
    if (arg_left()) {
        arg_long(&mode, 0, 0777);
    }
    if (arg_err()) {
        return ARG_ERR;
    }
    return os_create_directory(path, mode);
}

static pointer prim_current_input_port(void)
{
    if (arg_err()) {
        return ARG_ERR;
    }
    s_return(sc, sc->inport);
}

static pointer prim_current_output_port(void)
{
    if (arg_err()) {
        return ARG_ERR;
    }
    s_return(sc, sc->outport);
}

/// *Procedure* (*delete-environment-variable!* _name_)
///
/// From SRFI 170
///
/// Ensure the environment variable _name_ is not currently defined.
///
static pointer prim_delete_environment_variable(void)
{
    const char *name;

    arg_string(&name);
    if (arg_err()) {
        return ARG_ERR;
    }
    return os_set_environment_variable(name, 0);
}

/// *Procedure* (*real-path* _path_) => _string_
///
/// From R7RS
///
/// Return an absolute pathname derived from _path_ that names the
/// same file and whose resolution does not involve `.` or `..` or
/// symbolic links.
///
static pointer prim_real_path(void)
{
    const char *path;

    arg_string(&path);
    if (arg_err()) {
        return ARG_ERR;
    }
    return os_real_path(path);
}

/// *Procedure* (*file-exists?* _path_)
///
/// From R7RS
///
/// Return `#t` if there is a file, directory or other file system
/// object under the pathname _path_. Else return `#f`. Symbolic links
/// are followed; a symbolic link pointing to a nonexistent pathname
/// returns `#f`.
///
static pointer prim_file_exists_p(void)
{
    const char *path;

    arg_string(&path);
    if (arg_err()) {
        return ARG_ERR;
    }
    return os_file_exists_p(path);
}

/// *Procedure* (*delete-directory* _path_)
///
/// From SRFI 170
///
/// Delete the empty directory named _path_ in the file system. Raise
/// an error in case _path_ does not exist, is not a directory, is not
/// empty, or cannot be deleted for some reason.
///
static pointer prim_delete_directory(void)
{
    const char *path;

    arg_string(&path);
    if (arg_err()) {
        return ARG_ERR;
    }
    return os_delete_directory(path);
}

/// *Procedure* (*delete-file* _path_)
///
/// From SRFI 170
///
/// Delete the file named _path_ in the file system. Raise an error in
/// case _path_ does not exist or cannot be deleted for some reason.
///
/// This procedure can delete regular files, pipes, sockets and
/// devices given the appropriate permissions. It generally cannot
/// delete directories; use *delete-directory* for that.
///
static pointer prim_delete_file(void)
{
    const char *path;

    arg_string(&path);
    if (arg_err()) {
        return ARG_ERR;
    }
    return os_delete_file(path);
}

static pointer prim_environment_p(void)
{
    return obj_predicate(is_environment);
}

static pointer prim_eof_object_p(void)
{
    pointer x;

    arg_obj(&x);
    if (arg_err()) {
        return ARG_ERR;
    }
    s_retbool(x == sc->EOF_OBJ);
}

/// *Constant* *script-real-path*
///
/// Custom in Desert Island Scheme
///
/// Holds the real-path of the Scheme script being run. If not running
/// a script, the value is `#f`.
///

/// *Procedure* (*command-line*)
///
/// From R7RS
///
/// Return the command line arguments passed to the Scheme script as a
/// list of strings. The first string is the script name. If not
/// running a script, the value if the empty list `()`.
///
static pointer prim_command_line(void)
{
    return arg_err() ? ARG_ERR : _s_return(sc, g_command_line);
}

/// *Procedure* (*exit* [_code_])
///
/// From R7RS
///
/// Perform a normal exit of the Scheme process. If _code_ is given,
/// it's an integer 0..255 to return as the exit code to the operating
/// system. The default _code_ is 0 indicating success.
///
static pointer prim_exit(void)
{
    long ret = 0;

    if (arg_left()) {
        arg_long(&ret, 0, 255);
    }
    if (arg_err()) {
        return ARG_ERR;
    }
    sc->retcode = ret;
    return sc->NIL;
}

static const char help_message[]
    = "------------------------------------------------------------------"
      "----\n"
      "This is Desert Island Scheme, a bare-bones scripting environment for\n"
      "the Scheme programming language.\n"
      "\n"
      "You are in the REPL (\"read-eval-print loop\"). Here you can "
      "experiment\n"
      "with code before saving it into a script file. Try (+ 1 2 (* 3 4 5))\n"
      "or (displayln \"Hello world\")\n"
      "\n"
      "Type (exit) including the parentheses to leave Scheme.\n"
      "Type (apropos \"file\") to find every procedure with \"file\" in its "
      "name.\n"
      "Type (load \"file.scm\") to run all the code in a Scheme source "
      "file.\n"
      "If the file defines things, you can keep using them in the REPL.\n"
      "\n"
      "DIScheme can run Unix scripts: #! /usr/bin/env discheme\n"
      "DIScheme does not have a compiler, debugger or profiler.\n"
      "DIScheme covers most of the R7RS-small language standard plus parts\n"
      "of many SRFI extensions. It has almost no non-standard features.\n"
      "\n"
      "When all else fails, read the manual. Happy hacking!\n"
      "------------------------------------------------------------------"
      "----\n";

/// *Procedure* (*help*)
///
/// From SRFI nnn
///
/// Display online help.
///
static pointer prim_help(void)
{
    if (arg_err()) {
        return ARG_ERR;
    }
    putstr(sc, help_message);
    return _s_return(sc, sc->T);
}

/// *Procedure* (*features*)
///
/// From R7RS
///
/// Return a list of the feature identifiers which *cond-expand*
/// treats as true.
///
static pointer prim_features(void)
{
    return arg_err() ? ARG_ERR : _s_return(sc, g_features);
}

/// *Procedure* (*pid*)
///
/// From R7RS
///
/// Return the process ID of the Scheme process.
///
static pointer prim_pid(void)
{
    return arg_err() ? ARG_ERR : _s_return(sc, mk_integer(sc, getpid()));
}

/// *Procedure* (*umask*)
///
/// From R7RS
///
/// Return the current umask (Unix file permission bitmask) of the
/// Scheme process.
///
static pointer prim_umask(void)
{
    return arg_err() ? ARG_ERR
                     : _s_return(sc, mk_integer(sc, umask(umask(0777))));
}

/// *Procedure* (*set-umask* _new-umask_)
///
/// From R7RS
///
/// Set the current umask of the Scheme process to _new-umask_. Return
/// the *old* umask.
///
static pointer prim_set_umask(void)
{
    long u;

    arg_long(&u, 0, 0777);
    return arg_err() ? ARG_ERR : _s_return(sc, mk_integer(sc, umask(u)));
}

/// *Procedure* (*user-uid*)
///
/// From SRFI 170
///
///
static pointer prim_user_uid(void)
{
    return arg_err() ? ARG_ERR : _s_return(sc, mk_integer(sc, getuid()));
}

/// *Procedure* (*user-gid*)
///
/// From SRFI 170
///
///
static pointer prim_user_gid(void)
{
    return arg_err() ? ARG_ERR : _s_return(sc, mk_integer(sc, getgid()));
}

/// *Procedure* (*user-effective-uid*)
///
/// From SRFI 170
///
///
static pointer prim_user_effective_uid(void)
{
    return arg_err() ? ARG_ERR : _s_return(sc, mk_integer(sc, geteuid()));
}

/// *Procedure* (*user-effective-gid*)
///
/// From SRFI 170
///
///
static pointer prim_user_effective_gid(void)
{
    return arg_err() ? ARG_ERR : _s_return(sc, mk_integer(sc, getegid()));
}

/// *Procedure* (*user-supplementary-gids*)
///
/// From SRFI 170
///
///
static pointer prim_user_supplementary_gids(void)
{
    if (arg_err()) {
        return ARG_ERR;
    }
    return os_user_supplementary_gids();
}

#define arg_string_or_port arg_string

/// *Procedure* (*file-info* _path_ [_follow?_])
///
/// From SRFI 170
///
/// Return a fresh file-info object for _path_. If _follow?_ is true
/// the procedure follows symbolic links and reports on the files to
/// which they refer. If follow? is false the procedure checks the
/// actual file itself, even if it's a symlink. The follow? flag is
/// ignored if the file argument is a port.
///
static pointer prim_file_info(void)
{
    const char *path;
    int follow;

    arg_string_or_port(&path);
    arg_boolean(&follow);
    if (arg_err()) {
        return ARG_ERR;
    }
    return os_file_info(path, follow);
}

static pointer prim_file_info_gid(void)
{
    void *stat;

    arg_stat(&stat);
    return arg_err() ? ARG_ERR
                     : _s_return(sc, mk_integer(sc, os_file_info_gid(stat)));
}

static pointer prim_file_info_mode(void)
{
    void *stat;

    arg_stat(&stat);
    return arg_err() ? ARG_ERR
                     : _s_return(sc, mk_integer(sc, os_file_info_mode(stat)));
}

/// *Procedure* (*file-info:mtime* _file-info_) => _timespec_
///
/// From SRFI 170
///
/// Return the last modification time in a _file-info_ as a
/// _timespec_.
///
static pointer prim_file_info_mtime(void)
{
    void *stat;
    long sec, nsec;

    arg_stat(&stat);
    if (arg_err()) {
        return ARG_ERR;
    }
    os_file_info_mtime(stat, &sec, &nsec);
    return _s_return(sc, mk_timespec(sc, sec, nsec));
}

static pointer prim_file_info_size(void)
{
    void *stat;

    arg_stat(&stat);
    return arg_err() ? ARG_ERR
                     : _s_return(sc, mk_integer(sc, os_file_info_size(stat)));
}

static pointer prim_file_info_uid(void)
{
    void *stat;

    arg_stat(&stat);
    return arg_err() ? ARG_ERR
                     : _s_return(sc, mk_integer(sc, os_file_info_uid(stat)));
}

static pointer prim_file_info_p(void)
{
    pointer arg;

    arg_obj(&arg);
    if (arg_err()) {
        return ARG_ERR;
    }
    s_retbool(is_file_info(arg));
}

/// *Procedure* (*gc* _n_)
///
/// From Common Lisp
///
/// Run the garbage collector immediately. The GC is automatic and it
/// is normally not necessary to call this procedure.
///
static pointer prim_gc(void)
{
    if (arg_err()) {
        return ARG_ERR;
    }
    gc(sc, sc->NIL, sc->NIL);
    s_return(sc, sc->T);
}

static pointer prim_gc_verbose(void)
{
    int oldval, newval;

    arg_boolean(&newval);
    if (arg_err()) {
        return ARG_ERR;
    }
    oldval = sc->gc_verbose;
    sc->gc_verbose = newval;
    s_retbool(oldval);
}

/// *Procedure* (*get-environment-variable* _name_)
///
/// From R7RS, SRFI 98
///
/// Return the value of the environment variable _name_ as a string,
/// or `#f` if _name_ is not currently defined.
///
static pointer prim_get_environment_variable(void)
{
    const char *name;
    const char *value;

    arg_string(&name);
    if (arg_err()) {
        return ARG_ERR;
    }
    value = getenv(name);
    return _s_return(sc, value ? mk_string(sc, value) : sc->F);
}

/// *Procedure* (*get-environment-variables*)
///
/// From R7RS, SRFI 98
///
/// Return a fresh association list of all currently defined
/// environment variables. Mutating the list does not change the
/// variables.
///
static pointer prim_get_environment_variables(void)
{
    if (arg_err()) {
        return ARG_ERR;
    }
    return _s_return(
        sc, reverse_in_place(sc, sc->NIL, os_get_environment_variables()));
}

static pointer prim_input_port_p(void) { return obj_predicate(is_inport); }

static pointer prim_integer_p(void) { return obj_predicate(is_integer); }

static pointer prim_macro_p(void) { return obj_predicate(is_macro); }

/// *Procedure* (*make-string* _n_ [_char_])
///
/// From R7RS
///
/// Return a fresh _n_-character string containing _char_ repeated _n_
/// times. The default _char_ is undefined in R7RS; in Desert Island
/// Scheme it is a null character.
///
static pointer prim_make_string(void)
{
    long len;
    long fill = 0;

    arg_long(&len, 0, LONG_MAX);
    if (arg_left()) {
        arg_char(&fill);
    }
    if (arg_err()) {
        return ARG_ERR;
    }
    return _s_return(sc, mk_empty_string(sc, len, fill));
}

/// *Procedure* (*string-length* _string_)
///
/// From R7RS
///
/// Return the number of characters in _string_.
///
static pointer prim_string_length(void)
{
    pointer p;

    arg_obj_type(&p, is_string, "string");
    return arg_err() ? ARG_ERR : _s_return(sc, mk_integer(sc, strlength(p)));
}

/// *Procedure* (*string-ref* _string_ _index_) => _char_
///
/// From R7RS
///
/// Return the character at index _index_ in _string_. The _index_ of
/// the first character is zero.
///
static pointer prim_string_ref(void)
{
    pointer string;
    char *str;
    long index;

    arg_obj_type(&string, is_string, "string");
    arg_long(&index, 0, LONG_MAX);
    if (arg_err()) {
        return ARG_ERR;
    }
    if (index >= strlength(string)) {
        return _Error_1(sc, "string-ref: index out of bounds", 0);
    }
    str = strvalue(string);
    return _s_return(sc, mk_character(sc, ((unsigned char *)str)[index]));
}

/// *Procedure* (*string-set!* _string_ _index_ _char_)
///
/// From R7RS
///
/// Store _char_ as the character at index _index_ in _string_. The
/// _index_ of the first character in the string is zero.
///
static pointer prim_string_set(void)
{
    pointer string;
    char *str;
    long index, c;

    arg_obj_type(&string, is_string, "string");
    arg_long(&index, 0, LONG_MAX);
    arg_char(&c);
    if (arg_err()) {
        return ARG_ERR;
    }
    if (is_immutable(string)) {
        return _Error_1(
            sc, "string-set!: unable to alter immutable string:", string);
    }
    str = strvalue(string);
    if (index >= strlength(string)) {
        return _Error_1(sc, "string-set!: index out of bounds", 0);
    }
    str[index] = c;
    return _s_return(sc, string);
}

/// *Procedure* (*new-segment* _n_)
///
/// From TinyScheme
///
/// Allocates more memory for Scheme. _n_ is the number of new memory
/// segments to get.
///
static pointer prim_new_segment(void)
{
    long n;

    arg_long(&n, 0, LONG_MAX);
    if (arg_err()) {
        return ARG_ERR;
    }
    alloc_cellseg(sc, n);
    s_return(sc, sc->T);
}

static pointer prim_number_p(void) { return obj_predicate(is_number); }

static pointer prim_oblist(void)
{
    if (arg_err()) {
        return ARG_ERR;
    }
    s_return(sc, oblist_all_symbols(sc));
}

/// *Procedure* (*apropos* _query_)
///
/// From Common Lisp
///
/// List all top-level symbols matching _query_.
///
static pointer prim_apropos(void)
{
    const char *query;
    pointer x, xs;
    long i, n;

    arg_string(&query);
    if (arg_err()) {
        return ARG_ERR;
    }
    if (!strlen(query)) {
        return _s_return(sc, sc->F);
    }
    n = ivalue_unchecked(sc->oblist);
    for (i = 0; i < n; i++) {
        for (xs = vector_elem(sc->oblist, i); xs != sc->NIL; xs = cdr(xs)) {
            x = car(xs);
            if (strstr(symname(x), query)) {
                printf("%s\n", symname(x));
            }
        }
    }
    return _s_return(sc, sc->F);
}

static pointer prim_output_port_p(void) { return obj_predicate(is_outport); }

static pointer prim_port_p(void) { return obj_predicate(is_port); }

static pointer prim_procedure_p(void)
{
    pointer x;

    arg_obj(&x);
    if (arg_err()) {
        return ARG_ERR;
    }
    /*--
     * continuation should be procedure by the example
     * (call-with-current-continuation procedure?) ==> #t
     * in R^3 report sec. 6.9
     */
    s_retbool(is_proc(x) || is_closure(x) || is_continuation(x));
}

static pointer prim_real_p(void)
{
    /* All numbers are real */
    return obj_predicate(is_number);
}

/// *Procedure* (*set-environment-variable!* _name_)
///
/// From SRFI 170
///
/// Change the value of the environment variable _name_ to be _value_.
///
static pointer prim_set_environment_variable(void)
{
    const char *name;
    const char *value;

    arg_string(&name);
    arg_string(&value);
    if (arg_err()) {
        return ARG_ERR;
    }
    return os_set_environment_variable(name, value);
}

static pointer prim_set_file_mode(void)
{
    const char *path;
    long mode;

    arg_string(&path);
    arg_long(&mode, 0, 0777);
    if (arg_err()) {
        return ARG_ERR;
    }
    return os_set_file_mode(path, mode);
}

static pointer prim_set_working_directory(void)
{
    const char *path;

    arg_string(&path);
    if (arg_err()) {
        return ARG_ERR;
    }
    return os_set_working_directory(path);
}

static pointer prim_string_p(void) { return obj_predicate(is_string); }

static pointer prim_symbol_p(void) { return obj_predicate(is_symbol); }

/// === User and group database access
///

/// *Procedure* (*user-info* _uid/name_)
///
/// From SRFI 170
///
/// Return a fresh user-info object for _uid/name_.
///
static pointer prim_user_info(void)
{
    const char *name;
    long uid;

    arg_string_or_long(&name, &uid, 0, LONG_MAX);
    if (arg_err()) {
        return ARG_ERR;
    }
    return os_user_info(name, uid);
}

/// *Procedure* (*user-info?* _obj_)
///
/// From SRFI 170
///
/// Return `#t` is _obj_ is a user-info object. Else return `#f`.
///
static pointer prim_user_info_p(void)
{
    pointer arg;

    arg_obj(&arg);
    if (arg_err()) {
        return ARG_ERR;
    }
    s_retbool(is_user_info(arg));
}

/// *Procedure* (*user-info:name* _user-info_)
///
/// From SRFI 170
///
/// Return the username or "login name" as a string from the given
/// _user-info_ object. Note that this is usually different from the
/// "full name" used for display purposes.
///
static pointer prim_user_info_name(void)
{
    struct user_info *info;

    arg_user_info(&info);
    return arg_err() ? ARG_ERR : _s_return(sc, info->name);
}

/// *Procedure* (*user-info:uid* _user-info_)
///
/// From SRFI 170
///
static pointer prim_user_info_uid(void)
{
    struct user_info *info;

    arg_user_info(&info);
    return arg_err() ? ARG_ERR : _s_return(sc, mk_integer(sc, info->uid));
}

/// *Procedure* (*user-info:gid* _user-info_)
///
/// From SRFI 170
///
/// Return the group ID as a non-negative integer from the given
/// _user-info_ object.
///
static pointer prim_user_info_gid(void)
{
    struct user_info *info;

    arg_user_info(&info);
    return arg_err() ? ARG_ERR : _s_return(sc, mk_integer(sc, info->gid));
}

/// *Procedure* (*user-info:home-dir* _user-info_)
///
/// From SRFI 170
///
/// Return the user's home directory as a string from the given
/// _user-info_ object.
///
static pointer prim_user_info_home_dir(void)
{
    struct user_info *info;

    arg_user_info(&info);
    return arg_err() ? ARG_ERR : _s_return(sc, info->home_dir);
}

/// *Procedure* (*user-info:shell* _user-info_)
///
/// From SRFI 170
///
static pointer prim_user_info_shell(void)
{
    struct user_info *info;

    arg_user_info(&info);
    return arg_err() ? ARG_ERR : _s_return(sc, info->shell);
}

/// *Procedure* (*user-info:full-name* _user-info_)
///
/// From SRFI 170
///
/// Return the "full name", "real name" or "display name" from the
/// given _user-info_ object.
///
static pointer prim_user_info_full_name(void)
{
    struct user_info *info;

    arg_user_info(&info);
    return arg_err() ? ARG_ERR : _s_return(sc, info->full_name);
}

/// *Procedure* (*user-info:parsed-full-name* _user-info_)
///
/// From SRFI 170
///
static pointer prim_user_info_parsed_full_name(void)
{
    struct user_info *info;

    arg_user_info(&info);
    return arg_err() ? ARG_ERR : _s_return(sc, info->parsed_full_name);
}

static pointer prim_vector_p(void) { return obj_predicate(is_vector); }

static pointer prim_working_directory(void)
{
    return arg_err() ? ARG_ERR : os_working_directory();
}

static const struct primitive primitives[] = {
    { "<", prim_num_lt },
    { "<=", prim_num_le },
    { "=", prim_num_eq },
    { ">", prim_num_gt },
    { ">=", prim_num_ge },
    { "append", prim_append },
    { "apropos", prim_apropos },
    { "atom->string", prim_atom_to_string },
    { "boolean?", prim_boolean_p },
    { "bounded-length", prim_bounded_length },
    { "car", prim_car },
    { "cdr", prim_cdr },
    { "char?", prim_char_p },
    { "close-directory-list", prim_close_directory_list },
    { "closure?", prim_closure_p },
    { "command-line", prim_command_line },
    { "cons", prim_cons },
    { "cons*", prim_cons_star },
    { "create-directory", prim_create_directory },
    { "current-input-port", prim_current_input_port },
    { "current-output-port", prim_current_output_port },
    { "delete-directory", prim_delete_directory },
    { "delete-environment-variable!", prim_delete_environment_variable },
    { "delete-file", prim_delete_file },
    { "environment?", prim_environment_p },
    { "eof-object?", prim_eof_object_p },
    { "eq?", prim_eq_p },
    { "eqv?", prim_eqv_p },
    { "exit", prim_exit },
    { "features", prim_features },
    { "file-exists?", prim_file_exists_p },
    { "file-info", prim_file_info },
    { "file-info:gid", prim_file_info_gid },
    { "file-info:mode", prim_file_info_mode },
    { "file-info:mtime", prim_file_info_mtime },
    { "file-info:size", prim_file_info_size },
    { "file-info:uid", prim_file_info_uid },
    { "file-info?", prim_file_info_p },
    { "gc", prim_gc },
    { "gc-verbose", prim_gc_verbose },
    { "gensym", prim_gensym },
    { "get-environment-variable", prim_get_environment_variable },
    { "get-environment-variables", prim_get_environment_variables },
    { "get-output-string", prim_get_output_string },
    { "help", prim_help },
    { "input-port?", prim_input_port_p },
    { "integer?", prim_integer_p },
    { "iota", prim_iota },
    { "length", prim_length },
    { "list?", prim_list_p },
    { "macro?", prim_macro_p },
    { "make-list", prim_make_list },
    { "make-string", prim_make_string },
    { "new-segment", prim_new_segment },
    { "not", prim_not },
    { "null?", prim_null_p },
    { "number?", prim_number_p },
    { "oblist", prim_oblist },
    { "open-directory-list", prim_open_directory_list },
    { "open-input-output-string", prim_open_input_output_string },
    { "open-input-string", prim_open_input_string },
    { "open-output-string", prim_open_output_string },
    { "output-port?", prim_output_port_p },
    { "pair?", prim_pair_p },
    { "pid", prim_pid },
    { "port?", prim_port_p },
    { "posix-time", prim_posix_time },
    { "procedure?", prim_procedure_p },
    { "random-integer", prim_random_integer },
    { "read-directory", prim_read_directory },
    { "real-path", prim_real_path },
    { "real?", prim_real_p },
    { "reverse", prim_reverse },
    { "set-environment-variable!", prim_set_environment_variable },
    { "set-file-mode", prim_set_file_mode },
    { "set-umask", prim_set_umask },
    { "set-working-directory", prim_set_working_directory },
    { "string->atom", prim_string_to_atom },
    { "string->list", prim_string_to_list },
    { "string->symbol", prim_string_to_symbol },
    { "string-append", prim_string_append },
    { "string-length", prim_string_length },
    { "string-ref", prim_string_ref },
    { "string-set!", prim_string_set },
    { "string?", prim_string_p },
    { "substring", prim_substring },
    { "symbol->string", prim_symbol_to_string },
    { "symbol?", prim_symbol_p },
    { "timespec", prim_timespec },
    { "timespec-nanoseconds", prim_timespec_nanoseconds },
    { "timespec-seconds", prim_timespec_seconds },
    { "timespec<?", prim_timespec_lt_p },
    { "timespec=?", prim_timespec_eq_p },
    { "umask", prim_umask },
    { "user-effective-gid", prim_user_effective_gid },
    { "user-effective-uid", prim_user_effective_uid },
    { "user-gid", prim_user_gid },
    { "user-info", prim_user_info },
    { "user-info:full-name", prim_user_info_full_name },
    { "user-info:gid", prim_user_info_gid },
    { "user-info:home-dir", prim_user_info_home_dir },
    { "user-info:name", prim_user_info_name },
    { "user-info:parsed-full-name", prim_user_info_parsed_full_name },
    { "user-info:shell", prim_user_info_shell },
    { "user-info:uid", prim_user_info_uid },
    { "user-info?", prim_user_info_p },
    { "user-supplementary-gids", prim_user_supplementary_gids },
    { "user-uid", prim_user_uid },
    { "vector?", prim_vector_p },
    { "working-directory", prim_working_directory },
};

static const size_t nprimitive = sizeof(primitives) / sizeof(primitives[0]);

static pointer call_primitive(size_t i)
{
    const struct primitive *prim;
    pointer result;

    prim = &primitives[i];
    prim_name = prim->name;
    prim_args = sc->args;
    prim_arg_err = 0;
    prim_args_done = 0;
    result = prim->func();
    if (!prim_args_done) {
        prim_arg_err = "args not done";
    }
    if (prim_arg_err) {
        return _Error_1(sc, prim_arg_err, 0);
    }
    return result;
}

static const char *procname(pointer x)
{
    const char *name = 0;
    int n;

    n = procnum(x);
    if (n < (int)ndispatch) {
        name = dispatch_table[n].name;
    } else if (n < (int)(ndispatch + nprimitive)) {
        name = primitives[n - ndispatch].name;
    }
    return name ? name : "illegal";
}

/* kernel of this interpreter */
static void Eval_Cycle(scheme *sc, enum scheme_opcodes op)
{
    sc->op = op;
    for (;;) {
        if (sc->op < (int)ndispatch) {
            op_code_info *pcd = dispatch_table + sc->op;
            if (pcd->name != 0) { /* if built-in function, check arguments */
                char msg[STRBUFFSIZE];
                int ok = 1;
                int n = list_length(sc, sc->args);

                /* Check number of arguments */
                if (n < pcd->min_arity) {
                    ok = 0;
                    snprintf(msg, STRBUFFSIZE,
                        "%d %s: needs%s %d argument(s)", sc->op, pcd->name,
                        pcd->min_arity == pcd->max_arity ? "" : " at least",
                        pcd->min_arity);
                }
                if (ok && n > pcd->max_arity) {
                    ok = 0;
                    snprintf(msg, STRBUFFSIZE,
                        "%d %s: needs%s %d argument(s)", sc->op, pcd->name,
                        pcd->min_arity == pcd->max_arity ? "" : " at most",
                        pcd->max_arity);
                }
                if (ok) {
                    if (pcd->arg_tests_encoding != 0) {
                        int i = 0;
                        int j;
                        const char *t = pcd->arg_tests_encoding;
                        pointer arglist = sc->args;
                        do {
                            pointer arg = car(arglist);
                            j = (int)t[0];
                            if (j == TST_LIST[0]) {
                                if (arg != sc->NIL && !is_pair(arg))
                                    break;
                            } else {
                                if (!tests[j].fct(arg))
                                    break;
                            }

                            if (t[1] != 0) { /* last test is replicated as
                                                necessary */
                                t++;
                            }
                            arglist = cdr(arglist);
                            i++;
                        } while (i < n);
                        if (i < n) {
                            ok = 0;
                            snprintf(msg, STRBUFFSIZE,
                                "%s: argument %d must be: %s", pcd->name,
                                i + 1, tests[j].kind);
                        }
                    }
                }
                if (!ok) {
                    if (_Error_1(sc, msg, 0) == sc->NIL) {
                        return;
                    }
                    pcd = dispatch_table + sc->op;
                }
            }
            ok_to_freely_gc(sc);
            if (pcd->func(sc, (enum scheme_opcodes)sc->op) == sc->NIL) {
                return;
            }
        } else {
            if (call_primitive(sc->op - ndispatch) == sc->NIL) {
                return;
            }
        }
        if (sc->no_memory) {
            fprintf(stderr, "No memory!\n");
            return;
        }
    }
}

/* ========== Initialization of internal keywords ========== */

static void assign_syntax(scheme *sc, char *name)
{
    pointer x;

    x = oblist_add_by_name(sc, name);
    typeflag(x) |= T_SYNTAX;
}

static void assign_proc(scheme *sc, enum scheme_opcodes op, const char *name)
{
    pointer x, y;

    x = mk_symbol(sc, name);
    y = mk_proc(sc, op);
    new_slot_in_env(sc, x, y);
}

static pointer mk_proc(scheme *sc, enum scheme_opcodes op)
{
    pointer y;

    y = get_cell(sc, sc->NIL, sc->NIL);
    typeflag(y) = (T_PROC | T_ATOM);
    ivalue_unchecked(y) = (long)op;
    set_num_integer(y);
    return y;
}

/* Hard-coded for the given keywords. Remember to rewrite if more are
 * added!
 */
static int syntaxnum(pointer p)
{
    const char *s = strvalue(car(p));
    switch (strlength(car(p))) {
    case 2:
        if (s[0] == 'i')
            return OP_IF0; /* if */
        else
            return OP_OR0; /* or */
    case 3:
        if (s[0] == 'a')
            return OP_AND0; /* and */
        else
            return OP_LET0; /* let */
    case 4:
        switch (s[3]) {
        case 'e':
            return OP_CASE0; /* case */
        case 'd':
            return OP_COND0; /* cond */
        case '*':
            return OP_LET0AST; /* let* */
        default:
            return OP_SET0; /* set! */
        }
    case 5:
        switch (s[2]) {
        case 'g':
            return OP_BEGIN; /* begin */
        case 'l':
            return OP_DELAY; /* delay */
        case 'c':
            return OP_MACRO0; /* macro */
        default:
            return OP_QUOTE; /* quote */
        }
    case 6:
        switch (s[2]) {
        case 'm':
            return OP_LAMBDA; /* lambda */
        case 'f':
            return OP_DEF0; /* define */
        default:
            return OP_LET0REC; /* letrec */
        }
    default:
        return OP_C0STREAM; /* cons-stream */
    }
}

int scheme_init(scheme *sc)
{
    return scheme_init_custom_alloc(sc, malloc, free);
}

int scheme_init_custom_alloc(scheme *sc, func_alloc malloc, func_dealloc free)
{
    size_t i;
    pointer x;

    num_zero.is_fixnum = 1;
    num_zero.value.ivalue = 0;
    num_one.is_fixnum = 1;
    num_one.value.ivalue = 1;

    sc->gensym_cnt = 0;
    sc->malloc = malloc;
    sc->free = free;
    sc->last_cell_seg = -1;
    sc->sink = &sc->_sink;
    sc->NIL = &sc->_NIL;
    sc->T = &sc->_HASHT;
    sc->F = &sc->_HASHF;
    sc->EOF_OBJ = &sc->_EOF_OBJ;
    sc->free_cell = &sc->_NIL;
    sc->fcells = 0;
    sc->no_memory = 0;
    sc->inport = sc->NIL;
    sc->outport = sc->NIL;
    sc->save_inport = sc->NIL;
    sc->loadport = sc->NIL;
    sc->nesting = 0;
    sc->interactive_repl = 0;

    if (alloc_cellseg(sc, FIRST_CELLSEGS) != FIRST_CELLSEGS) {
        sc->no_memory = 1;
        return 0;
    }
    sc->gc_verbose = 0;
    dump_stack_initialize(sc);
    sc->code = sc->NIL;
    sc->tracing = 0;

    /* init sc->NIL */
    typeflag(sc->NIL) = (T_ATOM | MARK);
    car(sc->NIL) = cdr(sc->NIL) = sc->NIL;
    /* init T */
    typeflag(sc->T) = (T_ATOM | MARK);
    car(sc->T) = cdr(sc->T) = sc->T;
    /* init F */
    typeflag(sc->F) = (T_ATOM | MARK);
    car(sc->F) = cdr(sc->F) = sc->F;
    /* init sink */
    typeflag(sc->sink) = (T_PAIR | MARK);
    car(sc->sink) = sc->NIL;
    /* init c_nest */
    sc->c_nest = sc->NIL;

    sc->oblist = oblist_initial_value(sc);
    /* init global_env */
    new_frame_in_env(sc, sc->NIL);
    sc->global_env = sc->envir;
    /* init else */
    x = mk_symbol(sc, "else");
    new_slot_in_env(sc, x, sc->T);

    assign_syntax(sc, "lambda");
    assign_syntax(sc, "quote");
    assign_syntax(sc, "define");
    assign_syntax(sc, "if");
    assign_syntax(sc, "begin");
    assign_syntax(sc, "set!");
    assign_syntax(sc, "let");
    assign_syntax(sc, "let*");
    assign_syntax(sc, "letrec");
    assign_syntax(sc, "cond");
    assign_syntax(sc, "delay");
    assign_syntax(sc, "and");
    assign_syntax(sc, "or");
    assign_syntax(sc, "cons-stream");
    assign_syntax(sc, "macro");
    assign_syntax(sc, "case");

    for (i = 0; i < ndispatch; i++) {
        if (dispatch_table[i].name != 0) {
            assign_proc(sc, (enum scheme_opcodes)i, dispatch_table[i].name);
        }
    }
    for (i = 0; i < nprimitive; i++) {
        assign_proc(
            sc, (enum scheme_opcodes)(ndispatch + i), primitives[i].name);
    }

    /* initialization of global pointers to special symbols */
    sc->LAMBDA = mk_symbol(sc, "lambda");
    sc->QUOTE = mk_symbol(sc, "quote");
    sc->QQUOTE = mk_symbol(sc, "quasiquote");
    sc->UNQUOTE = mk_symbol(sc, "unquote");
    sc->UNQUOTESP = mk_symbol(sc, "unquote-splicing");
    sc->FEED_TO = mk_symbol(sc, "=>");
    sc->ERROR_HOOK = mk_symbol(sc, "*error-hook*");
    sc->COMPILE_HOOK = mk_symbol(sc, "*compile-hook*");

    return !sc->no_memory;
}

void scheme_set_input_port_file(scheme *sc, FILE *fin)
{
    sc->inport = port_from_file(sc, fin, port_input);
}

void scheme_set_input_port_string(scheme *sc, char *start, char *past_the_end)
{
    sc->inport = port_from_string(sc, start, past_the_end, port_input);
}

void scheme_set_output_port_file(scheme *sc, FILE *fout)
{
    sc->outport = port_from_file(sc, fout, port_output);
}

void scheme_set_output_port_string(
    scheme *sc, char *start, char *past_the_end)
{
    sc->outport = port_from_string(sc, start, past_the_end, port_output);
}

void scheme_deinit(scheme *sc)
{
    int i;

#if SHOW_ERROR_LINE
    char *fname;
#endif

    sc->oblist = sc->NIL;
    sc->global_env = sc->NIL;
    dump_stack_free(sc);
    sc->envir = sc->NIL;
    sc->code = sc->NIL;
    sc->args = sc->NIL;
    sc->value = sc->NIL;
    if (is_port(sc->inport)) {
        typeflag(sc->inport) = T_ATOM;
    }
    sc->inport = sc->NIL;
    sc->outport = sc->NIL;
    if (is_port(sc->save_inport)) {
        typeflag(sc->save_inport) = T_ATOM;
    }
    sc->save_inport = sc->NIL;
    if (is_port(sc->loadport)) {
        typeflag(sc->loadport) = T_ATOM;
    }
    sc->loadport = sc->NIL;
    sc->gc_verbose = 0;
    gc(sc, sc->NIL, sc->NIL);

    for (i = 0; i <= sc->last_cell_seg; i++) {
        sc->free(sc->alloc_seg[i]);
    }

#if SHOW_ERROR_LINE
    for (i = 0; i <= sc->file_i; i++) {
        if (sc->load_stack[i].kind & port_file) {
            fname = sc->load_stack[i].rep.stdio.filename;
            if (fname)
                sc->free(fname);
        }
    }
#endif
}

void scheme_load_named_file(scheme *sc, FILE *fin, const char *filename)
{
    dump_stack_reset(sc);
    sc->envir = sc->global_env;
    sc->file_i = 0;
    sc->load_stack[0].kind = port_input | port_file;
    sc->load_stack[0].rep.stdio.file = fin;
    sc->loadport = mk_port(sc, sc->load_stack);
    sc->retcode = 0;
    if (fin == stdin) {
        sc->interactive_repl = 1;
    }

#if SHOW_ERROR_LINE
    sc->load_stack[0].rep.stdio.curr_line = 0;
    if (fin != stdin && filename)
        sc->load_stack[0].rep.stdio.filename
            = store_string(sc, strlen(filename), filename, 0);
#endif

    sc->inport = sc->loadport;
    sc->args = mk_integer(sc, sc->file_i);
    Eval_Cycle(sc, OP_T0LVL);
    typeflag(sc->loadport) = T_ATOM;
    if (sc->retcode == 0) {
        sc->retcode = sc->nesting != 0;
    }
}

static void scheme_load_file_or_die(const char *filename)
{
    FILE *input;

    if (!(input = fopen(filename, "r"))) {
        die("could not open script file");
    }
    scheme_load_named_file(sc, input, filename);
    if (sc->retcode != 0) {
        die("error loading script file");
    }
    fclose(input);
}

void scheme_define(scheme *sc, pointer envir, pointer symbol, pointer value)
{
    pointer x;

    x = find_slot_in_env(sc, envir, symbol, 0);
    if (x != sc->NIL) {
        set_slot_in_env(sc, x, value);
    } else {
        new_slot_spec_in_env(sc, envir, symbol, value);
    }
}

static void generic_usage(FILE *out, int exitcode)
{
    fprintf(out, "usage: scheme                      REPL\n");
    fprintf(out, "usage: scheme <file> [<args...>]   Run script and exit\n");
    fprintf(out, "usage: scheme -h                   This help\n");
    fprintf(out, "usage: scheme -V                   Version info\n");
    exit(exitcode);
}

static void usage(void) { generic_usage(stderr, 2); }

static void version(void)
{
    const char **sp;
    const char *s;

    printf("This is Desert Island Scheme, a Scheme interpreter in one C "
           "file.\n\n");
    printf("(command \"discheme\")\n");
    printf("(scheme-id discheme)\n");
    printf("(languages scheme)\n");
    printf("(features");
    for (sp = features_as_strings; (s = *sp); sp++) {
        printf(" %s", s);
    }
    printf(")\n");
    printf("(discheme/specs)\n");
    printf("(discheme/unstable-spec 2019)\n");
    printf("(revision \"%s\")\n", SCHEME_VERSION);
    printf("(build-date \"%s\")\n", SCHEME_BUILD_DATE);
    printf("(c-type-bits (int %d) (long %d) (pointer %d))\n",
        (int)(sizeof(int) * CHAR_BIT), (int)(sizeof(long) * CHAR_BIT),
        (int)(sizeof(void *) * CHAR_BIT));
    s = getenv("LANG");
    printf("(env/LANG \"%s\")\n", s ? s : "");
    s = getenv("TERM");
    printf("(env/TERM \"%s\")\n", s ? s : "");
    exit(0);
}

int main(int argc, char **argv)
{
    const char *progname;
    const char *script;
    const char *arg;
    int hflag, vflag, retcode;

    (void)argc;
    script = 0;
    hflag = vflag = 0;
    progname = *argv++;
    while ((arg = *argv)) {
        if (!strcmp(arg, "--")) {
            argv++;
            break;
        } else if (!strcmp(arg, "-h") || !strcmp(arg, "-help")
            || !strcmp(arg, "--help")) {
            hflag = 1;
        } else if (!strcmp(arg, "-V")) {
            vflag = 1;
        } else if (arg[0] == '-') {
            usage();
        } else {
            script = arg;
            break;
        }
        argv++;
    }
    if (hflag) {
        generic_usage(stdout, 0);
    } else if (vflag) {
        version();
    }
    if (!(sc = calloc(1, sizeof(*sc)))) {
        die("out of memory");
    }
    if (!scheme_init(sc)) {
        die("could not initialize");
    }
    scheme_set_input_port_file(sc, stdin);
    scheme_set_output_port_file(sc, stdout);
    g_features = mk_string_list(features_as_strings, 1);
    g_command_line = mk_string_list((const char **)argv, 0);
    scheme_define(sc, sc->global_env, mk_symbol(sc, "script-real-path"),
        script ? os_real_path_or_false(script) : sc->F);
    if (script) {
        scheme_load_file_or_die(script);
    } else {
        if (!strcmp(progname, "vacation")) {
            prompt = "\xf0\x9f\x8c\xb4  ";
        } else if (!strcmp(progname, "easter")) {
            prompt = "\xf0\x9f\x97\xbf  ";
        }
        if (SCHEME_VERSION[0]) {
            printf("Desert Island Scheme (version %s)\n", SCHEME_VERSION);
        } else {
            printf("Desert Island Scheme\n");
        }
        scheme_load_named_file(sc, stdin, 0);
    }
    retcode = sc->retcode;
    scheme_deinit(sc);
    return retcode;
}
