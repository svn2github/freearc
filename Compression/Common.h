// Common definitions, used by various parts of FreeArc project
#ifndef FREEARC_COMMON_H
#define FREEARC_COMMON_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <limits.h>
#include <math.h>
#include <time.h>
#include <setjmp.h>

#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>

#ifdef FREEARC_WIN
#include <windows.h>
#include <direct.h>
#include <float.h>
#define strcasecmp stricmp
#ifndef __GNUC__
#define logb       _logb
#endif
#endif

#ifdef __cplusplus
extern "C" {
#endif

/******************************************************************************
** Базовые определения FREEARC ************************************************
******************************************************************************/
#if !defined(FREEARC_WIN) && !defined(FREEARC_UNIX)
#error "You must define OS!"
#endif

#if defined(FREEARC_INTEL_BYTE_ORDER)
#if _BIG_ENDIAN
#error "You're compiling for Motorola byte order, but FREEARC_INTEL_BYTE_ORDER was defined."
#endif
#elif defined(FREEARC_MOTOROLA_BYTE_ORDER)
#if _M_IX86 || __i386 || __x86_64
#error "You're compiling for Intel byte order, but FREEARC_MOTOROLA_BYTE_ORDER was defined."
#endif
#else
#error "You must define byte order!"
#endif

#ifdef FREEARC_WIN
#define UNSUPPORTED_PATH_DELIMITERS   "/"
#define PATH_DELIMITER                '\\'
#define STR_PATH_DELIMITER            "\\"
#else
#define UNSUPPORTED_PATH_DELIMITERS   ":\\"
#define PATH_DELIMITER                '/'
#define STR_PATH_DELIMITER            "/"
#endif

#define  DIRECTORY_DELIMITERS         "/\\"
#define  ALL_PATH_DELIMITERS          ":/\\"


/******************************************************************************
** Синонимы для простых типов, используемых в программе ***********************
******************************************************************************/
typedef unsigned long        ulong;
typedef unsigned int         uint,   UINT;
typedef unsigned short int   ushort;
typedef unsigned char        uchar;
#ifdef __GNUC__
#include <stdint.h>
typedef          uint64_t    uint64;
typedef          uint32_t    uint32;
typedef          uint16_t    uint16;
typedef          uint8_t     uint8,  byte, BYTE;
typedef          int64_t     sint64, int64;
typedef          int32_t     sint32, int32;
typedef          int16_t     sint16, int16;
typedef          int8_t      sint8,  int8;
#else
typedef          __int64     sint64, int64;
typedef unsigned __int64     uint64;
typedef          __int32     sint32, int32;
typedef unsigned __int32     uint32;
typedef          __int16     sint16, int16;
typedef unsigned __int16     uint16;
typedef          __int8      sint8,  int8;
typedef unsigned __int8      uint8,  byte, BYTE;
#endif

typedef unsigned             MemSize;          // объём памяти
typedef char*                FILENAME;         // имя файла
#ifdef FREEARC_WIN
typedef int64                FILESIZE;         // размер файла
#else
typedef off_t                FILESIZE;
#endif


/******************************************************************************
** Стандартные определения ****************************************************
******************************************************************************/
#define make4byte(a,b,c,d)       ((a)+256*((b)+256*((c)+256*(((uint32)d)))))
#define iterate(num, statement)  {for( int i=0; i<(num); i++) {statement;}}
#define iterate_var(i, num)      for( int i=0; i<(num); i++)
#define iterate_array(i, array)  for( int i=0; i<(array).size; i++)
#ifndef TRUE
#define TRUE                     1
#endif
#ifndef FALSE
#define FALSE                    0
#endif

#define PATH_CHARS               ":/\\"
#define is_path_char(c)          in_set(c, PATH_CHARS)
#define in_set(c, set)           (strchr (set, c ) != NULL)
#define in_set0(c, set)          (memchr (set, c, sizeof(set) ) != 0)
#define str_end(str)             (strchr (str,'\0'))
#define last_char(str)           (str_end(str) [-1])
#define strequ(a,b)              (strcmp((a),(b))==EQUAL)
#define namecmp                  strcasecmp
#define nameequ(s1,s2)           (namecmp(s1,s2)==EQUAL)
#define start_with(str,with)     (strncmp (str, with, strlen(with))==EQUAL)
#define end_with(str,with)       (nameequ (str_end(str)-strlen(with), with))
#define strdup_msg(s)            (strcpy (new char[strlen(s)+1], (s)))
#define find_extension(str)      (find_extension_in_entry (drop_dirname(str)))
#define mymax(a,b)               ((a)>(b)? (a) : (b))
#define mymin(a,b)               ((a)<(b)? (a) : (b))
#define inrange(x,a,b)           ((a)<=(x) && (x)<(b))
#define elements(arr)            (sizeof(arr)/sizeof(*arr))
#define endof(arr)               ((arr)+elements(arr))
#define zeroArray(arr)           (memset (arr, 0, sizeof(arr)))
#define EQUAL                    0   /* result of strcmp/memcmp for equal strings */

// Skip directory in filename
static inline char *drop_dirname (char *filename)
{
  char *p;
  for (p = &last_char(filename); p >= filename; p--)
  {
    if (is_path_char(*p))
      return p+1;
  }
  return filename;
}


// ****************************************************************************
// FILE OPERATIONS ************************************************************
// ****************************************************************************

#define MY_FILENAME_MAX 65536                  /* maximum length of filename */

#ifdef FREEARC_WIN

#include <io.h>
#include <fcntl.h>
#include <tchar.h>
typedef TCHAR* CFILENAME;
static inline void delete_file (CFILENAME name) {_tremove(name);}
static inline void create_dir  (CFILENAME name) {_tmkdir(name);}
static inline int  file_exists (CFILENAME name) {return _taccess(name,0) == 0;}
#define set_flen(stream,new_size)               (chsize( file_no(stream), new_size ))
#define get_flen(stream)                        (_filelengthi64(fileno(stream)))
#define myeof(file)                             (feof(file))
#define get_ftime(stream,tstamp)                getftime( file_no(stream), (struct ftime *) &tstamp )
#define set_ftime(stream,tstamp)                setftime( file_no(stream), (struct ftime *) &tstamp )
#define set_binary_mode(file)                   setmode(fileno(file),O_BINARY)

// Number of 100 nanosecond units from 01.01.1601 to 01.01.1970
#define EPOCH_BIAS    116444736000000000ull

static inline void WINAPI UnixTimeToFileTime( time_t time, FILETIME* ft )
{
  *(uint64*)ft = EPOCH_BIAS + time * 10000000ull;
}


#endif // FREEARC_WIN


#ifdef FREEARC_UNIX

#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>

#define __cdecl
#define SSIZE_T ssize_t

typedef char* CFILENAME;
typedef char  TCHAR;
#define _T
#define _tcschr         strchr
#define _tcscpy         strcpy
#define _stprintf       sprintf
#define _tcslen         strlen
#define _tstat          stat
#define _stat           stat

typedef int (*FARPROC) (void);

static inline void delete_file (char *name)     {remove(name);}
static inline void create_dir  (char *name)     {mkdir(name,0777);}
static inline int  file_exists (char *name)     {return access(name,0) == 0;}
#define get_flen(stream)                        (myfilelength( fileno (stream)))
#define set_binary_mode(file)
#define myeof(file)                             (get_flen(file) == ftell(file))
static inline off_t myfilelength (int h)
{
  off_t saved = lseek (h, 0, SEEK_CUR);
  off_t size  = lseek (h, 0, SEEK_END);
  lseek (h, saved, SEEK_SET);
  return size<0? -1 : size;
}

#endif // FREEARC_UNIX

#define rename_file(oldname,newname)            rename(oldname,newname)
#define delete_dir(name)                        rmdir(name)
#define file_read(file, buf, size)              fread  (buf, 1, size, file)
#define file_write(file, buf, size)             fwrite (buf, 1, size, file)

static inline int dir_exists (const TCHAR *name)
{
  struct _stat st;
  _tstat(name,&st);
  return (st.st_mode & S_IFDIR) != 0;
}


// ****************************************************************************
// ****************************************************************************
// ****************************************************************************

#ifdef FREEARC_INTEL_BYTE_ORDER

// Read unsigned 16/24/32-bit value at given address
#define value16(p)               (*(uint16*)(p))
#define value24(p)               (*(uint32*)(p) & 0xffffff)
#define value32(p)               (*(uint32*)(p))
#define value64(p)               (*(uint64*)(p))
// Write unsigned 16/24/32-bit value to given address
#define setvalue16(p,x)          (*(uint16*)(p) = (x))
#define setvalue24(p,x)          (*(uint32*)(p) = ((x) & 0xffffff) + (*(uint32*)(p) & 0xff000000))
#define setvalue32(p,x)          (*(uint32*)(p) = (x))
#define setvalue64(p,x)          (*(uint64*)(p) = (x))

#elif FREEARC_MOTOROLA_BYTE_ORDER
// routines for non-little-endian cpus, written by Joachim Henke
#if _ARCH_PPC
#if __GNUC__ == 4 && __GNUC_MINOR__ > 0 || __GNUC__ > 4
#define PPC_MCONSTR "Z"
#else
#define PPC_MCONSTR "Q"
#endif
#define PPC_LBRX(s,p,x)   __asm__ ("l"  s "brx %0,%y1" : "=r" (x) : PPC_MCONSTR (*p))
#define PPC_STBRX(s,p,x)  __asm__ ("st" s "brx %1,%y0" : "=" PPC_MCONSTR (*p) : "r" (x))
#endif

static inline uint16 value16 (void *p)
{
  uint16 x;
#if _ARCH_PPC
  uint16 *m = (uint16 *)p;
  PPC_LBRX("h", m, x);
#else
  uint8 *m = (uint8 *)p;
  x = m[0] + (m[1] << 8);
#endif
  return x;
}

static inline uint32 value24 (void *p)
{
  uint32 x;
#if __GNUC__ == 4 && __GNUC_MINOR__ > 2 || __GNUC__ > 4
  uint32 *m = (uint32 *)p;
  x = __builtin_bswap32(*m) & 0xffffff;
#elif _ARCH_PPC
  uint32 *m = (uint32 *)p;
  PPC_LBRX("w", m, x);
  x &= 0xffffff;
#else
  uint8 *m = (uint8 *)p;
  x = m[0] + (m[1] << 8) + (m[2] << 16);
#endif
  return x;
}

static inline uint32 value32 (void *p)
{
  uint32 x;
#if __GNUC__ == 4 && __GNUC_MINOR__ > 2 || __GNUC__ > 4
  uint32 *m = (uint32 *)p;
  x = __builtin_bswap32(*m);
#elif _ARCH_PPC
  uint32 *m = (uint32 *)p;
  PPC_LBRX("w", m, x);
#else
  uint8 *m = (uint8 *)p;
  x = m[0] + (m[1] << 8) + (m[2] << 16) + (m[3] << 24);
#endif
  return x;
}

static inline uint64 value64 (void *p)
{
  uint64 x;
#if _ARCH_PPC && __PPU__
  uint64 *m = (uint64 *)p;
  PPC_LBRX("d", m, x);
#else
  uint32 *m = (uint32 *)p;
  x = value32(m) + ((uint64)value32(m + 1) << 32);
#endif
  return x;
}

static inline void setvalue16 (void *p, uint16 x)
{
#if _ARCH_PPC
  uint16 *m = (uint16 *)p;
  PPC_STBRX("h", m, x);
#else
  uint8 *m = (uint8 *)p;
  m[0] = x;
  m[1] = x >> 8;
#endif
}

static inline void setvalue24 (void *p, uint32 x)
{
  uint8 *m = (uint8 *)p;
  m[0] = x;
  m[1] = x >> 8;
  m[2] = x >> 16;
}

static inline void setvalue32 (void *p, uint32 x)
{
#if __GNUC__ == 4 && __GNUC_MINOR__ > 2 || __GNUC__ > 4
  uint32 *m = (uint32 *)p;
  *m = __builtin_bswap32(x);
#elif _ARCH_PPC
  uint32 *m = (uint32 *)p;
  PPC_STBRX("w", m, x);
#else
  uint8 *m = (uint8 *)p;
  m[0] = x;
  m[1] = x >> 8;
  m[2] = x >> 16;
  m[3] = x >> 24;
#endif
}

static inline void setvalue64 (void *p, uint64 x)
{
#if _ARCH_PPC && __PPU__
  uint64 *m = (uint64 *)p;
  PPC_STBRX("d", m, x);
#else
  uint32 *m = (uint32 *)p;
  setvalue32(m, x);
  setvalue32(m + 1, x >> 32);
#endif
}

#endif //FREEARC_MOTOROLA_BYTE_ORDER

// Check for equality
#define val16equ(p,q)             (*(uint16*)(p) == *(uint16*)(q))
#define val24equ(p,q)             (   value24(p) ==    value24(q))
#define val32equ(p,q)             (*(uint32*)(p) == *(uint32*)(q))
#define val64equ(p,q)             (*(uint64*)(p) == *(uint64*)(q))

// Free memory block and set pointer to NULL
#ifndef FreeAndNil
#define FreeAndNil(p)            ((p) && (free(p), (p)=NULL))
#endif

// Exit code used to indicate serious problems in FreeArc utilities
#define FREEARC_EXIT_ERROR 2

// Переменные, используемые для сигнализации об ошибках из глубоко вложеных процедур
extern int jmpready;
extern jmp_buf jumper;

// Процедура сообщения о неожиданных ошибочных ситуациях
#ifndef CHECK
#  if defined(FREEARC_WIN) && defined(FREEARC_GUI)
#    define CHECK(a,b)           {if (!(a))  {if (jmpready) longjmp(jumper,1);  char *s=(char*)malloc(MY_FILENAME_MAX*4);  WCHAR *utf16=(WCHAR*) malloc(MY_FILENAME_MAX*4);  sprintf b;  utf8_to_utf16(s,utf16);  MessageBoxW(NULL, utf16, L"Error encountered", MB_ICONERROR);  ON_CHECK_FAIL();  exit(FREEARC_EXIT_ERROR);}}
#  elif defined(FREEARC_WIN)
#    define CHECK(a,b)           {if (!(a))  {if (jmpready) longjmp(jumper,1);  char *s=(char*)malloc(MY_FILENAME_MAX*4),  *oem=(char*)malloc(MY_FILENAME_MAX*4);  sprintf b;  utf8_to_oem(s,oem);  printf("\n%s",oem);  ON_CHECK_FAIL();  exit(FREEARC_EXIT_ERROR);}}
#  else
#    define CHECK(a,b)           {if (!(a))  {if (jmpready) longjmp(jumper,1);  char s[MY_FILENAME_MAX*4];  sprintf b;  printf("\n%s",s);  ON_CHECK_FAIL();  exit(FREEARC_EXIT_ERROR);}}
#  endif
#endif

#ifndef ON_CHECK_FAIL
#define ON_CHECK_FAIL()
#endif

// Устанавливает Jump Point с кодом возврата retcode
#define SET_JMP_POINT(retcode)                                                         \
{                                                                                      \
  if (!jmpready && setjmp(jumper) != 0)                                                \
    /* Сюда мы попадём при возникновении ошибки в одной из вызываемых процедур */      \
    {jmpready = FALSE; return retcode;}                                                \
  jmpready = TRUE;                                                                     \
}

// Снимает Jump Point
#define RESET_JMP_POINT()                                                              \
{                                                                                      \
  jmpready = FALSE;                                                                    \
}


// Include statements marked as debug(..)  only if we enabled debugging
#ifdef DEBUG
#define debug(stmt)  stmt
#else
#define debug(stmt)  ((void)0)
#endif

// Include statements marked as stat(..)  only if we enabled gathering stats
#ifdef STAT
#define stat(stmt)  stmt
#else
#define stat(stmt)  ((void)0)
#endif

// Define default parameter value only when compiled as C++
#ifdef __cplusplus
#define DEFAULT(x,n) x=n
#else
#define DEFAULT(x,n) x
#endif

#define then


// ****************************************************************************
// MEMORY ALLOCATION **********************************************************
// ****************************************************************************
#ifndef __cplusplus
#define throw()
#endif

#ifndef FREEARC_STANDALONE_TORNADO
void *MyAlloc(size_t size) throw();
void MyFree(void *address) throw();
#ifdef FREEARC_WIN
int SetLargePageSize();
void *MidAlloc(size_t size) throw();
void MidFree(void *address) throw();
void *BigAlloc(size_t size) throw();
void BigFree(void *address) throw();
#else
#define MidAlloc(size) MyAlloc(size)
#define MidFree(address) MyFree(address)
#define BigAlloc(size) MyAlloc(size)
#define BigFree(address) MyFree(address)
#endif

// ****************************************************************************
// Функции парсинга и арифметики **********************************************
// ****************************************************************************
void strncopy (char *to, char *from, int len);   // Копирует строчку from в to, но не более len символов
int  split (char *str, char splitter, char **result, int result_size);  // Разбить строку str на подстроки, разделённые символом splitter
char*subst (char *original, char *from, char *to);  // Заменяет в строке original все вхождения from на to
char*trim_spaces (char *s);                      // Пропускает пробелы в начале строки и убирает их в конце, модифицируя строку
char *str_replace_n (char *orig, char *from, int how_many, char *to);   // Replace from:how_many substring and put result in new allocated area
char *str_replace   (char *orig, char *from, char *to);    // Replace substring and put result in new allocated area
#endif // !FREEARC_STANDALONE_TORNADO
MemSize parseInt (char *param, int *error);  // If the string param contains an integer, return it - otherwise set error=1
MemSize parseMem (char *param, int *error);  // Similarly, but the string param can contain a suffix b/k/m/g/^, representing units of memory, or in the case of '^' (default), the relevant power of 2
void showMem (MemSize mem, char *result);    // Returns a string with the amount of memory


// Round first number *down* to divisible by second one
static inline MemSize roundDown (MemSize a, MemSize b)
{
  return b>1? a/b*b : a;
}

// Round first number *up* to divisible by second one
static inline MemSize roundUp (MemSize a, MemSize b)
{
  return b>1? roundDown(a-1,b)+b : a;
}


// Whole part of number's binary logarithm (logb) - please ensure that n > 0
static inline MemSize lb (MemSize n)
{
    MemSize result;
#if __INTEL_COMPILER
    result = _bit_scan_reverse(n);
#elif _MSC_VER >= 1400
    _BitScanReverse((DWORD *)&result, n);
#elif __GNUC__ == 3 && __GNUC_MINOR__ > 3 || __GNUC__ > 3
    result = __builtin_clz(n) ^ (8 * sizeof(unsigned int) - 1);
#else
    result = 0;
    if (n > 0xffff) result = 16, n >>= 16;
    if (n > 0xff)   result += 8, n >>= 8;
    if (n > 0xf)    result += 4, n >>= 4;
    if (n > 0x3)    result += 2, n >>= 2;
    if (n > 0x1)    result += 1;
#endif
    return result;
}

// Эта процедура округляет число к ближайшей сверху степени
// базы, например f(13,2)=16
static inline MemSize roundup_to_power_of (MemSize n, MemSize base)
{
    MemSize result = base;
    if (!(--n))
        return 1;
    if (base == 2)
        result <<= lb(n);
    else
        while (n /= base)
            result *= base;
    return result;
}

// Эта процедура округляет число к ближайшей снизу степени
// базы, например f(13,2)=8
static inline MemSize rounddown_to_power_of (MemSize n, MemSize base)
{
    MemSize result = 1;
    if (!n)
        return 1;
    if (base == 2)
        result <<= lb(n);
    else
        while (n /= base)
            result *= base;
    return result;
}

// Эта процедура округляет число к логарифмически ближайшей степени
// базы, например f(9,2)=8  f(15,2)=16
static inline MemSize round_to_nearest_power_of (MemSize n, MemSize base)
{
    MemSize result;
    uint64 nn = ((uint64)n)*n/base;
    if (nn==0)  return 1;
    for (result=base; (nn/=base*base) != 0; result *= base);
    return result;
}

// Превращает число в строку, разделённую точками: "1.234.567"
static inline char* show3 (uint64 n, char *buf)
{
    char *p = buf + 27;
    int i = 4;

    *p = '\0';
    do {
        if (!--i) *--p = '.', i = 3;
        *--p = '0' + (n % 10);
    } while (n /= 10);

    return p;
}

// Заменить символы из множества from на символ to
static inline char *replace (char *str, char* from, char to)
{
  char *p;
  for (p=str; *p; p++)
    if (in_set (*p, from))
      *p = to;
  return str;
}

// Возращает числовое значение символа, рассматриваемого как шестнадцатеричная цифра
static inline int char2int(char c) {return isdigit(c)? c-'0' : tolower(c)-'a';}

#ifdef FREEARC_WIN
// Windows charset conversion routines
WCHAR *utf8_to_utf16 (const char  *utf8,  WCHAR *utf16);  // Converts UTF-8 string to UTF-16
char  *utf16_to_utf8 (const WCHAR *utf16, char  *utf8);   // Converts UTF-16 string to UTF-8
char  *utf8_to_oem   (const char  *utf8,  char  *oem);    // Converts UTF-8 string to OEM
char  *oem_to_utf8   (const char  *oem,   char  *utf8);   // Converts OEM string to UTF-8
#endif

#ifndef FREEARC_NO_TIMING
// Вывод заголовка окна
void EnvSetConsoleTitle (CFILENAME title);  // Установить заголовок консольного окна
void EnvResetConsoleTitle (void);  // Восстановить заголовок, который был в начале работы программы

// Timing execution
double GetGlobalTime     (void);   // Returns number of wall-clock seconds since some moment
double GetThreadCPUTime  (void);   // Returns number of seconds spent in this thread
#endif


#ifdef __cplusplus
}       // extern "C"
#endif

/******************************************************************************
** Bounds-checked arrays ******************************************************
******************************************************************************/

#ifdef DEBUG
#define ARRAYD(type,name,size)  Array<type> name
template <class ELEM>
struct Array
{
    int n;
    ELEM *p;
    Array(int _n)             {n=_n; p=(ELEM*)malloc(sizeof(ELEM)*n);}
    ~Array()                  {free(p);}
    ELEM& operator [](int i)  {CHECK( 0<=i && i<n, (s,"INDEXING ERROR: %d instead of [0,%d)", i, n));
                               return p[n];}
    operator void*()          {return p;}
};
#else
#define ARRAYD(type,name,size)  type name[size]
#endif


/******************************************************************************
** END. ***********************************************************************
******************************************************************************/

#endif  // FREEARC_COMMON_H
