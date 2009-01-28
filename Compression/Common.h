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

#if !defined(FREEARC_INTEL_BYTE_ORDER) && !defined(FREEARC_MOTOROLA_BYTE_ORDER)
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
#ifdef FREEARC_UNIX
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
typedef unsigned long        uint32;
typedef unsigned short int   uint16;
typedef unsigned char        uint8,  byte, BYTE;
typedef   signed long        sint32, int32;
typedef   signed short int   sint16, int16;
typedef   signed char        sint8,  int8;

#ifdef __GNUC__
typedef          long long   sint64, int64;
typedef unsigned long long   uint64;
#elif _MSC_EXTENSIONS || _VISUALC || __INTEL_COMPILER || __BORLANDC__ || __WATCOMC__
typedef          __int64     sint64, int64;
typedef unsigned __int64     uint64;
#else
typedef          long long   sint64, int64;
typedef unsigned long long   uint64;
#endif
#endif //FREEARC_UNIX

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

#endif // FREEARC_WIN


#ifdef FREEARC_UNIX

#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>

typedef char* CFILENAME;
typedef char  TCHAR;
#define _T
#define _tcschr   strchr
#define _stprintf sprintf
#define _tcslen   strlen
#define _tstat    stat
#define _stat     stat

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
#define setvalue24(p,x)          (*(uint32*)(p) = ((x)&0xffffff)+(*(uint*)(p)&0xff000000))
#define setvalue32(p,x)          (*(uint32*)(p) = (x))
#define setvalue64(p,x)          (*(uint64*)(p) = (x))

#elif FREEARC_MOTOROLA_BYTE_ORDER
// routines for non-little-endian cpus, written by Joachim Henke

static inline uint16 value16 (void *p)
{
  uint16 x;
#if defined(__GNUC__) && defined(__powerpc__)
  uint16 *m = (uint16 *)p;
  asm volatile ("lhbrx %0,0,%1" : "=r" (x) : "r" (m));
#else
  uint8 *m = (uint8 *)p;
  x = m[0] + (m[1] << 8);
#endif
  return x;
}

static inline uint32 value24 (void *p)
{
  uint32 x;
#if defined(__GNUC__) && defined(__powerpc__)
  uint32 *m = (uint32 *)p;
  asm volatile ("lwbrx %0,0,%1" : "=r" (x) : "r" (m));
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
#if defined(__GNUC__) && defined(__powerpc__)
  uint32 *m = (uint32 *)p;
  asm volatile ("lwbrx %0,0,%1" : "=r" (x) : "r" (m));
#else
  uint8 *m = (uint8 *)p;
  x = m[0] + (m[1] << 8) + (m[2] << 16) + (m[3] << 24);
#endif
  return x;
}

static inline uint64 value64 (void *p)
{
  uint64 x;
#if defined(__GNUC__) && defined(__powerpc64__)
  uint64 *m = (uint64 *)p;
  asm volatile ("ldbrx %0,0,%1" : "=r" (x) : "r" (m));
#else
  uint32 *m = (uint32 *)p;
  x = value32(m) + ((uint64)value32(m + 1) << 32);
#endif
  return x;
}

static inline void setvalue16 (void *p, uint16 x)
{
#if defined(__GNUC__) && defined(__powerpc__)
  uint16 *m = (uint16 *)p;
  asm volatile ("sthbrx %1,0,%2" : "=m" (m) : "r" (x), "r" (m));
#else
  uint8 *m = (uint8 *)p;
  m[0] = x;
  m[1] = x >> 8;
#endif
}

static inline void setvalue32 (void *p, uint32 x)
{
#if defined(__GNUC__) && defined(__powerpc__)
  uint32 *m = (uint32 *)p;
  asm volatile ("stwbrx %1,0,%2" : "=m" (m) : "r" (x), "r" (m));
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
#if defined(__GNUC__) && defined(__powerpc64__)
  uint64 *m = (uint64 *)p;
  asm("stdbrx %1,0,%2" : "=m" (m) : "r" (x), "r" (m));
#else
  uint32 *m = (uint32 *)p;
  setvalue32(m, x);
  setvalue32(m + 1, x >> 32);
#endif
}

#endif //FREEARC_MOTOROLA_BYTE_ORDER

// Free memory block and set pointer to NULL
#ifndef FreeAndNil
#define FreeAndNil(p)            ((p) && (free(p), (p)=NULL))
#endif

// Переменные, используемые для сигнализации об ошибках из глубоко вложеных процедур
extern int jmpready;
extern jmp_buf jumper;

// Процедура сообщения о неожиданных ошибочных ситуациях
#ifndef CHECK
#  if defined(FREEARC_WIN) && defined(FREEARC_GUI)
#    define CHECK(a,b)           {if (!(a))  {if (jmpready) longjmp(jumper,1); char *s=(char*)malloc(MY_FILENAME_MAX*4); WCHAR *utf16=(WCHAR*) malloc(MY_FILENAME_MAX*4);  sprintf b;  utf8_to_utf16(s,utf16);  MessageBoxW(NULL, utf16, L"Error encountered", MB_ICONERROR);  exit(1);}}
#  elif defined(FREEARC_WIN)
#    define CHECK(a,b)           {if (!(a))  {if (jmpready) longjmp(jumper,1); char *s=(char*)malloc(MY_FILENAME_MAX*4), *oem=(char*)malloc(MY_FILENAME_MAX*4); sprintf b; utf8_to_oem(s,oem); printf("\n%s",oem); abort();}}
#  else
#    define CHECK(a,b)           {if (!(a))  {if (jmpready) longjmp(jumper,1); char s[MY_FILENAME_MAX*4]; sprintf b; printf("\n%s",s); abort();}}
#  endif
#endif

// Устанавливает Jump Point с кодом возврата retcode
#define SET_JMP_POINT(retcode)                                                         \
{                                                                                      \
  if (!jmpready && setjmp(jumper) != 0)                                                \
    /* Сюда мы попадём при возникновении ошибки в одной из вызываемых процедур */      \
    {jmpready = FALSE; return retcode;}                                                \
  jmpready = TRUE;                                                                     \
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

MemSize parseInt (char *param, int *error);      // Если строка param содержит целое число - возвратить его, иначе установить error=1
MemSize parseMem (char *param, int *error);      // Аналогично, только строка param может содержать суффиксы b/k/m/g/^, что означает соответствующие единицы памяти (по умолчанию - '^', т.е. степень двойки)
void showMem (MemSize mem, char *result);        // Возвращает текстовое описание объёма памяти
void strncopy (char *to, char *from, int len);   // Копирует строчку from в to, но не более len символов
int  split (char *str, char splitter, char **result, int result_size);  // Разбить строку str на подстроки, разделённые символом splitter
char*subst (char *original, char *from, char *to);  // Заменяет в строке original все вхождения from на to
char*trim_spaces (char *s);                      // Пропускает пробелы в начале строки и убирает их в конце, модифицируя строку
char *str_replace_n (char *orig, char *from, int how_many, char *to);   // Replace from:how_many substring and put result in new allocated area
char *str_replace   (char *orig, char *from, char *to);    // Replace substring and put result in new allocated area


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


// Whole part of number's binary logarithm (please ensure that n>0)
static inline MemSize lb (MemSize n)
{
  MemSize i;
  for (i=0; n>1; i++, n/=2);
  return i;
}

// Эта процедура округляет число к ближайшей сверху степени
// базы, например f(13,2)=16
static inline MemSize roundup_to_power_of (MemSize n, MemSize base)
{
    MemSize result;
    if (n==1)  return 1;
    for (result=base, n--; (n/=base) != 0; result *= base);
    return result;
}

// Эта процедура округляет число к ближайшей снизу степени
// базы, например f(13,2)=8
static inline MemSize rounddown_to_power_of (MemSize n, MemSize base)
{
    MemSize result;
    if (n==1)  return 1;
    for (result=1; (n/=base) != 0; result *= base);
    return result;
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

// Вывод заголовка окна
void EnvSetConsoleTitle (CFILENAME title);  // Установить заголовок консольного окна
void EnvResetConsoleTitle (void);  // Восстановить заголовок, который был в начале работы программы

#ifndef FREEARC_NO_TIMING
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
