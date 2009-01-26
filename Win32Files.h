#include <HsBase.h>
#include <mingw/io.h>
#include <mingw/wchar.h>
#include <mingw/sys/stat.h>

INLINE HsInt    __w_find_sizeof       ( void ) { return sizeof(struct _wfinddatai64_t); }
INLINE unsigned __w_find_attrib       ( struct _wfinddatai64_t* st ) { return st->attrib;      }
INLINE time_t   __w_find_time_create  ( struct _wfinddatai64_t* st ) { return st->time_create; }
INLINE time_t   __w_find_time_access  ( struct _wfinddatai64_t* st ) { return st->time_access; }
INLINE time_t   __w_find_time_write   ( struct _wfinddatai64_t* st ) { return st->time_write;  }
INLINE __int64  __w_find_size         ( struct _wfinddatai64_t* st ) { return st->size;        }
INLINE wchar_t* __w_find_name         ( struct _wfinddatai64_t* st ) { return st->name;        }

INLINE HsInt          __w_stat_sizeof ( void ) { return sizeof(struct _stati64); }
INLINE unsigned short __w_stat_mode   ( struct _stati64* st ) { return st->st_mode;  }
INLINE time_t         __w_stat_ctime  ( struct _stati64* st ) { return st->st_ctime; }
INLINE time_t         __w_stat_atime  ( struct _stati64* st ) { return st->st_atime; }
INLINE time_t         __w_stat_mtime  ( struct _stati64* st ) { return st->st_mtime; }
INLINE __int64        __w_stat_size   ( struct _stati64* st ) { return st->st_size;  }
