// C_LZMA.cpp - интерфейс FreeArc к алгоритму сжатия LZMA

#ifdef WIN32
#include <windows.h>
#include <initguid.h>
#else
#define INITGUID
#endif

// Enable multithreading support
#define COMPRESS_MF_MT

extern "C" {
#include "C_LZMA.h"
}

// Количество байт, записываемых за раз в LZMA Decode
#define LZ_CHUNKS (8<<20)

enum
{
  kBT2,
  kBT3,
  kBT4,
  kHC4,
  kHT4
};

static const char *kMatchFinderIDs[] =
{
  "BT2",
  "BT3",
  "BT4",
  "HC4",
  "HT4"
};

static int FindMatchFinder(const char *s)
{
  for (int m = 0; m < (int)(sizeof(kMatchFinderIDs) / sizeof(kMatchFinderIDs[0])); m++)
    if (!strcasecmp(kMatchFinderIDs[m], s))
      return m;
  return -1;
}

// Включим в один .o файл все необходимые подпрограммы
#include "C/LzmaDec.c"
#ifndef FREEARC_DECOMPRESS_ONLY
#include "C/LzmaEnc.c"
#include "C/LzFind.c"
#ifdef COMPRESS_MF_MT
#include "C/LzFindMt.c"
#endif
#endif

static void *SzAlloc(void *p, size_t size) { p = p; return MyAlloc(size); }
static void SzFree(void *p, void *address) { p = p; MyFree(address); }
static ISzAlloc allocMain[1] = { SzAlloc, SzFree };

SRes LzmaDec_AllocateUsingProperties(CLzmaDec *p, const CLzmaProps propNew, ISzAlloc *alloc)
{
  SizeT dicBufSize;
  RINOK(LzmaDec_AllocateProbs2(p, &propNew, alloc));
  dicBufSize = propNew.dicSize;
  if (p->dic == 0 || dicBufSize != p->dicBufSize)
  {
    LzmaDec_FreeDict(p, alloc);
    p->dic = (Byte *)alloc->Alloc(alloc, dicBufSize);
    if (p->dic == 0)
    {
      LzmaDec_FreeProbs(p, alloc);
      return SZ_ERROR_MEM;
    }
  }
  p->dicBufSize = dicBufSize;
  p->prop = propNew;
  return SZ_OK;
}

// Input buffer size
uint RangeDecoderBufferSize (uint dict)
{return compress_all_at_once? dict : LARGE_BUFFER_SIZE;}




#ifndef FREEARC_DECOMPRESS_ONLY

int lzma_compress  ( int dictionarySize,
                     int hashSize,
                     int algorithm,
                     int numFastBytes,
                     int matchFinder,
                     int matchFinderCycles,
                     int posStateBits,
                     int litContextBits,
                     int litPosBits,
                     CALLBACK_FUNC *callback,
                     void *auxdata )
{
  CallbackInStream  inStream  (callback, auxdata);
  CallbackOutStream outStream (callback, auxdata);
  NCompress::NLZMA::CEncoder* encoder = new NCompress::NLZMA::CEncoder;

  bool eos = (1==1);  // use End-Of-Stream marker because we don't know filesize apriori

  if (encoder->SetupProperties (dictionarySize,
                                hashSize,
                                posStateBits,
                                litContextBits,
                                litPosBits,
                                algorithm,
                                numFastBytes,
                                matchFinder,
                                matchFinderCycles,
                                GetCompressionThreads() > 1,
                                eos) != S_OK)
  {
    delete encoder;
    return FREEARC_ERRCODE_INVALID_COMPRESSOR;
  }

  HRESULT result = encoder->Code (&inStream, &outStream, 0, 0, 0);
  delete encoder;
  if (inStream.errcode)
    return inStream.errcode;
  if (outStream.errcode)
    return outStream.errcode;
  if (result == E_OUTOFMEMORY)
  {
    return FREEARC_ERRCODE_NOT_ENOUGH_MEMORY;
  }
  else if (result != S_OK)
  {
    //fprintf(stderr, "\nEncoder error = %X\n", (unsigned int)result);
    return FREEARC_ERRCODE_GENERAL;
  }
  return 0;
}

#endif  // !defined (FREEARC_DECOMPRESS_ONLY)

int lzma_decompress( int dictionarySize,
                     int hashSize,
                     int algorithm,
                     int numFastBytes,
                     int matchFinder,
                     int matchFinderCycles,
                     int posStateBits,
                     int litContextBits,
                     int litPosBits,
                     CALLBACK_FUNC *callback,
                     void *auxdata )
{
  int errcode = FREEARC_OK;
  bool first_read = TRUE;

  UInt32 _inPos = 0, _inSize = 0, _inBufferSize = RangeDecoderBufferSize(dictionarySize);
  Byte  *_inBuf = (Byte*) MyAlloc(_inBufferSize);
  if (_inBuf == NULL)  return FREEARC_ERRCODE_NOT_ENOUGH_MEMORY;

  CLzmaProps LzmaProps;
  LzmaProps.pb = posStateBits;
  LzmaProps.lc = litContextBits;
  LzmaProps.lp = litPosBits;
  LzmaProps.dicSize = dictionarySize;

  CLzmaDec _state;
  LzmaDec_Construct(&_state);
  RINOK(LzmaDec_AllocateUsingProperties(&_state, LzmaProps, allocMain));    ////тестировать res
  LzmaDec_Init(&_state);

  for (;;)
  {
    if (_inPos == _inSize)
    {
      // Read whole buffer in the first call if compress_all_at_once==TRUE
      _inSize = errcode = first_read || !compress_all_at_once? callback ("read", _inBuf, _inBufferSize, auxdata) : 0;
      if (errcode < 0)  break;
      _inPos = 0;              // current position inside input data
      first_read = FALSE;
    }

    SizeT dicPos = _state.dicPos;
    SizeT curSize = mymin(_state.dicBufSize - dicPos, HUGE_BUFFER_SIZE);     // Write outdata in 8mb chunks

    ELzmaFinishMode finishMode = LZMA_FINISH_ANY;
    SizeT inSizeProcessed = _inSize - _inPos;
    ELzmaStatus status;
    SRes res = LzmaDec_DecodeToDic(&_state, dicPos + curSize, _inBuf + _inPos, &inSizeProcessed, finishMode, &status);

    _inPos += (UInt32)inSizeProcessed;
    SizeT outSizeProcessed = _state.dicPos - dicPos;

    bool finished = (inSizeProcessed == 0 && outSizeProcessed == 0);

    if (res != 0 || _state.dicPos == _state.dicBufSize || finished)
    {
      errcode = callback ("write", _state.dic, _state.dicPos, auxdata);
      if (res != 0)     {errcode = FREEARC_ERRCODE_BAD_COMPRESSED_DATA; break;}
      if (errcode < 0)  break;
      if (finished)     {errcode = (status == LZMA_STATUS_FINISHED_WITH_MARK ? S_OK : S_FALSE); break;}
    }
    if (_state.dicPos == _state.dicBufSize)
      _state.dicPos = 0;
  }

  LzmaDec_Free(&_state, allocMain);
  MyFree(_inBuf);
  return errcode;
}


/*-------------------------------------------------*/
/* Реализация класса LZMA_METHOD                  */
/*-------------------------------------------------*/

// Если строка str начинается со start, то возвратить адрес остатка, иначе - NULL
char* start_from (char* str, char* start)
{
  while (*start && *str==*start)  str++, start++;
  return *start? NULL : str;
}

// Конструктор, присваивающий параметрам метода сжатия значения по умолчанию
LZMA_METHOD::LZMA_METHOD()
{
  dictionarySize    = 64*mb;
  hashSize          = 0;
  algorithm         = 1;
  numFastBytes      = 32;
  matchFinder       = kHT4;
  matchFinderCycles = 0;    // библиотека LZMA определит количество циклов автоматически, исходя из matchFinder и numFastBytes
  posStateBits      = 2;
  litContextBits    = 3;
  litPosBits        = 0;
}

// Функция распаковки
int LZMA_METHOD::decompress (CALLBACK_FUNC *callback, void *auxdata)
{
  // Use faster function from DLL if possible
  static FARPROC f = LoadFromDLL ("lzma_decompress");
  if (!f) f = (FARPROC) lzma_decompress;

  return ((int (*)(int, int, int, int, int, int, int, int, int, CALLBACK_FUNC*, void*)) f)
                         (dictionarySize,
                          hashSize,
                          algorithm,
                          numFastBytes,
                          matchFinder,
                          matchFinderCycles,
                          posStateBits,
                          litContextBits,
                          litPosBits,
                          callback,
                          auxdata);
}

#ifndef FREEARC_DECOMPRESS_ONLY

// Функция упаковки
int LZMA_METHOD::compress (CALLBACK_FUNC *callback, void *auxdata)
{
  // Use faster function from DLL if possible
  static FARPROC f = LoadFromDLL ("lzma_compress");
  if (!f) f = (FARPROC) lzma_compress;

  SetDictionary (dictionarySize);   // Ограничим размер словаря чтобы сжатие влезало в 4гб памяти :)
  // Если LZMA будет использовать multithreading matchfinder,
  // то нет смысла считать время работы по основному треду - вместо этого
  // следует использовать wall clock time всего процесса упаковки
  if ((algorithm || matchFinder!=kHC4) && GetCompressionThreads()>1)
      addtime = -1;   // это сигнал на использование wall clock time
  return ((int (*)(int, int, int, int, int, int, int, int, int, CALLBACK_FUNC*, void*)) f)
                         (dictionarySize,
                          hashSize,
                          algorithm,
                          numFastBytes,
                          matchFinder,
                          matchFinderCycles,
                          posStateBits,
                          litContextBits,
                          litPosBits,
                          callback,
                          auxdata);
}


// Записать в buf[MAX_METHOD_STRLEN] строку, описывающую метод сжатия и его параметры (функция, обратная к parse_LZMA)
void LZMA_METHOD::ShowCompressionMethod (char *buf)
{
  char DictionaryStr[100], HashStr[100], fcStr[100], pbStr[100], lcStr[100], lpStr[100], algStr[100];
  showMem (dictionarySize, DictionaryStr);
  showMem (hashSize, HashStr);
  LZMA_METHOD defaults;
  sprintf (fcStr, matchFinderCycles!=defaults.matchFinderCycles? ":mc%d" : "", matchFinderCycles);
  sprintf (pbStr, posStateBits     !=defaults.posStateBits     ? ":pb%d" : "", posStateBits);
  sprintf (lcStr, litContextBits   !=defaults.litContextBits   ? ":lc%d" : "", litContextBits);
  sprintf (lpStr, litPosBits       !=defaults.litPosBits       ? ":lp%d" : "", litPosBits);
  matchFinder==kHT4? (algorithm==2? sprintf (algStr, "a%d", algorithm)
                                  : sprintf (algStr, algorithm==0? "fast" : "normal"))
                   : sprintf (algStr, "%s:%s", algorithm==0? "fast": algorithm==1? "normal": "max", kMatchFinderIDs [matchFinder]);
  for (char *p=algStr; *p; p++)   *p = tolower(*p);  // strlwr(algStr);
  sprintf (buf, "lzma:%s%s%s:%s:%d%s%s%s%s",
                      DictionaryStr,
                      hashSize? ":h" : "",
                      hashSize? HashStr : "",
                      algStr,
                      numFastBytes,
                      fcStr,
                      pbStr,
                      lcStr,
                      lpStr);
  //printf("\n%s\n",buf);
}

// Посчитать, сколько памяти требуется для упаковки заданным методом
MemSize LZMA_METHOD::GetCompressionMem (void)
{
  SetDictionary (dictionarySize);   // Ограничим размер словаря чтобы сжатие влезало в 4гб памяти :)
  switch (matchFinder) {
    case kBT2:    return NBT2::CalcHashSize(dictionarySize,hashSize)*sizeof(NBT2::CIndex) + dictionarySize*9 + NBT2::ReservedAreaSize(dictionarySize) + 256*kb + RangeEncoderBufferSize(dictionarySize);
    case kBT3:    return NBT3::CalcHashSize(dictionarySize,hashSize)*sizeof(NBT3::CIndex) + dictionarySize*9 + NBT3::ReservedAreaSize(dictionarySize) + 256*kb + RangeEncoderBufferSize(dictionarySize);
    case kBT4:    return NBT4::CalcHashSize(dictionarySize,hashSize)*sizeof(NBT4::CIndex) + dictionarySize*9 + NBT4::ReservedAreaSize(dictionarySize) + 256*kb + RangeEncoderBufferSize(dictionarySize);
    case kHC4:    return NHC4::CalcHashSize(dictionarySize,hashSize)*sizeof(NHC4::CIndex) + dictionarySize*5 + NHC4::ReservedAreaSize(dictionarySize) + 256*kb + RangeEncoderBufferSize(dictionarySize);
    case kHT4:    return NHT4::CalcHashSize(dictionarySize,hashSize)*sizeof(NHT4::CIndex) + dictionarySize   + NHT4::ReservedAreaSize(dictionarySize) + 256*kb + RangeEncoderBufferSize(dictionarySize);
    default:      CHECK (FALSE, (s,"lzma::GetCompressionMem - unknown matchFinder %d", matchFinder));
  }
}

MemSize calcDictSize (LZMA_METHOD *p, MemSize mem)
{
  double mem4 = mymax (double(mem)-256*kb, 0);
  switch (p->matchFinder) {
    case kBT2:    return (MemSize)floor(mem4/9.5);
    case kBT3:    return (MemSize)floor(mem4/11.5);
    case kBT4:    return (MemSize)floor(mem4/11.5);
    case kHC4:    return (MemSize)floor(mem4/7.5);
    case kHT4:    return (MemSize)floor(mem4/1.75);
    default:      CHECK (FALSE, (s,"lzma::calcDictSize - unknown matchFinder %d", p->matchFinder));
  }
}

// Ограничить использование памяти при упаковке
void LZMA_METHOD::SetCompressionMem (MemSize mem)
{
  if (mem<=0)  return;
  hashSize       = 0;
  dictionarySize = calcDictSize (this, mem);

  if (dictionarySize<32*kb) {

  // Если словарь получился слишком маленьким - переключимся на самый простой алгоритм сжатия.
  //   (на настоящий момент, при одинаковых минимимальных требованиях к памяти
  //     у всех оставшихся алгоритмов, это не имеет никакого смысла):
  //  if (matchFinder!=HC4)
  //    matchFinder = HC4,  SetCompressionMem (mem);
  //  else
      dictionarySize = 32*kb;
  }
  // Округлим словарь вниз до 32 kb, 48 kb, 64 kb...
  uint t = 1 << lb(dictionarySize);
  if (t/2*3 <= dictionarySize)
        dictionarySize = t/2*3;
  else  dictionarySize = t;
}

// Ограничить использование памяти при распаковке
void LZMA_METHOD::SetDecompressionMem (MemSize mem)
{
  if (mem<=0)  return;
  dictionarySize = mem;
  // Округлим словарь вниз до 32 kb, 48 kb, 64 kb...
  uint t = 1 << lb(dictionarySize);
  if (t/2*3 <= dictionarySize)
        dictionarySize = t/2*3;
  else  dictionarySize = t;
}

// Установить размер словаря.
void LZMA_METHOD::SetDictionary (MemSize mem)
{
  if (mem<=0)  return;
  dictionarySize = mymin (mem, roundDown (calcDictSize (this, UINT_MAX), mb));
}

#endif  // !defined (FREEARC_DECOMPRESS_ONLY)

MemSize LZMA_METHOD::GetDecompressionMem (void)
{
  return dictionarySize + RangeDecoderBufferSize(dictionarySize);
}

// Конструирует объект типа LZMA_METHOD с заданными параметрами упаковки
// или возвращает NULL, если это другой метод сжатия или допущена ошибка в параметрах
COMPRESSION_METHOD* parse_LZMA (char** parameters)
{
  if (strcmp (parameters[0], "lzma") == 0) {
    // Если название метода (нулевой параметр) - "lzma", то разберём остальные параметры

    LZMA_METHOD *p = new LZMA_METHOD;
    p->matchFinder = INT_MAX;
    int error = 0;  // Признак того, что при разборе параметров произошла ошибка

    // Переберём все параметры метода (или выйдем раньше при возникновении ошибки при разборе очередного параметра)
    while (*++parameters) {
      char *param = *parameters;  bool optional = param[0]=='*';  optional && param++;
           if (start_from (param, "d"))    p->dictionarySize    = parseMem (param+1, &error);
      else if (start_from (param, "h"))   {MemSize h            = parseMem (param+1, &error);
                                           if (error)  goto unnamed;  else p->hashSize = h;}
      else if (start_from (param, "a"))    p->algorithm         = parseInt (param+1, &error);
      else if (start_from (param, "fb"))   p->numFastBytes      = parseInt (param+2, &error);
      else if (start_from (param, "mc"))   p->matchFinderCycles = parseInt (param+2, &error);
      else if (start_from (param, "lc"))   p->litContextBits    = parseInt (param+2, &error);
      else if (start_from (param, "lp"))   p->litPosBits        = parseInt (param+2, &error);
      else if (start_from (param, "pb"))   p->posStateBits      = parseInt (param+2, &error);
      else if (start_from (param, "mf"))   p->matchFinder       = FindMatchFinder (param[2]=='='? param+3 : param+2);
      else if (strequ (param, "fastest"))  p->algorithm = 0,  p->matchFinder==INT_MAX && (p->matchFinder = kHT4),  p->numFastBytes = 32,   p->matchFinderCycles = 1;
      else if (strequ (param, "fast"))     p->algorithm = 0,  p->matchFinder==INT_MAX && (p->matchFinder = kHT4),  p->numFastBytes = 32,   p->matchFinderCycles = 0;
      else if (strequ (param, "normal"))   p->algorithm = 1,  p->matchFinder==INT_MAX && (p->matchFinder = kHT4),  p->numFastBytes = 32,   p->matchFinderCycles = 0;
      else if (strequ (param, "max"))      p->algorithm = 2,  p->matchFinder==INT_MAX && (p->matchFinder = kBT4),  p->numFastBytes = 128,  p->matchFinderCycles = 0;
      else if (strequ (param, "ultra"))    p->algorithm = 2,  p->matchFinder==INT_MAX && (p->matchFinder = kBT4),  p->numFastBytes = 128,  p->matchFinderCycles = 128;
      else {
unnamed:// Сюда мы попадаем, если в параметре опущено его наименование
        // Это может быть строка - имя MatchFinder'а, целое число - значение numFastBytes,
        // или обозначение памяти - значение dictionarySize
        error = 0;
        int n = FindMatchFinder (param);
        if (n>=0)
          p->matchFinder = n;
        else {
          n = parseInt (param, &error);
          if (!error)  p->numFastBytes = n;
          else         {error=0; MemSize n = parseMem (param, &error);
                        if (!error)  p->dictionarySize = n;}
        }
      }
      if (!optional)
        if (error || p->matchFinder<0)  {delete p; return NULL;}  // Ошибка при парсинге параметров метода
    }
    if (p->matchFinder == INT_MAX)
      p->matchFinder = kHT4;   // default match finder
    return p;
  } else
    return NULL;   // Это не метод lzma
}

static int LZMA_x = AddCompressionMethod (parse_LZMA);   // Зарегистрируем парсер метода LZMA
