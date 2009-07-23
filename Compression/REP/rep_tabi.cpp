#include "../NewCompression.h"

#include "rep.cpp"

// Реализация стандартного интерфейса методов сжатия COMPRESSION_METHOD
struct REP_METHOD : COMPRESSION_METHOD
{
  // Параметры этого метода сжатия
  MemSize BlockSize;        // Размер буфера. Совпадения ищутся только в пределах этой дистанции. Расход памяти - BlockSize+BlockSize/4
  int     MinCompression;   // Минимальный процент сжатия. Если выходные данные больше, то вместо них будут записаны оригинальные (несжатые) данные
  int     MinMatchLen;      // Минимальная длина строки, при которой она будет заменяться ссылкой на предыдущее вхождение
  int     Barrier;          // Граница, после которой допускается использовать совпадения меньшего размера (поскольку lzma/ppmd всё равно пропустит их)
  int     SmallestLen;      // Этот меньший размер
  int     HashSizeLog;      // Логарифм размера хеша (в 4-байтовых словах). Большие значения увеличивают сжатие, но замедляют его. При нулевом значении оптимальный размер вычисляется автоматически
  int     Amplifier;        // Коэффициент "усиления" поиска

  // Конструктор, присваивающий параметрам метода сжатия значения по умолчанию
  REP_METHOD();
//  rep_method (TABI_ELEMENT* params) : p(params) {};

  // Функции распаковки и упаковки
  virtual int decompress (CALLBACK_FUNC *callback, void *auxdata);
#ifndef FREEARC_DECOMPRESS_ONLY
  virtual int compress   (CALLBACK_FUNC *callback, void *auxdata);

  // Записать в buf[MAX_METHOD_STRLEN] строку, описывающую метод сжатия и его параметры (функция, обратная к parse_REP)
  virtual void ShowCompressionMethod (char *buf);

  // Получить/установить объём памяти, используемой при упаковке/распаковке, размер словаря или размер блока
  virtual void    SetCompressionMem     (MemSize mem);
  virtual MemSize GetCompressionMem     (void);
  virtual void    SetDictionary         (MemSize dict) {BlockSize = dict;}
  virtual MemSize GetDictionary         (void)         {return BlockSize;}
  virtual void    SetDecompressionMem   (MemSize mem)  {BlockSize = mem;}
#endif
  virtual MemSize GetDecompressionMem   (void)         {return BlockSize;}
};

// Разборщик строки метода сжатия REP
COMPRESSION_METHOD* parse_REP (char** parameters);



int rep_server (char *service, TABI_ELEMENT* params)
{
  return rep_method(params).server(service);
}

int rep_register = RegisterCompressionMethod(rep_server);

