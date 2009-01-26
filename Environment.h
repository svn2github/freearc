#include <time.h>
#include "Compression/Common.h"

#define PRESENT_INT32

#ifdef  __cplusplus
extern "C" {
#endif

#define INIT_CRC 0xffffffff

void SetFileDateTime (const CFILENAME Filename, time_t t); // Установить время/дату модификации файла
void FormatDateTime (char *buf, int bufsize, time_t t);    // Отформатировать время/дату для команды листинга
CFILENAME GetExeName (CFILENAME buf, int bufsize);         // Вернуть имя исполняемого файла программы
unsigned GetPhysicalMemory (void);                         // Объём физической памяти компьютера
unsigned GetMaxMemToAlloc (void);                          // Макс. объём памяти который мы можем выделить в адресном пространстве нашего процесса
unsigned GetAvailablePhysicalMemory (void);                // Объём свободной физической памяти компьютера
void TestMalloc (void);                                    // Печатает статистику свободной памяти
int GetProcessorsCount (void);                             // Общее количество процессоров (точнее, физических ядер) в системе. Используется для определения того, сколько "тяжёлых" вычислительных потоков целесообразно запустить в программе
uint UpdateCRC (void *Addr, uint Size, uint StartCRC);     // Обновить CRC содержимым блока данных
uint CalcCRC (void *Addr, uint Size);                      // Вычислить CRC блока данных
void memxor (char *dest, char *src, uint size);            // От-xor-ить два блока данных
int systemRandomData (char *rand_buf, int rand_size);
void BuildPathTo (CFILENAME name);                         // Создать каталоги на пути к name

#ifdef  __cplusplus
}
#endif
