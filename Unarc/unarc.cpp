// to do: отбор файлов по именам ("name" или "dir/name"),
//        дешифрование данных/заголовка
//        добавление ".arc", listfiles/-ap/-kb
#include <stdlib.h>
#include <stdio.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <string.h>
#include <wchar.h>

// SFX module is just unarc.cpp compiled with FREEARC_SFX defined
#ifdef FREEARC_SFX
#define NAME           "SFX"
#else
#define NAME           "unpacker"
#endif

#define HEADER1        "FreeArc 0.51 "
#define HEADER2        "  http://freearc.org  2009-04-28\n"

// Доступ к структуре архива
#include "ArcStructure.h"

// Весь диалог с пользователем описан в сменных модулях, включаемых здесь
#ifdef FREEARC_GUI
#include "gui\gui.h"
#include "gui\gui.cpp"
#else
#include "CUI.h"
#endif
UI UI;


/******************************************************************************
** Информация о выполняемой деархиватором команде *****************************
******************************************************************************/
class COMMAND
{
public:
  char cmd;             // Выполняемая команда
  FILENAME arcname;     // Имя обрабатываемого командой архива
  FILENAME *filenames;  // Имена обрабатываемых командой файлов из архива
  FILENAME outpath;     // Опция -dp
  FILENAME runme;       // Файл, запускаемый после распаковки
  BOOL wipeoutdir;      // Удалить файлы из outpath после завершения работы runme?
  BOOL ok;              // Команда выполняется успешно?
  int  silent;          // Опция -s
  BOOL yes;             // Опция -o+
  BOOL no;              // Опция -o-
  BOOL noarcext;        // Опция --noarcext
  BOOL nooptions;       // Опция --

  bool list_cmd()  {return cmd=='l' || cmd=='v';}   // True, если это команда получения листинга архива

  // Разбор командной строки
  COMMAND (int argc, char *argv[])
  {
#ifdef FREEARC_WIN
    // Instead of those ANSI-codepage encoded argv[] strings provide true UTF-8 data!
    WCHAR **argv_w = CommandLineToArgvW (GetCommandLineW(), &argc);
    argv = (char**) malloc ((argc+1) * sizeof(*argv));
    for (int i=0; i<argc; i++)
    {
      argv[i] = (char*) malloc (_tcslen (argv_w[i]) * 4 + 1);
      utf16_to_utf8 (argv_w[i], argv[i]);
      argv[i] = (char*) realloc (argv[i], strlen(argv[i]) + 1);
    }
    argv[argc] = NULL;
#endif
    // Default options
    noarcext  = FALSE;
    nooptions = FALSE;
    outpath = "";
    runme = NULL;
    wipeoutdir = FALSE;
    yes = FALSE;
    no  = FALSE;
    silent = 0;
#ifdef FREEARC_SFX
    arcname = argv[0];
    cmd     = 'x';

#ifdef FREEARC_INSTALLER
    // Installer by default extracts itself into some temp directory, runs setup.exe and then remove directory's contents
    if (argv[1] == NULL)
    {
        silent = 2;

        // Get TEMP path and convert it into UTF-8
        CFILENAME TempPathW = (TCHAR*)   malloc (MY_FILENAME_MAX * 4);
         FILENAME TempPath  = (FILENAME) malloc (MY_FILENAME_MAX * 4);
        GetTempPathW(MY_FILENAME_MAX, TempPathW);
        utf16_to_utf8 (TempPathW, TempPath);

        // Create unique tempdir
        outpath = (FILENAME) malloc (MY_FILENAME_MAX * 4);
        for (unsigned i = (unsigned) GetTickCount(); ; )
        {
            i = i*54322457 + 137;
            sprintf (outpath, "%s%s%u", TempPath, "installer", i);
            utf8_to_utf16 (outpath, TempPathW);
            if (_wmkdir(TempPathW) == 0)   break;  // Break on success
        }
        free(TempPathW);

        // Run setup.exe from this dir
        runme   = (FILENAME) malloc (MY_FILENAME_MAX * 4);
        sprintf (runme, "%s%s%s", outpath, STR_PATH_DELIMITER, "setup.exe");

        // Delete extracted files afterwards
        wipeoutdir = TRUE;
    }
#endif

    // Parse options
    for (ok=TRUE; ok && *++argv; )
    {
      if (argv[0][0]=='-' || strequ(argv[0],"/?") || strequ(argv[0],"/help"))
      {
             if (strequ(argv[0],"-l"))       cmd = 'l', silent = silent || 2;
        else if (strequ(argv[0],"-v"))       cmd = 'v', silent = silent || 2;
        else if (strequ(argv[0],"-e"))       cmd = 'e', silent = silent || 2;
        else if (strequ(argv[0],"-x"))       cmd = 'x', silent = silent || 2;
        else if (strequ(argv[0],"-t"))       cmd = 't', silent = silent || 2;
        else if (strequ(argv[0],"-y"))       yes = TRUE;
        else if (strequ(argv[0],"-n"))       no  = TRUE;
        else if (start_with(argv[0],"-d"))   outpath = argv[0]+2;
        else if (strequ(argv[0],"-s"))       silent = 1;
        else if (strequ(argv[0],"-s0"))      silent = 0;
        else if (strequ(argv[0],"-s1"))      silent = 1;
        else if (strequ(argv[0],"-s2"))      silent = 2;
        else if (strequ(argv[0],"--"))       nooptions=TRUE;
        else ok=FALSE;
      }
      else break;
    }

    filenames = argv;            // the rest of arguments are filenames
    if (ok)  return;

    // Display help
    char *helpMsg = (char*) malloc(1000+strlen(arcname));
    sprintf (helpMsg,
#ifdef FREEARC_GUI
           HEADER1 NAME HEADER2
#else
           HEADER2
#endif
           "Usage: %s [options] [filenames...]\n"
           "Available options:\n"
#ifndef FREEARC_GUI
           "  -l       - display archive listing\n"
           "  -v       - display verbose archive listing\n"
#endif
           "  -x       - extract files\n"
           "  -e       - extract files without pathnames\n"
           "  -t       - test archive integrity\n"
           "  -d{Path} - set destination path\n"
           "  -y       - answer Yes on all overwrite queries\n"
           "  -n       - answer No  on all overwrite queries\n"
           "  -s[1,2]  - silent mode\n"
           "  --       - no more options\n"
           , drop_dirname(arcname));
#ifdef FREEARC_GUI
    MessageBoxW (NULL, MYFILE(helpMsg).displayname(), _T("Command-line help"), MB_OK | MB_ICONERROR);
#else
    printf("%s", MYFILE(helpMsg).displayname());
#endif

#else
    cmd     = ' ';
    arcname = NULL;
    for (ok=TRUE; ok && *++argv; )
    {
      if (argv[0][0]=='-')
      {
        if (strequ(argv[0],"--noarcext"))    noarcext =TRUE;
        else if (strequ(argv[0],"-o+"))      yes      =TRUE;
        else if (strequ(argv[0],"-o-"))      no       =TRUE;
        else if (start_with(argv[0],"-dp"))  outpath = argv[0]+3;
        else if (strequ(argv[0],"--"))       nooptions=TRUE;
        else ok=FALSE;
      }
      else if (cmd==' ')   cmd = argv[0][0], ok = ok && strlen(argv[0])==1;
      else if (!arcname)   arcname = argv[0];
      else break;
    }

    filenames = argv;            // the rest of arguments are filenames
    ok = ok && strchr("lvtex",cmd) && arcname;
    if (ok)  return;
    printf(HEADER2
           "Usage: unarc command [options] archive[.arc] [filenames...]\n"
           "Available commands:\n"
           "  l - display archive listing\n"
           "  v - display verbose archive listing\n"
           "  e - extract files into current directory\n"
           "  x - extract files with pathnames\n"
           "  t - test archive integrity\n"
           "Available options:\n"
           "  -dp{Path}   - set destination path\n"
           "  -o+         - overwrite existing files\n"
           "  -o-         - don't overwrite existing files\n"
           "  --noarcext  - don't add default extension to archive name\n"
           "  --          - no more options\n");
#endif
  }

  // TRUE, если i-й файл каталога dirblock следует включить в обработку
  BOOL accept_file (DIRECTORY_BLOCK *dirblock, int i)
  {
    if (!*filenames)  return TRUE;            // В командной строке не указано ни одного имени файла - значит, нужно обрабатывать любой файл
    for (FILENAME *f=filenames; *f; f++) {
      if (strequ (dirblock->name[i], *f))
        return TRUE;                          // О! Совпало!
    }
    return FALSE;                             // Совпадающего имени не найдено
  }
};


/******************************************************************************
** Реализация команды получения листинга архива *******************************
******************************************************************************/
#ifdef FREEARC_GUI
void ListHeader (COMMAND &) {}
void ListFooter (COMMAND &) {}
void ListFiles (DIRECTORY_BLOCK *, COMMAND &) {}
#else

uint64 total_files, total_bytes, total_packed;

void ListHeader (COMMAND &command)
{
  if (command.cmd=='l')
      printf ("Date/time                  Size Filename\n"
              "----------------------------------------\n");
  else
      printf ("Date/time              Attr            Size          Packed      CRC Filename\n"
              "-----------------------------------------------------------------------------\n");
  total_files=total_bytes=total_packed=0;
}

void ListFooter (COMMAND &command)
{
  if (command.cmd=='l')
      printf ("----------------------------------------\n");
  else
      printf ("-----------------------------------------------------------------------------\n");
  printf ("%.0lf files, %.0lf bytes, %.0lf compressed", double(total_files), double(total_bytes), double(total_packed));
}

void ListFiles (DIRECTORY_BLOCK *dirblock, COMMAND &command)
{
  int  b=0;                // current_data_block
  bool Encrypted = FALSE;  // текущий солид-блок зашифрован?
  uint64 packed=0;
  iterate_var (i, dirblock->total_files) {
    // Увеличим номер солид-блока если мы вышли за последний принадлежащий ему файл
    if (i >= dirblock->block_end(b))
      b++;
    // Если это первый файл в солид-блоке - соберём block-related информацию
    if (i == dirblock->block_start(b))
    { // Запишем на первый файл в блоке весь его упакованный размер
      packed = dirblock->data_block[b].compsize;
      // Запомним информацию о солид-блоке для использования её со всеми файлами из этого солид-блока
      char *c = dirblock->data_block[b].compressor;
      Encrypted = strstr (c, "+aes-")!=NULL || strstr (c, "+serpent-")!=NULL || strstr (c, "+blowfish-")!=NULL || strstr (c, "+twofish-")!=NULL;
    }


    if (command.accept_file (dirblock, i)) { //   Если этот файл требуется обработать
      unsigned long long filesize = dirblock->size[i];
      char timestr[100];  FormatDateTime (timestr, 100, dirblock->time[i]);

      if (command.cmd=='l')
          printf (dirblock->isdir[i]? "%s       -dir-" : "%s %11.0lf", timestr, double(filesize));
      else
          printf ("%s %s %15.0lf %15.0lf %08x", timestr, dirblock->isdir[i]? ".D.....":".......", double(filesize), double(packed), dirblock->crc[i]);
      printf ("%c", Encrypted? '*':' ');

      // Print filename using console encoding
      static char filename[MY_FILENAME_MAX*4];
      dirblock->fullname (i, filename);
      static MYFILE file;  file.setname (filename);
      printf ("%s\n", file.displayname());

      total_files++;
      total_bytes  += filesize;
      total_packed += packed;    packed = 0;
    }
  }
}
#endif

/******************************************************************************
** Реализация команд распаковки и тестирования архивов ************************
******************************************************************************/

// Переменные, отражающие состояние процесса чтения входных данных
MYFILE *infile;          // Файл архива, из которого идёт чтение
FILESIZE bytes_left;     // Кол-во байт, которое осталось прочитать до исчерпания упакованных данных этого солид-блока

// Переменные, отражающие состояние процесса записи распакованных данных
COMMAND *cmd;             // Выполняемая команда
DIRECTORY_BLOCK *dir;     // Каталог, которому принадлежат распаковываемые файлы
int curfile;              //   Номер в каталоге текущего распаковываемого файла
BOOL included;            //   Текущий файл включён в обработку или мы просто пропускаем его?
int extractUntil;         //   Номер последнего файла, который нужно извлечь из этого солид-блока
MYFILE outfile;           // Файл, извлекаемый из архива
char fullname[MY_FILENAME_MAX*4]; // Полное имя распаковываемого сейчас файла
FILESIZE bytes_to_write;  // Сколько байт в текущем файле осталось записать
FILESIZE writtenBytes;    // Сколько байт всего было распаковано в текущем архиве
FILESIZE archive_pos;     // Текущая позиция в архиве
CRC crc;                  // CRC данных, записанных в файл
enum PASS {FIRST_PASS, SECOND_PASS};  // Первый/второй проход по солид-блоку (первый - распаковка каталогов и пустых файлов, второй - всех остальных)

// Процедура экстренного выхода
void quit(void)   {if (outfile.isopen())  outfile.close(), delete_file(outfile.filename);
                   exit (FREEARC_EXIT_ERROR);}

// Действие при ошибке в CHECK()
#undef  ON_CHECK_FAIL
#define ON_CHECK_FAIL()   quit()

// * Нижеследующие процедуры предоставляют абстрактные средства работы с текущим выходным файлом,
// * скрывая такие детали, как различия команд e/x/t, различие между каталогами и файлами,
// * и то, что часть файлов может быть исключена из обработки

// Открыть очередной выходной файл и напечатать сообщение о его распаковке
void outfile_open (PASS pass)
{
  crc = INIT_CRC;
  bytes_to_write = dir->size[curfile];
  if (pass==SECOND_PASS && bytes_to_write==0)
    return;  // Directories and empty files were extracted in first pass
  included = cmd->accept_file (dir, curfile);
  char *xname = cmd->cmd=='e'? dir->name[curfile]
                             : dir->fullname (curfile, fullname);
  outfile.setname (xname);

  if (included && cmd->cmd!='t')
    if (dir->isdir[curfile])
      {if (cmd->cmd!='e')  BuildPathTo (outfile.filename), create_dir (outfile.filename);}
    else
      {if (outfile.exists())
       {
         if (cmd->no)  included = FALSE;
         else if (!cmd->yes)
         {
           char answer = UI.AskOverwrite (outfile.displayname(), dir->size[curfile], dir->time[curfile]);
           switch (answer)
           {
             case 'y': break;
             case 'n': included = FALSE;  break;
             case 'a': cmd->yes = TRUE;   break;
             case 's': cmd->no  = TRUE;   included = FALSE;  break;
             case 'q': quit();
           }
         }
       }
       if (included)  outfile.open (WRITE_MODE);}

  if (pass==FIRST_PASS || dir->size[curfile]>0)   // Не писать повторно о распаковке каталогов/пустых файлов
    if (!(dir->isdir[curfile] && cmd->cmd!='x'))  // Не сообщать о тестировании каталогов ;)
      if (!UI.ProgressFile (dir->isdir[curfile], included? (cmd->cmd=='t'? "Testing":"Extracting"):"Skipping", MYFILE(xname).displayname(), bytes_to_write))
        quit();
}

// Записать данные в выходной файл
void outfile_write (void *buf, int size)
{
  crc = UpdateCRC (buf, size, crc);
  if (included && cmd->cmd!='t' && size)
    outfile.write(buf,size);
  if (!UI.ProgressWrite (writtenBytes += size))  quit();
}

// Закрыть выходной файл
void outfile_close()
{
  if (included)
  {
    CHECK ((crc^INIT_CRC) == dir->crc[curfile], (s,"ERROR: file %s failed CRC check", outfile.utf8name));
    if (cmd->cmd!='t' && !dir->isdir[curfile])
      outfile.close();
      outfile.SetFileDateTime (dir->time[curfile]);
  }
  included = FALSE;
}

// Callback-функция чтения/записи для распаковщика
int callback_func (const char *what, void *buf, int size, void *auxdata)
{
  if (strequ (what, "read")) {
    int read_bytes = mymin (bytes_left, size);
    if (read_bytes==0)  return 0;
    if (!UI.ProgressRead (archive_pos))  quit();
    int len = infile->tryRead (buf, read_bytes);
    if (len>0)  bytes_left -= len,  archive_pos += len;
    return len;

  } else if (strequ (what, "write")) {
    int origsize = size;
    if (curfile > extractUntil)  return FREEARC_ERRCODE_NO_MORE_DATA_REQUIRED;   // Нам попался тупой распаковщик, не способный завершить распаковку по требованию :(
    while (size>0) {
      int n = mymin (bytes_to_write, size);   // Записываем сколько осталось до конца файла или
      outfile_write (buf,n);                  // сколько осталось данных в буфере - смотря что меньше
      bytes_to_write -= n;
      if (bytes_to_write==0) {                // Если файл записан до конца - перейдём к следующему
        outfile_close();
        if (++curfile > extractUntil)  return FREEARC_ERRCODE_NO_MORE_DATA_REQUIRED;   // Если все файлы, которые мы должны распаковать из этого блока, уже извлечены, то попросить распаковщик завершить распаковку
        outfile_open(SECOND_PASS);
      }
      buf=(uint8*)buf+n; size-=n;
    }
    return origsize;     // Сигнализировать успешную запись и попросить продолжить распаковку

  } else return FREEARC_ERRCODE_NOT_IMPLEMENTED;
}

// Распаковать или протестировать файлы из солид-блока с номером block_num каталога dirblock
void ExtractFiles (DIRECTORY_BLOCK *dirblock, int block_num, COMMAND &command)
{
  cmd = &command;
  dir = dirblock;
  BLOCK& data_block (dirblock->data_block [block_num]);
  extractUntil = -1;                        // В эту переменную будет записан номер последнего файла в солид-блоке, который нужно обработать
  // Переберём все файлы в этом блоке
  for (curfile = dirblock->block_start(block_num); curfile < dirblock->block_end(block_num); curfile++) {
    if (command.accept_file (dirblock, curfile))           // Если этот файл требуется обработать
    {
      if (dir->size[curfile]==0) {   // то если это каталог или пустой файл - сделаем это сразу
        outfile_open (FIRST_PASS);
        outfile_close(); }
      else
        extractUntil = curfile;      // а иначе - запомним, что нужно распаковать блок как минимум до этого файла
    }
  }
  if (extractUntil >= 0) {                       // Если в этом блоке нашлось что распаковывать - значит, распакуем! :)
    infile = &dirblock->arcfile;                 //   Архивный файл
    infile->seek (archive_pos = data_block.pos); //   Начало данных солид-блока в архиве
    bytes_left = data_block.compsize;            //   Размер упакованных данных в солид-блоке
    curfile = dirblock->block_start (block_num); // Номер первого файла в этом солид-блоке
    outfile_open (SECOND_PASS);                  // Откроем первый выходной файл
    int result = MultiDecompress (data_block.compressor, callback_func, NULL);
    CHECK (result!=FREEARC_ERRCODE_INVALID_COMPRESSOR, (s,"ERROR: unsupported compression method %s", data_block.compressor));
    CHECK (result>=0 || result==FREEARC_ERRCODE_NO_MORE_DATA_REQUIRED, (s,"ERROR: archive data corrupted (decompression fails)"));
    outfile_close();                             // Закроем последний выходной файл
  }
}

#ifdef FREEARC_INSTALLER
// Wipes entire directory with all its subdirs
void wipedir(TCHAR *dir)
{
    // List all entries in this directory
    CFILENAME dirstar  = (TCHAR*) malloc (MY_FILENAME_MAX * sizeof(TCHAR));
    CFILENAME fullname = (TCHAR*) malloc (MY_FILENAME_MAX * sizeof(TCHAR));
    _stprintf (dirstar, _T("%s%s*"), dir, _T(STR_PATH_DELIMITER));
    WIN32_FIND_DATA FindData[1];
    HANDLE h = FindFirstFileW (dirstar, FindData);
    if (h) do {
        // For every entry except for "." and ".., remove entire subdir (if it's a directory) or remove just file itself
        if (_tcscmp(FindData->cFileName,_T("."))  &&  _tcscmp(FindData->cFileName,_T("..")))
        {
            _stprintf (fullname, _T("%s%s%s"), dir, _T(STR_PATH_DELIMITER), FindData->cFileName);
            if (FindData->dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)
                wipedir (fullname);
            else
                DeleteFile (fullname);
        }
    } while (FindNextFile(h,FindData));
    FindClose(h);
    RemoveDirectory (dir);
    free(fullname); free(dirstar);
}
#endif


/******************************************************************************
** Основная программа *********************************************************
******************************************************************************/

// Читает структуру архива и вызывает в зависимости от выполняемой команды
// ListFiles для каждого блока каталога или ExtractFiles для каждого солид-блока
void ProcessArchive (COMMAND &command)
{
  static ARCHIVE arcinfo (command.arcname);
  arcinfo.read_structure();                                           // Прочитаем структуру архива
  // Выведем заголовок операции на экран и запросим у пользователя разрешение на распаковку SFX
  if (!UI.AllowProcessing (command.cmd, command.silent, MYFILE(command.arcname).displayname(), &arcinfo.arcComment[0], arcinfo.arcComment.size, command.outpath)) {
    command.ok = FALSE;  return;
  }
  if (command.cmd!='t')  outfile.SetBaseDir (UI.GetOutDir());

  writtenBytes = 0;
  if (command.list_cmd())  ListHeader (command);
  else                     UI.BeginProgress (arcinfo.arcfile.size());
  iterate_array (i, arcinfo.control_blocks_descriptors) {             // Переберём все служебные блоки в архиве...
    BLOCK& block_descriptor = arcinfo.control_blocks_descriptors[i];
    if (block_descriptor.type == DIR_BLOCK) {                         // ... и отберём из них блоки каталога
      DIRECTORY_BLOCK dirblock (arcinfo, block_descriptor);           // Прочитаем блок каталога
      if (command.list_cmd())                                         // Если это команда получения листинга
        ListFiles (&dirblock, command);                               //   то выполним её
      else
        iterate_array (i, dirblock.data_block)                        //   иначе - переберём все солид-блоки в каталоге
          ExtractFiles (&dirblock, i, command);                       //     и для каждого из них выполним процедуру тестирования/распаковки
    }
  }
  if (command.list_cmd())  ListFooter (command);
  else                     UI.EndProgress();

#ifdef FREEARC_INSTALLER
  // Run setup.exe after unpacking
  if (command.runme)
  {
      CFILENAME tmp  = (TCHAR*) malloc (MY_FILENAME_MAX * 4);
      CFILENAME tmp2 = (TCHAR*) malloc (MY_FILENAME_MAX * 4);

      // Execute command.runme in the directory command.outpath
      RunProgram (utf8_to_utf16 (command.runme, tmp), utf8_to_utf16 (command.outpath, tmp2), command.wipeoutdir);

      // Wipe outdir after installation was completed
      if (command.wipeoutdir)
          wipedir (utf8_to_utf16 (command.outpath, tmp));

      free(tmp); free(tmp2);
  }
#endif
}


int main (int argc, char *argv[])
{
  SetCompressionThreads (GetProcessorsCount());
  UI.DisplayHeader (HEADER1 NAME);
  COMMAND command (argc, argv);    // Распарсить команду
  if (command.ok)                  // Если парсинг был удачен и можно выполнить команду
    ProcessArchive (command);      //   Выполнить разобранную команду
  printf ("\n");
  return command.ok? EXIT_SUCCESS : FREEARC_EXIT_ERROR;
}

