class UI
{
private:
  char outdir[MY_FILENAME_MAX*4];  //unicode: utf-8 encoding
public:
  UI();
  ~UI();
  void DisplayHeader (char* header);
  bool AllowProcessing (char cmd, int silent, FILENAME arcname, char* comment, int cmtsize, FILENAME outdir);
  FILENAME GetOutDir();
  void BeginProgress (uint64 totalBytes);
  bool ProgressRead  (uint64 readBytes);
  bool ProgressWrite (uint64 writtenBytes);
  bool ProgressFile  (bool isdir, const char *operation, FILENAME filename, uint64 filesize);
  void EndProgress();
  char AskOverwrite (FILENAME filename, uint64 size, time_t modified);
};

UI::UI()
{
}

UI::~UI()
{
}

void UI::DisplayHeader (char* header)
{
  printf ("%s", header);
}

void UI::BeginProgress (uint64 totalBytes)
{
}

bool UI::ProgressRead (uint64 readBytes)
{
  return TRUE;
}

bool UI::ProgressWrite (uint64 writtenBytes)
{
  return TRUE;
}

bool UI::ProgressFile (bool isdir, const char *operation, FILENAME filename, uint64 filesize)
{
  printf (isdir?  "%s %s" STR_PATH_DELIMITER "\n"  :  "%s %s (%llu bytes)\n",
          operation, filename, filesize);
  return TRUE;
}

void UI::EndProgress()
{
  printf ("All OK");
}

FILENAME UI::GetOutDir()
{
  return outdir;
}

bool UI::AllowProcessing (char cmd, int silent, FILENAME arcname, char* comment, int cmtsize, FILENAME _outdir)
{
  strcpy (outdir, _outdir);
  printf (". %s archive: %s\n",                       // Выведем имя обрабатываемого архива
    cmd=='l'||cmd=='v'? "Listing" : cmd=='t' ? "Testing" : "Extracting", drop_dirname(arcname));
  if (cmtsize>0)                                      // Выведем архивный комментарий
#ifdef FREEARC_WIN
{
    // Convert comment from UTF-8 to OEM encoding before printing
    char *oemname = (char*) malloc(cmtsize+1);
    strncpy (oemname, comment, cmtsize);
    oemname[cmtsize] = 0;
    utf8_to_oem (oemname, oemname);
    printf ("%s\n", oemname);
    free (oemname);
}
#else
    printf("%*.*s\n", cmtsize, cmtsize, comment);
#endif

#ifdef FREEARC_SFX
  // В SFX необходимо запросить согласие пользователя перед началом распаковки
  if (!silent)
  {
    char answer[256];
    printf ("Continue extraction (y/n)? ");
    gets (answer);
    if (! (strequ(answer,"y") || strequ(answer,"Y")))
    {
      printf ("Extraction aborted!\n");
      return FALSE;
    }
    printf("\n");
  }
#endif
  return TRUE;
}

char UI::AskOverwrite (FILENAME filename, uint64 size, time_t modified)
{
  char help[] = "Valid answers: Y - yes, N - no, A - overwrite all, S - skip all, Q - quit\n";
  again: printf ("Overwrite %s (y/n/a/s/q) ? ", filename);
  char answer[256];  gets (answer);  *answer = tolower(*answer);
  if (strlen(answer)!=1 || !strchr("ynasq", *answer))  {printf (help);  goto again;}
  if (*answer=='q') {printf ("Extraction aborted\n");  exit(1);}
  return *answer;
}

