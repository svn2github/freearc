{-# OPTIONS_GHC -cpp #-}
----------------------------------------------------------------------------------------------------
---- FreeArc archive manager                                                                  ------
----------------------------------------------------------------------------------------------------
module FileManager where

import Prelude hiding (catch)
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Char
import Data.IORef
import Data.List
import Data.Maybe
import System.IO.Unsafe
import System.Cmd
import System.Process
#if defined(FREEARC_WIN)
import System.Win32
#endif

import Graphics.UI.Gtk
import Graphics.UI.Gtk.ModelView as New

import Utils
import Errors
import Files
import FileInfo
import Charsets            (i18n, i18ns)
import Compression
import Encryption
import Options
import Cmdline
import UI
import ArhiveStructure
import ArhiveDirectory
import ArcExtract
import FileManPanel
import FileManUtils
import FileManDialogs
import FileManDialogAdd

----------------------------------------------------------------------------------------------------
---- Главное меню программы и тулбар под ним -------------------------------------------------------
----------------------------------------------------------------------------------------------------

uiDef =
  "<ui>"++
  "  <menubar>"++
  "    <menu name=\"File\" action=\"FileAction\">"++
  "      <menuitem name=\"Add\"      action=\"AddAction\" />"++
  "      <menuitem name=\"Modify\"   action=\"ModifyAction\" />"++
  "      <menuitem name=\"Join\"     action=\"JoinAction\" />"++
  "      <menuitem name=\"ArcInfo\"  action=\"ArcInfoAction\" />"++
  "      <menuitem name=\"Delete\"   action=\"DeleteAction\" />"++
  "      <menuitem name=\"Test\"     action=\"TestAction\" />"++
  "      <menuitem name=\"Extract\"  action=\"ExtractAction\" />"++
  "      <separator/>"++
  "      <menuitem name=\"Settings\" action=\"SettingsAction\" />"++
  "      <menuitem name=\"Exit\"     action=\"ExitAction\"/>"++
  "      <placeholder name=\"FileMenuAdditions\" />"++
  "    </menu>"++
  "    <menu name=\"Edit\" action=\"EditAction\">"++
  "      <menuitem name=\"Select\"   action=\"SelectAction\" />"++
  "      <menuitem name=\"Unselect\" action=\"UnselectAction\" />"++
  "      <menuitem name=\"Refresh\"  action=\"RefreshAction\" />"++
  "      <placeholder name=\"EditMenuAdditions\" />"++
  "    </menu>"++
  "  </menubar>"++
  "  <toolbar>"++
  "    <placeholder name=\"FileToolItems\">"++
  "      <toolitem name=\"Add\"      action=\"AddAction\" />"++
  "      <toolitem name=\"Modify\"   action=\"ModifyAction\" />"++
  "      <toolitem name=\"ArcInfo\"  action=\"ArcInfoAction\" />"++
  "      <toolitem name=\"Delete\"   action=\"DeleteAction\" />"++
  "      <toolitem name=\"Test\"     action=\"TestAction\" />"++
  "      <toolitem name=\"Extract\"  action=\"ExtractAction\" />"++
  "      <separator/>"++
  "      <toolitem name=\"Select\"   action=\"SelectAction\" />"++
  "      <toolitem name=\"Unselect\" action=\"UnselectAction\" />"++
  "      <toolitem name=\"Refresh\"  action=\"RefreshAction\" />"++
  "    </placeholder>"++
  "  </toolbar>"++
  "</ui>"


----------------------------------------------------------------------------------------------------
---- Визуальная часть файл-менеджера ---------------------------------------------------------------
----------------------------------------------------------------------------------------------------

myGUI run args = do
  fileManagerMode =: True
  startGUI $ do
  io$ parseCmdline ["l", "a"]   -- инициализация: display, логфайл
  -- Создадим окно индикатора прогресса и загрузим настройки/локализацию
  (windowProgress, clearStats) <- runIndicators
  -- Main menu
  standardGroup <- actionGroupNew "standard"
  i18menu <- i18ns$ split '_' "0050 File_0066 Edit"
  fileAct <- actionNew "FileAction" (i18menu!!0) Nothing Nothing
  editAct <- actionNew "EditAction" (i18menu!!1) Nothing Nothing
  mapM_ (actionGroupAddAction standardGroup) [fileAct, editAct]
  -- Menus and toolbars
  let anew name comment icon accel = do
        [i18name,i18comment] <- i18ns [name,comment]
        act <- actionNew (drop 5 name++"Action") i18name (Just i18comment) icon
        actionGroupAddActionWithAccel standardGroup act (Just "")  -- accel
        return act
  addAct      <- anew "0030 Add"        "0040 Add files to archive(s)"        (Just stockMediaRecord)     "<Alt>A"
  modifyAct   <- anew "0031 Modify"     "0041 Modify archive(s)"              (Just stockEdit)            "<Alt>M"
  joinAct     <- anew "0032 Join"       "0042 Join archives together"         (Just stockCopy)            "<Alt>J"
  arcinfoAct  <- anew "0086 ArcInfo"    "0087 Information about archive"      (Just stockInfo)            "<Alt>I"
  deleteAct   <- anew "0033 Delete"     "0043 Delete files (from archive)"    (Just stockDelete)          "Delete"
  testAct     <- anew "0034 Test"       "0044 Test files in archive(s)"       (Just stockSpellCheck)      "<Alt>T"
  extractAct  <- anew "0035 Extract"    "0045 Extract files from archive(s)"  (Just stockMediaPlay)       "<Alt>E"
  settingsAct <- anew "0064 Settings"   "0065 Edit program settings"          (Just stockPreferences)     "<Alt>S"
  exitAct     <- anew "0036 Exit"       "0046 Quit application"               (Just stockQuit)            "<Alt>Q"

  selectAct   <- anew "0037 Select"     "0047 Select files"                   (Just stockAdd)             "+"
  unselectAct <- anew "0038 Unselect"   "0048 Unselect files"                 (Just stockRemove)          "-"
  refreshAct  <- anew "0039 Refresh"    "0049 Reread archive/directory"       (Just stockRefresh)         "F5"
  ui <- uiManagerNew
  mid <- uiManagerAddUiFromString ui uiDef
  uiManagerInsertActionGroup ui standardGroup 0

  window <- windowNew
  (Just menuBar) <- uiManagerGetWidget ui "/ui/menubar"
  (Just toolBar) <- uiManagerGetWidget ui "/ui/toolbar"

  -- Список действий, выполняемых при закрытии окна файл-менеджера
  onExit <- newList
  window `onDestroy` do
    sequence_ =<< listVal onExit
    mainQuit

  (listUI, listView, listModel, listSelection, columns, onColumnTitleClicked) <- createFilePanel
  statusLabel  <- labelNew Nothing
  miscSetAlignment statusLabel 0 0.5
  messageCombo <- New.comboBoxNewText
  statusbar    <- statusbarNew
  ctx <- statusbarGetContextId statusbar ""
  statusbarPush statusbar ctx "    "
  widgetSetSizeRequest messageCombo 30 (-1)
  hBox <- hBoxNew False 0
  boxPackStart hBox statusLabel  PackNatural 2
  boxPackStart hBox messageCombo PackGrow    2
  boxPackStart hBox statusbar    PackNatural 0

  -- Создадим переменную для хранения текущего состояния файл-менеджера
  fm' <- newFM window listView listModel listSelection statusLabel messageCombo
  fmStackMsg fm' "              "

  -- Полоска навигации
  naviBar  <- hBoxNew False 0
  upButton <- button "0006   Up  "
  curdir   <- fmEntryWithHistory fm' "dir/arcname" (const$ return True) (fmCanonicalizePath fm')
  saveDirButton <- button "0007   Save  "
  boxPackStart naviBar (widget upButton)       PackNatural 0
#if defined(FREEARC_WIN)
  -- Меню выбора диска
  driveButton <- button "C:"
  driveMenu   <- makePopupMenu (chdir fm'.(++"\\").head.words) =<< getDrives
  driveButton `onClick` (widgetShowAll driveMenu >> menuPopup driveMenu Nothing)
  boxPackStart naviBar (widget driveButton)    PackNatural 0
  -- Менять надпись на кнопке выбора диска при переходе на другой диск
  fm' `fmOnChdir` do
    fm <- val fm'
    let drive = takeDrive (fm_current fm)
    setTitle driveButton (take 2 drive)  `on` drive
#endif
  boxPackStart naviBar (widget curdir)         PackGrow    0
  boxPackStart naviBar (widget saveDirButton)  PackNatural 0

  -- Целиком окно файл-менеджера
  vBox <- vBoxNew False 0
  set vBox [boxHomogeneous := False]
  boxPackStart vBox menuBar   PackNatural 0
  boxPackStart vBox toolBar   PackNatural 0
  boxPackStart vBox naviBar   PackNatural 0
  boxPackStart vBox listUI    PackGrow    0
  boxPackStart vBox hBox      PackNatural 0

  containerAdd window vBox


----------------------------------------------------------------------------------------------------
---- Сохранение/восстановление размера и положения главного окна и колонок в нём -------------------
----------------------------------------------------------------------------------------------------

  window `windowSetPosition` WinPosCenter
  --windowSetGeometryHints window (Just window) (Just (1,1)) (Just (32000,32000)) Nothing Nothing Nothing
  --widgetSetSizeRequest window 700 500
  --window `windowSetGravity` GravityStatic
  --window `windowSetPosition` WinPosNone
  --windowSetDefaultSize window 200 100

  -- Сохраним размер и положение главного окна после его перемещения
  window `onConfigure` \e -> do
    saveSizePos fm' window "MainWindow"
    return False

  -- При старте восстановим сохранённый размер окна
  restoreSizePos fm' window "MainWindow" "0 0 720 500"

  -- temporary: удалим старые теги :)
  mapM_ (fmDeleteTagFromHistory fm') $ words "MainWindowPos MainWindowSize ExtractDialogPos ExtractDialogSize AddDialogPos AddDialogSize SettingsDialogPos SettingsDialogSize ArcInfoPos ArcInfoSize"


  -- При закрытии программы сохраним ширину колонок
  onExit <<= do
    for columns $ \(name,col1) -> do
      w <- New.treeViewColumnGetWidth col1
      fmReplaceHistory fm' (name++"ColumnWidth") (show w)

  -- При старте восстановим сохранённую ширину колонок
  for columns $ \(name,col1) -> do
    w <- readInt  `fmap`  fmGetHistory1 fm' (name++"ColumnWidth") "150"
    New.treeViewColumnSetFixedWidth col1 w


----------------------------------------------------------------------------------------------------
---- Навигационная часть файл-менеджера ------------------------------------------------------------
----------------------------------------------------------------------------------------------------

--  for [upButton,saveDirButton] (`buttonSetFocusOnClick` False)

  -- Выводить errors/warnings внизу окна FreeArc
  showErrors' <- ref True
  errorHandlers   ++= [whenM (val showErrors') . postGUIAsync . fmStackMsg fm']
  warningHandlers ++= [whenM (val showErrors') . postGUIAsync . fmStackMsg fm']

  -- Отключает вывод сообщений об ошибках на время выполнения action
  let hideErrors action  =  bracket (showErrors' <=> False)  (showErrors' =: )  (\_ -> action)


  -- Перейти в заданный каталог/архив или выполнить команду
  let select filename = do
        fm <- val fm'
        handle (\e -> runFile filename (fm_curdir fm) False) $ do    -- при неудаче перехода запустим файл :)
          hideErrors $ do
            chdir fm' filename
            New.treeViewScrollToPoint (fm_view fm) 0 0
            --New.treeViewSetCursor (fm_view fm) [0] Nothing

  -- Переход в родительский каталог
  let goParentDir = do
        fm <- val fm'
        let path = fm_current fm
        chdir fm' ".."
        -- Выбираем каталог/архив, из которого мы только что вышли
        fmSetCursor fm' (takeFileName path)

  -- Запись текущего каталога в историю
  let saveCurdirToHistory = do
        fm <- val fm'
        fmAddHistory fm' (isFM_Archive fm.$bool "dir" "arcname") =<< fmCanonicalizePath fm' =<< val curdir


  -- При нажатии Enter на строке в списке открываем выбранный архив/каталог
  listView `New.onRowActivated` \path _ -> do
    select =<< fmFilenameAt fm' path

  -- При переходе в другой каталог/архив отобразить его имя в строке ввода
  fm' `fmOnChdir` do
    fm <- val fm'
    curdir =: fm_current fm
    -- Сохраняем в историю имена архивов
    isFM_Archive fm  &&&  fm_arcdir fm==""  &&&  saveCurdirToHistory
    -- Перечитаем историю с диска
    rereadHistory curdir

  -- Переходим в род. каталог по кнопке Up или нажатию BackSpace в списке файлов
  upButton `onClick` goParentDir
  listView `onKeyPress` \(Key {eventKeyName = name}) -> do
    let doit = (name=="BackSpace")
    when doit goParentDir
    return doit
    --let doit = ((take 2$ reverse$ name)/="L_")
    --when doit$ debugMsg name
    --return doit

  -- Сохранение выбранного архива/каталога в истории
  saveDirButton `onClick` do
    saveCurdirToHistory

  -- Открытие другого каталога или архива (Enter в строке ввода)
  entry curdir `onEntryActivate` do
    saveCurdirToHistory
    select =<< val curdir

  -- Открытие другого каталога или архива (выбор из истории)
  widget curdir `New.onChanged` do
    whenJustM_ (New.comboBoxGetActive$ widget curdir) $ \_ -> do
    saveCurdirToHistory
    select =<< val curdir

  -- Select/unselect files by mask
  let sel method msg = do
        whenJustM_ (fmInputString fm' "mask" msg (const$ return True) return) $ \mask -> do
          method fm' ((match mask).fdBasename)
  selectAct   `onActionActivate`  sel fmSelectFilenames   "0008 Select files"
  unselectAct `onActionActivate`  sel fmUnselectFilenames "0009 Unselect files"

  -- Обновить список файлов актуальными данными
  refreshAct `onActionActivate` do
    refreshCommand fm'

  -- Окно настроек программы
  settingsAct `onActionActivate` do
    settingsDialog fm'

  -- При нажатии заголовка столбца в списке файлов - сортировать по этому столбцу
  --   (при повторном нажатии - сортировать в обратном порядке)
  onColumnTitleClicked =: \column -> do
    fmModifySortOrder fm' (showSortOrder columns) (calcNewSortOrder column)
    refreshCommand fm'

  -- Отсортируем файлы по сохранённому критерию
  fmSetSortOrder fm' (showSortOrder columns) =<< fmRestoreSortOrder fm'

  -- При закрытии главного окна сохраним порядок сортировки
  onExit <<= do
    fmSaveSortOrder  fm' =<< fmGetSortOrder fm'


----------------------------------------------------------------------------------------------------
---- Выполнение команд файл-менеджера --------------------------------------------------------------
----------------------------------------------------------------------------------------------------

  -- При выполнении операций не выходим по исключениям, а печатаем сообщения о них в логфайл
  let handleErrors action x  =  do programTerminated =: False
                                   (action x `catch` handler) `finally` (programTerminated =: False)
        where handler ex = do
                programTerminated' <- val programTerminated
                errmsg <- case ex of
                   _ | programTerminated' -> i18n"0010 Operation interrupted!"
                   Deadlock               -> i18n"0011 No threads to run: infinite loop or deadlock?"
                   ErrorCall s            -> return s
                   other                  -> return$ showsPrec 0 other ""
                with' (val log_separator') (log_separator'=:) $ \_ -> do
                  log_separator' =: ""
                  io$ condPrintLineLn "w" errmsg
                return (error "Undefined result of myGUI::handleErrors")

  -- Выполнить команду архиватора
  let runWithMsg ([formatStart,formatSuccess,formatFail],msgArgs,cmd) = do
        -- Сообщение о начале выполнения команды
        msgStart <- i18n formatStart
        postGUIAsync$ fmStackMsg fm' (formatn msgStart msgArgs)
        -- Выполнить и посчитать число ошибок
        w <- count_warnings (parseCmdline cmd >>= mapM_ run)
        -- Сообщение об успешном выполнении либо кол-ве допущенных ошибок
        msgFinish <- i18n (if w==0  then formatSuccess  else formatFail)
        postGUIAsync$ fmStackMsg fm' (formatn msgFinish (msgArgs++[show w]))

  -- Commands executed by various buttons
  cmdChan <- newChan
  forkIO $ do
    foreverM $ do
      commands <- readChan cmdChan
      postGUIAsync$ do clearStats; widgetShowAll windowProgress
      mapM_ (handleErrors runWithMsg) commands
      whenM (isEmptyChan cmdChan)$ postGUIAsync$ do widgetHide windowProgress; refreshCommand fm'
      --uiDoneProgram
  let exec = writeChan cmdChan

  -- Упаковка данных
  addAct `onActionActivate` do
    addDialog fm' exec "a"

  -- Модификация архивов
  modifyAct `onActionActivate` do
    addDialog fm' exec "ch"

  -- Объединение архивов
  joinAct `onActionActivate` do
    addDialog fm' exec "j"

  -- Информация об архиве
  arcinfoAct `onActionActivate` do
    archiveOperation fm' $
      arcinfoDialog fm'

  -- Удаление файлов (из архива)
  deleteAct `onActionActivate` do
    fm <- val fm'
    files <- getSelection fm' (if isFM_Archive fm  then xCmdFiles  else const [])
    if null files  then fmErrorMsg fm' "0012 There are no files selected!" else do
    msg <- i18n$ case files of [_] | isFM_Archive fm -> "0160 Delete %1 from archive?"
                                   | otherwise       -> "0161 Delete %1?"
                               _   | isFM_Archive fm -> "0019 Delete %2 file(s) from archive?"
                                   | otherwise       -> "0020 Delete %2 file(s)?"
    whenM (askOkCancel window (formatn msg [head files, show3$ length files])) $ do
      fmDeleteSelected fm'
      if isFM_Archive fm
        -- Стереть файлы из архива
        then do closeFMArc fm'
                let arcname = fm_arcname fm
                exec [(["0228 Deleting from %1",
                        "0229 FILES SUCCESFULLY DELETED FROM %1",
                        "0230 %2 WARNINGS WHILE DELETING FROM %1"],
                       [takeFileName arcname],
                       ["d", "--noarcext", "--", arcname]++files)]
        -- Удалить файлы на диске
        else io$ mapM_ (ignoreErrors.fileRemove.(fm_dir fm </>)) files

  -- Тестирование архив(ов)
  testAct `onActionActivate` do
    archiveOperation fm' $
      extractDialog fm' exec "t"

  -- Распаковка архив(ов)
  extractAct `onActionActivate` do
    archiveOperation fm' $
      extractDialog fm' exec "x"
    rereadHistory curdir

  -- Выход из программы
  exitAct `onActionActivate`
    mainQuit

  -- Инициализируем состояние файл-менеджера каталогом/архивом, заданным в командной строке
  chdir fm' (head (args++["."]))
  fmStatusBarTotals fm'

  return window

