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
import Numeric hiding (readInt)
import System.IO.Unsafe
import System.Cmd
import System.Process
#if defined(FREEARC_WIN)
import System.Win32
import Foreign.Ptr
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
--      File: New Archive, Open Archive, New SFX, Change Drive, Select All, Select Group, Deselect Group, Invert Selection
--      Commands (или Actions): Add, Extract, Test, ArcInfo, View, Delete, Rename
--      Tools: Wizard (если таковой будет), Protect, Comment, Convert to EXE, Encrypt, Add Recovery record, Repair
--      Options: Configuration, Save settings, Load settings, View log, Clear log
--      Help: собственно сам Help, Goto Homepage (и/или Check for update), About

uiDef =
  "<ui>"++
  "  <menubar>"++
  "    <menu name=\"File\"     action=\"FileAction\">"++
  "      <menuitem name=\"OpenArchive\"        action=\"OpenArchiveAction\" />"++
  "      <separator/>"++
  "      <menuitem name=\"Select all\"         action=\"SelectAllAction\" />"++
  "      <menuitem name=\"Select\"             action=\"SelectAction\" />"++
  "      <menuitem name=\"Unselect\"           action=\"UnselectAction\" />"++
  "      <menuitem name=\"Invert selection\"   action=\"InvertSelectionAction\" />"++
  "      <menuitem name=\"Refresh\"            action=\"RefreshAction\" />"++
  "      <separator/>"++
  "      <placeholder name=\"FileMenuAdditions\" />"++
  "      <menuitem name=\"Exit\"               action=\"ExitAction\"/>"++
  "    </menu>"++
  "    <menu name=\"Commands\" action=\"CommandsAction\">"++
  "      <menuitem name=\"Add\"                action=\"AddAction\" />"++
  "      <menuitem name=\"Extract\"            action=\"ExtractAction\" />"++
  "      <menuitem name=\"Test\"               action=\"TestAction\" />"++
  "      <menuitem name=\"ArcInfo\"            action=\"ArcInfoAction\" />"++
  "      <menuitem name=\"Delete\"             action=\"DeleteAction\" />"++
  "    </menu>"++
  "    <menu name=\"Tools\"    action=\"ToolsAction\">"++
  "      <menuitem name=\"Lock\"               action=\"LockAction\" />"++
  "      <menuitem name=\"Comment\"            action=\"CommentAction\" />"++
  "      <menuitem name=\"Recompress\"         action=\"RecompressAction\" />"++
  "      <menuitem name=\"Convert to SFX\"     action=\"ConvertToSFXAction\" />"++
  "      <separator/>"++
  "      <menuitem name=\"Encrypt\"            action=\"EncryptAction\" />"++
  "      <menuitem name=\"Protect\"            action=\"ProtectAction\" />"++
  "      <menuitem name=\"Repair\"             action=\"RepairAction\" />"++
  "      <separator/>"++
  "      <menuitem name=\"Modify\"             action=\"ModifyAction\" />"++
  "      <menuitem name=\"Join archives\"      action=\"JoinArchivesAction\" />"++
  "    </menu>"++
  "    <menu name=\"Options\"  action=\"OptionsAction\">"++
  "      <menuitem name=\"Settings\"           action=\"SettingsAction\" />"++
  "      <separator/>"++
  "      <menuitem name=\"ViewLog\"            action=\"ViewLogAction\" />"++
  "      <menuitem name=\"ClearLog\"           action=\"ClearLogAction\" />"++
  "    </menu>"++
  "    <menu name=\"Help\"     action=\"HelpAction\">"++
  "      <menuitem name=\"MainHelp\"           action=\"MainHelpAction\" />"++
  "      <separator/>"++
  "      <menuitem name=\"CmdlineHelp\"        action=\"CmdlineHelpAction\" />"++
  "      <menuitem name=\"OpenHomepage\"       action=\"OpenHomepageAction\" />"++
  "      <menuitem name=\"OpenForum\"          action=\"OpenForumAction\" />"++
  "      <menuitem name=\"OpenWiki\"           action=\"OpenWikiAction\" />"++
  "      <menuitem name=\"CheckForUpdate\"     action=\"CheckForUpdateAction\" />"++
  "      <separator/>"++
  "      <menuitem name=\"About\"              action=\"AboutAction\" />"++
  "    </menu>"++
  "  </menubar>"++
  "  <toolbar>"++
  "    <placeholder name=\"FileToolItems\">"++
  "      <toolitem name=\"OpenArchive\"        action=\"OpenArchiveAction\" />"++
  "      <separator/>"++
  "      <toolitem name=\"Add\"                action=\"AddAction\" />"++
  "      <toolitem name=\"Extract\"            action=\"ExtractAction\" />"++
  "      <toolitem name=\"Test\"               action=\"TestAction\" />"++
  "      <toolitem name=\"ArcInfo\"            action=\"ArcInfoAction\" />"++
  "      <toolitem name=\"Delete\"             action=\"DeleteAction\" />"++
  "      <separator/>"++
  "      <toolitem name=\"Lock\"               action=\"LockAction\" />"++
  "      <toolitem name=\"Recompress\"         action=\"RecompressAction\" />"++
  "      <toolitem name=\"Convert to SFX\"     action=\"ConvertToSFXAction\" />"++
  "      <toolitem name=\"Join archives\"      action=\"JoinArchivesAction\" />"++
  "      <separator/>"++
  "      <toolitem name=\"Refresh\"            action=\"RefreshAction\" />"++
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
  -- Список ассоциаций клавиша->действие
  onKeyActions <- newList
  let onKey = curry (onKeyActions <<=)
  -- Создадим окно индикатора прогресса и загрузим настройки/локализацию
  (windowProgress, clearStats) <- runIndicators
  -- Main menu
  standardGroup <- actionGroupNew "standard"
  let action name  =  (concat$ map (mapHead toUpper)$ words$ drop 5 name)++"Action"   -- "9999 the name" -> "TheNameAction"
  let names = split ',' "0050 File,0258 Commands,0259 Tools,0260 Options,0261 Help"
  for names $ \name -> do
    label <- i18n name
    actionGroupAddAction standardGroup  =<<  actionNew (action name) label Nothing Nothing
  -- Menus and toolbars
  let anew name comment icon accel = do
        [i18name,i18comment] <- i18ns [name,comment]
        action <- actionNew (action name) i18comment (Just i18comment) icon
        action `set` [actionShortLabel := i18name]
        actionGroupAddActionWithAccel standardGroup action (Just accel)
        accel `onKey` actionActivate action
        return action
  --
  openAct     <- anew "0262 Open archive"     "0265 Open archive"                       (Just stockOpen)            "<Alt>O"
  selectAllAct<- anew "0263 Select all"       "0290 Select all files"                   (Just stockSelectAll)       "<Ctrl>A"
  selectAct   <- anew "0037 Select"           "0047 Select files"                       (Just stockAdd)             "KP_Add"
  unselectAct <- anew "0038 Unselect"         "0048 Unselect files"                     (Just stockRemove)          "KP_Subtract"
  invertSelAct<- anew "0264 Invert selection" "0291 Invert selection"                   (Nothing)                   "KP_Multiply"
  refreshAct  <- anew "0039 Refresh"          "0049 Reread archive/directory"           (Just stockRefresh)         "F5"
  exitAct     <- anew "0036 Exit"             "0046 Quit application"                   (Just stockQuit)            "<Alt>Q"

  addAct      <- anew "0030 Add"              "0040 Add files to archive(s)"            (Just stockMediaRecord)     "<Alt>A"
  extractAct  <- anew "0035 Extract"          "0045 Extract files from archive(s)"      (Just stockMediaPlay)       "<Alt>E"
  testAct     <- anew "0034 Test"             "0044 Test files in archive(s)"           (Just stockSpellCheck)      "<Alt>T"
  arcinfoAct  <- anew "0086 ArcInfo"          "0087 Information about archive"          (Just stockInfo)            "<Alt>I"
  deleteAct   <- anew "0033 Delete"           "0043 Delete files (from archive)"        (Just stockDelete)          "Delete"

  lockAct     <- anew "0266 Lock"             "0267 Lock archive from further changes"  (Just stockDialogAuthentication) "<Alt>L"
  commentAct  <- anew "0268 Comment"          "0269 Edit archive comment"               (Just stockEdit)            "<Alt>C"
  recompressAct<-anew "0293 Recompress"       "0294 Recompress files in archive"        (Just stockGotoBottom)      "<Alt>R"
  toSfxAct    <- anew "0270 Convert to SFX"   "0271 Convert archive to SFX"             (Just stockConvert)         "<Alt>S"
  encryptAct  <- anew "0272 Encrypt"          "0273 Encrypt archive contents"           (Nothing)                   ""
  addRrAct    <- anew "0274 Protect"          "0275 Add Recovery record to archive"     (Nothing)                   "<Alt>P"
  repairAct   <- anew "0379 Repair"           "0380 Repair damaged archive"             (Nothing)                   ""
  modifyAct   <- anew "0031 Modify"           "0041 Modify archive(s)"                  (Just stockEdit)            "<Alt>M"
  joinAct     <- anew "0032 Join archives"    "0042 Join archives together"             (Just stockCopy)            "<Alt>J"

  settingsAct <- anew "0064 Settings"         "0065 Edit program settings"              (Just stockPreferences)     ""
  viewLogAct  <- anew "0276 View log"         "0277 View logfile"                       (Nothing)                   ""
  clearLogAct <- anew "0278 Clear log"        "0279 Clear logfile"                      (Nothing)                   ""

  helpAct     <- anew "0280 Main help"        "0281 Help on using FreeArc"              (Just stockHelp)            "F1"
  helpCmdAct  <- anew "0282 Cmdline help"     "0283 Help on FreeArc command line"       (Just stockHelp)            ""
  homepageAct <- anew "0284 Open Homepage"    "0285 Open program site"                  (Just stockHome)            ""
  openForumAct<- anew "0373 Open forum"       "0374 Open program forum"                 (Nothing)                   ""
  openWikiAct <- anew "0375 Open wiki"        "0376 Open program wiki"                  (Nothing)                   ""
  whatsnewAct <- anew "0286 Check for update" "0287 Check for new program version"      (Just stockDialogInfo)      ""
  aboutAct    <- anew "0288 About"            "0289 About"                              (Just stockAbout)           ""

  menufile <- findFile configFilePlaces aMENU_FILE
  uiData   <- if menufile>""  then fileGetBinary menufile  else return uiDef

  ui <- uiManagerNew
  uiManagerAddUiFromString ui uiData
  uiManagerInsertActionGroup ui standardGroup 0

  window <- windowNew
  (Just menuBar) <- uiManagerGetWidget ui "/ui/menubar"
  (Just toolBar) <- uiManagerGetWidget ui "/ui/toolbar"

  (listUI, listView, listModel, listSelection, columns, onColumnTitleClicked) <- createFilePanel
  statusLabel  <- labelNew Nothing
  miscSetAlignment statusLabel 0 0.5
  messageCombo <- New.comboBoxNewText
  statusbar    <- statusbarNew
  ctx <- statusbarGetContextId statusbar ""
  statusbarPush statusbar ctx "    "
  widgetSetSizeRequest messageCombo 30 (-1)
  lowBox <- vBoxNew False 0
  boxPackStart lowBox statusLabel  PackNatural 2
  boxPackStart lowBox messageCombo PackGrow    2
  --boxPackStart lowBox statusbar    PackNatural 0

  -- Создадим переменную для хранения текущего состояния файл-менеджера
  fm' <- newFM window listView listModel listSelection statusLabel messageCombo
  fmStackMsg fm' "              "

  -- Отрихтуем тулбар
  let toolbar = castToToolbar toolBar
  toolbarCaptions <- fmGetHistoryBool fm' "ToolbarCaptions" True
  toolbar `set` [toolbarStyle := if toolbarCaptions then ToolbarBoth else ToolbarIcons]
  toolbar `toolbarSetIconSize` iconSizeLargeToolbar
  n <- toolbarGetNItems toolbar
  for [0..n-1] $ \i -> do
    Just button <- toolbarGetNthItem toolbar i
    toolItemSetHomogeneous button False

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
  boxPackStart vBox lowBox    PackNatural 0

  containerAdd window vBox


  -- Список действий, выполняемых при закрытии окна файл-менеджера
  onExit <- newList
  window `onDestroy` do
    sequence_ =<< listVal onExit
    mainQuit

  -- Список ассоциаций клавиша->действие
  listView `onKeyPress` \event -> do
    x <- lookup (eventKey event) `fmap` listVal onKeyActions
    case x of
      Just action -> do action; return True
      Nothing     -> return False


----------------------------------------------------------------------------------------------------
---- Сохранение/восстановление размера и положения главного окна и колонок в нём -------------------
----------------------------------------------------------------------------------------------------

  window `windowSetPosition` WinPosCenter
  --windowSetGeometryHints window (Just window) (Just (1,1)) (Just (32000,32000)) Nothing Nothing Nothing
  --widgetSetSizeRequest window 700 500
  --window `windowSetGravity` GravityStatic
  --window `windowSetPosition` WinPosNone
  --windowSetDefaultSize window 200 100

  -- При старте восстановим сохранённый размер окна
  restoreSizePos fm' window "MainWindow" "-10000 -10000 720 500"

  -- Сохраним размер и положение главного окна после его перемещения
  window `onConfigure` \e -> do
    saveSizePos fm' window "MainWindow"
    return False

  -- Запомним, было ли окно максимизировано
  window `onWindowState` \e -> do
    let isMax x = case x of
                    WindowStateMaximized -> True
                    _                    -> False
    saveMaximized fm' "MainWindow" (any isMax (eventWindowState e))
    return False


  -- При закрытии программы сохраним порядок и ширину колонок
  onExit <<= do
    colnames  <-  New.treeViewGetColumns listView  >>=  mapM New.treeViewColumnGetTitle
    fmReplaceHistory fm' "ColumnOrder" (unwords$ catMaybes colnames)
    for columns $ \(name,col1) -> do
      w <- New.treeViewColumnGetWidth col1
      fmReplaceHistory fm' (name++"ColumnWidth") (show w)

  -- При старте восстановим сохранённые порядок и ширину колонок
  order <- (reverse.words) `fmap` fmGetHistory1 fm' "ColumnOrder" ""
  for order $ \colname -> do
    whenJust (lookup colname columns) $
      New.treeViewMoveColumnFirst listView
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

  -- Перехват и обработка ошибок
  errorMsg <- ref ""
  errorHandlers ++= [(errorMsg =:)]
  let withErrorHandler onError = handle$ \e->do operationTerminated =: False
                                                fmErrorMsg fm' =<< val errorMsg
                                                sequence_ onError
  -- При возникновении ошибки выдать её пользователю
  let msgboxOnError = withErrorHandler []
  -- При возникновении ошибки выдать её пользователю и заверщить выполнение программы
  let terminateOnError = withErrorHandler [shutdown "" aEXIT_CODE_FATAL_ERROR]


  -- Перейти в заданный каталог/архив или выполнить команду
  let select filename = do
        fm <- val fm'
        handle (\e -> (operationTerminated =: False) >> runFile filename (fm_curdir fm) False) $ do    -- при неудаче перехода запустим файл :)
          hideErrors $ do
            chdir fm' filename
            New.treeViewScrollToPoint (fm_view fm) 0 0
            --New.treeViewSetCursor (fm_view fm) [0] Nothing

  -- Переход в родительский каталог
  let goParentDir = do
        fm <- val fm'
        unless (isFM_Archive fm  &&  isURL(fm_arcname fm)  &&  fm_arcdir fm=="") $ do  -- Запретить Up из архива в инете
        chdir fm' ".."
        -- Выбираем каталог/архив, из которого мы только что вышли
        fmSetCursor fm' (takeFileName$ fm_current fm)

  -- Запись текущего каталога в историю
  let saveCurdirToHistory = do
        fm <- val fm'
        fmAddHistory fm' (isFM_Archive fm.$bool "dir" "arcname") =<< fmCanonicalizePath fm' =<< val curdir


  -- При нажатии Enter на строке в списке открываем выбранный архив/каталог
  listView `New.onRowActivated` \path column -> do
    fm <- val fm'
    file <- fmFileAt fm' path
    unless (isFM_Archive fm  &&  not(fdIsDir file)) $ do  -- Run command don't yet work directly from archives
    select (fmname file)

  -- При single-click на свободном пространстве справа/снизу снимаем отметку со всех файлов,
  -- при double-click там же выбираем все файлы
  listView `onButtonPress` \e -> do
    path <- New.treeViewGetPathAtPos listView (round$ eventX e, round$ eventY e)
    coltitle <- case path of
                  Just (_,column,_) -> New.treeViewColumnGetTitle column >>== fromMaybe ""
                  _                 -> return ""
    -- Пустая строка в coltitle означает клик за пределами списка файлов
    coltitle=="" &&& e.$eventButton==LeftButton &&&
      ((if e.$eventClick==SingleClick  then fmUnselectAll  else fmSelectAll) fm'  >>  return True)

  -- При переходе в другой каталог/архив отобразить его имя в строке ввода
  fm' `fmOnChdir` do
    fm <- val fm'
    curdir =: fm_current fm
    -- Сохраняем в историю имена архивов
    isFM_Archive fm  &&&  fm_arcdir fm==""  &&&  saveCurdirToHistory
    -- Перечитаем историю с диска
    rereadHistory curdir

  -- При переходе в другой каталог/архив - отобразить его имя в заголовке окна
  fm' `fmOnChdir` do
    fm <- val fm'
    let title | isFM_Archive fm  =  takeFileName (fm_arcname fm) </> fm_arcdir fm
              | otherwise        =  takeFileName (fm_dir fm)  |||  fm_dir fm
    set (fm_window fm) [windowTitle := title++" - "++aARC_NAME]

  -- Переходим в род. каталог по кнопке Up или нажатию BackSpace в списке файлов
  upButton  `onClick` goParentDir
  "BackSpace" `onKey` goParentDir

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

  -- Выполнить action над файлами, состоящими в соотношении makeRE с именем файла под курсором
  let byFile action makeRE = do
        filename <- fmGetCursor fm'
        action fm' ((match$ makeRE filename).fdBasename)

  -- Клавиши Shift/Ctrl/Alt-Plus/Minus с теми же операциями как в FAR
  "<Shift>KP_Add"      `onKey` fmSelectAll   fm'
  "<Shift>KP_Subtract" `onKey` fmUnselectAll fm'
  "<Ctrl>KP_Add"       `onKey` byFile fmSelectFilenames   (("*" ++).takeExtension)
  "<Ctrl>KP_Subtract"  `onKey` byFile fmUnselectFilenames (("*" ++).takeExtension)
  "<Alt>KP_Add"        `onKey` byFile fmSelectFilenames   ((++".*").dropExtension)
  "<Alt>KP_Subtract"   `onKey` byFile fmUnselectFilenames ((++".*").dropExtension)


  -- При нажатии заголовка столбца в списке файлов - сортировать по этому столбцу
  --   (при повторном нажатии - сортировать в обратном порядке)
  onColumnTitleClicked =: \column -> do
    fmModifySortOrder fm' (showSortOrder columns) (calcNewSortOrder column)
    refreshCommand fm'
    fmSaveSortOrder  fm' =<< fmGetSortOrder fm'  -- запишем в конфиг порядок сортировки

  -- Отсортируем файлы по сохранённому критерию
  fmSetSortOrder fm' (showSortOrder columns) =<< fmRestoreSortOrder fm'


----------------------------------------------------------------------------------------------------
---- Меню File -------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

  -- Открыть архив
  openAct `onActionActivate` do
    fm <- val fm'
    let curfile  =  if isFM_Archive fm  then fm_arcname fm  else fm_dir fm </> "."
    chooseFile window FileChooserActionOpen "0305 Open archive" aARCFILE_FILTER (return curfile) $ \filename -> do
      msgboxOnError $
        chdir fm' filename

  -- Select/unselect files by user-supplied mask
  let byDialog method msg = do
        whenJustM_ (fmInputString fm' "mask" msg (const$ return True) return) $ \mask -> do
          method fm' ((match mask).fdBasename)
  selectAct   `onActionActivate`  byDialog fmSelectFilenames   "0008 Select files"
  unselectAct `onActionActivate`  byDialog fmUnselectFilenames "0009 Unselect files"

  -- Выделить все файлы
  selectAllAct `onActionActivate` do
    fmSelectAll fm'

  -- Инвертировать выделение
  invertSelAct `onActionActivate` do
    fmInvertSelection fm'

  -- Обновить список файлов актуальными данными
  refreshAct `onActionActivate` do
    refreshCommand fm'

  -- Выход из программы
  exitAct `onActionActivate`
    mainQuit


----------------------------------------------------------------------------------------------------
---- Движок выполнения консольных команд внутри FM gui ---------------------------------------------
----------------------------------------------------------------------------------------------------

  -- При выполнении операций не выходим по исключениям, а печатаем сообщения о них в логфайл
  let myHandleErrors action x  =  do operationTerminated =: False
                                     action x `catch` handler
                                     operationTerminated =: False
        where handler ex = do
                errmsg <- case ex of
                   Deadlock    -> i18n"0011 No threads to run: infinite loop or deadlock?"
                   ErrorCall s -> return s
                   other       -> return$ show ex
                with' (val log_separator') (log_separator'=:) $ \_ -> do
                  log_separator' =: ""
                  io$ condPrintLineLn "w" errmsg
                return ()

  -- Выполнить команду архиватора
  let runWithMsg ([formatStart,formatSuccess,formatFail],msgArgs,cmd) = do
        -- Сообщение о начале выполнения команды
        msgStart <- i18n formatStart
        postGUIAsync$ fmStackMsg fm' (formatn msgStart msgArgs)
        --postGUIAsync$ fmStackMsg fm' (unwords cmd)
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
      mapM_ (myHandleErrors runWithMsg) commands
      whenM (isEmptyChan cmdChan)$ postGUIAsync$ do widgetHide windowProgress; refreshCommand fm'
      --uiDoneProgram
  let exec = writeChan cmdChan


----------------------------------------------------------------------------------------------------
---- Меню Commands ---------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

  -- Упаковка данных
  addAct `onActionActivate` do
    addDialog fm' exec "a" NoMode

  -- Распаковка архив(ов)
  extractAct `onActionActivate` do
    archiveOperation fm' $
      extractDialog fm' exec "x"
    rereadHistory curdir

  -- Тестирование архив(ов)
  testAct `onActionActivate` do
    archiveOperation fm' $
      extractDialog fm' exec "t"

  -- Информация об архиве
  arcinfoAct `onActionActivate` do
    msgboxOnError $
      archiveOperation fm' $
        arcinfoDialog fm' exec NoMode

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


----------------------------------------------------------------------------------------------------
---- Меню Tools ------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

  -- Защитить архив от записи
  lockAct `onActionActivate` do
    multiArchiveOperation fm' $ \archives -> do
      let msg = "0299 Lock archive(s)?"
      whenM (askOkCancel window (formatn msg [head archives, show3$ length archives])) $ do
        closeFMArc fm'
        for archives $ \arcname -> do
          exec [(["0300 Locking archive %1",
                  "0301 SUCCESFULLY LOCKED ARCHIVE %1",
                  "0302 %2 WARNINGS WHILE LOCKING ARCHIVE %1"],
                 [takeFileName arcname],
                 ["ch", "--noarcext", "-k", "--", arcname])]

  -- Изменить комментарий архива
  commentAct `onActionActivate` do
    msgboxOnError $
      archiveOperation fm' $
        arcinfoDialog fm' exec CommentMode

  -- Преобразовать архив в SFX
  recompressAct `onActionActivate` do
    addDialog fm' exec "ch" RecompressMode

  -- Преобразовать архив в SFX
  toSfxAct `onActionActivate` do
    addDialog fm' exec "ch" MakeSFXMode

  -- Зашифровать архив
  encryptAct `onActionActivate` do
    addDialog fm' exec "ch" EncryptionMode

  -- Добавить RR в архив
  addRrAct `onActionActivate` do
    addDialog fm' exec "ch" ProtectionMode

  -- Восстановить повреждённый архив
  repairAct `onActionActivate` do
    multiArchiveOperation fm' $ \archives -> do
      let msg = "0381 Repair archive(s)? Repaired archive(s) will be placed into files named fixed.*"
      whenM (askOkCancel window (formatn msg [head archives, show3$ length archives])) $ do
        closeFMArc fm'
        for archives $ \arcname -> do
          exec [(["0382 Repairing archive %1",
                  "0383 SUCCESFULLY REPAIRED ARCHIVE %1",
                  "0384 %2 WARNINGS WHILE REPAIRING ARCHIVE %1"],
                 [takeFileName arcname],
                 ["r", "--noarcext", "--", arcname])]

  -- Модификация архивов
  modifyAct `onActionActivate` do
    addDialog fm' exec "ch" NoMode

  -- Объединение архивов
  joinAct `onActionActivate` do
    addDialog fm' exec "j" NoMode


----------------------------------------------------------------------------------------------------
---- Меню Options ----------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

  -- Окно настроек программы
  settingsAct `onActionActivate` do
    settingsDialog fm'

  -- Действия с логфайлом
  let withLogfile action = do
        logfileHist <- fmGetHistory fm' "logfile"
        case logfileHist of
          logfile:_ | logfile>""  ->  action logfile
          _                       ->  fmErrorMsg fm' "0303 No log file specified in Settings dialog!"

  -- Просмотреть логфайл
  viewLogAct `onActionActivate` do
    withLogfile runViewCommand

  -- Удалить логфайл
  clearLogAct `onActionActivate` do
    withLogfile $ \logfile -> do
      msg <- i18n"0304 Clear logfile %1?"
      whenM (askOkCancel window (format msg logfile)) $ do
        filePutBinary logfile ""


----------------------------------------------------------------------------------------------------
---- Меню Help -------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

  -- Home/news page for the current locale
  homeURL  <- ((aARC_WEBSITE ++ "/") ++) ==<< i18n"0254 "
  newsURL  <- ((aARC_WEBSITE ++ "/") ++) ==<< i18n"0255 News.aspx"
  forumURL <- ("http://apps.sourceforge.net/phpbb/freearc/" ++) ==<< i18n"0371 viewforum.php?f=3"
  wikiURL  <- ("http://freearc.wiki.sourceforge.net/"       ++) ==<< i18n"0372 "

  -- Открыть URL
  let openWebsite url  =  runFile url "." False

  -- Открыть файл помощи
  let openHelp helpfile = do
        doc  <- i18n helpfile
        file <- findFile libraryFilePlaces ("../Documentation" </> doc)
        case file of
          "" -> return ()
          _  -> openWebsite file

  -- Прочитать ИД, сгенерённый для этого компьютера
  let getUserID = do
#ifndef FREEARC_WIN
        -- Имитация неработающего Windows Registry для других ОС
        let registryGetStr root branch key       = return Nothing
            registrySetStr root branch key value = return ()
            hKEY_LOCAL_MACHINE                   = ()
#endif
        -- Сначала ищем его в ини-файле
        userid <- fmGetHistory1 fm' "UserID" ""
        if userid/=""  then return (Just userid)  else do
        -- Если не получилось - читаем ключ предыдущей инсталляции из Windows Registry...
        userid <- do userid <- registryGetStr hKEY_LOCAL_MACHINE "SOFTWARE\\FreeArc" "UserID"
                     case userid of
                       Just userid -> return userid
                                      -- ... или в крайнем случае - генерим новый
                       Nothing     -> generateRandomBytes 8 >>== encode16
        -- И записываем его повсюду
        registrySetStr hKEY_LOCAL_MACHINE "SOFTWARE\\FreeArc" "UserID" userid
        fmReplaceHistory fm' "UserID" userid
        -- Возвращаем его только если запись в ини-файл была успешной
        userid1 <- fmGetHistory1 fm' "UserID" ""
        return (if userid==userid1  then Just userid  else Nothing)

  -- Возвращает True раз в сутки
  let daily = do
        last <- fmGetHistory1 fm' "LastCheck" ""
        now  <- getUnixTime
        let day = round$ 24.37*60*60
        if  last>""  &&  (now - readI last < day)  then return False  else do
        fmReplaceHistory fm' "LastCheck" (show now)
        now1 <- fmGetHistory1 fm' "LastCheck" ""
        return (show now==now1)

  -- Size of maximum memory block we can allocate in bytes
  maxBlock <- getMaxMemToAlloc

  -- Регистрирует использование программы и проверяет новости
  --  (manual=True - ручной вызов из меню, False - ежедневная автопроверка)
  let checkNews manual = do
        fmStackMsg fm' "0295 Checking for updates..."
        forkIO_ $ do
          -- Сообщим об использовании программы
          whenJustM_ getUserID $ \userid -> do
#ifdef FREEARC_WIN
            si <- getSystemInfo; let ramLimit = showMem (si.$siMaximumApplicationAddress.$ptrToWordPtr.$toInteger `roundTo` (4*mb))
#endif
            language <- i18n"0000 English"
            let url = aARC_WEBSITE ++ "/CheckNews.aspx?user=" ++ userid ++ "&version=" ++ urlEncode aARC_VERSION
                                   ++ "&OS%20family=" ++ iif isWindows "Windows" "Unix"
                                   ++ "&RAM=" ++ showMem (toInteger getPhysicalMemory `roundTo` (4*mb))
#ifdef FREEARC_WIN
                                   ++ "&address%20space=" ++ ramLimit
#endif
                                   ++ "&largest%20memory%20block=" ++ showMem (maxBlock `roundDown` (100*mb))
                                   ++ "&number%20of%20cores=" ++ show getProcessorsCount
                                   ++ "&language=" ++ urlEncode language
            --gui$ fmStackMsg fm' url
            ignoreErrors (fileGetBinary url >> return ())
          -- Проверим страницу новостей
          handleErrors
            -- Выполняется при недоступности страницы новостей
            (gui $ do
                msg <- i18n"0296 Cannot open %1. Do you want to check the page with browser?"
                whenM (askOkCancel window (format msg newsURL)) $ do
                  openWebsite newsURL)
            -- Попытка прочитать страницу новостей
            (fileGetBinary newsURL >>== (`showHex` "").crc32) $ \new_crc -> do
          -- Страница новостей успешно прочитана
          old_crc <- fmGetHistory1 fm' "news_crc" ""
          gui $ do
          fmStackMsg fm' ""
          if (new_crc == old_crc) then do
             msg <- i18n"0297 Nothing new at %1"
             manual &&& fmInfoMsg fm' (format msg newsURL)
           else do
             fmReplaceHistory fm' "news_crc" new_crc
             msg <- i18n"0298 Found new information at %1! Open the page with browser?"
             whenM (askOkCancel window (format msg newsURL)) $ do
               openWebsite newsURL

  -- Дважды в час проверять отсутствие новостей
  forkIO_ $ do
    whenM (fmGetHistoryBool fm' "CheckNews" True) $ do
      foreverM $ do
        whenM daily $ do
          checkNews False
        threadDelay (30*60*1000000::Int)  -- 60 minutes is too much for Int!


  -- Помощь по использованию GUI
  helpAct `onActionActivate` do
    openHelp "0256 FreeArc-GUI-Eng.htm"

  -- Помощь по использованию командной строки
  helpCmdAct `onActionActivate` do
    openHelp "0257 FreeArc036-eng.htm"

  -- Домашняя страница программы
  homepageAct `onActionActivate` do
    openWebsite homeURL

  -- Домашняя страница программы
  openForumAct `onActionActivate` do
    openWebsite forumURL

  -- Домашняя страница программы
  openWikiAct `onActionActivate` do
    openWebsite wikiURL

  -- Проверка обновлений на сайте
  whatsnewAct `onActionActivate` do
    checkNews True

  -- Диалог About
  aboutAct `onActionActivate` do
    bracketCtrlBreak "aboutDialogDestroy" aboutDialogNew widgetDestroy $ \dialog -> do
    dialog `set` [windowTransientFor   := window
                 ,aboutDialogName      := aARC_NAME
                 ,aboutDialogVersion   := aARC_VERSION_WITH_DATE
                 ,aboutDialogCopyright := "(c) "++aARC_EMAIL
                 ,aboutDialogComments  := "High-performance archiver"
                 ,aboutDialogWebsite   := homeURL
                 ,aboutDialogAuthors   := ["Igor Pavlov (author of LZMA and EXE filter)"
                                          ,"Dmitry Shkarin (author of PPMd)"
                                          ,"Ilya Grebnov (author of GRZipII and LZP filter)"
                                          ,"Alexander Djourik and Pavel Zhilin (authors of TTA)"
                                          ,"Dmitry Subbotin (author of Carryless rangecoder)"
                                          ,"Joachim Henke (coauthor of Tornado)"
                                          ,"Mark Shevchenko (author of GUI SFX and web site)"
                                          ,aARC_EMAIL++" (author of remaining parts)"
                 ]]
    dialogRun dialog
    return ()

  -- Включить поддержку URL в диалоге About
  aboutDialogSetUrlHook openWebsite


  -- Инициализируем состояние файл-менеджера каталогом/архивом, заданным в командной строке (при его отсутствии - текущим каталогом)
  terminateOnError $
    chdir fm' (head (args++["."]))
  fmStatusBarTotals fm'

  return window

