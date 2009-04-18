----------------------------------------------------------------------------------------------------
---- FreeArc archive manager: utility functions                                               ------
----------------------------------------------------------------------------------------------------
module FileManUtils where

import Prelude hiding (catch)
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Char
import Data.IORef
import Data.List
import Data.Maybe

import Graphics.UI.Gtk
import Graphics.UI.Gtk.ModelView as New

import Utils
import Errors
import Files
import FileInfo
import Options
import UIBase
import UI
import ArhiveDirectory
import ArcExtract

----------------------------------------------------------------------------------------------------
---- Текущее состояние файл-менеджера --------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |Текущее состояние файл-менеджера: список выбранных файлов, общий список файлов и прочая информация
data FM_State = FM_State { fm_window       :: Window
                         , fm_view         :: TreeView
                         , fm_model        :: New.ListStore FileData
                         , fm_selection    :: TreeSelection
                         , fm_statusLabel  :: Label
                         , fm_messageCombo :: (New.ComboBox, IORef Int)
                         , fm_filelist     :: [FileData]
                         , fm_history_file :: MVar FilePath
                         , fm_history      :: Maybe [String]
                         , fm_onChdir      :: [IO()]
                         , fm_sort_order   :: String
                         , subfm           :: SubFM_State
                         }

-- |Текущее состояние файл-менеджера: информация об отображаемом архиве или каталоге диска
data SubFM_State = FM_Archive   { subfm_archive  :: ArchiveInfo
                                , subfm_arcname  :: FilePath
                                , subfm_arcdir   :: FilePath
                                , subfm_filetree :: FileTree FileData
                                }
                 | FM_Directory { subfm_dir      :: FilePath
                                }

-- |True, если FM сейчас показывает архив
isFM_Archive (FM_State {subfm=FM_Archive{}}) = True
isFM_Archive _                               = False

fm_archive = subfm_archive.subfm
fm_arcname = subfm_arcname.subfm
fm_arcdir  = subfm_arcdir .subfm
fm_dir     = subfm_dir    .subfm

-- |Текущий архив+каталог в нём или каталог на диске
fm_current fm | isFM_Archive fm = fm_arcname fm </> fm_arcdir fm
              | otherwise       = fm_dir     fm

-- |Текущий каталог, показываемый в FM, или каталог, в котором находится текущий архив
fm_curdir fm | isFM_Archive fm = fm_arcname fm .$takeDirectory
             | otherwise       = fm_dir     fm

-- |Изменить имя архива, открытого в FM
fm_changeArcname arcname fm@(FM_State {subfm=subfm@FM_Archive{}}) =
                         fm {subfm = subfm {subfm_arcname=arcname}}


----------------------------------------------------------------------------------------------------
---- Операции над именами каталогов/файлов ---------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |Текущее местопребывание в файл-менеджере: каталог внутри архива или на диске
data PathInfo path  =  ArcPath path path | DiskPath path | Not_Exists  deriving (Eq,Show)

isArcPath ArcPath{} = True
isArcPath _         = False

-- |Парсит текущее местопребывание в FM в структуру PathInfo
splitArcPath fm' fullname = do
  fm <- val fm'
  -- Сравним fullname с именем открытого в fm архива (arcname)
  -- Если arcname - префикс fullname, то разобьём fullname на имя архива arcname и каталог внутри него
  let arcname = isFM_Archive fm.$bool "!^%^@!%" (fm_arcname fm)
  if arcname `isParentDirOf` fullname
    then return$ ArcPath arcname (fullname `dropParentDir` arcname)
    else do
  -- Проверим существование каталога с таким именем (или "", чтобы избежать зацикливания)
  d <- not(isURL fullname) &&& io(dirExist fullname)
  if d || fullname=="" then return$ DiskPath fullname
    else do
  -- Проверим существование файла с таким именем
  f <- io(fileExist fullname)
  if f then return$ ArcPath fullname ""
    else do
  -- Повторим все проверки, отрезав от fullname последнюю компоненту имени
  res <- splitArcPath fm' (takeDirectory fullname)
  -- Если результат - каталог внутри архива, то добавим отрезанную компоненту к имени каталога
  -- Иначе же оригинальный fullname ссылался на несуществующий в природе файл
  case res of
    ArcPath  dir name | isURL(takeDirectory fullname) == isURL fullname  -- Проверяем что мы не обрезали URL по самые гланды :D
                      -> return$ ArcPath dir (name </> takeFileName fullname)
    _                 -> return$ Not_Exists


-- |Перевести путь, записанный относительно текущего дискового каталога в FM, в абсолютный
fmCanonicalizeDiskPath fm' relname = do
  let name  =  unquote (trimRight relname)
  if (name=="")  then return ""  else do
  fm <- val fm'
  io$ myCanonicalizePath$ fm_curdir fm </> name

-- |Перевести путь, записанный относительно текущего положения в FM, в абсолютный
fmCanonicalizePath fm' relname = do
  fm <- val fm'
  case () of
   _ | isURL relname                              ->  return relname
     | isAbsolute relname                         ->  myCanonicalizePath relname
     | isURL (fm_current fm) || isFM_Archive fm   ->  return$ urlNormalize (fm_current fm) relname    -- Использовать свой Normalize для навигацуи внутри архивов и по URL
     | otherwise                                  ->  myCanonicalizePath (fm_current fm </> relname)

-- |Нормализовать путь, записанный относительно некоего URL
urlNormalize url relname =  dropTrailingPathSeparator$ concat$ reverse$ remove$ reverse$ splitPath (url++[pathSeparator]) ++ splitPath relname
  where remove (".":xs)    = remove xs
        remove ("./":xs)    = remove xs
        remove (".\\":xs)    = remove xs
        remove ("..":x:xs) = remove xs
        remove ("../":x:xs) = remove xs
        remove ("..\\":x:xs) = remove xs
        remove (x:xs)      = x : remove xs
        remove []          = []


----------------------------------------------------------------------------------------------------
---- FileData и FileTree ---------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |Структура, хранящая всю необходимую нам информацию о файле
data FileData = FileData
  { fdPackedDirectory       :: !MyPackedString   -- Имя каталога
  , fdPackedBasename        :: !MyPackedString   -- Имя файла без каталога, но с расширением
  , fdSize  :: {-# UNPACK #-}  !FileSize         -- Размер файла (0 для каталогов)
  , fdTime  :: {-# UNPACK #-}  !FileTime         -- Дата/время создания файла
  , fdIsDir :: {-# UNPACK #-}  !Bool             -- Это каталог?
  }

fiToFileData fi = FileData { fdPackedDirectory = fpPackedDirectory (fiStoredName fi)
                           , fdPackedBasename  = fpPackedBasename  (fiStoredName fi)
                           , fdSize            = fiSize  fi
                           , fdTime            = fiTime  fi
                           , fdIsDir           = fiIsDir fi }

fdDirectory  =  myUnpackStr.fdPackedDirectory
fdBasename   =  myUnpackStr.fdPackedBasename

-- |Виртуальное поле: полное имя файла, включая каталог и расширение
fdFullname fd  =  fdDirectory fd </> fdBasename fd

-- |Имя файла. Должно быть fdFullname для поддержки режима "плоского вывода" архивов/деревьев файлов
fmname = fdBasename

-- |Возвращает искусственный каталог с базовым именем name
fdArtificialDir name = FileData { fdPackedDirectory = myPackStr ""
                                , fdPackedBasename  = myPackStr name
                                , fdSize            = 0
                                , fdTime            = aMINIMAL_POSSIBLE_DATETIME
                                , fdIsDir           = True }



-- |Дерево файлов. Включает список файлов на этом уровне плюс поименованные поддеревья
--                        files   dirname subtree
data FileTree a = FileTree [a]  [(String, FileTree a)]

-- |Возвращает количество каталогов в дереве
ftDirs  (FileTree files subdirs) = length (removeDups (subdirs.$map fst  ++  files.$filter fdIsDir .$map fdBasename))
                                 + sum (map (ftDirs.snd) subdirs)

-- |Возвращает количество файлов в дереве
ftFiles (FileTree files subdirs) = length (filter (not.fdIsDir) files)  +  sum (map (ftFiles.snd) subdirs)

-- |Возврашает список файлов в заданном каталоге,
-- используя отображение artificial для генерации псевдо-файлов из имён вложенных каталогов
ftFilesIn dir artificial = f (splitDirectories dir)
 where
  f (path0:path_rest) (FileTree _     subdirs) = lookup path0 subdirs.$ maybe [] (f path_rest)
  f []                (FileTree files subdirs) = (files++map (artificial.fst) subdirs)
                                                  .$ keepOnlyFirstOn (filenameLower.fmname)

-- |Превращает список файлов в дерево
buildTree x = x
  .$splitt 0                                  -- Разбиваем на группы по каталогам, начиная с 0-го уровня
splitt n x = x
  .$sort_and_groupOn (dirPart n)              -- Сортируем/группируем по имени каталога очередного уровня
  .$partition ((=="").dirPart n.head)         -- Отделяем группу с файлами, находящимися непосредственно в этом каталоге
  .$(\(root,other) -> FileTree (concat root)  -- Остальные группы обрабатываем рекурсивно на (n+1)-м уровне
                               (map2s (dirPart n.head, splitt (n+1)) other))

-- Имя n-й части каталога
dirPart n = (!!n).(++[""]).splitDirectories.fdDirectory

io=id
