-- 1 for cascaded menus, nil for flat
cascaded = 1

-- Path to FreeArc
freearc = "\"xC:\\Program Files (x86)\\FreeArc\\bin\\FreeArc.exe\" "

-- Suffix for FreeArc archives
arcext = ".arc"

-- FreeArc archive extensions regexp
archive_extensions = "arc"
-- archive_extensions = ".*"   -- match any file


-- User-defined function that returns Right-Click Menu items for given filename(s)
--   filenames: array of UTF8-encoded filenames selected in Explorer
--   return value: array of menu items, each item is structure with the following fields:
--                   for commands: text, command, help
--                   for submenus: text, submenu, help (where submenu is, recursively, array of menu elements)
register_menu_handler (function (filenames)
  --if filenames.getn
  filename     = "\""..filenames[1].."\""
  filename_arc = "\""..filenames[1]..arcext.."\""
  path     = get_path(filenames[1])
  nameext  = drop_path(filenames[1])
  basename = drop_ext(nameext)
  ext      = get_ext(nameext)
  subdir   = basename..DIR_SEPARATOR

  -- Menu item for Compresion operation - the only menu item for non-archive files
  compress_item = {text = "Add to "..nameext..arcext,  command = freearc.."a --noarcext -- "..filename_arc.." "..filename,  help = "Compress the selected files using FreeArc"}

  -- If this is an archive - add more items to menu and optionally make it cascaded
  if string.match(ext,"^"..archive_extensions.."$") then
    menu = {
      {text = "Open with &FreeArc",   command = freearc..filename,                          help = "Open the selected archive(s) with FreeArc"},
      {text = "Extract to "..subdir,  command = freearc.."x -ad --noarcext -- "..filename,  help = "Extract the selected archive(s) to new folder"},
      {text = "Extract here",         command = freearc.."x --noarcext -- "..filename,      help = "Extract the selected archive(s) to the same folder"},
      {text = "Test",                 command = freearc.."t --noarcext -- "..filename,      help = "Test the selected archive(s)"},
      compress_item
    }

    if cascaded then
      menu = { {text = "FreeArc",  submenu = menu,  help = "FreeArc commands"} }
    end
  else
    menu = {compress_item}
  end

  return menu
end)


--os.execute ("start "..arg)