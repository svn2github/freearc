-- 1 for cascaded menus, nil for flat
cascaded = 1

-- Path to FreeArc
freearc = "\"C:\\Program Files (x86)\\FreeArc\\bin\\FreeArc.exe\""

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
  nameext  = drop_dir(filenames[1])
  path     = get_dir(filenames[1])
  basename = drop_ext(nameext)
  ext      = get_ext(nameext)
  subdir   = basename..DIR_SEPARATOR
  filename = "\""..nameext.."\""

  -- Menu item for Compresion operation - the only menu item for non-archive files or multiple selection
  if #filenames==1 then
    arcname = "\""..nameext..arcext.."\""
    add_options = ""   -- "-ep1": disabled due to bug in FreeArc
  else
    arcname = "\""..drop_dir(path)..arcext.."\""
    filename = ""
    for i,_ in ipairs(filenames) do
      filename = filename.." \""..drop_dir(filenames[i]).."\""
    end
    add_options = ""
  end
  compress_item = {text = "Add to "..arcname,  command = freearc.." a --noarcext "..add_options.." -- "..arcname.." "..filename,  help = "Compress the selected files using FreeArc"}

  -- If single archive is selected - add more items to menu and optionally make it cascaded
  if #filenames==1  and  string.match(string.lower(ext), "^"..archive_extensions.."$") then
    menu = {
      {text = "Open with &FreeArc",   command = freearc.." "..filename,                      help = "Open the selected archive(s) with FreeArc"},
      {text = "Extract to "..subdir,  command = freearc.." x -ad --noarcext -- "..filename,  help = "Extract the selected archive(s) to new folder"},
      {text = "Extract here",         command = freearc.." x --noarcext -- "..filename,      help = "Extract the selected archive(s) to the same folder"},
      {text = "Test",                 command = freearc.." t --noarcext -- "..filename,      help = "Test the selected archive(s)"},
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