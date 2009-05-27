-- Suffix for FreeArc archives
arcext = ".arc"

-- Suffix for FreeArc SFX-es
exeext = ".exe"

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
  ext      = string.lower(get_ext(nameext))
  subdir   = quote(basename..DIR_SEPARATOR)
  filename = quote(nameext)

  -- Menu items for Compresion operations - the only menu items for non-archive files
  if #filenames==1 then
    arcbase = nameext
    add_options = ""   -- "-ep1": disabled due to bug in FreeArc
  else
    arcbase = drop_dir(path) or "default"
    filename = ""
    for i,_ in ipairs(filenames) do
      filename = filename.." "..quote(drop_dir(filenames[i]))
    end
    add_options = ""
  end
  arcname = quote(arcbase..arcext)
  sfxname = quote(arcbase..exeext)
  compress_item = {text = "Add to "..arcname,  command = freearc.." a --noarcext "..add_options.." -- "..arcname.." "..filename,  help = "Compress the selected files using FreeArc"}
  compress_sfx_item = {text = "Add to SFX "..sfxname,  command = freearc.." a -sfx --noarcext "..add_options.." -- "..sfxname.." "..filename,  help = "Compress the selected files into SFX using FreeArc"}
  menu = {compress_item, compress_sfx_item}

  -- If single archive is selected - add more items to menu
  if #filenames==1  then
    -- FreeArc archive
    if string.match(ext,"^"..archive_extensions.."$") then
      menu = {
        {text = "Open with &FreeArc",   command = freearc.." "..filename,                      help = "Open the selected archive(s) with FreeArc"},
        {text = "Extract to "..subdir,  command = freearc.." x -ad --noarcext -- "..filename,  help = "Extract the selected archive(s) to new folder"},
        {text = "Extract here",         command = freearc.." x --noarcext -- "..filename,      help = "Extract the selected archive(s) to the same folder"},
        {text = "Test",                 command = freearc.." t --noarcext -- "..filename,      help = "Test the selected archive(s)"},
        {text = "Convert to SFX",       command = freearc.." s --noarcext -- "..filename,      help = "Convert the selected archive(s) to SFX"},
        compress_item, compress_sfx_item
      }

    -- rar/7z/zip/tar.gz/tar.bz2 archive
    elseif convert_enabled and (ext=="rar" or ext=="7z" or ext=="zip" or string.match(string.lower(nameext),"[.]tar[.]bz2$") or string.match(string.lower(nameext),"[.]tar[.]gz$")) then
      menu = {
        {text = "Convert to .arc",  command = all2arc.." "..filename,  help = "Convert selected archive(s) to FreeArc format"},
        compress_item, compress_sfx_item
      }
    end
  end

  if cascaded then
    menu = { {text = "FreeArc",  submenu = menu,  help = "FreeArc commands"} }
  end


  return menu
end)


--os.execute ("start "..arg)
