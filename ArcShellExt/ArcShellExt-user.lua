-- Suffix for FreeArc archives
arcext = ".arc"

-- Suffix for FreeArc SFX-es
exeext = ".exe"

-- FreeArc archive extensions regexp
archive_extensions = "arc"
-- archive_extensions = ".*"   -- match any file

-- FreeArc SFX extensions regexp
sfx_extensions = "exe"



-- User-defined function that returns Right-Click Menu items for given filename(s)
--   filenames: array of UTF8-encoded filenames selected in Explorer
--   return value: array of menu items, each item is structure with the following fields:
--                   for commands: text, command, help
--                   for submenus: text, submenu, help (where submenu is, recursively, array of menu items)
register_menu_handler (function (filenames)
  nameext  = drop_dir(filenames[1])
  path     = get_dir(filenames[1])
  basename = drop_ext(nameext)
  ext      = string.lower(get_ext(nameext))

  -- Menu items for Compresion operations - the only menu items for non-archive files
  if #filenames==1 then
    arcbase     = nameext
    subdir      = quote(basename..DIR_SEPARATOR)
    add_options = ""   -- "-ep1": disabled due to bug in FreeArc
    filename    = quote(nameext)
  else
    arcbase     = drop_dir(path) or "default"
    subdir      = quote("*"..DIR_SEPARATOR)
    add_options = ""
    filename    = ""
    for _,f in ipairs(filenames) do
      filename = filename.." "..quote(drop_dir(f))
    end
  end
  arcname = quote(arcbase..arcext)
  sfxname = quote(arcbase..exeext)
  compress_item     = {text = "Add to "    ..arcname,  command = freearc.." a --noarcext "     ..add_options.." -- "..arcname.." "..filename,  help = "Compress the selected files using FreeArc"}
  compress_sfx_item = {text = "Add to SFX "..sfxname,  command = freearc.." a -sfx --noarcext "..add_options.." -- "..sfxname.." "..filename,  help = "Compress the selected files into SFX using FreeArc"}
  menu = {compress_item, compress_sfx_item}

  -- SFX-related commands
  to_sfx_item   = {text = "Convert to SFX",    command = multi_command (freearc, " s --noarcext -- ",  filenames),  help = "Convert the selected archive(s) to SFX"}
  from_sfx_item = {text = "Convert from SFX",  command = multi_command (freearc, " s- --noarcext -- ", filenames),  help = "Convert the selected SFX(es) to normal archive(s)"}

  -- Check that all files selected are archives, SFX-es or non-FreeArc archives
  all_arcs     = 1
  all_sfxes    = 1
  all_archives = 1
  all_zips     = 1
  for _,f in ipairs(filenames) do
    nameext  = drop_dir(f)
    ext      = string.lower(get_ext(nameext))
    is_arc = string.match(ext,"^"..archive_extensions.."$")
    is_sfx = string.match(ext,"^"..sfx_extensions.."$")  and  check_for_sfx(f)
    is_zip = ext=="rar" or ext=="7z" or ext=="zip" or string.match(string.lower(nameext),"[.]tar[.]bz2$") or string.match(string.lower(nameext),"[.]tar[.]gz$") or string.match(string.lower(nameext),"[.]tar[.]lzma") or string.match(string.lower(nameext),"[.]tar[.]z$")
    all_arcs     = all_arcs     and is_arc
    all_sfxes    = all_sfxes    and is_sfx
    all_archives = all_archives and (is_arc or is_sfx)
    all_zips     = all_zips     and is_zip
    if not (all_archives or all_zips) then break end
  end

  -- If only FreeArc archives are selected - provide appropriate menu
  if all_archives then
    menu = {
      {text = #filenames==1 and "Open with &FreeArc",   command = freearc.." "..filename,                     help = "Open the selected archive(s) with FreeArc"},
      {text = "Extract to "..subdir,  command = multi_command (freearc, " x -ad --noarcext -- ", filenames),  help = "Extract the selected archive(s) to new folder"},
      {text = "Extract here",         command = multi_command (freearc, " x --noarcext -- ", filenames),      help = "Extract the selected archive(s) to the same folder"},
      {text = "Test",                 command = multi_command (freearc, " t --noarcext -- ", filenames),      help = "Test the selected archive(s)"},
      all_arcs  and to_sfx_item,     -- Provide "Convert to SFX" command if non-SFXes are selected
      all_sfxes and from_sfx_item    -- Provide "Convert from SFX" command if SFXes are selected
    }

  -- If only rar/7z/zip/tar.gz/tar.bz2 archives are selected - provide appropriate menu
  elseif convert_enabled and all_zips then
    menu = {
      {text = "Convert to .arc",      command = all2arc.."      -- "..filename,  help = "Convert selected archive(s) to FreeArc format"},
      {text = "Convert to .arc SFX",  command = all2arc.." -sfx -- "..filename,  help = "Convert selected archive(s) to FreeArc SFX"}
    }
  end

  if cascaded then
    menu = { {text = "FreeArc",  submenu = menu,  help = "FreeArc commands"} }
  end


  return menu
end)


--os.execute ("start "..arg)
