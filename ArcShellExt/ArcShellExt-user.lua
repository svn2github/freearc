-- This file uses UTF8 encoding without BOM

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
    subdir      = basename..DIR_SEPARATOR
    add_options = ""   -- "-ep1": disabled due to bug in FreeArc
    filename    = quote(nameext)
  else
    arcbase     = drop_dir(path) or "default"
    subdir      = "*"..DIR_SEPARATOR
    add_options = ""
    filename    = ""
    for _,f in ipairs(filenames) do
      filename = filename.." "..quote(drop_dir(f))
    end
  end
  arcname = arcbase..arcext
  sfxname = arcbase..exeext
  menu = {
    append (command.add2arc,  {param = arcname,  command = freearc.." a --noarcext "     ..add_options.." -- \"%s\" "..filename}),
    append (command.add2sfx,  {param = sfxname,  command = freearc.." a -sfx --noarcext "..add_options.." -- \"%s\" "..filename}),
  }

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
      #filenames==1 and append (command.open,         {                  command = freearc.." "..filename}),
                        append (command.extractTo,    {param = subdir,   command = multi_command (freearc, " x -ad --noarcext -- ", filenames)}),
                        append (command.extractHere,  {                  command = multi_command (freearc, " x --noarcext -- ", filenames)}),
                        append (command.test,         {                  command = multi_command (freearc, " t --noarcext -- ", filenames)}),
      all_arcs      and append (command.arc2sfx,      {                  command = multi_command (freearc, " s --noarcext -- ",  filenames)}),
      all_sfxes     and append (command.sfx2arc,      {                  command = multi_command (freearc, " s- --noarcext -- ", filenames)}),
      #filenames>1  and append (command.help,         {param = arcname,  command = freearc.." j --noarcext -- \"%s\" "..filename}),
    }

  -- If only rar/7z/zip/tar.gz/tar.bz2 archives are selected - provide appropriate menu
  elseif all_zips then
    menu = {
      append (command.zip2arc,  {command = all2arc.."      -- "..filename}),
      append (command.zip2sfx,  {command = all2arc.." -sfx -- "..filename}),
    }
  end

  if cascaded then
    menu = { {text = "FreeArc",  submenu = menu,  help = "FreeArc commands"} }
  end


  return menu
end)


--os.execute ("start "..arg)
