
user_menu_builder = nil
items = nil

-- system function that registers user functions that build and process menus
function register_menu_handler (user_func)
  user_menu_builder = user_func
end

-- system function that is called from C side to build the menu when user Right-Clicks file(s) in Explorer
function build_menu (...)
  filenames = arg  -- filenames selected in Explorer are passed as argument list

  -- Call user function to get list of menu items
  menu = user_menu_builder (filenames)

  -- Iterate menu items passing them to C function that really adds them to the menu
  items = {}
  for _,item in ipairs(menu) do
    i = add_menu_item (item.text)
    items[i] = item
  end
end

-- function called from C to get help on menu item i
function get_help(i)
  return items[i].help
end

-- function called from C to get command for menu item i
function get_command(i)
  return items[i].command
end




-- user-defined function that returns Right-Click Menu items for given filename(s)
register_menu_handler (function (filenames)
  --if filenames.getn    .. filenames[1]
  return {
    {text = "Lua: open with notepad",    command = "notepad \""..filenames[1].."\"",         help = "Open the selected file(s) with notepad"},
    {text = "Lua: open with wordpad",    command = "write   \""..filenames[1].."\"",         help = "Open the selected file(s) with wordpad"},
  }
--  return {
--    {text = "Open with &FreeArc",    command = "",                              help = "Open the selected archive(s) with FreeArc"},
--    {text = "Extract to new folder", command = "x -ad --noarcext --",           help = "Extract the selected archive(s) to new folder"},
--    {text = "Extract here",          command = "x --noarcext --",               help = "Extract the selected archive(s) to the same folder"},
--    {text = "Test",                  command = "t --noarcext --",               help = "Test the selected archive(s)"},
--    {text = "Compress with FreeArc", command = "a --noarcext -- default.arc",   help = "Compress the selected files using FreeArc"}
--  }
end)

