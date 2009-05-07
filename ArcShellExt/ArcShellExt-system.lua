
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
