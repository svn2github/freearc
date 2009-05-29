
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
  menu_up = 0
  make_menu = function(menu)
    for _,item in pairs(menu) do
      if item and item[1] then
        make_menu (item)    -- recursive call to handle menu items array

      elseif item and item.text and item.help then -- skip empty menu items
        if item.submenu  then menu_down=1  else menu_down=0 end
        i = add_menu_item (item.text, menu_down, menu_up)
        items[i] = item
        menu_up = 0
        if item.submenu then
          make_menu (item.submenu)    -- recursive call to handle submenu
          menu_up = menu_up+1
        end
      end
    end
  end
  make_menu(menu)
end

-- function called from C to get help on menu item i
function get_help(i)
  return items[i].help
end

-- function called from C to get command for menu item i
function get_command(i)
  return items[i].command
end

-- Enabled commands information
command = {}
setmetatable(command, {__index = function(table, key)
                                   return {}
                                 end})



-- Auxiliary functions ---------------------------------------------

-- OS-dependent directory separator
DIR_SEPARATOR = package.config:sub(1,1)

-- Return filename directory: c:\dir\file.ext -> c:\dir
function get_dir(filename)
  return (string.match (filename, "(.*)"..DIR_SEPARATOR..".+"))
end

-- Return filename without directory: c:\dir\file.ext -> file.ext
function drop_dir(filename)
  return (string.match (filename, ".*"..DIR_SEPARATOR.."(.+)"))
end

-- Return extension: file.ext -> ext
-- Filename passed shouldn't contain path!
function get_ext(filename)
  return string.match (filename, ".+[.](.*)") or ""
end

-- Drop extension: file.ext -> file
-- Filename passed shouldn't contain path!
function drop_ext(filename)
  return string.match (filename, "(.+)[.].*") or filename
end

-- Add "" to string passed
function quote(filename)
  return "\""..filename.."\""
end


-- FreeArc-specific functions -----------------------------------------

-- Check for SFX
function check_for_sfx(filename)
  f = io.open(filename, "rb")
  f:seek("end", -256)
  data = f:read(256)
  f:close()
  return (string.find (data, "ArC\1", 1, true))
end

-- Build multi-action FreeArc command
function multi_command(command, options, filenames)
  for i,f in ipairs(filenames) do
    if i>1 then command = command .. " ;" end
    command = command .. options .. quote(f)
  end
  return command
end

