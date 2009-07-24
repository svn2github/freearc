require("tabi")

function server(e)                   -- e is a string encoding all parameters passed to function
  local p = tabi.decode(e)           -- p is a table, decoded from e
  local i = p.i                      -- required parameter
  local s = p.s or ""                -- optional parameter with default value
  local c = string.sub(s,i,1)
  return tabi.encode(c)              -- return value is a string encoding single value returned from function
end

function client()
  local p = {i=1,s="str"}            -- p is a table with all parameters to pass
  local r = server(tabi.encode(p))   -- we encode p to string, and pass it to server()
  print (tabi.decode(r)[""])         -- r is string encoding server answer: we decode it to table and get "default" value
end

client()
