tabi = {
  decode = function(s)
    local t = {}
    for pair in string.gmatch(s,"[^,]+") do
      k,v = string.match(pair,"(.*)=(.*)")
      t[k] = v
    end
    return t
  end,

  encode = function(t)
    if type(t)~="table" then return tabi.encode{[""]=t} end
    local s = ""
    for k,v in pairs(t) do
      s = s .. "," .. k .. "=" .. v
    end
    return string.sub(s,2)
  end
}


