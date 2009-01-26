-- FreeArc script: custom logging facility

-- Filename of custom logfile
logfile = "c:/temp/arc.custom.log"
a = "none"

onArchiveStart (function (params)
  a = params.arcname
end)

onError (function (params)
  printMessage("ERROR", params)
end)

onWarning (function (params)
  printMessage("WARNING", params)
end)

function printMessage(header,params)
  f = io.open(logfile, "a")
  f:write("--------   " .. os.date("%b %d %Y %X") .. ", archive " .. a .. "\n")
  f:write(header .. ": " .. params.message .. "\n\n")
  f:close()
end

