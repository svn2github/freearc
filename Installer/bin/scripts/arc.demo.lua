-- FreeArc script: demonstration of various events and their parameters

print "Hi from Lua!"

onProgramStart (function (params)
  printInfo("onProgramStart", params)
end)

onProgramDone (function (params)
  printInfo("onProgramDone", params)
end)

onCommandStart (function (params)
  printInfo("onCommandStart", params)
end)

onCommandDone (function (params)
  printInfo("onCommandDone", params)
end)

onArchiveStart (function (params)
  printInfo("onArchiveStart", params)
end)

onArchiveDone (function (params)
  printInfo("onArchiveDone", params)
end)

onError (function (params)
  printInfo("onError", params)
end)

onWarning (function (params)
  printInfo("onWarning", params)
end)

function printInfo(header,params)
  print(header)
  for i,v in pairs(params) do
    print("  ",i," => ",v)
  end
end

