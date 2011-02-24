load(system.file('HTTPResponse.oneFile.RData',package='Rack'))
env$rack.input = RhttpdInputStream$new(postBody)
Multipart$parse(env)
