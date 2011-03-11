URLMap <- setRefClass(
    'URLMap',
    contains = 'Middleware',
    fields = c('map'),
    methods = list(
	initialize = function(...){
	    apps <- list(...)
	    map <<- list()
	    for (url in names(apps)){
		app <- apps[[url]]
		if (is(app,'function'))
		    map[[length(map)+1]] <<- App$new(app)
		else if (is_rackable(app))
		    map[length(map)+1] <<- app
		else
		    stop("App for url ",url," not rackable")
	    }
	    names(map) <<- names(apps)
	    callSuper()
	},
	call = function(env){
	    path <- env[['PATH_INFO']]
	    for (url in names(map)){
		if (length(grep(url,path)))
		    return(map[[url]]$call(env))
	    }
	    body <- paste("File not found:",path,"\n")
	    list(
		status=404L, 
		headers = list(
		    "Content-Type" = "text/plain",
		    "Content-Length" = as.character(nchar(body)),
		    "X-Cascade" = "pass"
		),
		body = body
	    )
	}
    )
)
