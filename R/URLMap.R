URLMap <- setRefClass(
    'URLMap',
    fields = c('map'),
    methods = list(
	initialize = function(...){
	    apps <- list(...)
	    map <<- list()
	    for (url in names(apps)){
		x <- apps[[url]]
		if (is(x,'function'))
		    map[[length(map)+1]] <<- App$new(x)
		else if (is_rookable(x))
		    map[[length(map)+1]] <<- x
		else
		    stop("App for url ",url," not rookable")
	    }
	    names(map) <<- names(apps)
	    callSuper()
	},
	call = function(env){
	    path <- env[['PATH_INFO']]
	    for (url in names(map)){
	        #cat('matching url',url,'to',path,'\n')
		if (grepl(url,path))
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
