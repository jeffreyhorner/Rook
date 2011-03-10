library(brew)
library(Hmisc)

Brewery <- setRefClass(
    'Brewery',
    contains = 'Middleware',
    fields = c('url','root'),
    methods = list(
	initialize = function(url,root,...){
	    url <<- paste('^',url,sep='')
	    root <<- root
	    callSuper(...)
	},
	call = function(env){
	    req <- Rack::Request$new(env)
	    res <- Rack::Response$new()
	    brew_env <- new.env()
	    env[['req']] = req;
	    env[['res']] = res;
	    path = env[["PATH_INFO"]]
	    file_path = file.path(root,path)
	    cat('brewing',file_path,'\n')
	    if (length(grep(url,path))>0 && file.exists(file_path)){
		oldwd <- setwd(dirname(file_path))
		on.exit(setwd(oldwd))
		res$write(capture.output(brew(basename(file_path),envir=brew_env)))
		res$finish()
	    } else
		app$call(env)
	}
    )
)

app <- Rack::Builder$new(
    Rack::Static$new(
	urls = c('css','images','javascript'),
	root = '.'
    ),
    Brewery$new(url='brew',root='.'),
    Rack::App$new(function(env) {
	req <- Rack::Request$new(env)
	res <- Rack::Response$new()
	url <- paste(req$script_name(),'brew/useR2007.rhtml',sep='')
	cat('calling redirect to ',url,'\n')
	res$redirect(paste(req$script_name(),'brew/useR2007.rhtml',sep=''))
	res$finish()
    })
)
