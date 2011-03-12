library(brew)
library(Hmisc)

Brewery <- setRefClass(
    'Brewery',
    contains = 'Middleware',
    fields = c('url','root','opt'),
    methods = list(
	initialize = function(url,root,...){
	    url <<- paste('^',url,sep='')
	    root <<- root
	    opt <<- list2env(list(...))
	    callSuper()
	},
	call = function(env){
	    req <- Rack::Request$new(env)
	    res <- Rack::Response$new()
	    opt[['req']] <<- req;
	    opt[['res']] <<- res;
	    path = env[["PATH_INFO"]]
	    file_path = file.path(root,path)
	    if (length(grep(url,path))>0 && file.exists(file_path)){
		oldwd <- setwd(dirname(file_path))
		on.exit(setwd(oldwd))
		res$write(capture.output(brew(basename(file_path),envir=opt)))
		res$finish()
	    } else
		app$call(env)
	}
    )
)
dir.create(file.path(tempdir(),'plots'),showWarnings=FALSE)
app <- Rack::Builder$new(
    Rack::Static$new(
	urls = c('/css','/images','/javascript'),
	root = '.'
    ),
    Rack::Static$new(urls='/plots',root=tempdir()),
    Brewery$new(
	url='/brew',
	root='.',
	imagepath=file.path(tempdir(),'plots'),
	imageurl='../plots/'
    ),
    Rack::App$new(function(env) {
	req <- Rack::Request$new(env)
	res <- Rack::Response$new()
	res$redirect(req$to_url('/brew/useR2007.rhtml'))
	res$finish()
    })
)
