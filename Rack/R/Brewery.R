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
