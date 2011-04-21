Static <- setRefClass(
    'Static',
    'contains' = 'Middleware',
    fields = c('urls','file_server'),
    methods = list(
	initialize = function(urls=c(),root=pwd(),...){
	    urls <<- paste('^',urls,sep='')
	    file_server <<- Rook::File$new(root)
	    callSuper(...)
	},
	call = function(env){
	    path <- env[["PATH_INFO"]]
	    if (any(sapply(urls,function(i)length(grep(i,path))>0)))
		file_server$call(env)
	    else
		app$call(env)
	}
    )
)
