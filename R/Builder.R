Builder <- setRefClass(
    'Builder',
    contains = 'App',
    methods = list(
	initialize = function(...){
	    objs <- list(...)
	    if (length(objs) > 1){
		for (i in 1:(length(objs)-1)){
		    if (inherits(objs[[i]],'Middleware'))
			objs[[i]]$set_app(objs[[i+1]])
		    else
			stop("Argument ",i,"is not a Middleware object")
		}
	    }
	    callSuper(app=objs[[1]])
	}
    )
)
