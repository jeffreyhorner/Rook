Builder <- setRefClass(
    'Builder',
    contains = 'App',
    methods = list(
	initialize = function(...){
	    objs <- list(...)
	    if (length(objs) > 1){
		for (i in 1:(length(objs)-1)){
		    objs[[i]]$set_app(objs[[i+1]])
		}
	    }
	    callSuper(app=objs[[1]])
	}
    )
)
