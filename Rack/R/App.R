# Abstract Rack::App and Rack::Middleware that Builder and related apps inherit from.
setRefClass(
    'App',
    fields = c('app'),
    methods = list(
	initialize = function(app=function(){},...) {
	    app <<- app
	    callSuper(...)
	},
	call = function(env){
	    if (is(app,'refClass')) app$call(env)
	    else if (is(app,'function')) app(env)
	    else stop('App not Rack aware')
	}
    )
)

setRefClass(
    'Middleware',
    methods = list(
	initialize = function(...) {
	    callSuper(...)
	},
	set_app = function(app){
	    app <<- app
	}
    )
)
