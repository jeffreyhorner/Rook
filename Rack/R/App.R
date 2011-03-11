is_rackable <- function(app){
    if (is(app,'refClass')) TRUE
    else if (is(app,'function')) TRUE
    else FALSE
}

# Abstract Rack::App and Rack::Middleware that Builder and related apps inherit from.
App <- setRefClass(
    'App',
    fields = c('app'),
    methods = list(
	initialize = function(app=NULL,...) {
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

Middleware <- setRefClass(
    'Middleware',
    contains = 'App',
    methods = list(
	initialize = function(...) {
	    callSuper(...)
	},
	set_app = function(app){
	    app <<- app
	}
    )
)
