URLMap <- setRefClass(
    'URLMap',
    contains = 'Middleware',
    fields = c('url','app'),
    methods = list(
	initialize = function(location,app,...){
	    map <<- map
	},
	call = function(env){
	}
    )
)
