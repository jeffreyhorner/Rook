Redirect <- setRefClass(
    'Redirect',
    fields = c('url'),
    methods = list(
	initialize = function(url,...){
	    url <<- url
	    callSuper(...)
	},
	call = function(env){
	    req <- Rack::Request$new(env)
	    res <- Rack::Response$new()
	    res$redirect(req$to_url(url))
	    res$finish()
	}
    )
)

