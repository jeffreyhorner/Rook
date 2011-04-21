Redirect <- setRefClass(
    'Redirect',
    fields = c('url'),
    methods = list(
	initialize = function(url,...){
	    url <<- url
	    callSuper(...)
	},
	call = function(env){
	    req <- Rook::Request$new(env)
	    res <- Rook::Response$new()
	    res$redirect(req$to_url(url))
	    res$finish()
	}
    )
)

