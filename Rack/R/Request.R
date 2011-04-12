Request <- setRefClass(
    'Request',
    fields = c('FORM_DATA_MEDIA_TYPES','PARSEABLE_DATA_MEDIA_TYPES','env'),
    methods = list(
	initialize = function(env,...){
	    env <<- env

	    # The set of form-data media-types. Requests that do not indicate
	    # one of the media types presents in this list will not be eligible
	    # for form-data / param parsing.
	    FORM_DATA_MEDIA_TYPES <<- c(
	      'application/x-www-form-urlencoded',
	      'multipart/form-data'
	    )

	    # The set of media-types. Requests that do not indicate
	    # one of the media types presents in this list will not be eligible
	    # for param parsing like soap attachments or generic multiparts.
	    PARSEABLE_DATA_MEDIA_TYPES <<- c(
	      'multipart/related',
	      'multipart/mixed'
	    )

	    callSuper(...)
	},
	body = function()            env[["rack.input"]],
	scheme = function()          env[["rack.url_scheme"]],
	script_name = function()     env[["SCRIPT_NAME"]],
	path_info = function()       env[["PATH_INFO"]],
	port = function()            as.integer(env[["SERVER_PORT"]]),
	request_method = function()  env[["REQUEST_METHOD"]],
	query_string = function()    env[["QUERY_STRING"]],
	content_length = function()  env[['CONTENT_LENGTH']],
	content_type = function()    env[['CONTENT_TYPE']],
	media_type = function(){
	    if (is.null(content_type())) return(NULL)
	    tolower(strsplit(content_type(),'\\s*[;,]\\s*')[[1]][1])
	},
	media_type_params = function(){
	    if (is.null(content_type())) return(NULL)
	    params <- list()
	    for(i in strsplit(content_type(),'\\s*[;,]\\s*')[[1]][-1]){
		x <- strsplit(i,'=')[[1]]
		params[[tolower(x[1])]] <- x[2]
	    }
	    params
	},
	content_charset = function() media_type_params()[['charset']],
	host_with_port = function(){
	    if(exists('HTTP_X_FORWARDED_HOST',env)){
		x <- strsplit(env[['HTTP_X_FORWARDED_HOST']],',\\s?')[[1]]
		return(x[length(x)])
	    } else if (exists('HTTP_HOST',env)){
		env[['HTTP_HOST']]
	    } else {
		if (exists('SERVER_NAME',env))
		    host <- env[['SERVER_NAME']]
		else
		    host <- env[['SERVER_ADDR']]
		paste(host,env[['SERVER_PORT']],sep=':')
	    }
	},
	host = function() sub(':\\d+','',host_with_port(),perl=TRUE),
	script_name = function(s=NULL){
	    if (!is.null(s) && is.character(s)) env[['SCRIPT_NAME']] <<- s
	    env[['SCRIPT_NAME']]
	},
	path_info = function(s=NULL){
	    if (!is.null(s) && is.character(s)) env[['PATH_INFO']] <<- s
	    env[['PATH_INFO']]
	},
	delete = function()	request_method() == 'DELETE',
	get = function()	request_method() == 'GET',
	head = function()	request_method() == 'HEAD',
	options = function()	request_method() == 'OPTIONS',
	post = function()	request_method() == 'POST',
	put = function()	request_method() == 'PUT',
	trace = function()	request_method() == 'TRACE',
	form_data = function(){
	    (post() && !is.null(media_type())) || any(FORM_DATA_MEDIA_TYPES==media_type())
	},
	parseable_data = function(){
	    any(PARSEABLE_DATA_MEDIA_TYPES==media_type())
	},
	GET = function(){
	    if (!exists('rack.request.query_list',env))
		env[['rack.request.query_list']] <<- Utils$parse_query(query_string())
	    env[['rack.request.query_list']]
	},
	POST = function(){
	    if (!exists('rack.input',env))
		stop("Missing rack.input")
	    if (exists('rack.request.form_list',env))
		env[['rack.request.form_list']]
	    else if (form_data() || parseable_data()){
		env[['rack.request.form_list']] <<- Multipart$parse(env)
		if (length(env[['rack.request.form_list']]) == 0){
		    form_vars <- env[['rack.input']]$read()
		    cat('form_vars',rawToChar(form_vars),'\n')
		    env[['rack.request.form_list']] <<- Utils$parse_query(rawToChar(form_vars))
		}
	    }
	    env[['rack.request.form_list']]
	},
	params = function() c(GET(),POST()) ,
	referer = function(){
	    if (!is.null(env[['HTTP_REFERER']])) env[['HTTP_REFERER']] else '/'
	},
	referrer = function() referer(),
	user_agent = function() env[['HTTP_USER_AGENT']],
	cookies = function(){ },
	xhr = function() {
	    (exists('HTTP_X_REQUESTED_WITH',env) && 
	    env[['HTTP_X_REQUESTED_WITH']] == 'XMLHttpRequest')
	},
	url = function(){
	    x <- paste(scheme(),'://',host(),sep='')
	    if ( (scheme() == 'https' && port() != 443) || (scheme() == 'http' && port() != 80))
		x <- paste(x,':',port(),sep='')
	    x <- paste(x,fullpath(),sep='')
	    x
	},
	path = function() paste(script_name(),path_info(),sep=''),
	fullpath = function(){
	    if (is.null(query_string()))
		path()
	    else
		paste(path(),'?',query_string(),sep='')
	},
	to_url = function(url,...) {
	    newurl <- paste(script_name(),url,sep='')
	    opt <- list(...)
	    if (length(opt)){
		    newurl <- paste(
			newurl,'?',
			paste(names(opt),opt,sep='=',collapse='&'),
			sep=''
		    )
	    }
	    newurl
	},
	accept_encoding = function(){
	},
	ip = function() env[['REMOTE_ADDR']]
    )
)
