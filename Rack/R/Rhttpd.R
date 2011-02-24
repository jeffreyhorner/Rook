RhttpdApp <- setRefClass(
    'RhttpdApp',
    fields = c('name','handler','path','handlerEnv','httpdOrig'),
    methods = list(
	initialize = function(handler=NULL,name=NULL,...){
	    if (!is.null(name) && !is.character(name))
		stop("Name must be a character")

	    if (is.function(handler)){
		.self$handler <- handler
		.self$name <- if (is.character(name)) name else as.character(substitute(handler))
		if (length(.self$name) > 1)
		    stop("Need a valid name for this function")
		.self$handlerEnv <- NULL
	    } else if (is.character(handler) && file.exists(handler)){
		.self$handler <- handler
		.self$name <- if (is.character(name)) name else sub('.[rRsS]$','',basename(handler),perl=TRUE)
		.self$handlerEnv <- new.env(parent=globalenv())
		sys.source(.self$handler,envir=.self$handlerEnv)
		if (!exists(.self$name,.self$handlerEnv))
		    stop("Cannot find ",name," in handler",handler)
		handlerEnv$.mtime <<- as.integer(file.info(handler)$mtime)
	    } else {
		stop("Need a valid function or file for the handler")
	    }
	    .self$path <- ifelse(.self$name=='httpd','', paste('/custom',.self$name,'',sep='/'))
	    .self$httpdOrig <- NULL
	    callSuper(...)
	},
	invoke_handler = function(env=NULL){
	    if (!is.null(handlerEnv)){
		mtime <- as.integer(file.info(handler)$mtime)
		if (mtime > handlerEnv$.mtime){
		    sys.source(handler,envir=handlerEnv)
		    handlerEnv$.mtime <<- mtime
		}
		handlerEnv[[name]](env)
	    } else {
		handler(env)
	    }
	}
    )
)

RhttpdInputStream <- setRefClass(
    'RhttpdInputStream',
    fields = c('postBody','pos'),
    methods = list(
	initialize = function(postBody=NULL,...){
	    if (is.null(postBody)) {
		postBody <<- raw(0) # empty post body
	    } else if (is.character(postBody)){
		postBody <<- charToRaw(paste(names(postBody),postBody,sep='=',collapse='&'))
	    } else {
		postBody <<- postBody
	    }
	    pos <<- 1
	    callSuper(...)
	},
	read_lines = function(n = -1L){
	    if (n==0 || pos > length(postBody)) return(character())
	    nls <- which(charToRaw('\n')==postBody[pos:length(postBody)])
	    rv <- character(ifelse(n>-1L,n,0))
	    lc <- 1
	    for (i in nls){
		rv[lc] <- rawToChar(postBody[pos:(pos+i-1)])
		pos <<- pos + pos + i
		lc <<- lc + 1
		if (pos > length(postBody) || lc > n)
		    break
	    }
	    if (lc <= n && pos <= length(postBody))
		rv[lc] <- rawToChar(postBody[pos:length(postBody)])
	    rv
	},
	read = function(l = -1L){
	    if (l == 0 || pos >= length(postBody)) return(raw())
	    if (l < 0){
		rv <- postBody[pos:length(postBody)]
		pos <<- length(postBody)
		return(rv)
	    } else {
		rv <- postBody[pos:(pos+(l-1))]
		pos <<- pos + l
		return(rv)
	    }
	},
	rewind = function(){
	    pos <<- 1
	}
    )
)

RhttpdErrorStream <- setRefClass(
    'RhttpdErrorStream',
    methods = list(
	flush = function() { base::flush(stderr()) },
	cat = function(...,sep=" ",fill=FALSE,labels=NULL)
	{ base::cat(...,sep=sep,fill=fill,labels=lables,file=stderr()) }
    )
)

Rhttpd <- setRefClass(
    'Rhttpd',
    fields = c('appList'),
    methods = list(
	initialize = function(...){
	    appList <<- list()
	    callSuper(...)
	},
	start = function(listen='127.0.0.1',port=8181){
	    if (length(appList) == 0)
		add(RhttpdApp$new(system.file('exampleApps/RackTestApp.R',package='Rack')))
	    
	    .Internal(startHTTPD(listen, port))
	    cat('\nServer started on host',listen,'and port',port,'. App urls are:\n\n')
	    invisible(lapply(names(appList),function(i){
		cat('\thttp://',listen,':',port,appList[[i]]$path,'\n',sep='')
	    }))
	},
	stop = function(){
	    .Internal(stopHTTPD())
	},
	add = function(app=NULL){

	    if (!inherits(app,'RhttpdApp'))
		stop("Need an RhttpdApp object")

	    appList[[app$name]] <<- app
	    if(app$name=='httpd'){
		.self$httpdOrig <- tools:::httpd
		assignInNamespace(
		    app$name, 
		    function(path,query,postBody,headers) handler(app,path,query,postBody,headers),
		    'tools'
		)
	    } else {
		assign(
		    app$name, 
		    function(path,query,postBody,headers) handler(app,path,query,postBody,headers),
		    tools:::.httpd.handlers.env
		)
	    }

	    invisible(TRUE)
	},
	remove = function(app=NULL){
	    if (inherits(app,'RhttpdApp'))
		name <- app$name
	    else if (is.character(app))
		name <- app
	    else
		stop("Can only remove by object or by app name")

	    if (is.null(appList[[name]])) return(FALSE)

	    appList[[name]] <<- NULL
	    ret <- FALSE
	    if(name=='httpd' && !is.null(httpdOrig)){
		tools:::httpd <- httpdOrig
		ret <- TRUE
	    } else if (exists(name,tools:::.httpd.handlers.env)){
		tools:::.httpd.handlers.env[[name]] <- NULL
		ret <- TRUE
	    }
	    invisible(ret)
	},
	parse_headers = function(headers,env){

	    hlines <- strsplit(rawToChar(headers),'\n')[[1]]

	    lapply(
		strsplit(hlines,': '),
		function(x) {
		    assign(
			paste('HTTP_',gsub('-','_',gsub('(\\w+)','\\U\\1',x[1],perl=TRUE)),sep=''),
			x[2],
			env
			)
		}
	    )
	},
	build_env = function(appPath,path,query,postBody,headers){
	    env <- new.env(hash=TRUE,parent=emptyenv())

	    parse_headers(headers,env)

	    # remove HTTP_ from content length and type
	    if (exists('HTTP_CONTENT_LENGTH',env) && exists('HTTP_CONTENT_TYPE',env)){
		assign('CONTENT_LENGTH',env$`HTTP_CONTENT_LENGTH`,env)
		assign('CONTENT_TYPE',env$`HTTP_CONTENT_TYPE`,env)
		rm('HTTP_CONTENT_LENGTH','HTTP_CONTENT_TYPE',envir=env)
	    }

	    assign('SCRIPT_NAME',appPath,env)
	    assign('PATH_INFO',sub(appPath,'',path,fixed=TRUE),env)
	    assign('QUERY_STRING',
		ifelse(is.null(query),
		    '',
		    paste(names(query),query,sep='=',collapse='&')
		    ),
		env
		)
	    assign('REQUEST_METHOD',ifelse(is.null(postBody),'GET','POST'),env)

	    hostport <- strsplit(get('HTTP_HOST',env),':',fixed=TRUE)[[1]]

	    assign('SERVER_NAME',hostport[1],env)
	    assign('SERVER_PORT',hostport[2],env)

	    assign('rack.version',packageDescription('Rack',fields='Version'),env)
	    assign('rack.url_scheme','http',env)
	    assign('rack.input',RhttpdInputStream$new(postBody),env)
	    assign('rack.errors',RhttpdErrorStream$new(),env)

	    env
	},
	handler = function(app,path,query,postBody,headers){
	    res <- try(app$invoke_handler(Rhttpd$build_env(app$path,path,query,postBody,headers)))
	    if (inherits(res,'try-error') || (is.character(res) && length(res) == 1))
		res
	    else {
		if (is.character(res$body) && length(res$body) > 1){
		    res$body <- paste(res$body,collapse='')
		}
		contentType <- res$headers$`Content-Type`;
		res$headers$`Content-Type` <- NULL;
		list(
		    payload = res$body,
		    `content-type` = contentType,
		    headers = res$headers,
		    `status code` = res$status
		    )
	    }
	}
    )
)$new()
