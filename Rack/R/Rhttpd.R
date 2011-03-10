RhttpdApp <- setRefClass(
    'RhttpdApp',
    fields = c('app','name','path','appEnv'),
    methods = list(
	initialize = function(app=NULL,name=NULL,...){
	    if (is.null(name) || !is.character(name))
		stop("Need a proper app 'name'")

	    .self$name <- name

	    if (is.character(app) && file.exists(app)){
		.self$appEnv <- new.env(parent=globalenv())
		sys.source(app,envir=.self$appEnv)
		appEnv$.mtime <<- as.integer(file.info(app)$mtime)
		appEnv$.appFile <<- app

		if (exists(.self$name,.self$appEnv))
		    .self$app <- get(.self$name,.self$appEnv)
		else if (exists('app',.self$appEnv))
		    .self$app <- get('app',.self$appEnv)
		else
		    stop("Cannot find a suitable app in file",app)

	    } else {
		.self$app <- app
		.self$name <- name
		.self$appEnv <- NULL
	    }

	    if (
		!is.function(.self$app) && 
		!((is(.self$app,'refClass') && 'call' %in% getRefClass(.self$app)$methods()))
	    )
		stop("App must be either a closure or a reference class that implements 'call'")

	    .self$path <- ifelse(.self$name=='httpd','', paste('/custom',.self$name,'',sep='/'))
	    callSuper(...)
	},
	invoke_handler = function(env=NULL){
	    if (!is.null(appEnv)){
		oldwd <- setwd(dirname(appEnv$.appFile))
		on.exit(setwd(oldwd))

		file_path = basename(appEnv$.appFile)
		mtime <- as.integer(file.info(file_path)$mtime)

		if (mtime > appEnv$.mtime){
		    appEnv$.mtime <<- mtime
		    sys.source(file_path,envir=appEnv)
		    if (exists(name,appEnv))
			app <<- get(name,appEnv)
		    else if (exists('app',appEnv))
			app <<- get('app',appEnv)
		    else
			stop("Cannot find a suitable app in file",appEnv$.appFile)
		}
	    }
	    if (is(app,'function')) app(env)
	    else app$call(env)
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
		lc <- lc + 1
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
    fields = c('appList','listenAddr','httpdOrig'),
    methods = list(
	initialize = function(...){
	    appList <<- list()
	    listenAddr <<- '127.0.0.1'
	    callSuper(...)
	},
	full_url = function(i){
	    paste('http://',listenAddr,':',tools:::httpdPort,appList[[i]]$path,sep='')
	},
	launch = function(...){
	    .self$start(launch=FALSE,quiet=TRUE)		
	    # Try to create a new app from the supplied arguments
	    app <- RhttpdApp$new(...)
	    if (add(app)){
		appName <- app$name
		browseURL(full_url(which(appName == names(appList))))
		return()
	    }

	    stop("No app to launch")
	},
	open = function(x){
	    x <- as.integer(x)
	    if (!is.null(appList[[x]]))
		invisible(browseURL(full_url(x)))
	    else
		stop("No app at index ",x)
	},
	start = function(listen='127.0.0.1',port=getOption('help.ports'),launch=TRUE,quiet=FALSE){
	    if (length(appList) == 0)
		add(RhttpdApp$new(system.file('exampleApps/RackTestApp.R',package='Rack'),name='RackTest'))

	    env <- environment(tools::startDynamicHelp)
	    if(nzchar(Sys.getenv("R_DISABLE_HTTPD"))) {
		unlockBinding("httpdPort", env)
		assign('httpdPort',-1L,env)
		lockBinding("httpdPort", env)
		warning("httpd server disabled by R_DISABLE_HTTPD", immediate. = TRUE)
		utils::flush.console()
		return(tools:::httpdPort)
	    }
	    if(tools:::httpdPort > 0 && !quiet) 
		base::stop("server already running on port ",tools:::httpdPort)
	    else if (tools:::httpdPort < 0) 
		base::stop("server could not be started on an earlier attempt")
	    
	    ports <- port
	    if (is.null(ports)) {
		## Choose 10 random port numbers between 10000 and 32000.
		## The random seed might match
		## on multiple instances, so add the time as well.  But the
		## time may only be accurate to seconds, so rescale it to
		## 5 minute units.
		ports <- 10000 + 22000*((stats::runif(10) + unclass(Sys.time())/300) %% 1)
	    }
	    ports <- as.integer(ports)
	    for(i in seq_along(ports)) {
		## the next can throw an R-level error,
		## so do not assign port unless it succeeds.
		status <- .Internal(startHTTPD(listen, ports[i]))
		if (status == 0L) {
		    OK <- TRUE
		    listenAddr <<- listen
		    unlockBinding("httpdPort", env)
		    assign('httpdPort',ports[i],env)
		    lockBinding("httpdPort", env)
		    break
		}
		if (status != -2L) break
		## so status was -2, which means port in use
	    }
	    if (OK) {
		if (!quiet) message(" done")
		utils::flush.console()
		## FIXME: actually test the server
	    } else {
		warning("failed to start the httpd server", immediate. = TRUE)
		utils::flush.console()
		unlockBinding("httpdPort", env)
		assign('httpdPort',-1L,env)
		lockBinding("httpdPort", env)
		return()
	    }

	    if (!quiet){
		cat('\nServer started on host',listen,'and port',tools:::httpdPort,'. App urls are:\n\n')
		invisible(lapply(names(appList),function(i){
		    cat('\thttp://',listen,':',tools:::httpdPort,appList[[i]]$path,'\n',sep='')
		}))
	    }
	    if (launch){
		if (!quiet) cat("Launching",names(appList)[1],"\n")
		.self$launch(names(appList)[1],start=FALSE)
	    }
	},
	stop = function(){
	    env <- environment(tools::startDynamicHelp)
	    unlockBinding("httpdPort", env)
	    assign('httpdPort',0L,env) 
	    lockBinding("httpdPort", env)
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

	    if (debug()>1)
		str(as.list(env))
	    env
	},
	handler = function(app,path,query,postBody,headers){
	    if (debug()>0){
		cat('Request:',path,'\n')
	    }
	    res <- try(app$invoke_handler(Rhttpd$build_env(app$path,path,query,postBody,headers)))
	    if (inherits(res,'try-error') || (is.character(res) && length(res) == 1))
		res
	    else {
		# Only need to handle the case where body is a vector of strings
		# We presume that if res$body is a location to a file then the
		# app has so named it. We also presume that res$body may be
		# a raw vector, but we let the internal web server deal with that.
		if (is.character(res$body) && length(res$body) > 1){
		    res$body <- paste(res$body,collapse='')
		}
		contentType <- res$headers$`Content-Type`;
		res$headers$`Content-Type` <- NULL;
		ret <- list(
		    payload = res$body,
		    `content-type` = contentType,
		    headers = paste(names(res$headers),': ',res$headers,sep=''),
		    `status code` = res$status
		)

		# Change the name of payload to file in the case that
		# payload *is* a filename
		if (!is.null(names(res$body)) && names(res$body)[1] == 'file'){
		    names(ret) <- c('file',names(ret)[-1])
		    # Delete content length as Rhttpd will add it
		    res$headers$`Content-Length` <- NULL;
		    ret$headers = paste(names(res$headers),': ',res$headers,sep='')
		}

		if (debug()>0){
		    cat('Response:\n')
		    str(ret)
		}
		ret
	    }
	},
	print = function() {
	    if (tools:::httpdPort > 0){
		cat("Server started on ",listenAddr,":",tools:::httpdPort,"\n",sep='')
	    } else {
		cat("Server stopped\n")
	    }
	    if (length(appList) == 0){
		cat("No apps installed\n")
		return()
	    }
	    for (i in 1:length(appList)){
		cat('[',i,'] ',full_url(i),'\n',sep='')
	    }
	    cat("To open an app in your browser, type Rhttpd$open(i) where i is a number from the list above \n")
	    invisible()
	},
	show = function() print(),
	debug = function(){
	    d <- getOption('Rhttpd_debug')
	    if (!is.null(d))
		as.integer(d)
	    else
		0
	}
    )
)$new()
