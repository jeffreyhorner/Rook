RhttpdApp <- setRefClass(
    'RhttpdApp',
    fields = c('app','name','path','appEnv'),
    methods = list(
	initialize = function(app=NULL,name=NULL,...){
	    if (is.null(name) || !is.character(name))
		base::stop("Need a proper app 'name'")

	    .self$name <- name

	    if (is.character(app) && file.exists(app)){
		oldwd <- setwd(dirname(app))
		on.exit(setwd(oldwd))
		.self$appEnv <- new.env(parent=globalenv())
		sys.source(basename(app),envir=.self$appEnv)
		appEnv$.mtime <<- as.integer(file.info(basename(app))$mtime)
		appEnv$.appFile <<- normalizePath(app)

		if (exists(.self$name,.self$appEnv,inherits=FALSE))
		    .self$app <- get(.self$name,.self$appEnv)
		else if (exists('app',.self$appEnv,inherits=FALSE))
		    .self$app <- get('app',.self$appEnv)
		else
		    base::stop("Cannot find a suitable app in file ",app)

	    } else {
		.self$app <- app
		.self$name <- name
		.self$appEnv <- NULL
	    }

	    if (!is_rookable(.self$app))
		base::stop("App is not rookable'")

	    .self$path <- ifelse(.self$name=='httpd','', paste('/custom',.self$name,sep='/'))
	    callSuper(...)
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
	{ base::cat(...,sep=sep,fill=fill,labels=labels,file=stderr()) }
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
	finalize = function(){
	    if (length(appList) == 0) return()
	    for (i in 1:length(appList)){
		remove(appList[[i]])
	    }
	},
	full_url = function(i){
	    paste('http://',listenAddr,':',tools:::httpdPort,appList[[i]]$path,sep='')
	},
	launch = function(...){
	    .self$start(quiet=TRUE)		
	    # Try to create a new app from the supplied arguments
	    app <- RhttpdApp$new(...)
	    if (add(app)){
		appName <- app$name
		browseURL(full_url(which(appName == names(appList))))
		invisible()
	    } else {
		base::stop("No app to launch")
	    }
	},
	open = function(x){
	    if (missing(x)) return(print())
	    if (is.numeric(x) || is.integer(x)){
		x <- as.integer(x)
		if (!is.null(appList[[x]]))
		    return(invisible(browseURL(full_url(x))))
		else
		    base::stop("No app at index ",x)
	    } else if (is.character(x)){
		for (i in 1:length(appList)){
		    if (appList[[i]]$name==x){
			return(invisible(browseURL(full_url(x))))
		    }
		}
		base::stop("No app named",x)
	    }
	    base::stop("Argument must be an integer or character")
	},
	browse = function(x) open(x),
	start = function(listen='127.0.0.1',port=getOption('help.ports'),quiet=FALSE){
	    if (length(appList) == 0)
		add(RhttpdApp$new(system.file('exampleApps/RookTestApp.R',package='Rook'),name='RookTest'))

	    env <- environment(tools::startDynamicHelp)
	    if(nzchar(Sys.getenv("R_DISABLE_HTTPD"))) {
		unlockBinding("httpdPort", env)
		assign('httpdPort',-1L,env)
		lockBinding("httpdPort", env)
		warning("httpd server disabled by R_DISABLE_HTTPD", immediate. = TRUE)
		utils::flush.console()
		return(tools:::httpdPort)
	    }

	    if(grepl('rstudio',base::.Platform$GUI,ignore.case=TRUE)){
		if (!missing(port))
		    warning("RStudio has already started the web server on port ",tools:::httpdPort)
		return(invisible())
	    }

	    if(tools:::httpdPort > 0){
		if (quiet) return(invisible())
		base::stop("server already running on port ",tools:::httpdPort,". Consider calling the stop() method first.")
	    } else if (tools:::httpdPort < 0) {
		if (quiet) return(invisible())
		base::stop("server could not be started on an earlier attempt")
	    }

	    
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
		return(invisible())
	    }

	    if (!quiet){
		cat('\nServer started on host',listen,'and port',tools:::httpdPort,'. App urls are:\n\n')
		invisible(lapply(names(appList),function(i){
		    cat('\thttp://',listen,':',tools:::httpdPort,appList[[i]]$path,'\n',sep='')
		}))
	    }
	    invisible()
	},
	stop = function(){
	    env <- environment(tools::startDynamicHelp)
	    unlockBinding("httpdPort", env)
	    assign('httpdPort',0L,env) 
	    lockBinding("httpdPort", env)
	    invisible(.Internal(stopHTTPD()))
	},
	add = function(app=NULL,name=NULL){

	    if (!inherits(app,'RhttpdApp'))
		app <- RhttpdApp$new(name=name,app=app)
	    if (!inherits(app,'RhttpdApp'))
		base::stop("Need an RhttpdApp object")

	    appList[[app$name]] <<- app
	    if(app$name=='httpd'){
		.self$httpdOrig <- tools:::httpd
		assignInNamespace(
		    app$name,
		    function(path,query,postBody,headers) 
			.self$handler(app$name,path,query,postBody,headers), 
		    'tools'
		)
	    } else {
		assign(
		    app$name, 
		    function(path,query,postBody,headers) 
			.self$handler(app$name,path,query,postBody,headers), 
		    tools:::.httpd.handlers.env
		)
	    }

	    invisible(TRUE)
	},
	remove = function(app=NULL,all=FALSE){
	    if (all==TRUE){
		lapply(names(appList),remove)
		return(invisible(TRUE))
	    }
	    if (inherits(app,'RhttpdApp'))
		name <- app$name
	    else if (is.character(app))
		name <- app
	    else if (is.numeric(app) || is.integer(app))
		name <- appList[[app]]$name
	    else
		base::stop("Can only remove by object, app name, or index.")

	    if (is.null(appList[[name]])) return(FALSE)

	    appList[[name]] <<- NULL
	    ret <- FALSE
	    if(name=='httpd' && !is.null(httpdOrig)){
		tools:::httpd <- httpdOrig
		ret <- TRUE
	    } else if (exists(name,tools:::.httpd.handlers.env)){
		rm(list=name,pos=tools:::.httpd.handlers.env)
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

	    assign('rook.version',packageDescription('Rook',fields='Version'),env)
	    assign('rook.url_scheme','http',env)
	    assign('rook.input',RhttpdInputStream$new(postBody),env)
	    assign('rook.errors',RhttpdErrorStream$new(),env)

	    if (debug()>1)
		str(as.list(env))
	    env
	},
	handler = function(appName,path,query,postBody,headers){
	    if (debug()>0){
		cat('Request:',path,'\n')
	    }
	    app <- appList[[appName]]
	    if (is.null(app)){
		base::stop("No app installed named ",appName)
		return()
	    }
	    if (!is.null(app$appEnv)){
		file_path = app$appEnv$.appFile
		mtime <- as.integer(file.info(file_path)$mtime)


		if (mtime > app$appEnv$.mtime){
		    add(name=appName,app=file_path)
		}
		app <- appList[[appName]]
		if (is.null(app)){
		    stop("No app installed named ",appName)
		    return()
		}

		oldwd <- setwd(dirname(app$appEnv$.appFile))
		on.exit(setwd(oldwd))
	    }
	    env <- build_env(app$path,path,query,postBody,headers)
	    if (is(app$app,'function')) {
		res <- try(app$app(env))
	    } else {
		res <- try(app$app$call(env))
	    }
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
		cat("No applications installed\n")
		return(invisible())
	    }
	    len <- max(nchar(names(appList)))
	    for (i in 1:length(appList)){
		appName <- sprintf(paste('%-',len,'s',sep=''),names(appList)[i])
		cat('[',i,'] ',appName,' ',full_url(i),'\n',sep='')
	    }
	    cat("\nCall browse() with an index number or name to run an application.\n")
	    invisible()
	},
	show = function() print(),
	debug = function(){
	    d <- getOption('Rhttpd_debug')
	    if (!is.null(d)) as.integer(d)
	    else 0
	}
    )
)
