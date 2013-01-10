.rApacheInputStream <- setRefClass(
    'rApacheInputStream',
    methods = list(
	read_lines = function(n = -1L){
	    if (n<=0) return(character())
	    readLines(n=n,warn=FALSE)
	},
	read = function(l = -1L){
	    if (l <= 0 ) return(raw())
	    receiveBin(l)
	},
	rewind = function(){
	    warning("rApache doesn't support rewind()")
	}
    )
)

.rApacheErrorStream <- setRefClass(
    'rApacheErrorStream',
    methods = list(
	flush = function() { base::flush(stderr()) },
	cat = function(...,sep=" ",fill=FALSE,labels=NULL)
	{ base::cat(...,sep=sep,fill=fill,labels=labels,file=stderr()) }
    )
)

Server <- setRefClass(
    'rApacheServer',
    fields = c('appPath','appList'),
    methods = list(
	initialize = function(...){
	    callSuper(...)
	},
	AppPath = function(appPath){
	    if (length(appList) == 0) return()
	    appPath <<- appPath
	},
	build_env = function(){
	    env <- new.env(hash=TRUE,parent=emptyenv())

	    lapply(names(SERVER$headers_in),function(h){
		assign(
		    paste('HTTP_',gsub('-','_',gsub('(\\w+)','\\U\\1',h,perl=TRUE)),sep=''),
		    SERVER$headers_in[[h]],
		    env)
	    })

	    cat(paste(names(env),collapse=' '),file=stderr())

	    if (exists('HTTP_CONTENT_LENGTH',env))
		assign('CONTENT_LENGTH',get('HTTP_CONTENT_LENGTH',env),env)
	    else 
		assign('CONTENT_LENGTH',SERVER$clength,env)

	    if (exists('HTTP_CONTENT_TYPE',env))
		assign('CONTENT_TYPE',get('HTTP_CONTENT_TYPE',env),env)
	    else
		assign('CONTENT_TYPE',SERVER$content_type,env)

	    assign('SCRIPT_NAME',SERVER$cmd_path,env)

	    assign('PATH_INFO',sub(SERVER$cmd_path,'',SERVER$uri),env)

       # Ensure only one leading forward slash
       env$PATH_INFO <- sub('^/+','/',env$PATH_INFO)

	    assign('QUERY_STRING',SERVER$args,env)
	    assign('QUERY_STRING',ifelse(is.null(SERVER$args),'',SERVER$args),env)
	    assign('REQUEST_METHOD',SERVER$method,env)

        assign('REMOTE_HOST',SERVER$remote_host,env)
        assign('REMOTE_ADDR',SERVER$remote_ip,env)

	    hostport <- strsplit(get('HTTP_HOST',env),':',fixed=TRUE)[[1]]

	    assign('SERVER_NAME',hostport[1],env)
	    assign('SERVER_PORT',hostport[2],env)

	    assign('rook.version',packageDescription('Rook',fields='Version'),env)
	    if (exists('HTTP_ORIGIN',env)){
		assign(
		    'rook.url_scheme',
		    strsplit(get('HTTP_ORIGIN',env),':',fixed=TRUE)[[1]][1]
		    ,env
		)
	    }
	    assign('rook.input',.rApacheInputStream$new(),env)
	    assign('rook.errors',.rApacheErrorStream$new(),env)

	    env
	},
	call = function(app){
	    if (is(app,'refClass')) res <- try(app$call(build_env()))
	    else if (is(app,'function')) res <- try(app(build_env()))
	    else stop('App not Rook aware')

	    if (inherits(res,'try-error')){
		warning('App returned try-error object')
		return(HTTP_INTERNAL_SERVER_ERROR)
	    }

	    setContentType(res$headers$`Content-Type`)
	    res$headers$`Content-Type` <- NULL
	    lapply(names(res$headers),function(n)setHeader(n,res$headers[[n]]))

	    # If body is named, then better be a file.
	    if (!is.null(names(res$body)) && names(res$body)[1] == 'file'){
		sendBin(readBin(res$body[1],'raw',n=file.info(res$body[1])$size))
	    } else {
		sendBin(res$body)
	    }

	    ifelse(res$status==200,OK,res$status)
	}
    )
)$new()
