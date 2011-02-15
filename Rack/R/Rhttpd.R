RhttpdApp <- setRefClass(
    'RhttpdApp',
    fields = c('name','handler','path'),
    methods = list(
	initialize = function(name=NULL,handler=NULL,...){
	    if (is.null(name) || !is.character(name))
		stop("Need a valid app name")
	    if (is.null(handler) || typeof(handler) != 'closure')
		stop("Need a valid function for the handler")
	    name <<- name[1]
	    handler <<- handler
	    path <<- ifelse(name=='httpd','', paste('/custom',name,'',sep='/'))
	    callSuper(...)
	},
	invokeHandler = function(env=NULL){
	    handler(env)
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
	readLines = function(n = -1L){
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

RackTestApp <- function(env){
    envstr <- paste(capture.output(str(as.list(env)),file=NULL),collapse='\n')
    postBody <- env$rack.input$readRaw()
    save(env,postBody,file=tempfile('request.'))
    postBodyChar <- rawToChar(postBody,multiple=TRUE)
    postBodyChar[postBodyChar==''] <- '\\000'
    postBodyChar <- paste(postBodyChar,collapse='')
    randomString <- function() paste(letters[floor(runif(10,0,26))],collapse='')
    randomNumber <- function() runif(1,0,26)
    payload <- paste(
    '<HTML><head><style type="text/css">\n',
    'table { border: 1px solid #8897be; border-spacing: 0px; font-size: 10pt; }',
    'td { border-bottom:1px solid #d9d9d9; border-left:1px solid #d9d9d9; border-spacing: 0px; padding: 3px 8px; }',
    'td.l { font-weight: bold; width: 10%; }\n',
    'tr.e { background-color: #eeeeee; border-spacing: 0px; }\n',
    'tr.o { background-color: #ffffff; border-spacing: 0px; }\n',
    '</style></head><BODY><H1>Canonical Test for Rack</H1>\n',
    sprintf('<form enctype="multipart/form-data" method=POST action="%s/%s?called=%s">',env$SCRIPT_NAME,randomString(),randomNumber()),
    'Enter a string: <input type=text name=name value=""><br>\n',
    'Enter another string: <input type=text name=name value=""><br>\n',
    'Upload a file: <input type=file name=fileUpload><br>\n',
    'Upload another file: <input type=file name=anotherFile><br>\n',
    '<input type=submit name=Submit><br><br>',
    'Environment:<br><pre>',envstr,'</pre><br>',
    'Post Body (raw):<br>',paste(postBody,collapse=' '), '<br><br>',
    'Post Body (char):<br><pre style="border: 1px solid black">',postBodyChar, '</pre><br>',
	sep=''
    )
    list(
	status=200,
	headers <- list(
	    'Content-Type' = 'text/html'
	),
	body = payload
    )
}

startRhttpd <- function(app=NULL,listen="127.0.0.1",port=8181){

    if (is.null(app)){
	app <- RhttpdApp$new(name='RackTestApp',handler=RackTestApp)
    }

    # Stop any server already running
    .Internal(stopHTTPD())

    # Now start the internal R web server. NOTE that
    # this merges with the R console's REPL loop, so if you shut
    # down R, you're shutting down the web server too.
    if(app$name=='httpd'){
	assignInNamespace(
	    app$name, 
	    function(path,query,postBody,headers) RhttpdHandler(app,path,query,postBody,headers),
	    'tools'
	)
    } else {
	assign(
	    app$name, 
	    function(path,query,postBody,headers) RhttpdHandler(app,path,query,postBody,headers),
	    tools:::.httpd.handlers.env
	)
    }
    cat('\n\nYour app is located here: http://',listen,':',port,app$path,'\n\n',sep='')
    .Internal(startHTTPD(listen, port))
}

parseRhttpdHeaders <- function(headers,env){

    # For now, we treat the raw vector as a character.
    # Later on, we'll need to parse the raw vector to properly
    # decode *TEXT values.

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
}

buildRhttpdEnv <- function(appPath, path,query,postBody,headers){
    env <- new.env(hash=TRUE,parent=emptyenv())

    parseRhttpdHeaders(headers,env)

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
}

RhttpdHandler <- function(app,path,query,postBody,headers){
    cat('\n<Headers>\n',rawToChar(headers),'\n</Headers>\n',file=stderr())
    res <- try(app$invokeHandler(buildRhttpdEnv(app$path,path,query,postBody,headers)))
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

stopRhttpd <- function(){
    .Internal(stopHTTPD())
}
