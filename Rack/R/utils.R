Utils <- setRefClass(
    'Utils',
    fields = c(
	'HTTP_STATUS_CODES',
	'STATUS_WITH_NO_ENTITY_BODY',
	'CHAR_TO_STATUS_CODE',
	'DEFAULT_SEP'
    ),
    methods = list(
	initialize = function(...){
	    HTTP_STATUS_CODES <<- new.env(hash=TRUE)
	    # Generated with:
	    #   curl -s http://www.iana.org/assignments/http-status-codes | \
	    #     ruby -ane 'm = /^(\d{3}) +(\S[^\[(]+)/.match($_) and
	    #                puts "      `#{m[1]}`  <- \x27#{m[2].strip}\x27"'
	    with(HTTP_STATUS_CODES,{
		`100`  <- 'Continue'
		`101`  <- 'Switching Protocols'
		`102`  <- 'Processing'
		`200`  <- 'OK'
		`201`  <- 'Created'
		`202`  <- 'Accepted'
		`203`  <- 'Non-Authoritative Information'
		`204`  <- 'No Content'
		`205`  <- 'Reset Content'
		`206`  <- 'Partial Content'
		`207`  <- 'Multi-Status'
		`208`  <- 'Already Reported'
		`226`  <- 'IM Used'
		`300`  <- 'Multiple Choices'
		`301`  <- 'Moved Permanently'
		`302`  <- 'Found'
		`303`  <- 'See Other'
		`304`  <- 'Not Modified'
		`305`  <- 'Use Proxy'
		`306`  <- 'Reserved'
		`307`  <- 'Temporary Redirect'
		`400`  <- 'Bad Request'
		`401`  <- 'Unauthorized'
		`402`  <- 'Payment Required'
		`403`  <- 'Forbidden'
		`404`  <- 'Not Found'
		`405`  <- 'Method Not Allowed'
		`406`  <- 'Not Acceptable'
		`407`  <- 'Proxy Authentication Required'
		`408`  <- 'Request Timeout'
		`409`  <- 'Conflict'
		`410`  <- 'Gone'
		`411`  <- 'Length Required'
		`412`  <- 'Precondition Failed'
		`413`  <- 'Request Entity Too Large'
		`414`  <- 'Request-URI Too Long'
		`415`  <- 'Unsupported Media Type'
		`416`  <- 'Requested Range Not Satisfiable'
		`417`  <- 'Expectation Failed'
		`422`  <- 'Unprocessable Entity'
		`423`  <- 'Locked'
		`424`  <- 'Failed Dependency'
		`425`  <- 'Reserved for WebDAV advanced'
		`426`  <- 'Upgrade Required'
		`500`  <- 'Internal Server Error'
		`501`  <- 'Not Implemented'
		`502`  <- 'Bad Gateway'
		`503`  <- 'Service Unavailable'
		`504`  <- 'Gateway Timeout'
		`505`  <- 'HTTP Version Not Supported'
		`506`  <- 'Variant Also Negotiates'
		`507`  <- 'Insufficient Storage'
		`508`  <- 'Loop Detected'
		`509`  <- 'Unassigned'
		`510`  <- 'Not Extended'
	    })

	    STATUS_WITH_NO_ENTITY_BODY <<- as.integer(c(100:199,204,304))

	    CHAR_TO_STATUS_CODE <<- new.env(hash=TRUE)
	    lapply(names(as.list(HTTP_STATUS_CODES)),function(x){
		    code <- as.integer(x)
		    assign(x,code,CHAR_TO_STATUS_CODE)
		    assign(gsub(' ','_',tolower(HTTP_STATUS_CODES[[x]])),code,CHAR_TO_STATUS_CODE)
	    })

	    DEFAULT_SEP <<- '[&;] *'
	    callSuper(...)
	},
	escape = function(s) { 
	    x <- strsplit(s,"")[[1L]]
	    z <- grep('[^ a-zA-Z0-9_.-]',x,perl=TRUE)
	    if (length(z)){
		y <- sapply(x[z],function(i) paste('%',paste(toupper(as.character(charToRaw(i))),collapse='%'),sep=''))
		x[z] <- y
	    }
	    s <- paste(x,collapse='')
	    chartr(' ','+',s)
	},
	unescape = function(s) chartr('+',' ',utils::URLdecode(s)),
	parse_query = function(qs, d=NULL) {
	    if (is.null(d)) d <- DEFAULT_SEP

	    x <- strsplit(qs,d,perl=TRUE)[[1L]]
	    if (length(x) == 0) return(list())

	    z <- x != ''
	    params <- list()
	    if (length(z)){
		parseEqual <- function(i){
		    m <- regexpr('=',i)[1L]
		    if (m > 0){
			if (m == 1){
			    params[['']] <<- unescape(i)
			} else { 
			    ilen <- nchar(i)
			    if (m == ilen){
				params[[i]] <<- ''
			    } else {
				params[[substr(i,1,m-1)]] <<- escape(substr(i,m+1,ilen))
			    }
			}
		    } else {
			params[[i]] <<- NA
		    }
		}
		lapply(x[z],parseEqual)
	    }
	    params
	},
	#parse_nested_query = function() {},
	#normalize_params = function () {},
	buildQuery = function(params) {
	    # TODO: call escape here, need to vectorize it first
	    paste(names(params),params,sep='=',collapse='&')
	},
	#build_nested_query = function() {},"
	escape_html = function(string) {
	    string <- gsub('&','&amp;',string)
	    string <- gsub('<','&lt;',string)
	    string <- gsub("'",'&#39;',string)
	    string <- gsub('"','&quot;',string)
	    string
	},
	#selectBestEncoding = function(){},
	set_cookie_header = function(header,key=NULL,value='',expires=NULL,path=NULL,domain=NULL,secure=FALSE,httpOnly=FALSE){
	    if (is.null(key)) return(invisible())
	    cookie <- paste( escape(key), paste(sapply(value,escape),collapse='&'), sep='=')
	    if (!is.null(expires))
		cookie <- paste(cookie,'; expires=',rfc2822(expires),sep='')
	    if (!is.null(path))
		cookie <- paste(cookie,'; path=',path,sep='')
	    if (!is.null(domain))
		cookie <- paste(cookie,'; domain=',domain,sep='')
	    if (secure)
		cookie <- paste(cookie,'; secure',sep='')
	    if (httpOnly)
		cookie <- paste(cookie,'; HttpOnly',sep='')

	    if (is.null(header$`Set-Cookie`) || header$`Set-Cookie` == '')
		header$`Set-Cookie` <- cookie
	    else 
		header$`Set-Cookie` <- paste(header$`Set-Cookie`,cookie,sep='\n')
	},
	delete_cookie_header = function(header,key=NULL,value='',expires=NULL,path=NULL,domain=NULL,secure=FALSE,httpOnly=FALSE){
	    if (is.null(header$`Set-Cookie`)) return(invisible())
	    cookies <- strsplit(header$`Set-Cookie`,'\n')[[1L]]
	    if (length(cookies) == 0) return(invisible())

	    d <- ifelse(is.null(domain),
		grepl(paste('^',escape(key),'=',sep=''),cookies,perl=TRUE),
		grepl(paste('^',escape(key),'=.*domain=',domain,sep=''),cookies,perl=TRUE)
	    )

	    header$`Set-Cookie` <- paste(cookies[!d],collapse='\n')

	    set_cookie_header(header,key,'',timeZero(),path,domain,secure,httpOnly)
	},
	bytesize = function(string=NULL) { nchar(string,type='bytes') },
	raw.match = function(needle,haystack,all=TRUE) .Call(rack:::rawmatch,needle,haystack,all),
	timezero = function() structure(0,class=c('POSIXct','POSIXt')),
	rfc2822 = function(ts){
	    format(ts,format="%a, %d %b %Y %T GMT",tz='GMT')
	},
	status_code = function(status=NULL){
	    if (is.null(status) || status == '') return(as.integer(500))
	    if(is.character(status)){
		code <- try(get(gsub(' ','_',tolower(status)),CHAR_TO_STATUS_CODE,inherits=FALSE),silent=TRUE)
		ifelse(inherits(code,'try-error'),500,code)
	    } else {
		as.integer(status)
	    }
	}
    )
)$new()

#UploadedFile <- setRefClass(
#    'UploadedFile',
#)

Multipart <- setRefClass(
    'Multipart',
    fields = c('EOL'),
    methods = list(
	initialize = function(...){
	    EOL <<- '\r\n'
	    callSuper(...)
	},
	parse = function(env){
	    if (!exists('CONTENT_TYPE',env)) return(NULL)

	    # return value
	    params  <- list()

	    input <- env$`rack.input`
	    input$rewind()

	    content_length = as.integer(env$CONTENT_LENGTH)

	    # Some constants regarding boundaries
	    boundary <- paste('--',gsub('^multipart/.*boundary="?([^";,]+)"?','\\1',env$CONTENT_TYPE,perl=TRUE),sep='')
	    boundary_size <- Utils$bytesize(boundary)
	    boundaryEOL <- paste(boundary,EOL,sep='')
	    boundaryEOL_size <- boundary_size + Utils$bytesize(EOL)
	    EOLEOL = paste(EOL,EOL,sep='')
	    EOLEOL_size = Utils$bytesize(EOLEOL)
	    EOL_size = Utils$bytesize(EOL)

	    read_buffer <- input$read(boundaryEOL_size)
	    read_buffer_len <- length(read_buffer)
	    unread <- content_length - boundary_size

	    i <- Utils$raw.match(boundaryEOL,read_buffer,all=FALSE)
	    if (!length(i) || i != 1){
		warning("bad content body")
		return(NULL)
	    }

	    # Never read more than bufsize bytes.
	    bufsize <- 16384
	    fill_buffer <- function(){
		buf <- input$read(ifelse(bufsize < unread, bufsize, unread))
		buflen <- length(buf)
		if (buflen > 0){
		    read_buffer <<- c(read_buffer,buf)
		    read_buffer_len <<- length(read_buffer)
		    unread <<- unread - buflen
		}
	    }

	    # Slices off the beginning part of read_buffer.
	    slice_buffer <- function(i,size){
		slice <- if(i > 1) read_buffer[1:(i-1)] else read_buffer[1] 
		read_buffer <<- if(size < read_buffer_len) read_buffer[(i+size):read_buffer_len] else raw()
		read_buffer_len <<- length(read_buffer)
		slice
	    }

	    # prime the read_buffer
	    read_buffer <- raw()
	    fill_buffer()

	    while(TRUE) {
		head <- value <- NULL
		filename <- content_type <- name <- NULL
		while(is.null(head)){
		    i <- Utils$raw.match(EOLEOL,read_buffer,all=FALSE)
		    if (length(i)){
			head <- slice_buffer(i,EOLEOL_size)
			break
		    } else if (unread){
			fill_buffer()
		    } else {
			break # we've read everything and still haven't seen a valid head
		    }
		}
		if (is.null(head)){
		    warning("Bad post payload: searching for a header")
		    return(NULL)
		} 

		# cat("Head:",rawToChar(head),"\n")
		# they're 8bit clean
		head <- rawToChar(head)

		token <- '[^\\s()<>,;:\\"\\/\\[\\]?=]+'
		condisp <- paste('Content-Disposition:\\s*',token,'\\s*',sep='')
		dispparm <- paste(';\\s*(',token,')=("(?:\\"|[^"])*"|',token,')*',sep='')

		rfc2183 <- paste('(?m)^',condisp,'(',dispparm,')+$',sep='')
		broken_quoted <- paste('(?m)^',condisp,'.*;\\sfilename="(.*?)"(?:\\s*$|\\s*;\\s*',token,'=)',sep='')
		broken_unquoted = paste('(?m)^',condisp,'.*;\\sfilename=(',token,')',sep='')

		if (length(grep(rfc2183,head,perl=TRUE))){
		    first_line <- sub(condisp,'',strsplit(head,'\r\n')[[1L]][1],perl=TRUE)
		    pairs <- strsplit(first_line,';',fixed=TRUE)[[1L]]
		    fnmatch <- '\\s*filename=(.*)\\s*'
		    if (any(grepl(fnmatch,pairs,perl=TRUE))){
			filename <- pairs[grepl(fnmatch,pairs,perl=TRUE)][1]
			filename <- gsub('"','',sub(fnmatch,'\\1',filename,perl=TRUE))
		    }
		} else if (length(grep(broken_quoted,head,perl=TRUE))){
		    filename <- sub(broken_quoted,'\\1',strsplit(head,'\r\n')[[1L]][1],perl=TRUE)
		} else if (length(grep(broken_unquoted,head,perl=TRUE))){
		    filename <- sub(broken_unquoted,'\\1',strsplit(head,'\r\n')[[1L]][1],perl=TRUE)
		}

		if (!is.null(filename) && filename!=''){
		    filename = Utils$unescape(filename)
		}

		headlines <- strsplit(head,EOL,fixed=TRUE)[[1L]]
		content_type_re <- '(?mi)Content-Type: (.*)'
		content_types <- headlines[grepl(content_type_re,headlines,perl=TRUE)]
		if (length(content_types)){
		    content_type <- sub(content_type_re,'\\1',content_types[1],perl=TRUE)
		}

		name <- sub('(?si)Content-Disposition:.*\\s+name="?([^";]*).*"?','\\1',head,perl=TRUE)

		while(TRUE){
		    i <- Utils$raw.match(boundary,read_buffer,all=FALSE)
		    if (length(i)){
			value <- slice_buffer(i,boundary_size)
			if (length(value)){

			    # Drop EOL only values
			    if (length(value) == 2 && length(Utils$raw.match(EOL,value)))
				break

			    if (!is.null(filename) || !is.null(content_type)){
				data <- list()
				if (!is.null(filename))
				    data$filename <- strsplit(filename,'[\\/]',perl=TRUE)[[1L]]
				data$tempfile <- tempfile('Multipart')
				if (!is.null(content_type))
				    data$content_type <- content_type
				data$head <- head
				con <- file(data$tempfile,open='wb')
				writeBin(value,con)
				close(con)
				params[[name]] <- data
			    } else {
				len <- length(value)
				# Trim trailing EOL
				if (len > 2 && length(Utils$raw.match(EOL,value[(len-1):len],all=FALSE)))
				    len <- len -2
				params[[name]] <- Utils$escape(rawToChar(value[1:len]))
			    }
			} 
			break
		    } else if (unread){
			fill_buffer()
		    } else {
			break # we've read everything and still haven't seen a valid value
		    }
		}
		if (is.null(value)){
		    # bad post payload
		    warning("Bad post payload: searching for a body part")
		    return(NULL)
		}

		# Now search for ending markers or the beginning of another part
		while (read_buffer_len < 2 && unread) fill_buffer()

		if (read_buffer_len < 2 && unread == 0){
		    # Bad stuff at the end. just return what we've got
		    # and presume everything is okay.
		    return(params)
		}

		# Valid ending
		if (length(Utils$raw.match('--',read_buffer[1:2],all=FALSE)))
		    return(params)

		# Skip past the EOL.
		if (length(Utils$raw.match(EOL,read_buffer[1:EOL_size],all=FALSE))){
		    slice_buffer(1,EOL_size)
		} else {
		    warning("Bad post body: EOL not present")
		    return(params)
		}

		# another sanity check before we try to parse another part
		if ((read_buffer_len + unread) < boundary_size){
		    warning("Bad post body: unknown trailing bytes")
		    return(params)
		}
	    }
	},
	build = function(params){
	}
    )
)$new()
