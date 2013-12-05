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
   escape = function(s=NULL) {
      if (is.null(s)) base::stop("Need a character vector argument")
      unlist(lapply(s,function(s){
            x <- strsplit(s,"")[[1L]]
            z <- grep('[^ a-zA-Z0-9_.-]',x,perl=TRUE)
            if (length(z)){
               y <- sapply(x[z],function(i) paste('%',paste(toupper(as.character(charToRaw(i))),collapse='%'),sep=''))
               x[z] <- y
            }
            s <- paste(x,collapse='')
            chartr(' ','+',s)
      }))
   },
   unescape = function(s=NULL){
      if(is.null(s)) base::stop("Need a character vector argument")
      unlist(lapply(s,function(s)
            utils::URLdecode(chartr('+',' ',s))
      ))
   },
	parse_query = function(qs=NULL, d=DEFAULT_SEP) {
	    if (is.null(qs)) base::stop("Need a character vector argument")

	    x <- strsplit(qs,d,perl=TRUE)[[1L]]
	    if (length(x) == 0) return(list())

	    z <- x != ''
	    params <- new.env()
            params$params <- list()
	    if (length(z)){
		parseEqual <- function(i,params){
		    m <- regexpr('=',i)[1L]
		    if (m > 0){
			if (m == 1){
			    params$params[['']] <- unescape(i)
			} else { 
			    ilen <- nchar(i)
			    if (m == ilen){
				params$params[[i]] <- ''
			    } else {
				# handle array parameters
				paramName <- substr(i,1,m-1)
				paramValue <- unescape(substr(i,m+1,ilen))
				paramSet <- FALSE
				if (grepl("\\[\\]$", paramName)) {
				   paramName <- sub("\\[\\]$", "", paramName)
				   if (paramName %in% names(params$params)) {
					params$params[[paramName]] <- c(params$params[[paramName]], paramValue)
					paramSet <- TRUE
				   }
				}
				if (!paramSet) {
				   params$params[[paramName]] <- paramValue
				}
			    }
			}
		    } else {
			params$params[[i]] <- NA
		    }
		}
		lapply(x[z],parseEqual,params)
	    }
	    params$params
	},
	#parse_nested_query = function() {},
	#normalize_params = function () {},
	build_query = function(params=NULL) {
	    if (is.null(params)) base::stop("Need named list argument")
	    # TODO: call escape here, need to vectorize it first
	    paste(names(params),params,sep='=',collapse='&')
	},
	#build_nested_query = function() {},"
	escape_html = function(string=NULL) {
	    if (is.null(string)) base::stop("Need a character vector argument")
	    string <- gsub('&','&amp;',string)
	    string <- gsub('<','&lt;',string)
	    string <- gsub('>','&gt;',string)
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

	    set_cookie_header(header,key,'',timezero(),path,domain,secure,httpOnly)
	},
	bytesize = function(string=NULL) {
	    if (is.null(string)) base::stop("Need a character vector")
	    nchar(string,type='bytes')
	},
	raw.match = function(needle=NULL,haystack=NULL,all=TRUE) .Call(Rook:::rawmatch,needle,haystack,all),
	timezero = function() structure(0,class=c('POSIXct','POSIXt')),
	rfc2822 = function(ts=NULL){
	    if (is.null(ts) || !inherits(ts,'POSIXt'))
		base::stop("Need a POSIXt object")
	    format(ts,format="%a, %d %b %Y %H:%M:%S GMT",tz='GMT')
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

	    input <- env$`rook.input`
	    input$rewind()

	    content_length = as.integer(env$CONTENT_LENGTH)

        # Bail if not a multipart content body
        if (!grepl('multipart',env$CONTENT_TYPE))
            return(NULL)

	    # Some constants regarding boundaries
	    boundary <- paste('--',gsub('^multipart/.*boundary="?([^";,]+)"?','\\1',env$CONTENT_TYPE,perl=TRUE),sep='')
	    boundary_size <- Utils$bytesize(boundary)
	    boundaryEOL <- paste(boundary,EOL,sep='')
	    boundaryEOL_size <- boundary_size + Utils$bytesize(EOL)
	    EOLEOL = paste(EOL,EOL,sep='')
	    EOLEOL_size = Utils$bytesize(EOLEOL)
	    EOL_size = Utils$bytesize(EOL)

            buf <- new.env()
	    buf$bufsize <- 16384 # Never read more than bufsize bytes.
	    buf$read_buffer <- input$read(boundaryEOL_size)
	    buf$read_buffer_len <- length(buf$read_buffer)
	    buf$unread <- content_length - boundary_size

	    i <- Utils$raw.match(boundaryEOL,buf$read_buffer,all=FALSE)
	    if (!length(i) || i != 1){
		warning("bad content body")
		input$rewind()
		return(NULL)
	    }

	    fill_buffer <- function(x){
		buf <- input$read(ifelse(x$bufsize < x$unread, x$bufsize, x$unread))
		buflen <- length(buf)
		if (buflen > 0){
		    x$read_buffer <- c(x$read_buffer,buf)
		    x$read_buffer_len <- length(x$read_buffer)
		    x$unread <- x$unread - buflen
		}
	    }

	    # Slices off the beginning part of read_buffer.
	    slice_buffer <- function(i,size,x){
		slice <- if(i > 1) x$read_buffer[1:(i-1)] else x$read_buffer[1] 
		x$read_buffer <- if(size < x$read_buffer_len) x$read_buffer[(i+size):x$read_buffer_len] else raw()
		x$read_buffer_len <- length(x$read_buffer)
		slice
	    }

	    # prime the read_buffer
	    buf$read_buffer <- raw()
	    fill_buffer(buf)

	    while(TRUE) {
		head <- value <- NULL
		filename <- content_type <- name <- NULL
		while(is.null(head)){
		    i <- Utils$raw.match(EOLEOL,buf$read_buffer,all=FALSE)
		    if (length(i)){
			head <- slice_buffer(i,EOLEOL_size,buf)
			break
		    } else if (buf$unread){
			fill_buffer(buf)
		    } else {
			break # we've read everything and still haven't seen a valid head
		    }
		}
		if (is.null(head)){
		    warning("Bad post payload: searching for a header")
		    input$rewind()
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
		    i <- Utils$raw.match(boundary,buf$read_buffer,all=FALSE)
		    if (length(i)){
			value <- slice_buffer(i,boundary_size,buf)
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

				# handle array parameters
				paramValue <- Utils$escape(rawToChar(value[1:len]))
				paramSet <- FALSE
				if (grepl("\\[\\]$", name)) {
				   name <- sub("\\[\\]$", "", name)
				   if (name %in% names(params)) {
					params[[name]] <- c(params[[name]], paramValue)
					paramSet <- TRUE
				   }
				}
				if (!paramSet) {
				   params[[name]] <- paramValue
				}
			    }
			} 
			break
		    } else if (buf$unread){
			fill_buffer(buf)
		    } else {
			break # we've read everything and still haven't seen a valid value
		    }
		}
		if (is.null(value)){
		    # bad post payload
		    input$rewind()
		    warning("Bad post payload: searching for a body part")
		    return(NULL)
		}

		# Now search for ending markers or the beginning of another part
		while (buf$read_buffer_len < 2 && buf$unread) fill_buffer(buf)

		if (buf$read_buffer_len < 2 && buf$unread == 0){
		    # Bad stuff at the end. just return what we've got
		    # and presume everything is okay.
		    input$rewind()
		    return(params)
		}

		# Valid ending
		if (length(Utils$raw.match('--',buf$read_buffer[1:2],all=FALSE))){
		    input$rewind()
		    return(params)
		} 
		# Skip past the EOL.
		if (length(Utils$raw.match(EOL,buf$read_buffer[1:EOL_size],all=FALSE))){
		    slice_buffer(1,EOL_size,buf)
		} else {
		    warning("Bad post body: EOL not present")
		    input$rewind()
		    return(params)
		}

		# another sanity check before we try to parse another part
		if ((buf$read_buffer_len + buf$unread) < boundary_size){
		    warning("Bad post body: unknown trailing bytes")
		    input$rewind()
		    return(params)
		}
	    }
	},
	build = function(params){
	}
    )
)$new()

Mime <- setRefClass(
    'Mime',
    fields = 'MIME_TYPES',
    methods = list(
	initialize = function(...){
	    MIME_TYPES <<- new.env(hash=TRUE)
	    with(MIME_TYPES,{
	      ".3gp"     <- "video/3gpp"
	      ".a"       <- "application/octet-stream"
	      ".ai"      <- "application/postscript"
	      ".aif"     <- "audio/x-aiff"
	      ".aiff"    <- "audio/x-aiff"
	      ".asc"     <- "application/pgp-signature"
	      ".asf"     <- "video/x-ms-asf"
	      ".asm"     <- "text/x-asm"
	      ".asx"     <- "video/x-ms-asf"
	      ".atom"    <- "application/atom+xml"
	      ".au"      <- "audio/basic"
	      ".avi"     <- "video/x-msvideo"
	      ".bat"     <- "application/x-msdownload"
	      ".bin"     <- "application/octet-stream"
	      ".bmp"     <- "image/bmp"
	      ".bz2"     <- "application/x-bzip2"
	      ".c"       <- "text/x-c"
	      ".cab"     <- "application/vnd.ms-cab-compressed"
	      ".cc"      <- "text/x-c"
	      ".chm"     <- "application/vnd.ms-htmlhelp"
	      ".class"   <- "application/octet-stream"
	      ".com"     <- "application/x-msdownload"
	      ".conf"    <- "text/plain"
	      ".cpp"     <- "text/x-c"
	      ".crt"     <- "application/x-x509-ca-cert"
	      ".css"     <- "text/css"
	      ".csv"     <- "text/csv"
	      ".cxx"     <- "text/x-c"
	      ".deb"     <- "application/x-debian-package"
	      ".der"     <- "application/x-x509-ca-cert"
	      ".diff"    <- "text/x-diff"
	      ".djv"     <- "image/vnd.djvu"
	      ".djvu"    <- "image/vnd.djvu"
	      ".dll"     <- "application/x-msdownload"
	      ".dmg"     <- "application/octet-stream"
	      ".doc"     <- "application/msword"
	      ".dot"     <- "application/msword"
	      ".dtd"     <- "application/xml-dtd"
	      ".dvi"     <- "application/x-dvi"
	      ".ear"     <- "application/java-archive"
	      ".eml"     <- "message/rfc822"
	      ".eps"     <- "application/postscript"
	      ".exe"     <- "application/x-msdownload"
	      ".f"       <- "text/x-fortran"
	      ".f77"     <- "text/x-fortran"
	      ".f90"     <- "text/x-fortran"
	      ".flv"     <- "video/x-flv"
	      ".for"     <- "text/x-fortran"
	      ".gem"     <- "application/octet-stream"
	      ".gemspec" <- "text/x-script.ruby"
	      ".gif"     <- "image/gif"
	      ".gz"      <- "application/x-gzip"
	      ".h"       <- "text/x-c"
	      ".htc"     <- "text/x-component"
	      ".hh"      <- "text/x-c"
	      ".htm"     <- "text/html"
	      ".html"    <- "text/html"
	      ".ico"     <- "image/vnd.microsoft.icon"
	      ".ics"     <- "text/calendar"
	      ".ifb"     <- "text/calendar"
	      ".iso"     <- "application/octet-stream"
	      ".jar"     <- "application/java-archive"
	      ".java"    <- "text/x-java-source"
	      ".jnlp"    <- "application/x-java-jnlp-file"
	      ".jpeg"    <- "image/jpeg"
	      ".jpg"     <- "image/jpeg"
	      ".js"      <- "application/javascript"
	      ".json"    <- "application/json"
	      ".log"     <- "text/plain"
	      ".m3u"     <- "audio/x-mpegurl"
	      ".m4v"     <- "video/mp4"
	      ".man"     <- "text/troff"
	      ".manifest"<- "text/cache-manifest"
	      ".mathml"  <- "application/mathml+xml"
	      ".mbox"    <- "application/mbox"
	      ".mdoc"    <- "text/troff"
	      ".me"      <- "text/troff"
	      ".mid"     <- "audio/midi"
	      ".midi"    <- "audio/midi"
	      ".mime"    <- "message/rfc822"
	      ".mml"     <- "application/mathml+xml"
	      ".mng"     <- "video/x-mng"
	      ".mov"     <- "video/quicktime"
	      ".mp3"     <- "audio/mpeg"
	      ".mp4"     <- "video/mp4"
	      ".mp4v"    <- "video/mp4"
	      ".mpeg"    <- "video/mpeg"
	      ".mpg"     <- "video/mpeg"
	      ".ms"      <- "text/troff"
	      ".msi"     <- "application/x-msdownload"
	      ".odp"     <- "application/vnd.oasis.opendocument.presentation"
	      ".ods"     <- "application/vnd.oasis.opendocument.spreadsheet"
	      ".odt"     <- "application/vnd.oasis.opendocument.text"
	      ".ogg"     <- "application/ogg"
	      ".ogv"     <- "video/ogg"
	      ".p"       <- "text/x-pascal"
	      ".pas"     <- "text/x-pascal"
	      ".pbm"     <- "image/x-portable-bitmap"
	      ".pdf"     <- "application/pdf"
	      ".pem"     <- "application/x-x509-ca-cert"
	      ".pgm"     <- "image/x-portable-graymap"
	      ".pgp"     <- "application/pgp-encrypted"
	      ".pkg"     <- "application/octet-stream"
	      ".pl"      <- "text/x-script.perl"
	      ".pm"      <- "text/x-script.perl-module"
	      ".png"     <- "image/png"
	      ".pnm"     <- "image/x-portable-anymap"
	      ".ppm"     <- "image/x-portable-pixmap"
	      ".pps"     <- "application/vnd.ms-powerpoint"
	      ".ppt"     <- "application/vnd.ms-powerpoint"
	      ".ps"      <- "application/postscript"
	      ".psd"     <- "image/vnd.adobe.photoshop"
	      ".py"      <- "text/x-script.python"
	      ".qt"      <- "video/quicktime"
	      ".ra"      <- "audio/x-pn-realaudio"
	      ".rake"    <- "text/x-script.ruby"
	      ".ram"     <- "audio/x-pn-realaudio"
	      ".rar"     <- "application/x-rar-compressed"
	      ".rb"      <- "text/x-script.ruby"
	      ".rdf"     <- "application/rdf+xml"
	      ".roff"    <- "text/troff"
	      ".rpm"     <- "application/x-redhat-package-manager"
	      ".rss"     <- "application/rss+xml"
	      ".rtf"     <- "application/rtf"
	      ".ru"      <- "text/x-script.ruby"
	      ".s"       <- "text/x-asm"
	      ".sgm"     <- "text/sgml"
	      ".sgml"    <- "text/sgml"
	      ".sh"      <- "application/x-sh"
	      ".sig"     <- "application/pgp-signature"
	      ".snd"     <- "audio/basic"
	      ".so"      <- "application/octet-stream"
	      ".svg"     <- "image/svg+xml"
	      ".svgz"    <- "image/svg+xml"
	      ".swf"     <- "application/x-shockwave-flash"
	      ".t"       <- "text/troff"
	      ".tar"     <- "application/x-tar"
	      ".tbz"     <- "application/x-bzip-compressed-tar"
	      ".tcl"     <- "application/x-tcl"
	      ".tex"     <- "application/x-tex"
	      ".texi"    <- "application/x-texinfo"
	      ".texinfo" <- "application/x-texinfo"
	      ".text"    <- "text/plain"
	      ".tif"     <- "image/tiff"
	      ".tiff"    <- "image/tiff"
	      ".torrent" <- "application/x-bittorrent"
	      ".tr"      <- "text/troff"
	      ".txt"     <- "text/plain"
	      ".vcf"     <- "text/x-vcard"
	      ".vcs"     <- "text/x-vcalendar"
	      ".vrml"    <- "model/vrml"
	      ".war"     <- "application/java-archive"
	      ".wav"     <- "audio/x-wav"
	      ".webm"    <- "video/webm"
	      ".wma"     <- "audio/x-ms-wma"
	      ".wmv"     <- "video/x-ms-wmv"
	      ".wmx"     <- "video/x-ms-wmx"
	      ".wrl"     <- "model/vrml"
	      ".wsdl"    <- "application/wsdl+xml"
	      ".xbm"     <- "image/x-xbitmap"
	      ".xhtml"   <- "application/xhtml+xml"
	      ".xls"     <- "application/vnd.ms-excel"
	      ".xml"     <- "application/xml"
	      ".xpm"     <- "image/x-xpixmap"
	      ".xsl"     <- "application/xml"
	      ".xslt"    <- "application/xslt+xml"
	      ".yaml"    <- "text/yaml"
	      ".yml"     <- "text/yaml"
	      ".zip"     <- "application/zip"
	    })
	    callSuper(...)
	},
	file_extname = function(fname=NULL){
	    if (is.null(fname))
		base::stop("need an argument of character")
	    paste('.',rev(strsplit(fname,'.',fixed=TRUE)[[1]])[1],sep='')
	},
	mime_type=function(ext=NULL,fallback='application/octet-stream'){
	    if (is.null(ext) || !nzchar(ext) || is.null(MIME_TYPES[[ext]]))
		fallback
	    else
		MIME_TYPES[[ext]]
	}
    )
)$new()
