File <- setRefClass(
    'File',
    fields = c('root','path_info','path'),
    methods = list(
	initialize  = function(root,...){
	    root <<- root
	    callSuper(...)
	},
	call = function(env){
	    path_info <<- Utils$unescape(env[["PATH_INFO"]])

	    if (length(grep('..',path_info,fixed=TRUE))){
			return(forbidden())
		}

      if (grepl('#',path_info))
        path_info <<- strsplit(path_info,'#')[[1]]

      if (grepl('\\?',path_info))
        path_info <<- strsplit(path_info,'\\?',)[[1]]

	    path <<- normalizePath(file.path(root,path_info))

	    if (file_test('-d',path)){
        if(!grepl(".*/$", path_info)){
          return(redirect(paste(env[["SCRIPT_NAME"]], env[["PATH_INFO"]], "/", sep=""), status=301))
        }
			newpath <- file.path(path, "index.html")
			if(file.exists(newpath)){
				path <<- normalizePath(newpath)
				serving()
			} else {
				return(indexdir())
			}
		} else if (file.exists(path)){
			serving()
		} else {
			not_found()
		}
	},
	forbidden = function(){
	    body = 'Forbidden\n'
	    list(
		status=403L,
		headers = list(
		    'Content-type' = 'text/plain',
		    'Content-Length'  = as.character(nchar(body)),
		    'X-Cascade' = 'pass'
		),
		body = body
	    )
	},
	indexdir = function(){
    body <- paste(list.files(path), collapse="\n")
		list(
			status=200L,
			headers = list(
				'Content-type' = 'text/plain',
				'Content-Length'  = as.character(nchar(body))
			),
			body = body
		)		
	},
  redirect = function(location){
    res <- Response$new()
    res$redirect(location, status=302)
    res$finish()    
  },
	serving = function(){
	    fi <- file.info(path)
	    if (fi$size > 0) {
		body = readBin(path,'raw',fi$size)
	    } else {
		body <- path
		names(body) <- 'file'
	    }
	    list (
		status=200L,
		headers = list(
		    'Last-Modified' = Utils$rfc2822(fi$mtime),
		    'Content-Type' = Mime$mime_type(Mime$file_extname(basename(path))),
		    'Content-Length' = as.character(fi$size)
		),
		body=body
	    )
	},
	not_found = function(){
	    body <- paste("File not found:",path_info,"\n")
	    list(
		status=404L, 
		headers = list(
		    "Content-Type" = "text/plain",
		    "Content-Length" = as.character(nchar(body)),
		    "X-Cascade" = "pass"
		),
		body = body
	    )
	}
    )
)
