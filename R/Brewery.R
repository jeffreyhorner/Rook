Brewery <- setRefClass(
   'Brewery',
   contains = 'Middleware',
   fields = c('url','root','opt'),
   methods = list(
      initialize = function(url,root,...){
         library("brew")
         url <<- sub("/+$","",url)
         root <<- normalizePath(root,mustWork=TRUE)
         opts <- list(...)
         if (length(opts)>0){
            opt <<- try(list2env(opts),silent=TRUE)
            if (inherits(opt,'try-error'))
               stop('Optional arguments must be named')
         } else {
            opt <<- new.env()
         }
         callSuper()
      },
      call = function(env){
         req <- Rook::Request$new(env)
         res <- Rook::Response$new()
         opt[['req']] <<- req
         opt[['res']] <<- res
         path <- env[["PATH_INFO"]]
         file_path <- try(
            normalizePath(file.path(root,path),mustWork=TRUE),
            silent=TRUE
         )
         if (!inherits(file_path, 'try-error') &&
             grepl(paste('^',url,sep=''),path) &&
             !grepl(paste('^',url,'$',sep=''),path) &&
             grepl(paste('^',root,url,.Platform$file.sep,sep=''),file_path)){

            oldwd <- setwd(dirname(file_path))
            on.exit(setwd(oldwd))
            res$write(
               paste(capture.output(brew::brew(basename(file_path),envir=opt)),
                  collapse="\n")
               )
            res$finish()
         } else {
            app$call(env)
         }
      }
   )
)
