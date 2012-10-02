Brewery <- setRefClass(
   'Brewery',
   contains = 'Middleware',
   fields = c('url','root','opt'),
   methods = list(
      initialize = function(url,root,...){
         url <<- paste('^',url,sep='')
         root <<- root
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
         opt[['req']] <<- req;
         opt[['res']] <<- res;
         path = env[["PATH_INFO"]]
         file_path = file.path(root,path)
         cat('wd:',getwd(),'file_path:',file_path,'\n',file=stderr())
         cat('url:',url,'path:',path,'\n',file=stderr())
         if (length(grep(url,path))>0 && file.exists(file_path)){
            oldwd <- setwd(dirname(file_path))
            on.exit(setwd(oldwd))
            res$write(
               paste(capture.output(brew(basename(file_path),envir=opt)),
                  collapse="\n")
               )
            res$finish()
         } else
         app$call(env)
      }
   )
)
