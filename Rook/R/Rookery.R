.RookeryApp <- setRefClass(
   'RookeryApp',
   fields = c('app','name','appEnv','configured','workingDir'),
   methods = list(
      initialize = function(app=NULL,name=NULL,...){
         if (is.null(name) || !is.character(name)){
            base::warning("Need a proper app 'name'")
            .self$configured <- FALSE
            return(callSuper(...))
         }

         .self$name <- name

         if (is.character(app) && file.exists(app)){
            .self$workingDir <- dirname(app)
            oldwd <- setwd(.self$workingDir)
            on.exit(setwd(oldwd))
            appEnv <<- new.env(parent=globalenv())
            appEnv$.appFile <<- normalizePath(basename(app))
            appEnv$.mtime <<- as.integer(file.info(appEnv$.appFile)$mtime)
            sys.source(appEnv$.appFile,envir=.self$appEnv)

            if (exists(.self$name,.self$appEnv,inherits=FALSE))
               .self$app <- get(.self$name,.self$appEnv)
            else if (exists('app',.self$appEnv,inherits=FALSE))
               .self$app <- get('app',.self$appEnv)
            else {
               base::warning("Cannot find a suitable app in file ",app)
               .self$app <- NULL
            }
         } else {
            base::warning("File does not exist: ",app)
            .self$configured <- FALSE;
         }

         if (!is_rookable(.self$app)){
            base::warning("App ",name," is not rookable'")
            .self$configured <- FALSE;
         } else {
            .self$configured <- TRUE;
         }

         callSuper(...)
      }
   )
)

.Rookery <- setRefClass(
   'Rookery',
   fields = c('req','res','appHash','messages'),
   methods = list(
      initialize = function(...){
         appHash <<- new.env()
         messages <<- list(
            emptypath = c(
               '<h2>Oops! option Rook.Rookery.paths is NULL</h2>',
               '<p>You must set this to a character vector containing',
               'valid directories where Rook apps live.</p>'
            ),
            nodots = c(
               '<h2>Apps cannot be named . or ..</h2>'
            )
         )
         callSuper(...)
      },
      message = function(name,opt=NULL){
         msg <- paste(messages,collapse='\n')
         if (!is.null(opt))
            msg <- sprintf(msg,opt)
         res$header('Content-Type','text/html')
         res$write(msg)
      },
      findSuitableApp = function(appName){
         if (appName %in% c('.','..')){
            message('nodots')
            return(NULL)
         }
         paths <- getOption('Rook.Rookery.paths')

         if (is.null(paths)){
            message('emptypath')
            return(NULL)
         }
         #for (p in paths){
         #   appReg <- paste('^',appName,'$',sep='')
         #   if (any(grepl(appReg,basename(list.dirs(p,recursive=FALSE))))){
         #   } else if (
         #}
      },
      listAllApps = function(){
      },
      call = function(env){
         req <<- Request$new(env)
         res <<- Response$new()

         # Captures foo from "/foo/.*". Presumes leading /.
         appName <- strsplit(req$path_info(),'/',fixed=TRUE)[[1]][2]
         if (is.na(appName)){
            listAllApps()
         } else {
            app <- findSuitableApp(appName)

            if (!is.null(app)){
               new_path_info <- req$path_info()
               req$path_info(sub(paste("/",appName,sep=''),'',new_path_info))
               oldwd <- setwd(app$workingDir)
               on.exit(setwd(oldwd))
               if (is(app$app,'function')) {
                  return(app$app(env))
               } else {
                  return(app$app$call(env))
               }
            }
         }

         #res$write("<pre>")
         #res$write(paste(capture.output(ls.str(env)),collapse='\n'))
         #res$write("</pre>")

         res$finish()
      }
   )
)
