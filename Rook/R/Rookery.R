Rookery <- setRefClass(
   'Rookery',
   methods = list(
      call = function(env){
         req <- Request$new(env)
         res <- Response$new()

         paths <- getOption('Rook.Rookery.paths')

         if (is.null(paths)){
            res$header('Content-Type','text/html')
            res$write('<h2>Oops! option Rook.Rookery.paths is NULL</h2>')
            res$write("<p>You must set this to a character vector containing")
            res$write("valid directories where Rook apps live.</p>")
            return(res$finish())
         }

         # Captures foo from "/foo/.*". Presumes leading /.
         app <- strsplit(req$path_info(),'/',fixed=TRUE)[[1]][2]

         res$write("<pre>")
         res$write(paste(capture.output(ls.str(env)),collapse='\n'))
         res$write("</pre>")

         res$finish()
      }
   )
)$new()
