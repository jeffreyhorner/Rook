\name{Middleware-class}
\Rdversion{1.1}
\docType{class}
\alias{Middleware-class}
\alias{Middleware}

\title{Class \code{Middleware}}
\description{
An abstract class for building Rook Middleware applications. \code{Middleware} applications either handle the incoming web request or hand off the request to the Rook app defined in the field of the same name.
}
\examples{
# Middleware applications are typically instantiated in the argument list of
# Builder$new(), but here is stand-alone example.
# 
# Once your browser loads the app, you will see something like this in
# your location bar: http://127.0.0.1:28649/custom/middle. Add '/foo'
# onto the end of that and reload.

setRefClass(
    'FooBar',
    contains = 'Middleware',
    methods = list(
        initialize = function(...){
            # app to defer to.
            callSuper(app=App$new(function(env){
                res <- Response$new()
                res$write("<h1>I'm the deferred app.</h1>")
                res$finish()
            }))
        },
        call = function(env){
            req <- Request$new(env)
            res <- Response$new()
            if (length(grep('foo',req$path_info()))){
                res$write("<h1>I'm the middleware app.</h1>")        
                return(res$finish())
            } else {
                app$call(env)
            }
        }
    )
)
s <- Rhttpd$new()
\dontrun{
s$start(quiet=TRUE)
}
s$add(name="middle",app=getRefClass('FooBar')$new())
\dontrun{
s$browse('middle') # Opens a browser window to the app.
}
s$remove(all=TRUE)
rm(s)
}
\seealso{
The following classes implement Middleware: 
    \code{\link{Brewery}} and \code{\link{Static}}.
}
\section{Methods}{
  \describe{
    \item{\code{set_app(app)}:}{ \code{app} is a \code{\link{Rook}} application that will handle the request if this Middleware app does not. }
  }
}
\keyword{classes}
