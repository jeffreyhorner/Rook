\name{URLMap-class}
\Rdversion{1.1}
\docType{class}
\alias{URLMap-class}
\alias{URLMap}

\title{Class \code{URLMap}}
\description{
A \code{\link{Rook}} application that maps url paths to other \code{Rook} applications.
}
\seealso{ 
\code{\link{Rhttpd}}.
}
\examples{
s <- Rhttpd$new()
s$add(
    name="pingpong",
    app=Rook::URLMap$new(
        '/ping' = function(env){
            req <- Rook::Request$new(env)
            res <- Rook::Response$new()
            res$write(sprintf('<h1><a href="\%s">Pong</a></h1>',req$to_url("/pong")))
            res$finish()
        },
        '/pong' = function(env){
            req <- Rook::Request$new(env)
            res <- Rook::Response$new()
            res$write(sprintf('<h1><a href="\%s">Ping</a></h1>',req$to_url("/ping")))
            res$finish()
        },
        '/?' = function(env){
            req <- Rook::Request$new(env)
            res <- Rook::Response$new()
            res$redirect(req$to_url('/pong'))
            res$finish()
        }
    )
)
s$start(quiet=TRUE)
\dontrun{
s$browse('pingpong')
}
s$remove('pingpong')
s$stop()
rm(s)
}
\keyword{classes}
\section{Methods}{
  \describe{
    \item{\code{new(...)}:}{ Creates a \code{Rook} application. All arguments must be \code{Rook} applications and named as in the example.}
  }
}
