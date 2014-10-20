\name{Response-class}
\Rdversion{1.1}
\docType{class}
\alias{Response-class}
\alias{Response}

\title{Class \code{Response}}
\description{
A convenience class for creating \code{\link{Rook}} responses.
}
\seealso{
\code{\link{Rhttpd}} and \code{\link{Request}}.
}
\examples{
s <- Rhttpd$new()
\dontrun{
s$start(quiet=TRUE)
}
s$add(name="response",
    app=function(env){
        req <- Request$new(env)
        res <- Response$new()
        res$write('hello')
        res$finish()
    }
)
\dontrun{
s$browse('response') # Opens a browser window to the app.
}
s$remove(all=TRUE)
rm(s)
}
\keyword{classes}
\section{Methods}{
  \describe{
    \item{\code{header(key, value)}:}{ Sets an HTTP header for the
      response. Both \code{key} and \code{value} must be character strings. If \code{value} is missing, then the \code{header} value is returned.}
    \item{\code{redirect(target, status=302)}:}{ Sets up an HTTP redirect to the \code{target} url. }
    \item{\code{write(str)}:}{ Takes a character vector and appends it to the response body. }
    \item{\code{new(body='', status=200, headers=list())}:}{ Create a new Response object. \code{body} is a character vector, \code{status} is an HTTP status value. \code{headers} is a named list.}
    \item{\code{set_cookie(key, value)}:}{ Sets an HTTP cookie for the response. Both \code{key} and \code{value} must be character strings.}
    \item{\code{delete_cookie(key, value)}:}{ Sends appropriate HTTP header to delete the associated cookie on the client. \code{key} and \code{value} must be character strings.}
    \item{\code{finish()}:}{ Returns the response according to the Rook specification.}
  }
}
