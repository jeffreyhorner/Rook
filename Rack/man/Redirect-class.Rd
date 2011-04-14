\name{Redirect-class}
\Rdversion{1.1}
\docType{class}
\alias{Redirect-class}
\alias{Redirect}

\title{Class \code{Redirect}}
\description{
A \code{\link{Rack}} application whose only role is to return an HTTP redirect header to the given url.
}
\seealso{
See \code{\link{Brewery}} for an example. 
}
\keyword{classes}
\section{Methods}{
  \describe{
    \item{\code{new(url)}:}{ Returns a \code{Rack} object. \code{url} is a character string whose value is a full or relative url to which the browser is redirected.}
  }
}
