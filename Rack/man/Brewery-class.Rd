\name{Brewery-class}
\Rdversion{1.1}
\docType{class}
\alias{Brewery-class}
\Alias{Brewery}

\title{Class \code{"Brewery"}}
A Middleware class for mapping URLs to files that are susequently passed to brew. When a file is brewed, the two variables \code{req} (an object of class Request) and \code{res} (an object of class Response) are available for use.
}
\section{Extends}{
Class \code{"\linkS4class{Middleware}"}, directly.
Class \code{"\linkS4class{App}"}, by class "Middleware", distance 2.
}
\keyword{classes}
\section{Fields}{
  \describe{
    \item{\code{app}:}{The Rack app to defer to if nothing to brew.}
    \item{\code{url}:}{Regexp describing which URLs will be brewed.}
    \item{\code{root}:}{Directory root where brew files reside.}
    \item{\code{opt}:}{Additional arguments passed to \code{initialize()}. Will be placed along with \code{req} and \code{res} in an environment passed to the brew function.}
  }
}
\section{Methods}{
  \describe{
    \item{\code{call(env)}:}{ Called by web server}
    \item{\code{initialize(url, root, ...)}:}{ Called when invoking \code{new()}.}
  }

The following methods are inherited (from the corresponding class):
initialize ("Middleware"), set_app ("Middleware"), call ("App"), initialize ("App")
}
