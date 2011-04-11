\name{Middleware-class}
\Rdversion{1.1}
\docType{class}
\alias{Middleware-class}
\alias{Middleware}

\title{Class \code{"Middleware"}}
\description{
An Abstract class for building Rack Middleware applications. Middleware apps either handle the incoming web request or hand off the request to the Rack app defined in the field of the same name.
}
\section{Extends}{
Class \code{"\linkS4class{App}"}, directly.
}
\seealso{
The following classes implement Middleware: 
    \code{\linkS4class{Brewery}},\code{\linkS4class{Static}}, and
    \code{\linkS4class{URLMap}}.    
}
\section{Fields}{
  \describe{
    \item{\code{app}:}{A Rackable App.}
  }
}
\section{Methods}{
  \describe{
    \item{\code{set_app(app)}:}{ The app that will handle the request if this Middleware app does not. }
    \item{\code{initialize(...)}:}{ create a new Middleware object. Called by extending class's \code{initialize()} method.}
  }

The following methods are inherited (from the corresponding class):
call ("App"), initialize ("App")
}
\keyword{classes}
