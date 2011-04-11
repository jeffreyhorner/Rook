\name{App-class}
\Rdversion{1.1}
\docType{class}
\alias{App-class}
\alias{App}

\title{Class \code{"App"}}
\description{
Abstract class from which Middleware and Builder inherit.
}
\section{Fields}{
  \describe{
    \item{\code{app}:}{A Rackable app.}
  }
}
\section{Methods}{
  \describe{
    \item{\code{call(env)}:}{ Invokes the applications (called by the web server). }
    \item{\code{initialize(app, ...)}:}{ Sets the \code{app} field.}
  }
}
\seealso{is_rackable}
\keyword{classes}
