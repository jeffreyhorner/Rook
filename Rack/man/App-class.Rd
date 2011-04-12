\name{App-class}
\Rdversion{1.1}
\docType{class}
\alias{App-class}
\alias{App}

\title{Class \code{"App"}}
\description{
Abstract class from which Middleware and Builder inherit. Provides the \code{app} field.

\code{"App"} can also be used to instantiate referenc classed applications wrapped around a function. See \code{"\link{Middleware}"} for an example.
}
\section{Fields}{
  \describe{
    \item{\code{app}:}{A Rackable app.}
  }
}
\seealso{
\code{"\link{is_rackable}"}, \code{"\link{Builder}"}, and \code{"\link{Middleware}"}.
}
\keyword{classes}
