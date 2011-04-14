\name{Static-class}
\Rdversion{1.1}
\docType{class}
\alias{Static-class}
\alias{Static}

\title{Class \code{Static}}
\description{
A \code{\link{Middleware}} class for serving static files from a root directory given a set of url paths.
}
\seealso{
See \code{\link{Builder}} for an example.
}
\keyword{classes}
\section{Methods}{
  \describe{
    \item{\code{new(urls, root)}:}{ Creates a new object. \code{urls} is a character vector whose elements must start with a \code{'/'}. \code{root} is a length 1 character vector whose value must be a valid directory. }
  }
}
