\name{Mime-class}
\Rdversion{1.1}
\docType{class}
\alias{Mime-class}
\alias{Mime}

\title{Class \code{Mime} and object \code{Mime}}
\description{
A convenience object for determining the MIME type of a file name.
}
\examples{
Mime$file_extname('foo.png')
Mime$mime_type('.png')
}
\keyword{classes}
\section{Methods}{
  \describe{
    \item{\code{file_extname(fname=NULL)}:}{ Returns the file extensions for the given file.}
    \item{\code{mime_type(ext=NULL, fallback='application/octet-stream')}:}{ Returns the MIME type given the file extension. Be sure to include the dot character in \code{ext}. If no match is found, then the fallback MIME type is returned.}
  }
}
