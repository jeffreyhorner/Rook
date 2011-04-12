\name{Builder-class}
\Rdversion{1.1}
\docType{class}
\alias{Builder-class}
\alias{Builder}

\title{Class \code{"Builder"}}
\description{
A convenience object for combining various Middleware with a default application to create a more complex Rack application.
}
\examples{

# The following is the Hmisc example. Explore the folder
# system.file('exampleApps/Hmisc/',package='Rack') for more information.
\dontrun{
library(Hmisc)
dir.create(file.path(tempdir(),'plots'),showWarnings=FALSE)
app <- Builder$new(
    Static$new(
	urls = c('/css','/images','/javascript'),
	root = '.'
    ),
    Static$new(urls='/plots',root=tempdir()),
    Brewery$new(
	url='/brew',
	root='.',
	imagepath=file.path(tempdir(),'plots'),
	imageurl='../plots/'
    ),
    Redirect$new('/brew/useR2007.rhtml')
)
}}
\seealso{
\code{"\link{Static}"}, \code{"\link{Brewery}"}, and \code{"\link{Redirect}"}.
}
\section{Extends}{
Class \code{"\link{App}"}, directly.

}
\keyword{classes}
\section{Methods}{
  \describe{
    \item{\code{new(...)}:}{ Arguments can be any Middleware object while the last argument in the list must be a valid Rack application. That is, it will handle the incoming request without deferring to another application. }
  }
}
