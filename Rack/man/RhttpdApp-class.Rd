\name{RhttpdApp-class}
\Rdversion{1.1}
\docType{class}
\alias{RhttpdApp-class}
\alias{RhttpdApp}

\title{Class \code{RhttpdApp}}
\description{
Creates a Rack application ready to add to an \code{\link{Rhttpd}} server.
}
\details{
The internal web server allows dispatching to user-defined closures located in tools:::.httpd.handlers.env. For instance, if a handler named 'foo' is placed there, then the url path to that handler is /custom/foo. 

\code{RhttpdApp} along with \code{\link{Rhttpd}} hide these details by allowing a user to create application objects specifying only their name and the application. There is currently a restriction of 64 characters or less on application names.

NOTE: When a file is given as the value of the \code{app} argument to \code{new()}, it is monitored for timestamp changes. If a changes occurs in the modification time as returned by \code{\link{file.info}}, then the file is sourced prior to handling subsequent requests.
}
\seealso{
\code{\link{Rhttpd}}.
}
\examples{

# You must have the Hmisc package installed to run this example.
s <- Rhttpd$new()
\dontrun{
s$add(RhttpdApp$new(
    name='Hmisc',
    app=system.file('exampleApps/Hmisc/config.R',package='Rack')
))
s$start(quiet=TRUE)
s$browse(1)
s$remove(1)
}

#  Stops the server but doesn't uninstall the app
s$stop()

rm(s)
}
\keyword{classes}
\section{Methods}{
  \describe{
    \item{\code{new(app, name)}:}{ Creates an object of class \code{RhttpdApp}. Argument \code{app} can be any \code{\link{Rack}} aware object or it can be a location to a file whose source creates a Rack aware object. That object must be named either \code{'app'} or the value of \code{name}. \code{name} is a character vector.}
  }
}
