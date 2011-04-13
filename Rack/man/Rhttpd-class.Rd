\name{Rhttpd-class}
\Rdversion{1.1}
\docType{class}
\alias{Rhttpd-class}
\alias{Rhttpd}

\title{Class \code{"Rhttpd"}}
\description{
A convenience class for installing and running Rack applications.
}
\seealso{
\code{"\link{RhttpdApp}"}
}
\examples{

# Create an Rhttpd object and start the internal web server. Note that
# if there are no applications added, then the default RackTest app
# located here:
#  system.file('exampleApps/RackTestApp.R',package='Rack')
# is automatically added.

s <- Rhttpd$new()
s$start(quiet=TRUE)
s$print()
\dontrun{
s$browse(1)
}
#  Stops the server but doesn't uninstall the app
s$stop()

s$remove(1)
rm(s)

}
\keyword{classes}
\section{Methods}{
  \describe{
    \item{\code{open(x)} or \code{browse(x)}:}{ Calls browsURL on the installed Rack application designated by \code{x}. \code{x} is either an integer or a character. See the output of \code{print()}.}
    \item{\code{print()} or \code{show()}:}{ Lists the installed Rack applications.}
    \item{\code{remove(app)}:}{ Removes the application known to the server. \code{app} can be an \code{RhttpdApp} object previously added, the name of the application as a character string, or an index as a numeric or integer value. See the output of \code{print()}.}
    \item{\code{full_url(i)}:}{ Returns the absolute url to the application for the given index.}
    \item{\code{start(listen='127.0.0.1', port=getOption('help.ports'), quiet=FALSE)}:}{ Starts the server on the given \code{listen} address and \code{port}. \code{listen} is always character string. Note that if there are no applications added to the object prior to starting, then the RackTestApp located in \code{"system.file('exampleApps/RackTestApp.R',package='Rack')"} is automatically added.}
    \item{\code{new()}:}{ Create a new \code{Rhttpd} object. }
    \item{\code{launch(...)}:}{ Combines the steps of starting the server, creating an \code{RhttpdApp} object from \code{...}, adding it to the server, and openning the app in the browser.}
    \item{\code{debug()}:}{ Returns the integer value provided by \code{getOption('Rhttpd_debug')} or 0 if the option is NULL. }
    \item{\code{stop()}:}{ Stops the server. }
    \item{\code{add(app)}:}{ Adds a new \code{Rack} application to the server. }
  }
}
