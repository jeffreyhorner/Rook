\name{Rhttpd-class}
\Rdversion{1.1}
\docType{class}
\alias{Rhttpd-class}
\alias{Rhttpd}

\title{Class \code{Rhttpd}}
\description{
A convenience class for installing and running Rack applications.
}
\details{
\code{Rhttpd} wraps all the functionality of the internal web server into a reference class. It hides the details of starting and stopping the server and adding and removing \code{Rack} applications from the server.

\code{RhttpdApp} along with \code{\link{Rhttpd}} hide these details by allowing a user to create application objects specifying only their name and the application. There is currently a restriction of 64 characters or less on application names.

NOTE: When a file is given as the value of the \code{app} argument to \code{new()}, it is monitored for timestamp changes. If a changes occurs in the modification time as returned by \code{\link{file.info}}, then the file is sourced prior to handling subsequent requests.
}
\seealso{
\code{\link{RhttpdApp}}
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
s$add(RhttpdApp$new(name='hmisc',
    app=system.file('exampleApps/Hmisc/config.R',package='Rack')))
s$add(RhttpdApp$new(name='hello',
    app=system.file('exampleApps/helloworld.R',package='Rack')))
s$add(RhttpdApp$new(name='helloref',
    app=system.file('exampleApps/helloworldref.R',package='Rack')))
s$add(RhttpdApp$new(name='summary',
    app=system.file('exampleApps/summary.R',package='Rack')))
}
#  Stops the server but doesn't uninstall the app
s$print()
s$stop()
lapply(names(s$appList),s$remove)
rm(s)
}
\section{Fields}{
  \describe{
    \item{\code{appList}:}{Named list of all \code{Rack} applications added to this server.}
  }
}
\keyword{classes}
\section{Methods}{
  \describe{
    \item{\code{open(x)} or \code{browse(x)}:}{ Calls \code{\link{browseURL}} on the installed Rack application designated by \code{x}. \code{x} is either an integer or a character string. See the output of \code{print()}.}
    \item{\code{print()} or \code{show()}:}{ Lists the installed Rack applications.}
    \item{\code{remove(app)}:}{ Removes the application known to the server. \code{app} can be an \code{RhttpdApp} object previously added, the name of the application as a character string, or an index as a numeric or integer value. See the output of \code{print()}.}
    \item{\code{full_url(i)}:}{ Returns the absolute url to the application for the given index.}
    \item{\code{start(listen='127.0.0.1', port=getOption('help.ports'), quiet=FALSE)}:}{ Starts the server on the given \code{listen} address and \code{port}. \code{listen} is always character string. Note that if there are no applications added to the object prior to starting, then the RackTestApp located in \code{system.file('exampleApps/RackTestApp.R',package='Rack')} is automatically added.}
    \item{\code{new()}:}{ Create a new \code{Rhttpd} object. }
    \item{\code{launch(...)}:}{ Combines the steps of starting the server, creating an \code{RhttpdApp} , adding it to the server, and openning the app in the browser. \code{...} arguement is passed to \code{RhttpdApp$new()}.}
    \item{\code{debug()}:}{ Returns the integer value provided by \code{getOption('Rhttpd_debug')} or 0 if the option is NULL. }
    \item{\code{stop()}:}{ Stops the server. }
    \item{\code{add(app)}:}{ Adds a new \code{Rack} application to the server. }
  }
}
