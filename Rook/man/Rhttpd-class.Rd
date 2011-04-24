\name{Rhttpd-class}
\Rdversion{1.1}
\docType{class}
\alias{Rhttpd-class}
\alias{Rhttpd}

\title{Class \code{Rhttpd}}
\description{
\code{Rhttpd} is a convenience class for installing and running Rook
applications. It hides the details of starting and stopping the server
and adding and removing \code{Rook} applications from the server.

Users starts by creating one \code{Rhttpd} object, then adding
applications to it, and then starting the server (see the section
\dQuote{Examples} for a typical session). There are no restrictions on
creating more than one server object, but know that it only manages the
applications that are added to it and not others.

Applications can be added and removed regardless of whether or
not the server is running.  Stopping the server does not remove
any applications. Adding an application with the same name as
one already installed simply overwrites the one installed. If
the server is started with no applications installed, it will
install the application named \code{RookTestApp} located in:

\code{system.file('exampleApps/RookTestApp.R',package='Rook')}.

Also, see \code{\link{browseURL}} to learn how to get R to
automatically launch your favorite web browser.
} 

\seealso{
\code{\link{RhttpdApp}} 
}
\examples{

# Create an Rhttpd object and start the internal web server. Note that
# if there are no applications added, then the default RookTest app in
# system.file('exampleApps/RookTestApp.R',package='Rook') is automatically
# added.

s <- Rhttpd$new()
\dontrun{
s$start(quiet=TRUE)
s$browse(1)
}
s$print()

# Be sure to install the Hmisc package before installing and running
# this application. You will want to; it's a pretty good one.
# s$add(
#    app=system.file('exampleApps/Hmisc/config.R',package='Rook'),
#    name='hmisc')

s$add(
    app=system.file('exampleApps/helloworld.R',package='Rook'),
    name='hello')
s$add(
    app=system.file('exampleApps/helloworldref.R',package='Rook'),
    name='helloref')
s$add(
    app=system.file('exampleApps/summary.R',package='Rook'),
    name='summary')

s$print()

#  Stops the server but doesn't uninstall the app
\dontrun{
s$stop()
}
s$remove(all=TRUE)
rm(s)
}
\keyword{classes}
\section{Methods}{
  \describe{
    \item{\code{open(x)} or \code{browse(x)}:}{ Calls \code{\link{browseURL}} on the installed Rook application designated by \code{x}. \code{x} is either an integer or a character string. See the output of \code{print()}.}
    \item{\code{print()} or \code{show()}:}{ Lists the installed Rook applications.}
    \item{\code{remove(app,all=FALSE)}:}{ Removes the application known to the server. \code{app} can be an \code{RhttpdApp} object previously added, the name of the application as a character string, or an index as a numeric or integer value. See the output of \code{print()}.}
    \item{\code{full_url(i)}:}{ Returns the absolute url to the application for the given index.}
    \item{\code{start(listen='127.0.0.1', port=getOption('help.ports'), quiet=FALSE)}:}{ Starts the server on the given \code{listen} address and \code{port}. \code{listen} is always character string. Note that if there are no applications added to the object prior to starting, then the RookTestApp located in \code{system.file('exampleApps/RookTestApp.R',package='Rook')} is automatically added.}
    \item{\code{new()}:}{ Create a new \code{Rhttpd} object. }
    \item{\code{launch(...)}:}{ Combines the steps of starting the server, creating an \code{RhttpdApp} object, adding it to the server, and opening the app in the browser. \code{...} argument is passed to \code{RhttpdApp$new()}.}
    \item{\code{debug()}:}{ Returns the integer value provided by \code{getOption('Rhttpd_debug')} or 0 if the option is NULL. }
    \item{\code{stop()}:}{ Stops the server. }
    \item{\code{add(app=NULL,name=NULL)}:}{ Adds a new \code{Rook} application to the server. \code{app} can be an \code{\link{RhttpdApp}} object or any \code{Rook} application. \code{name} is a character string and is ignored if \code{app} is an \code{RhttpdApp} object.}
  }
}
