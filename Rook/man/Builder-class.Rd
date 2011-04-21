\name{Builder-class}
\Rdversion{1.1}
\docType{class}
\alias{Builder-class}
\alias{Builder}

\title{Class \code{Builder}}
\description{
A convenience object for combining various \code{Middleware} with a default application to create a more complex Rook application.
}
\examples{

# The following is the Hmisc example. Explore the folder
# system.file('exampleApps/Hmisc',package='Rook') for more information.
s <- Rhttpd$new()
\dontrun{
library(Hmisc)
dir.create(file.path(tempdir(),'plots'),showWarnings=FALSE)
s$add( name="Hmisc",
    app=Builder$new(
        Static$new(
            urls = c('/css','/images','/javascript'),
            root = system.file('exampleApps/Hmisc',package='Rook')
        ),
        Static$new(urls='/plots',root=tempdir()),
        Brewery$new(
            url='/brew',
            root= system.file('exampleApps/Hmisc',package='Rook'),
            imagepath=file.path(tempdir(),'plots'),
            imageurl='../plots/'
        ),
        Redirect$new('/brew/useR2007.rhtml')
    )
)
s$start(quiet=TRUE)
s$browse('Hmisc') # Opens a browser window to the application.
}
s$remove(all=TRUE)
s$stop()
}
\seealso{
\code{\link{Rhttpd}}, \code{\link{Static}}, \code{\link{Brewery}}, and \code{\link{Redirect}}.
}
\keyword{classes}
\section{Methods}{
  \describe{
    \item{\code{new(...)}:}{ Arguments can be any \code{Middleware} object while the last argument in the list must be a valid \code{Rook} application. That is, it will handle the incoming request without deferring to another application. }
  }
}
