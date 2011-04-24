\name{File-class}
\Rdversion{1.1}
\docType{class}
\alias{File-class}
\alias{File}

\title{Class \code{File}}
\description{
A Rook application that serves static files from a root directory, according to the path info of the Rook request.
}
\examples{
# This example serves all your files in /etc (on UNIX and Mac only).
#
# Note that when you open the application, you will see the word
# 'Forbidden'. "File" doesn't serve directories, so you must amend the
# url in the location bar with the file you want to view. Try adding /passwd.

s <- Rhttpd$new()
\dontrun{
s$start(quiet=TRUE)
}
s$add(name="etc",app=File$new('/etc'))
\dontrun{
s$browse('etc') # Opens a browser window to the app.
}
s$remove(all=TRUE)
rm(s)
}
\section{Methods}{
\describe{
    \item{\code{new(root):}}{
	\code{root} is the name of the directory from where to serve files.
    }
}}
\seealso{
\code{\link{Rhttpd}}.
}
\keyword{classes}
