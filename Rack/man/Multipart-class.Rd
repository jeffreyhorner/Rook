\name{Multipart-class}
\Rdversion{1.1}
\docType{class}
\alias{Multipart-class}
\alias{Multipart}

\title{Class \code{"Multipart"}}
\description{
A convenience object for parsing multipart/form-data POST payloads.
}
\examples{
s <- Rhttpd$new()
s$start(quiet=TRUE)
s$add(RhttpdApp$new(name="multi",
    app=function(env){
	req <- Request$new(env)
	res <- Response$new()
	res$write('<form enctype="multipart/form-data" method=POST>')
	res$write('Upload a file: <input type=file name=fileUpload>')
	res$write('<input type=submit></form><br>')
	post <- Multipart$parse(env)
	if (length(post)){
	    poststr <- paste(capture.output(str(post),file=NULL),collapse='\n')
	    res$write(c('<pre>',poststr,'</pre>'))
	}
	res$finish()
    }
))
\dontrun{
s$browse('multi') # Opens a browser window to the app.
}
rm(s)
}
\seealso{
\code{"\link{Rhttpd}"}, \code{"\link{RhttpdApp}"}, \code{"\link{Request}"}, and \code{"\link{Response}"}.
}
\section{Methods}{
  \describe{
    \item{\code{parse(env)}:}{ Returns parsed POST payload as a named list. \code{env} is a Rack created environment. see the \code{"\link{Rack}"} specification for more information.}
  }
}
\keyword{classes}
