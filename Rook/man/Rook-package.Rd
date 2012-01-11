\name{Rook-package}
\alias{Rook-package}
\alias{Rook}
\docType{class}
\title{
Rook: A web server interface and package for R
}
\description{
    This help page defines the Rook specification. It borrows heavily
    from Ruby's Rack project: \url{http://rack.rubyforge.org/}.

    After reading this document, read the \code{\link{Rhttpd}} help file
    as it will get you familiar with installing and running \code{Rook}
    applications. Then explore the example applications located in:

    \code{system.file('exampleApps',package='Rook')}.
}

\section{Rook applications}{
A Rook application is an R reference class object that implements a 'call'
method or an R closure that takes exactly one argument, an environment,
and returns a list with three named elements: \code{'status'}, \code{'headers'},
and \code{'body'}.
}
\section{Hello World}{

Here is a basic Rook application as a closure that implements 'hello world':

\preformatted{
function(env){
    body = paste('<h1>Hello World! This is Rook',env$rook.version,'.</h1>')
    list(
        status = 200L,
        headers = list(
            'Content-Type' = 'text/html'
        ),
        body = body
    )
}
}

And the equivalent reference class example:

\preformatted{
setRefClass(
  'HelloWorld',
  methods = list(
    call = function(env){
      list(
        status = 200L,
        headers = list(
         'Content-Type' = 'text/html'
        ),
        body = paste('<h1>Hello World! This is Rook',env$rook.version,'.</h1>')
      )
    }
  )
)
}
}

\section{The Environment}{

The environment argument is a true R environment object which the
application is free to modify. It is required to contain the following
variables:
\describe{
  \item{REQUEST_METHOD}{ The HTTP request method, such as "GET" or "POST". This
    cannot ever be an empty string, and so is always required. }

  \item{SCRIPT_NAME}{ The initial portion of the request URL's "path" that
    corresponds to the application object, so that the application knows
    its virtual "location". This may be an empty string, if the application
    corresponds to the "root" of the server.}

  \item{PATH_INFO}{ The remainder of the request URL's "path", designating the
    virtual "location" of the request's target within the application. This
    may be an empty string, if the request URL targets the application root
    and does not have a trailing slash. This value may be percent-encoded
    when I originating from a URL.}

  \item{QUERY_STRING}{ The portion of the request URL that follows the ?,
    if any. May be empty, but is always required!}

\item{SERVER_NAME, SERVER_PORT}{ When combined with SCRIPT_NAME and PATH_INFO,
    these variables can be used to complete the URL. Note however that
    HTTP_HOST, if present, should be used in preference to SERVER_NAME for
    reconstructing the request URL. SERVER_NAME and SERVER_PORT can never
    be empty strings, and so are always required.}

\item{HTTP_ Variables}{ Variables corresponding to the client-supplied
    HTTP request headers (i.e., variables whose names begin with HTTP_). The
    presence or absence of these variables should correspond with the presence
    or absence of the appropriate HTTP header in the request.}
}

In addition, the environment must include the following Rook-specific variables:

\describe{
 \item{rook.version}{ This version of Rook.}
 \item{rook.url_scheme}{'http' or 'https', depending on the request URL.}
 \item{rook.input}{See \dQuote{The Input Stream} section.}
 \item{rook.errors}{See \dQuote{The Error Stream} section.}
}
}
\section{The Input Stream}{
The rook.input variable must contain an object created from a reference
class that implements \code{read_lines()}, \code{read()}, and \code{rewind()}:

\describe{
    \item{\code{read_lines(l=-1L)}:}{takes one argument, the number of lines to read. Includes partial ending line.}
    \item{\code{read(l=-1L)}:}{takes one argument, the number of bytes to read. Returns a raw vector.}
    \item{\code{rewind()}:}{Rewinds the input stream back to the beginning.}
}
}

\section{The Error Stream}{
The rook.error variable must contain an object created from a reference
class that implements \code{flush()} and \code{cat()}:
\describe{
    \item{\code{flush()}:}{called with no arguments and makes the error stream immediately appear.}
    \item{\code{cat(...,sep=" ",fill=FALSE,labels=NULL)}:}{called with the same arguments as R's \code{"\link[base]{cat}"} without the \code{file} and append \code{argument}.}
}
}

\section{The Response}{

Rook applications return a list with three named elements: \code{'status'}, \code{'headers'},
and \code{'body'}.

\subsection{\code{'status'}}{
An HTTP status value as integer and must be greater than or equal to 100. 
}

\subsection{\code{'headers'}}{
A named list that contains only character values corresponding to valid HTTP headers.
}
\subsection{\code{'body'}}{
Either a character or raw vector. If the character vector is named
with value \code{'file'} then value of the vector is interpreted as the
location of a file.
}}
\author{ Jeffrey Horner <jeffrey.horner@gmail.com> }
\keyword{package}
