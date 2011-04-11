\name{Rack-package}
\alias{Rack-package}
\alias{Rack}
\docType{class}
\title{
Rack: A web server interface and package for R
}
\description{
    This help page defines the Rack specification. For others see:
}

\section{Rack applications}{
A Rack application is an R reference class object that implements a 'call'
method or an R closure that takes exactly one argument, an environment,
and returns a list with three named elements: the 'status', the 'headers',
and the 'body'.
}
\section{Hello World}{

Here is a basic Rack application as a closure that implements 'hello world':

\preformatted{
    function(env){
	body = paste('<h1>Hello World! This is Rack',env$rack.version,'.</h1>')
	list(
	    status = 200L,
	    headers = list(
		'Content-Type' = 'text/html'
	    ),
	    body = body
	)
    }
}

And the equivalent referenc class example:

\preformatted{
    setRefClass(
	'HelloWorld',
	methods = list(
	    call = function(env){
		body = paste('<h1>Hello World! This is Rack',env$rack.version,'.</h1>')
		list(
		    status = 200L,
		    headers = list(
			'Content-Type' = 'text/html'
		    ),
		    body = body
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
    these variables can be used to complete the URL. Note, however, that
    HTTP_HOST, if present, should be used in preference to SERVER_NAME for
    reconstructing the request URL. SERVER_NAME and SERVER_PORT can never
    be empty strings, and so are always required.}

\item{HTTP_ Variables}{ Variables corresponding to the client-supplied
    HTTP request headers (i.e., variables whose names begin with HTTP_). The
    presence or absence of these variables should correspond with the presence
    or absence of the appropriate HTTP header in the request.}
}

In addtion, the environment must include the following Rack-specific variables:

\describe{
 \item{rack.version}{ This version of Rack.}
 \item{rack.url_scheme}{http or https, depending on the request URL.}
 \item{rack.input}{See below, the input stream.}
 \item{rack.errors}{See below, the error stream.}
}
}
\section{The Input Stream}{

The rack.input variable must contain an object created from a reference
class and respond to readLines, read, and rewind:

\describe{
    \item{readLines}{takes one argument, the number of lines to read. Includes partial ending line.}
    \item{read}{takes one argument, the number of bytes to read. Returns a raw vector.}
    \item{rewind}{Rewinds the input stream back to the beginning.}
}
}

\section{The Error Stream}{

The rack.error variable must contain an object created from a reference
class and must respond to flush and cat:
\describe{
    \item{flush}{called with no arguments and makes the error stream immediately appear.}
    \item{cat}{called with the same arguments as R's cat without the file and append argument.}
}
}

\section{The Response}{

\subsection{The Status}{
This is an HTTP status value and must be greater than or equal to 100.
}

\subsection{The Headers}{
This is a named list that contains string values only corresponding to valid HTTP headers.
}
\subsection{The Body}{
This is either a character or raw vector. If the character vector is
named with value 'file' then value of the vector is interpreted as the
location of a file.
}
}
\author{ Jeffrey Horner <jeffrey.horner@gmail.com> }
\keyword{ package }
