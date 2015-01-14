Rook: A web server interface for R
=======================================

This specification defines the interface between web servers and R applications.

Rook applications
-----------------

A Rook application is an R reference class object that implements a 'call'
method or an R closure that takes exactly one argument, an environment,
and returns a list with three named elements: the 'status', the 'headers',
and the 'body'.

Hello World
-----------

Here is a basic Rook application as a closure that implements 'hello world':

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

And the equivalent referenc class example:

    setRefClass(
	'HelloWorld',
	methods = list(
	    call = function(env){
		body = paste('<h1>Hello World! This is Rook',env$rook.version,'.</h1>')
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

The Environment
---------------

The environment argument is a true R environment object which the
application is free to modify. It is required to contain the following
variables:

- REQUEST_METHOD: The HTTP request method, such as "GET" or "POST". This
    cannot ever be an empty string, and so is always required.

- SCRIPT_NAME:	The initial portion of the request URL‘s "path" that
    corresponds to the application object, so that the application knows
    its virtual "location". This may be an empty string, if the application
    corresponds to the "root" of the server.

- PATH_INFO:  The remainder of the request URL‘s "path", designating the
    virtual "location" of the request‘s target within the application. This
    may be an empty string, if the request URL targets the application root
    and does not have a trailing slash. This value may be percent-encoded
    when I originating from a URL.

- QUERY_STRING:	The portion of the request URL that follows the ?,
    if any. May be empty, but is always required!

- SERVER_NAME, SERVER_PORT:   When combined with SCRIPT_NAME and PATH_INFO,
    these variables can be used to complete the URL. Note, however, that
    HTTP_HOST, if present, should be used in preference to SERVER_NAME for
    reconstructing the request URL. SERVER_NAME and SERVER_PORT can never
    be empty strings, and so are always required.

- HTTP_ Variables:    Variables corresponding to the client-supplied
    HTTP request headers (i.e., variables whose names begin with HTTP_). The
    presence or absence of these variables should correspond with the presence
    or absence of the appropriate HTTP header in the request.

In addtion, the environment must include the following Rook-specific variables:

- rook.version:	    This version of Rook.
- rook.url_scheme:    http or https, depending on the request URL.
- rook.input:	    See below, the input stream.
- rook.errors:	    See below, the error stream.

The Input Stream
----------------

The rook.input variable must contain an object created from a reference
class and respond to read_lines, read, and rewind:

read_lines: takes one argument, the number of lines to read. Includes partial ending line.
read: takes one argument, the number of bytes to read. Returns a raw vector.
rewind: Rewinds the input stream back to the beginning.

The Error Stream
----------------

The rook.error variable must contain an object created from a reference
class and must respond to flush and cat:

flush: called with no arguments and makes the error stream immediately appear.
cat: called with the same arguments as R's cat without the file and append argument.

The Response
============

The Status
----------

This is an HTTP status value and must be greater than or equal to 100.

The Headers
-----------

This is a named list that contains string values only corresponding to valid HTTP headers.

The Body
--------

This is either a character or raw vector. If the character vector is
named with value 'file' then value of the vector is interpreted as the
location of a file.
