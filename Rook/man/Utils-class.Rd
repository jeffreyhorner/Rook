\name{Utils-class}
\Rdversion{1.1}
\docType{class}
\alias{Utils-class}
\alias{Utils}

\title{Class \code{Utils}}
\description{
A convenience object for working with various aspects of web requests and responses.
}
\seealso{
\code{\link{Multipart}}.
}
\examples{
Utils$bytesize('foo')
Utils$escape('foo bar')
Utils$unescape('foo+bar')
Utils$escape_html('foo <bar>')
Utils$escape('foo <bar>')
Utils$escape('foo\n<bar>')
Utils$status_code('OK')
Utils$status_code('Found')
Utils$status_code('Not Found')
x <- Utils$parse_query('foo=1&bar=baz')
x
Utils$rfc2822(Sys.time())
Utils$timezero()
Utils$build_query(x)
rm(x)
}
\keyword{classes}
\section{Methods}{
  \describe{
    \item{\code{bytesize(string=NULL)}:}{ Returns size in bytes for \code{string}, a character vector. }
    \item{\code{unescape(s=NULL)}:}{ returns the url decoded value of the character vector \code{s}. Also replaces the \code{'+'} character with a space. }
    \item{\code{status_code(status=NULL)}:}{ returns integer value for the given HTTP \code{status}, which can either be numeric or or a character vector describing the status. Returns \code{as.integer(500)} if \code{status} is NULL.}
    \item{\code{escape_html(string=NULL)}:}{ replaces \code{"&"}, \code{"<"}, \code{">"}, \code{"'"}, and \code{'"'} with entity equivalents. }
    \item{\code{raw.match(needle=NULL, haystack=NULL, all=TRUE)}:}{ returns index position of \code{needle} in  \code{haystack}. All matched indexes are returned by default. \code{needle} is either a raw vector or character string. \code{haystack} is a raw vector.}
    \item{\code{parse_query(qs=NULL, d=DEFAULT_SEP)}:}{ Creates a named list from the the query string \code{qs}. \code{d} is the separator value and defaults to \code{'[&;] *'}.}
    \item{\code{rfc2822(ts=NULL)}:}{ Formats \code{ts} in RFC2822 time. \code{ts} must be a \code{\link{POSIXt}} object.}
    \item{\code{escape(s=NULL)}:}{ Transforms any non-printable characters found in \code{s} to their percent-encoded equivalents.}
    \item{\code{build_query(params=NULL)}:}{ Creates a query string from the named list given in \code{params}. }
    \item{\code{timezero()}:}{ Returns a \code{POSIXct} object set to UNIX epoch. }
    \item{\code{set_cookie_header(header, key, value, expires, path, domain, secure, httpOnly)}:}{ Sets an HTTP cookie header in the environment \code{header}. All arguments except \code{expires} are length 1 character vectors, while \code{expires} must be a \code{POSIXct} object. }
    \item{\code{delete_cookie_header(header, key, value, expires, path, domain, secure, httpOnly)}:}{ Deletes the HTTP cookie header. }
  }
}
