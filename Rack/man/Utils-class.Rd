\name{Utils-class}
\Rdversion{1.1}
\docType{class}
\alias{Utils-class}
\alias{Utils}

\title{Class \code{"Utils"}}
\description{
A convenience object for working with various aspects of web requests and responses.
}
\seealso{
\code{"\link{Multipart}"}.
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
    \item{\code{bytesize(string)}:}{ Returns size in bytes for \code{string}, a character vector. }
    \item{\code{unescape(s)}:}{ returns the url decoded value of the character vector \code{s}.Also replaces the space character with \code{'+'}. }
    \item{\code{status_code(status)}:}{ returns integer value for the given HTTP \code{status}, which can either be numeric or or a character vector describing the status.}
    \item{\code{escape_html(string)}:}{ replaces \code{"&"}, \code{"<"}, \code{">"}, \code{"'"}, and \code{'"'} with entity equivalents. }
    \item{\code{raw.match(needle, haystack, all=TRUE)}:}{ returns index positions of \code{needle} in  \code{haystack}. Returns all mathed indexes by default. \code{needle} is either a raw vector or character string. \code{haystack} is a raw vector.}
    \item{\code{parse_query(qs, d)}:}{ Creates a named list from the the query string \code{qs}. \code{d} is the separator value and defaults to \code{'[&;] *'}.}
    \item{\code{rfc2822(ts)}:}{ Formats \code{ts} as RFC2822 time. }
    \item{\code{escape(s)}:}{ Transforms any non-printable characters found in \code{s} to their percent-encoded equivalents.}
    \item{\code{build_query(params)}:}{ Creates a query string from the named list given in \code{params}. }
    \item{\code{timezero()}:}{ Returns UNIX epoch object of class \code{POSIXct}. }
    \item{\code{delete_cookie_header(header, key, value, expires, path, domain, secure, httpOnly)}:}{ Sets an HTTP cookie header in the environment \code{header}. All arguments except \code{expires} are length 1 character vectors, while \code{expires} must be a \code{POSIXct} object. }
    \item{\code{set_cookie_header(header, key, value, expires, path, domain, secure, httpOnly)}:}{ Deletes the HTTP cookie header. }
  }
}
