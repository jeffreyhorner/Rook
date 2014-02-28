\name{Request-class}
\Rdversion{1.1}
\docType{class}
\alias{Request-class}
\alias{Request}

\title{Class \code{Request}}
\description{
A convenience class for working with a \code{\link{Rook}} environment. Be sure to see the example at the end of this help file.
}
\examples{
#
# The following example prints out the result of each method.
# 
ls_str <- function(s) paste(capture.output(str(s),file=NULL),collapse='\n')
s <- Rhttpd$new()
\dontrun{
s$start(quiet=TRUE)
}
s$add(name="request",
    app=function(env){
        req <- Request$new(env)
        res <- Response$new()
        res$set_cookie('imacookie','42')
        action <- req$to_url('/foo',bar=1,baz='three')
        res$write('<form enctype="multipart/form-data" method=POST action="')
        res$write(action)
        res$write('">')
        res$write('Upload a file: <input type=file name=fileUpload>')
        res$write('<input type=submit></form><br><pre>')
        res$write(c('parseable_data: ',req$parseable_data(),'\n'))
        res$write(c('url: ',req$url(),'\n'))
        res$write(c('request_method: ',req$request_method(),'\n'))
        res$write(c('GET: ',ls_str(req$GET()),'\n'))
        res$write(c('post: ',req$post(),'\n'))
        res$write(c('media_type: ',req$media_type(),'\n'))
        res$write(c('query_string: ',req$query_string(),'\n'))
        res$write(c('fullpath: ',req$fullpath(),'\n'))
        res$write(c('referer: ',req$referer(),'\n'))
        res$write(c('cookies: ',ls_str(req$cookies()),'\n'))
        res$write(c('content_charset: ',req$content_charset(),'\n'))
        res$write(c('head: ',req$head(),'\n'))
        res$write(c('accept_encoding: ',req$accept_encoding(),'\n'))
        res$write(c('content_length: ',req$content_length(),'\n'))
        res$write(c('form_data: ',req$form_data(),'\n'))
        res$write(c('xhr: ',req$xhr(),'\n'))
        res$write(c('params: ',ls_str(req$params()),'\n'))
        res$write(c('media_type_params:\n',ls_str(req$media_type_params()),'\n'))
        res$write(c('user_agent: ',req$user_agent(),'\n'))
        res$write(c('put: ',req$put(),'\n'))
        res$write(c('get: ',req$get(),'\n'))
        res$write(c('path: ',req$path(),'\n'))
        res$write(c('body: ',ls_str(req$body()),'\n'))
        res$write(c('port: ',req$port(),'\n'))
        res$write(c('host_with_port: ',req$host_with_port(),'\n'))
        res$write(c('scheme: ',req$scheme(),'\n'))
        res$write(c('ip: ',req$ip(),'\n'))
        res$write(c('options: ',req$options(),'\n'))
        res$write(c('to_url: ',req$to_url('foo',bar=1,baz='two'),'\n'))
        res$write(c('host: ',req$host(),'\n'))
        res$write(c('POST: ',ls_str(req$POST()),'\n'))
        res$write(c('trace: ',req$trace(),'\n'))
        res$write(c('script_name: ',req$script_name(),'\n'))
        res$write(c('content_type: ',req$content_type(),'\n'))
        res$write(c('delete: ',req$delete(),'\n'))
        res$write(c('path_info: ',req$path_info(),'\n'))
        res$write(c('\nRac env: ',ls_str(as.list(env)),'\n'))
        res$finish()
    }
)
\dontrun{
s$browse('request') # Opens a browser window to the app.
}
s$remove(all=TRUE)
rm(s)
}
\section{Methods}{
  \describe{
    \item{\code{parseable_data()}:}{ Returns a boolean value determining if the POST payload is parseable. }
    \item{\code{url()}:}{ Returns url as a character string containing the scheme, host, port, and possibly the GET query string if supplied.}
    \item{\code{request_method()}:}{ Returns the HTTP method as a character string, e.g. 'GET', 'POST', etc. }
    \item{\code{GET()}:}{ Returns a named list containing the variables parsed from the query string. }
    \item{\code{post()}:}{ Returns TRUE if the current request method is 'POST', FALSE otherwise. }
    \item{\code{new(env)}:}{ Instantiates a new \code{Request} object for the given \code{Rook} environment.}
    \item{\code{media_type()}:}{ Returns the media type for the current request as a character string.}
    \item{\code{query_string()}:}{ Returns the unparsed query string. }
    \item{\code{fullpath()}:}{ Returns the same string as url() but without the scheme, host, and port. }
    \item{\code{referer()} or \code{referrer()}:}{ Returns the referring url.  }
    \item{\code{cookies()}:}{ Returns any cookies in the request as a named list. }
    \item{\code{content_charset()}:}{ Returns the content charset as a character string. }
    \item{\code{head()}:}{ Returns TRUE if the HTTP method is 'HEAD', FALSE otherwise.}
    \item{\code{accept_encoding()}:}{ Returns the accept encoding header as a character string.}
    \item{\code{content_length()}:}{ Returns content length header value as a string. }
    \item{\code{form_data()}:}{ Returns TRUE if there's form data, e.g. POST data with the request, FALSE otherwise.}
    \item{\code{xhr()}:}{ Returns the x-requested-with header value as a character string.}
    \item{\code{params()}:}{ Returns the combination of \code{POST()} and \code{GET()} in one named list.}
    \item{\code{media_type_params()}:}{ Returns any media type parameters from the content type as a named list. }
    \item{\code{user_agent()}:}{ Returns the user-agent header value as a character string. }
    \item{\code{put()}:}{ Returns TRUE if the current request is a 'PUT'. }
    \item{\code{get()}:}{ Returns TRUE if the current request is a 'GET'. }
    \item{\code{path()}:}{ Returns a character string like \code{fullpath()} but without the query string. }
    \item{\code{body()}:}{ Returns the 'rook.input' object from the environment. See \code{\link{RhttpdInputStream}} for more information.}
    \item{\code{port()}:}{ Returns the server port as an integer.e}
    \item{\code{host_with_port()}:}{ Returns the host and port as a character string separated by ':'. }
    \item{\code{scheme()}:}{ Returns the scheme, e.g. 'http' or 'https', as a character string. }
    \item{\code{ip()}:}{ Returns the remote IP address as a character string. }
    \item{\code{options()}:}{ Returns TRUE if the current request is 'OPTIONS'. }
    \item{\code{to_url(url, ...)}:}{ Concatenates the script name with the \code{url} argument along with any named parameters passed via \code{...} .}
    \item{\code{host()}:}{ Returns the server host as a character string. }
    \item{\code{POST()}:}{ Returns a named list containing the variables parsed from the POST payload.}
    \item{\code{trace()}:}{ Returns TRUE if the current request is 'TRACE'. }
    \item{\code{script_name(s=NULL)}:}{ Returns the script name of the application, e.g. '/custom/multi'. Also, if \code{s} is not NULL, sets the script name to \code{s}. }
    \item{\code{content_type()}:}{ Returns the content-type header value as a character string. }
    \item{\code{delete()}:}{ Returns TRUE if the current request is 'DELETE'. }
    \item{\code{path_info(s=NULL)}:}{ Returns the portion of the url after the script name as a character string. If \code{s} is not NULL, sets the path info to \code{s}.}
  }
}
\seealso{
\code{\link{Rhttpd}} and \code{\link{Response}}.
}
\keyword{classes}
