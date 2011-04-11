\name{Utils-class}
\Rdversion{1.1}
\docType{class}
\alias{Utils-class}

\title{Class \code{"Utils"}}
\description{
%%  ~~ A concise (1-5 lines) description of what the class is. ~~
}
\section{Extends}{

All reference classes extend and inherit methods from \code{"\linkS4class{envRefClass}"}.

}
\references{
%%  ~~put references to the literature/web site here~~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%%  ~~objects to See Also as \code{\link{~~fun~~}}, ~~~
%%  ~~or \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
}
\examples{
showClass("Utils")
}
\keyword{classes}
\section{Fields}{
  \describe{
    \item{\code{HTTP_STATUS_CODES}:}{Object of class \code{ANY} ~~ }
    \item{\code{STATUS_WITH_NO_ENTITY_BODY}:}{Object of class \code{ANY} ~~ }
    \item{\code{CHAR_TO_STATUS_CODE}:}{Object of class \code{ANY} ~~ }
    \item{\code{DEFAULT_SEP}:}{Object of class \code{ANY} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{\code{bytesize(string)}:}{ ~~ }
    \item{\code{unescape(s)}:}{ ~~ }
    \item{\code{status_code(status)}:}{ ~~ }
    \item{\code{escape_html(string)}:}{ ~~ }
    \item{\code{initialize(...)}:}{ ~~ }
    \item{\code{raw.match(needle, haystack, all)}:}{ ~~ }
    \item{\code{parse_query(qs, d)}:}{ ~~ }
    \item{\code{rfc2822(ts)}:}{ ~~ }
    \item{\code{escape(s)}:}{ ~~ }
    \item{\code{build_query(params)}:}{ ~~ }
    \item{\code{delete_cookie_header(header, key, value, expires, path, domain, secure, httpOnly)}:}{ ~~ }
    \item{\code{timezero()}:}{ ~~ }
    \item{\code{set_cookie_header(header, key, value, expires, path, domain, secure, httpOnly)}:}{ ~~ }
  }
}
