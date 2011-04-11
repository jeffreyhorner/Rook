\name{Response-class}
\Rdversion{1.1}
\docType{class}
\alias{Response-class}

\title{Class \code{"Response"}}
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
showClass("Response")
}
\keyword{classes}
\section{Fields}{
  \describe{
    \item{\code{body}:}{Object of class \code{ANY} ~~ }
    \item{\code{length}:}{Object of class \code{ANY} ~~ }
    \item{\code{status}:}{Object of class \code{ANY} ~~ }
    \item{\code{headers}:}{Object of class \code{ANY} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{\code{header(key, value)}:}{ ~~ }
    \item{\code{redirect(target, status)}:}{ ~~ }
    \item{\code{write(str)}:}{ ~~ }
    \item{\code{initialize(body, status, headers, ...)}:}{ ~~ }
    \item{\code{set_cookie(key, value)}:}{ ~~ }
    \item{\code{delete_cookie(key, value)}:}{ ~~ }
    \item{\code{finish()}:}{ ~~ }
  }
}
