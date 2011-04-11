\name{Static-class}
\Rdversion{1.1}
\docType{class}
\alias{Static-class}

\title{Class \code{"Static"}}
\description{
%%  ~~ A concise (1-5 lines) description of what the class is. ~~
}
\section{Extends}{
Class \code{"\linkS4class{Middleware}"}, directly.
Class \code{"\linkS4class{App}"}, by class "Middleware", distance 2.

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
showClass("Static")
}
\keyword{classes}
\section{Fields}{
  \describe{
    \item{\code{app}:}{Object of class \code{ANY} ~~ }
    \item{\code{urls}:}{Object of class \code{ANY} ~~ }
    \item{\code{file_server}:}{Object of class \code{ANY} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{\code{call(env)}:}{ ~~ }
    \item{\code{initialize(urls, root, ...)}:}{ ~~ }
  }

The following methods are inherited (from the corresponding class):
initialize ("Middleware"), set_app ("Middleware"), call ("App"), initialize ("App")
}
