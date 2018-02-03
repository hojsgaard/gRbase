\name{zzz-gRfit}
\alias{gRfit}
\alias{getFit}
\alias{getFit<-}
\alias{getFit<-.gRfit}
\alias{getFit.gRfit}
\alias{print.gRfit}
\alias{summary.gRfit}
%\alias{fit}


\title{Old: Class "gRfit" - fitted graphical models}
\description{
 Objects of class \code{gRfit} are created when the function \code{fit}
 is applied to a \code{\link{gModel}} object. When adding new types of
 gModel objects, one must also supply the appropritate \code{fit}
 function. The \code{gRfit} object contains the output of the fit which
 can be accessed by \code{getFit}. Separate \code{print} and
 \code{summary} methods exist for \code{gRfit} objects.
}
\usage{
fit(object, ...)
getFit(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{An object for which a fit method has been defined}.
  \item{x}{an object of class \code{gRfit} as created from \code{fit}
    applied to a \code{\link{gModel}} object.}
  \item{...}{Additional arguments}
}
%\details{
%}
\value{
  \code{fit} creates an object of class \code{gRfit}.
  \code{getFit} returns the fit information created by the fitting
  algorithm. 
}
%\references{ ~put references to the literature/web site here ~ }
\author{Søren Højsgaard, \email{sorenh@math.aau.dk}, \cr Claus Dethlefsen, \email{cld@rn.dk}}
%\note{ ~~further notes~~ }

\keyword{internal}

\seealso{\code{\link{gModel}}.}

% \examples{
% data(reinis)
% reinis <- as.gmData(reinis)

% m1 <- hllm(~.^. , reinis) 
% m1 <- fit(m1,engine="loglm")
% }
\keyword{models}% at least one, from doc/KEYWORDS

