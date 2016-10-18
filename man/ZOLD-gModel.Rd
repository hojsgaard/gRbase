\name{zzz-gModel}
\alias{gModel}
\alias{gModel-class}

\alias{formula}
\alias{formula.gModel}
\alias{formula<-}
\alias{formula<-.gModel}

\alias{gmData}
\alias{gmData<-}
\alias{gmData.gModel}
\alias{gmData<-.gModel}

\alias{print.gModel}

\title{Old: Class "gModel" - graphical models}
\description{
  The general class \code{gModel} contains a formula object and a
  \code{gmData} object. Implementations of different
  specific graphical model classes can inherit from this class and
  provide methods for parsing the formula. This is illustrated in the
  implementation of a class for hierarchical log--linear models,
  \code{\link{hllm}}.
}
\usage{
gModel(formula, gmData)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{an object of class \code{\link{formula}}.}
  \item{gmData}{an object of class \code{\link{gmData}}.}
}
%\details{
%}
\value{
  \code{gModel} creates an object of class \code{gModel} with the two
  components \code{formula} and \code{gmData}. These components can be
  retrieved or replaced using the accessor functions of the same
  names. Also, a \code{gModel} object may be manipulated using the
  \code{dynamicGraph} interface.
}
%\references{ ~put references to the literature/web site here ~ }
\author{Søren Højsgaard, \email{sorenh@math.aau.dk}, \cr Claus Dethlefsen, \email{cld@rn.dk}}
%\note{ ~~further notes~~ }

\seealso{\code{\link{gmData}}, \code{\link{gRfit}}, \code{\link{hllm}}.}

\keyword{internal}

\examples{
data(rats)
rats <- as.gmData(rats)

m1 <- gModel(~.^. , rats)
m1.form <- formula(m1)
m1.data <- gmData(m1)
observations(gmData(m1)) <- observations(rats)[1:10,]
}
\keyword{models}% at least one, from doc/KEYWORDS

