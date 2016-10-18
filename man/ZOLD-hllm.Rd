\name{zzz-hllm}
\alias{hllm}
\alias{fit.hllm}
\alias{stepwise.hllm}
\alias{hllm-class}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Old: Hierarchical log-linear models}
\description{
  NOTICE: THIS FUNCTION IS DEFUNCT. PLEASE USE THE gRim PACKAGE FOR
  HIERARCHICAL LOG LINEAR MODELS.
  
  An implementation of hierarchical log-linear models using the
  framework of \code{\link{gRbase}}. A model object is defined using
  \code{hllm}, fitted using \code{fit} (which calls \code{\link[MASS]{loglm}})
  and a model search performed 
  using \code{stepwise}. The models may be displayed and manipulated
  using the \code{\link{gRbase}}.
}
\usage{
hllm(formula = ~.^1, gmData, marginal)
\method{fit}{hllm}(object, engine="loglm", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{an object of class \code{\link{formula}}. The right
    hand side of the formula is a list of the generators separated by
    \code{+}. A generator is specified by variable names with separated
    by \code{*}. Commonly used models have short hand notations:
    saturated model (\code{~.^.}), main effects (\code{~.^1}), all k'th
    order interactions (\code{~.^k}). } 
  \item{gmData}{an object of class \code{\link{gmData}}.}
  \item{marginal}{an optional argument specifying a subset of the
    variables from the \code{gmData} object.}
  \item{object}{A hllm object}.
  \item{engine}{Definining the fitting engine. For hllm objects only
    "loglm" is implemented.}
  \item{...}{Additional arguments}
}
%\details{
%  ~~ If necessary, more details than the __description__  above ~~
%}
\value{\code{hllm} returns an object of class \code{hllm}, inheriting
  from the superclass \code{gModel}.
}
\author{Søren Højsgaard, \email{sorenh@math.aau.dk}, \cr Claus Dethlefsen, \email{cld@rn.dk}}

\keyword{internal}

\seealso{\code{\link{gmData}}, \code{\link{gRfit}}, \code{\link{ggm}}
}
% \examples{
% data(reinis)
% reinis <- as.gmData(reinis)
% m2 <-
% hllm(~smoke*phys*protein+mental*phys+mental*family+smoke*systol*protein,
% reinis)
% m2 <- fit(m2,engine="loglm")
% ## plot(m2)
% }
\keyword{models}% at least one, from doc/KEYWORDS

