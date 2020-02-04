\name{gRbase}
\alias{gRbase}
\title{
  The package `gRbase': summary information
}
\description{
  This package provides a basis for graphical modelling in R and in
  particular for other graphical modelling packages, most notably
  \pkg{gRim}, \pkg{gRain} and \pkg{gRc}.
}
\section{Details}{
  \pkg{gRbase} provides the following:
  \itemize{
    \item Implementation of various graph algorithms, including maximum
    cardinality search, maximal prime subgraph decomposition,
    triangulation. See the vignette \code{graphs}.

    \item Implementation of various "high level" array operations, including
    multiplication/division, marginalization, slicing, permutation. See
    the vignette \code{ArrayOps}.

    \item Implementation of various "low level" array operations. See
    the vignette \code{ArrayOpsPrim}. 
    
    \item A collection of datasets
    
    \item A general framework for setting up data and model structures
    and provide examples for fitting hierarchical log linear 
    models for contingency tables and graphical Gaussian models for the
    multivariate normal distribution. (Notice: This last part is not maintained / developed further.)
    
  }
}
\section{Authors}{
  Soren Hojsgaard, Department of Mathematical Sciences, Aalborg
  University, Denmark

  Contributions from Claus Dethlefsen, Clive  Bowsher, David Edwards.
  
}
\section{Acknowledgements}{
  Thanks to the other members of the gR initiative, in particular to
  David Edwards for providing functions for formula-manipulation.
}
\references{
  Hojsgaard, S., Edwards, D., Lauritzen, S. (2012) Graphical models with
  R. Springer. ISBN: 978-1-4614-2298-3
  
  Lauritzen, S. L. (2002). gRaphical Models in R.
  \emph{R News}, 3(2)39.
}
\keyword{multivariate}
\keyword{models}
\keyword{graphs}
