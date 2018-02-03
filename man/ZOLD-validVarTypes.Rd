\name{zzz-validVarTypes}
\alias{validVarTypes}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Old: Admissible variable types in gmData objects }
\description{
The variable types in a \code{\link{gmData}} object must be from a
vector predefined types which may be 
inspected by the command \code{validVarTypes()}. The available types
may be extended by the package developers as demonstrated in the
example.
%The
%types of the variables are important for the way they are displayed
%using the package \pkg{dynamicGraph}. The type is also important when
%the models are fitted to data.
}
\usage{
validVarTypes()
}
%- maybe also 'usage' for other objects documented here.
%\details{
%  ~~ If necessary, more details than the __description__  above ~~
%}
\value{
  A character vector with the names of the admissible variable types.
%  ~Describe the value returned
%  If it is a LIST, use
%  \item{comp1 }{Description of 'comp1'}
%  \item{comp2 }{Description of 'comp2'}
%  ...
}
%\references{ ~put references to the literature/web site here ~ }
\author{Søren Højsgaard, \email{sorenh@math.aau.dk}, \cr Claus Dethlefsen, \email{cld@rn.dk}}
%\note{ ~~further notes~~ }

% ~Make other sections like Warning with \section{Warning }{....} ~
\keyword{internal}
\seealso{ \code{\link{gmData}}}
\examples{
oldtypes <- validVarTypes()
validVartypes <- function() c(oldtypes, "MyVarType")
validVartypes()
}
\keyword{ models }% at least one, from doc/KEYWORDS
