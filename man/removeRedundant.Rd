\name{Setoperations}
\alias{is.insetlist}
\alias{isin}
\alias{is.subsetof}
\alias{subsetof}
\alias{removeRedundant}
\alias{maximalSets}
\alias{minimalSets}


%- Also NEED an '\alias' for EACH other topic documented here.
\title{Set operations}
\description{
  Miscellaneous set operations.
}
\usage{
is.subsetof(x, set)
is.insetlist(x, setlist, index=FALSE)
removeRedundant(setlist, maximal = TRUE, index = FALSE)
maximalSets(setlist,  index = FALSE)
minimalSets(setlist,  index = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x, set}{Vectors representing sets}
  \item{setlist}{List of vectors (representing a set of subsets)}
  \item{maximal}{Logical; see section 'Details' for a description.}
  \item{index}{Logical; should indices (in setlist) be returned or a set
  of subsets.}
}
\details{
  'setlist' is a list of vectors representing a set of subsets;
  i.e. V1,...VQ where Vk is a subset of some base set V. 

  \code{is.insetlist}: Checks if the set x is in one of the Vk's.

  \code{removeRedundant}: Returns those Vk which are not contained in other
  subsets; i.e. gives the maximal sets. If maximal is FALSE then returns
  the minimal sets; i.e. Vk is returned if Vk is contained in one of the
  other sets Vl and there are no set Vn contained in Vk.
  
  Notice that the comparisons are made by turning the elements into
  characters and then comparing these. Hence 1 is identical to "1". 
  
}
% \value{
%   ~Describe the value returned
%   If it is a LIST, use
%   \item{comp1 }{Description of 'comp1'}
%   \item{comp2 }{Description of 'comp2'}
%   ...
% }
%\references{ ~put references to the literature/web site here ~ }
\author{Søren Højsgaard, \email{sorenh@math.aau.dk}}
% \note{ ~~further notes~~ 

%  ~Make other sections like Warning with \section{Warning }{....} ~
% }
%%\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{

is.subsetof(c(1,2),c(1,2,3))
is.subsetof(c(1,2,3), c(1,2))

l <- list(c(1,2),c(1,2,3),c(2,4),c(5,6), 5)

#subsetofList(c(1,2), l)
#subsetofList(c(1,2,3,4), l)

removeRedundant(l)
removeRedundant(l, maximal=FALSE)

is.insetlist (c(2,4), l)
is.insetlist (c(2,8), l)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
