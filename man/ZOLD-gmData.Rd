\name{zzz-gmData}
\alias{newgmData}
\alias{as.gmData}
\alias{as.gmData.array}
\alias{as.gmData.data.frame}
\alias{as.gmData.table}
\alias{latent}
\alias{latent.gmData}
\alias{latent<-}
\alias{latent<-.gmData}
\alias{nLevels}
\alias{nLevels.gmData}
\alias{nLevels<-}
\alias{nLevels<-.gmData}
\alias{description}
\alias{description.gmData}
\alias{description<-}
\alias{description<-.gmData}
\alias{obs}
\alias{observations}
\alias{observations.gmData}
\alias{observations<-}
\alias{observations<-.gmData}
\alias{print.gmData}
\alias{shortNames}
\alias{shortNames.gmData}
\alias{shortNames<-}
\alias{shortNames<-.gmData}
\alias{summary.gmData}
\alias{valueLabels}
\alias{valueLabels.gmData}
\alias{valueLabels<-}
\alias{valueLabels<-.gmData}
\alias{varNames}
\alias{varNames.gmData}
\alias{varNames<-}
\alias{varNames<-.gmData}
\alias{varTypes}
\alias{varTypes.gmData}
\alias{varTypes<-}
\alias{varTypes<-.gmData}

\alias{dataOrigin}
\alias{dataOrigin.gmData}

\alias{ordinal<-}
\alias{ordinal<-.gmData}

\alias{ordinal}
\alias{ordinal.gmData}

\alias{nominal<-}
\alias{nominal<-.gmData}

\alias{nominal}
\alias{nominal.gmData}

 
\title{Old: Class "gmData" graphical meta data}
\description{A common class for representing data. No matter the actual
  representation of data, the important characteristics are contained in
  a graphical metadata object. }
\section{Objects from the Class}{
  Objects can be created by calls of the form
  \code{newgmData(
    varNames, varTypes, nLevels, latent, valueLabels,
    observations, description)}.

  More often, gmData objects will be
  created from a data.frame or table.

  
  A \code{gmData} object contains the abstraction of data into a
  meta data object including variable names and types etc. However, the
  actual data might not be present or may be represented by a reference
  to data, such as a database file. Also, it may be possible to work
  without data, which may be valuable if the point of interest is in the
  model alone. Separating the specification of the
  variables from data has the benefit, that some properties of a model can be
  investigated without any reference to data, for example
  decomposability and collapsibility.
}

\usage{
newgmData(varNames,
                   varTypes=rep(validVarTypes()[1],length(varNames)),
                   nLevels=NULL,
                   latent=NULL,
                   valueLabels=NULL,
                   observations=NULL,
                   description=NULL,
                   shortNames=NULL
                   )
}
\arguments{
  \item{varNames}{a vector of strings with names of variables.}
  \item{varTypes}{a vector of strings with values from
    \code{\link{validVarTypes}} giving the types of the variables;
    typical types are "Discrete","Ordinal","Continuous", but others can
    be defined. The types can be abbreviated.}
  \item{nLevels}{a numeric vector with integer values for discrete or
    ordinal variables giving the number of levels.}
  \item{latent}{a vector of strings with names of the latent variables.}
  \item{valueLabels}{a list of vectors of strings with names of the
    levels for each discrete or ordinal variable.}
  \item{observations}{an object containing the observations, eg. a
    dataframe or a table.}
  \item{description}{a string describing the origin of the data.}
  \item{shortNames}{a vector of strings giving a short name of each
    variable.}
}
\value{An object of class \code{gmData} holds information about the data
  and can be retrieved and changed by accessor functions.}

\author{Søren Højsgaard, \email{sorenh@math.aau.dk}, \cr Claus Dethlefsen, \email{dethlef@math.aau.dk}}
%\note{ ~~further notes~~ }

% ~Make other sections like Warning with \section{Warning }{....} ~

\details{

  If neither nLevels nor valueLabels are given, then all categorical
  variables are assumed to be binary. If valueLabels are given then
  nLevels are infered from these. valueLabels / nLevels are recycled if
  necessary. 

}

\seealso{\code{demo(gmData)}}

\keyword{internal}

\examples{

vn <- c("a","b","c","d")
z<-newgmData(vn,varTypes=c("dis","dis","con","con"))
summary(z)
z<-newgmData(vn,varTypes=c("dis","dis","con","con"),nLevels=c(4,3,NA,NA))
summary(z)
z<-newgmData(vn,varTypes=c("dis","dis","con","con"),nLevels=c(4,NA,NA,NA))
summary(z)
z<-newgmData(vn,varTypes=c("dis","dis","ord","con"),valueLabels=list("a"=1:2, "b"=1:4))
summary(z)

ccnames <- c("asia", "smoke", "tub", "lung", "bronc", "either", "xray", "dysp")
gmd <- newgmData(ccnames,valueLabels=c("yes","no"), description="Chest clinic")
summary(gmd)

data(mathmark)
as.gmData(mathmark)

data(HairEyeColor)
as.gmData(HairEyeColor)

}

\keyword{models}% at least one, from doc/KEYWORDS
