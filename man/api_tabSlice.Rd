% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api_array_tabX.R
\name{api_tabSlice}
\alias{api_tabSlice}
\alias{tabSlice}
\alias{tabSlice2}
\alias{tabSlicePrim}
\alias{tabSliceMult}
\alias{tabSlice2Entries}
\title{Array slices}
\usage{
tabSlice(
  tab,
  slice = NULL,
  margin = names(slice),
  drop = TRUE,
  as.array = FALSE
)

tabSlice2(tab, slice, margin.idx, drop = TRUE, as.array = FALSE)

tabSlicePrim(tab, slice, drop = TRUE)

tabSliceMult(tab, slice, val = 1, comp = 0)

tabSlice2Entries(tab, slice, complement = FALSE)
}
\arguments{
\item{tab}{An array with named dimnames.}

\item{slice}{A list defining the slice.}

\item{margin}{Names of variables in slice.}

\item{drop}{If TRUE then dimensions with only one level will be
dropped from the output.}

\item{as.array}{If the resulting array is one-dimensional the
result will by default be a vector with no dim attribute unless
as.array is TRUE.}

\item{margin.idx}{Indec of variables in slice.}

\item{val}{The values that entries in the slice will be multiplied
with.}

\item{comp}{The values that entries NOT in the slice will be
multiplied with.}

\item{complement}{If TRUE the complement of the entries are
returned.}
}
\description{
Functions for extracting slices of arrays
}
\examples{

x = HairEyeColor
s = list(Hair=c("Black", "Brown"), Eye=c("Brown", "Blue"))

s1 = tabSlice(x, slice=s); s1

tabSlice2Entries(x, slice=s)
tabSlice2Entries(x, slice=s, complement=TRUE)

## tabSliceMult 
s2 = tabSliceMult(x, slice=s); s2

sp = list(c(1,2), c(1,2), TRUE)
tabSlicePrim(x, slice=sp)
tabSlice(x, slice=s)
}
\author{
Søren Højsgaard, \email{sorenh@math.aau.dk}
}
