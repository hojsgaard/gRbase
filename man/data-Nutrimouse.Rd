% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DATA_grbase.R
\docType{data}
\name{data-Nutrimouse}
\alias{data-Nutrimouse}
\alias{Nutrimouse}
\title{The Nutrimouse Dataset}
\format{
A data frame with 40 observations on 143 variables of which two are factors and 141 are numeric.

\describe{
\item{\code{genotype}}{a factor with levels \code{wt}
\code{ppar}}
\item{\code{diet}}{a factor with levels \code{coc}
\code{fish} \code{lin} \code{ref} \code{sun}}
}
}
\source{
The data were provided by Pascal Martin from the Toxicology
and Pharmacology Laboratory, National Institute for Agronomic
Research, France.
}
\usage{
data(Nutrimouse)
}
\description{
The data come from a study of the effects of five dietary regimens with
different fatty acid compositions on liver lipids and hepatic gene
expression in 40 mice.
}
\details{
The data come from a study of the effects of five dietary regimens with
different fatty acid compositions on liver lipids and hepatic gene
expression in wild-type and PPAR-alpha-deficient mice (Martin et al., 2007).

There were 5 replicates per genotype and diet combination.

There are two design variables: (i) genotype, a factor with two levels:
wild-type (wt) and PPAR-alpha-deficient (ppar), and (ii) diet, a factor with
five levels. The oils used for experimental diet preparation were: corn and
colza oils (50/50) for a reference diet (ref); hydrogenated coconut oil for
a saturated fatty acid diet (coc); sunflower oil for an Omega6 fatty
acid-rich diet (sun); linseed oil for an Omega3-rich diet (lin); and
corn/colza/enriched (43/43/14) fish oils (fish).

There are 141 response variables: (i) the log-expression levels of 120 genes
measured in liver cells, and (ii) the concentrations (in percentages) of 21
hepatic fatty acids measured by gas chromatography.
}
\examples{

data(Nutrimouse)

}
\references{
Martin, P. G. P., Guillou, H., Lasserre, F., D'jean,
S., Lan, A., Pascussi, J.-M., San Cristobal, M., Legrand, P.,
Besse, P. and Pineau, T. (2007). Novel aspects of
PPARa-mediated regulation of lipid and xenobiotic metabolism
revealed through a multrigenomic study. Hepatology 54, 767-777.
}
\keyword{datasets}
