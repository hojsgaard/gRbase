#' The Nutrimouse Dataset
#' 
#' The data come from a study of the effects of five dietary regimens with
#' different fatty acid compositions on liver lipids and hepatic gene
#' expression in 40 mice.
#' 
#' The data come from a study of the effects of five dietary regimens with
#' different fatty acid compositions on liver lipids and hepatic gene
#' expression in wild-type and PPAR-alpha-deficient mice (Martin et al., 2007).
#'
#' There were 5 replicates per genotype and diet combination.
#' 
#' There are two design variables: (i) genotype, a factor with two levels:
#' wild-type (wt) and PPAR-alpha-deficient (ppar), and (ii) diet, a factor with
#' five levels. The oils used for experimental diet preparation were: corn and
#' colza oils (50/50) for a reference diet (ref); hydrogenated coconut oil for
#' a saturated fatty acid diet (coc); sunflower oil for an Omega6 fatty
#' acid-rich diet (sun); linseed oil for an Omega3-rich diet (lin); and
#' corn/colza/enriched (43/43/14) fish oils (fish).
#' 
#' There are 141 response variables: (i) the log-expression levels of 120 genes
#' measured in liver cells, and (ii) the concentrations (in percentages) of 21
#' hepatic fatty acids measured by gas chromatography.
#' 
#' @name Nutrimouse
#' @docType data
#' @format A data frame with 40 observations on the following 143 variables.
#'
#' \describe{
#'   \item{\code{genotype}}{a factor with levels \code{wt}
#'   \code{ppar}}
#' \item{\code{diet}}{a factor with levels \code{coc}
#'   \code{fish} \code{lin} \code{ref} \code{sun}}
#' \item{\code{X36b4}}{a numeric vector}
#' \item{\code{ACAT1}}{a numeric vector}
#' \item{\code{ACAT2}}{a numeric vector}
#' \item{\code{ACBP}}{a numeric vector}
#' \item{\code{ACC1}}{a numeric vector}
#' \item{\code{ACC2}}{a numeric vector}
#' \item{\code{ACOTH}}{a numeric vector}
#' \item{\code{ADISP}}{a numeric vector}
#' \item{\code{ADSS1}}{a numeric vector}
#' \item{\code{ALDH3}}{a numeric vector}
#' \item{\code{AM2R}}{a numeric vector}
#' \item{\code{AOX}}{a numeric vector}
#'   \item{\code{BACT}}{a numeric vector}
#' \item{\code{BIEN}}{a numeric
#'   vector}
#' \item{\code{BSEP}}{a numeric vector}
#'   \item{\code{Bcl.3}}{a numeric vector}
#' \item{\code{C16SR}}{a
#'   numeric vector}
#' \item{\code{CACP}}{a numeric vector}
#'   \item{\code{CAR1}}{a numeric vector}
#' \item{\code{CBS}}{a numeric
#'   vector}
#' \item{\code{CIDEA}}{a numeric vector}
#'   \item{\code{COX1}}{a numeric vector}
#' \item{\code{COX2}}{a numeric
#'   vector}
#' \item{\code{CPT2}}{a numeric vector}
#'   \item{\code{CYP24}}{a numeric vector}
#' \item{\code{CYP26}}{a
#'   numeric vector}
#' \item{\code{CYP27a1}}{a numeric vector}
#'   \item{\code{CYP27b1}}{a numeric vector}
#' \item{\code{CYP2b10}}{a
#'   numeric vector}
#' \item{\code{CYP2b13}}{a numeric vector}
#'   \item{\code{CYP2c29}}{a numeric vector}
#' \item{\code{CYP3A11}}{a
#'   numeric vector}
#' \item{\code{CYP4A10}}{a numeric vector}
#'   \item{\code{CYP4A14}}{a numeric vector}
#' \item{\code{CYP7a}}{a
#'   numeric vector}
#' \item{\code{CYP8b1}}{a numeric vector}
#'   \item{\code{FAS}}{a numeric vector}
#' \item{\code{FAT}}{a numeric
#'   vector}
#' \item{\code{FDFT}}{a numeric vector}
#' \item{\code{FXR}}{a
#'   numeric vector}
#' \item{\code{G6PDH}}{a numeric vector}
#'   \item{\code{G6Pase}}{a numeric vector}
#' \item{\code{GK}}{a numeric
#'   vector} \item{\code{GS}}{a numeric vector}
#' \item{\code{GSTa}}{a
#'   numeric vector}
#' \item{\code{GSTmu}}{a numeric vector}
#'   \item{\code{GSTpi2}}{a numeric vector}
#' \item{\code{HMGCoAred}}{a
#'   numeric vector}
#' \item{\code{HPNCL}}{a numeric vector}
#'   \item{\code{IL.2}}{a numeric vector}
#' \item{\code{L.FABP}}{a
#'   numeric vector}
#' \item{\code{LCE}}{a numeric vector}
#'   \item{\code{LDLr}}{a numeric vector}
#' \item{\code{LPK}}{a numeric
#'   vector}
#' \item{\code{LPL}}{a numeric vector}
#' \item{\code{LXRa}}{a
#'   numeric vector}
#' \item{\code{LXRb}}{a numeric vector}
#'   \item{\code{Lpin}}{a numeric vector}
#' \item{\code{Lpin1}}{a
#'   numeric vector}
#' \item{\code{Lpin2}}{a numeric vector}
#'   \item{\code{Lpin3}}{a numeric vector}
#' \item{\code{M.CPT1}}{a
#'   numeric vector}
#' \item{\code{MCAD}}{a numeric vector}
#'   \item{\code{MDR1}}{a numeric vector}
#' \item{\code{MDR2}}{a numeric
#'   vector}
#' \item{\code{MRP6}}{a numeric vector}
#' \item{\code{MS}}{a
#'   numeric vector}
#' \item{\code{MTHFR}}{a numeric vector}
#'   \item{\code{NGFiB}}{a numeric vector}
#' \item{\code{NURR1}}{a
#'   numeric vector}
#' \item{\code{Ntcp}}{a numeric vector}
#'   \item{\code{OCTN2}}{a numeric vector}
#' \item{\code{PAL}}{a numeric
#'   vector}
#' \item{\code{PDK4}}{a numeric vector}
#' \item{\code{PECI}}{a
#'   numeric vector}
#' \item{\code{PLTP}}{a numeric vector}
#'   \item{\code{PMDCI}}{a numeric vector}
#' \item{\code{PON}}{a numeric
#'   vector}
#' \item{\code{PPARa}}{a numeric vector}
#'   \item{\code{PPARd}}{a numeric vector}
#' \item{\code{PPARg}}{a
#'   numeric vector}
#' \item{\code{PXR}}{a numeric vector}
#'   \item{\code{Pex11a}}{a numeric vector}
#' \item{\code{RARa}}{a
#'   numeric vector}
#' \item{\code{RARb2}}{a numeric vector}
#'   \item{\code{RXRa}}{a numeric vector}
#' \item{\code{RXRb2}}{a
#'   numeric vector}
#' \item{\code{RXRg1}}{a numeric vector}
#'   \item{\code{S14}}{a numeric vector}
#' \item{\code{SHP1}}{a numeric
#'   vector}
#' \item{\code{SIAT4c}}{a numeric vector}
#'   \item{\code{SPI1.1}}{a numeric vector}
#' \item{\code{SR.BI}}{a
#'   numeric vector}
#' \item{\code{THB}}{a numeric vector}
#' \item{\code{THIOL}}{a numeric vector}
#' \item{\code{TRa}}{a numeric
#'   vector}
#' \item{\code{TRb}}{a numeric vector}
#'   \item{\code{Tpalpha}}{a numeric vector}
#' \item{\code{Tpbeta}}{a
#'   numeric vector}
#' \item{\code{UCP2}}{a numeric vector}
#'   \item{\code{UCP3}}{a numeric vector}
#' \item{\code{VDR}}{a numeric
#'   vector}
#' \item{\code{VLDLr}}{a numeric vector}
#'   \item{\code{Waf1}}{a numeric vector}
#' \item{\code{ap2}}{a numeric
#'   vector}
#' \item{\code{apoA.I}}{a numeric vector}
#'   \item{\code{apoB}}{a numeric vector}
#' \item{\code{apoC3}}{a
#'   numeric vector}
#' \item{\code{apoE}}{a numeric vector}
#'   \item{\code{c.fos}}{a numeric vector}
#' \item{\code{cHMGCoAS}}{a
#'   numeric vector}
#' \item{\code{cMOAT}}{a numeric vector}
#'   \item{\code{eif2g}}{a numeric vector}
#' \item{\code{hABC1}}{a
#'   numeric vector}
#' \item{\code{i.BABP}}{a numeric vector}
#'   \item{\code{i.BAT}}{a numeric vector}
#' \item{\code{i.FABP}}{a
#'   numeric vector}
#' \item{\code{i.NOS}}{a numeric vector}
#'   \item{\code{mABC1}}{a numeric vector}
#' \item{\code{mHMGCoAS}}{a
#'   numeric vector}
#' \item{\code{C14.0}}{a numeric vector}
#'   \item{\code{C16.0}}{a numeric vector}
#' \item{\code{C18.0}}{a
#'   numeric vector} \item{\code{C16.1n.9}}{a numeric vector}
#'   \item{\code{C16.1n.7}}{a numeric vector}
#' \item{\code{C18.1n.9}}{a
#'   numeric vector} \item{\code{C18.1n.7}}{a numeric vector}
#'   \item{\code{C20.1n.9}}{a numeric vector}
#' \item{\code{C20.3n.9}}{a
#'   numeric vector} \item{\code{C18.2n.6}}{a numeric vector}
#'   \item{\code{C18.3n.6}}{a numeric vector}
#' \item{\code{C20.2n.6}}{a
#'   numeric vector} \item{\code{C20.3n.6}}{a numeric vector}
#'   \item{\code{C20.4n.6}}{a numeric vector}
#' \item{\code{C22.4n.6}}{a
#'   numeric vector} \item{\code{C22.5n.6}}{a numeric vector}
#'   \item{\code{C18.3n.3}}{a numeric vector}
#' \item{\code{C20.3n.3}}{a
#'   numeric vector} \item{\code{C20.5n.3}}{a numeric vector}
#'   \item{\code{C22.5n.3}}{a numeric vector}
#' \item{\code{C22.6n.3}}{a
#'   numeric vector} }
#' 
#' @references Martin, P. G. P., Guillou, H., Lasserre, F., D<e9>jean,
#'     S., Lan, A., Pascussi, J.-M., San Cristobal, M., Legrand, P.,
#'     Besse, P. and Pineau, T. (2007). Novel aspects of
#'     PPARa-mediated regulation of lipid and xenobiotic metabolism
#'     revealed through a multrigenomic study. Hepatology 54, 767-777.
#'
#' @source The data were provided by Pascal Martin from the Toxicology
#'     and Pharmacology Laboratory, National Institute for Agronomic
#'     Research, French.
#'
#' @keywords datasets
#' @examples
#' 
#' data(Nutrimouse)
#' 
"Nutrimouse"
