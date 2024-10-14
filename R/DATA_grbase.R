#' Coronary artery disease data
#' 
#' A cross classified table with observational data from a Danish
#' heart clinic.  The response variable is CAD (coronary artery
#' disease, some times called heart attack).
#' @concept dataset
#' @details
#'
#' Notice that data are collected at a heart clinic, so data do not
#' represent the population, but are conditional on patients having
#' ended up at the clinic.
#' 
#' * cad1: Complete dataset, 236 cases.
#'
#' * cad2: Incomplete dataset, 67 cases. Information on (some of) the
#'     variables 'Hyperchol', 'Smoker' and 'Inherit' is missing.
#' 
#' @name data_cad
#' @aliases cad1 cad2
#' @docType data
#' @format A data frame with 236 observations on the following 14 variables.
#'
#' \describe{
#'   \item{\code{Sex}}{Sex; a factor with levels \code{Female} \code{Male}}
#'
#'   \item{\code{AngPec}}{Angina pectoris (chest pain attacks); a
#'   factor with levels \code{Atypical} \code{None} \code{Typical}}
#'
#'   \item{\code{AMI}}{Acute myocardic infarct; a factor with
#'   levels \code{Definite} \code{NotCertain}}
#'
#'   \item{\code{QWave}}{A reading from an electrocardiogram; a
#'   factor with levels \code{No} \code{Yes}; Yes means pathological and is a sign of previous myocardial infarction. }
#'
#'   \item{\code{QWavecode}}{a factor with levels \code{Nonusable}
#'   \code{Usable}. An assesment of whether QWave is reliable.}
#'
#'   \item{\code{STcode}}{a factor with levels
#'   \code{Nonusable} \code{Usable}. An assesment of whether STchange is reliable.}
#'
#'   \item{\code{STchange}}{A reading from an electrocardiogram; a factor
#'   with levels \code{No} \code{Yes}. An STchange indicates a blockage of the coronary artery.}
#'
#'   \item{\code{SuffHeartF}}{Sufficient heart frequency; a factor with levels \code{No}, \code{Yes}}
#' 
#'   \item{\code{Hypertrophi}}{a factor with levels \code{No}, \code{Yes}. Hypertrophy refers to an
#'   increased size of the heart muscle due to exercise. }
#'
#'   \item{\code{Hyperchol}}{a factor with levels \code{No} \code{Yes}. Hypercholesterolemia, also called high cholesterol,
#'    is the presence of high levels of cholesterol in the blood.}
#'
#'   \item{\code{Smoker}}{Is the patient a smoker; a factor with levels \code{No}, \code{Yes}.}
#'
#'   \item{\code{Inherit}}{Hereditary predispositions for CAD; a factor with levels  \code{No}, \code{Yes}.}
#'
#'   \item{\code{Heartfail}}{Previous heart failures; a factor with  levels \code{No} \code{Yes}}
#'
#'   \item{\code{CAD}}{Coronary Artery Disease; a factor with levels
#'    \code{No} \code{Yes}}.  CAD refers to a reduction of blood flow
#'    to the heart muscle (commonly known as a heart attack). The
#'    diagnosis made from biopsies.
#'
#' }
#'
#' 
#' @references Hansen, J. F. (1980). The clinical diagnoisis of ichaeme heart disease du to
#' coronary artery disease. Danish Medical Bulletin
#'
#' Højsgaard, Søren and Thiesson, Bo (1995). BIFROST - Block
#' recursive models Induced From Relevant knowledge, Observations and
#' Statistical Techniques. Computational Statistics and Data Analysis, vol. 19,
#' p. 155-175
#' 
#' 
#'
#' @keywords datasets
#' @usage data(cad1)
#' 
#' @examples
#' 
#' data(cad1)
#' ## maybe str(cad1) ; plot(cad1) ...
#' 
"cad1"
"cad2"


#' Crown dieback in ash trees
#' 
#' This dataset comes from a study of symptoms of crown dieback, cankers and
#' symptoms caused by other pathogens and pests in ash trees (Fraxinus
#' excelsior). In all 454 trees were observed in two plots. There are 8
#' categorical variables, 6 of which are binary and two are trichotomous with
#' values representing increasing severity of symptoms, and one continuous
#' variable, tree diameter at breast height (DBH).
#' @concept dataset
#' @name data-ashtrees
#' @docType data
#' @format A data frame with 454 observations on the following 9 variables.
#' \describe{
#'   \item{\code{plot}}{a factor with levels \code{2} \code{6}}
#'   \item{\code{dieback}}{a factor with levels \code{0} \code{1} \code{2}}
#'   \item{\code{dead50}}{a factor with levels \code{0} \code{0.5} \code{1}}
#'   \item{\code{bushy}}{a factor with levels \code{0} \code{1}}
#'   \item{\code{canker}}{a factor with levels \code{BRNCH} \code{MAIN} \code{NONE}}
#'   \item{\code{wilt}}{a factor with levels \code{0} \code{1}}
#'   \item{\code{roses}}{a factor with levels \code{0} \code{1}}
#'   \item{\code{discolour}}{a factor with levels \code{0} \code{1}}
#'   \item{\code{dbh}}{a numeric vector}
#' }
#' 
#' @references Skovgaard JP, Thomsen IM, Skovgaard IM and Martinussen
#'     T (2009).  Associations among symptoms of dieback in even-aged
#'     stands of ash (Fraxinus excelsior L.). Forest Pathology.
#'
#' @keywords datasets
#' @usage data(ashtrees)
#'
#' @examples
#' data(ashtrees)
#' head(ashtrees)
#' 
"ashtrees"


#' Body Fat Data
#' 
#' Estimates of the percentage of body fat determined by underwater weighing
#' and various body circumference measurements for 252 men.
#' @concept dataset  
#' @name data-BodyFat
#' @docType data
#' @usage data(BodyFat)
#' @format A data frame with 252 observations on the following 15
#'     variables.
#'   \describe{
#'   \item{\code{Density}}{Density determined from underwater weighing, a numeric vector}
#'   \item{\code{BodyFat}}{Percent body fat from Siri's (1956) equation, a numeric vector}
#'   \item{\code{Age}}{in years, a numeric vector}
#'   \item{\code{Weight}}{in lbs, a numeric vector}
#'   \item{\code{Height}}{in inches, a numeric vector}
#'   \item{\code{Neck}}{circumference in cm, a numeric vector}
#'   \item{\code{Chest}}{circumference in cm, a numeric vector}
#'   \item{\code{Abdomen}}{circumference in cm, a numeric vector}
#'   \item{\code{Hip}}{circumference in cm, a numeric vector}
#'   \item{\code{Thigh}}{circumference in cm, a numeric vector}
#'   \item{\code{Knee}}{circumference in cm, a numeric vector}
#'   \item{\code{Ankle}}{circumference in cm, a numeric vector}
#'   \item{\code{Biceps}}{circumference in cm, a numeric vector}
#'   \item{\code{Forearm}}{circumference in cm, a numeric vector}
#'   \item{\code{Wrist}}{circumference in cm, a numeric vector}
#' }
#' @references Bailey, Covert (1994). _Smart Exercise: Burning Fat,
#'     Getting Fit_, Houghton-Mifflin Co., Boston, pp. 179-186.
#' 
#' Behnke, A.R. and Wilmore, J.H. (1974). _Evaluation and Regulation
#' of Body Build and Composition_, Prentice-Hall, Englewood Cliffs,
#' N.J.
#' 
#' Siri, W.E. (1956), "Gross composition of the body", in _Advances in
#' Biological and Medical Physics_, vol. IV, edited by J.H. Lawrence
#' and C.A.  Tobias, Academic Press, Inc., New York.
#' 
#' Katch, Frank and McArdle, William (1977). _Nutrition, Weight
#' Control, and Exercise_, Houghton Mifflin Co., Boston.
#' 
#' Wilmore, Jack (1976). _Athletic Training and Physical Fitness:
#' Physiological Principles of the Conditioning Process_, Allyn and
#' Bacon, Inc., Boston.
#' @source For more information see
#'     https://lib.stat.cmu.edu/datasets/bodyfat
#' @keywords datasets
#' @usage data(BodyFat)
#' 
#' @examples
#' 
#' data(BodyFat)
#' head(BodyFat)
#' 
"BodyFat"


#' Gene expression signatures for p53 mutation status in 250 breast cancer
#' samples
#' 
#' Perturbations of the p53 pathway are associated with more aggressive and
#' therapeutically refractory tumours. We preprocessed the data using Robust
#' Multichip Analysis (RMA). Dataset has been truncated to the 1000 most
#' informative genes (as selected by Wilcoxon test statistics) to simplify
#' computation. The genes have been standardised to have zero mean and unit
#' variance (i.e. z-scored).
#' 
#' The factor \code{code} defines whether there was a mutation in the p53
#' sequence (code=case) or not (code=control).
#' @concept dataset
#' @name data-breastcancer
#'
#' @docType data
#'
#' @format A data frame with 250 observations on 1001 variables. The
#'     first 1000 columns are numerical variables; the last column
#'     (named \code{code}) is a factor with levels \code{case} and
#'     \code{control}.
#'
#' @references Miller et al (2005, PubMed
#'     ID:16141321)
#'
#' @source Dr. Chris Holmes, c.holmes at stats
#'     dot. ox . ac .uk
#'
#' @keywords datasets
#' @usage data(breastcancer)
#' 
#' @examples
#' 
#' data(breastcancer)
#' ## maybe str(breastcancer) ; plot(breastcancer) ...
#' 
"breastcancer"



#' Lean meat contents of 344 pig carcasses
#' 
#' Measurement of lean meat percentage of 344 pig carcasses together with
#' auxillary information collected at three Danish slaughter houses
#' @concept dataset
#' @name data-carcass
#' 
#' @aliases carcass carcassall
#' @format carcassall: A data frame with 344 observations on the following 17
#' variables.
#'  \describe{
#'   \item{\code{weight}}{Weight of carcass}
#'   \item{\code{lengthc}}{Length of carcass from back toe to head (when
#'     the carcass hangs in the back legs)}
#'   \item{\code{lengthf}}{Length of carcass from back toe to front leg
#'     (that is, to the shoulder)}
#'   \item{\code{lengthp}}{Length of carcass from back toe to the pelvic bone}
#'   \item{\code{Fat02, Fat03, Fat11, Fat12, Fat13, Fat14, Fat16}}{Thickness of fat
#'     layer at different locations on the back of the carcass (FatXX
#'     refers to thickness at (or rather next to) rib no. XX. Notice that
#'     02 is closest to the head}
#'   \item{\code{Meat11, Meat12, Meat13}}{Thickness of meat layer at different
#'     locations on the back of the carcass, see description above}
#'   \item{\code{LeanMeat}}{Lean meat percentage determined by dissection}
#'   \item{\code{slhouse}}{Slaughter house; a factor with levels \code{a} \code{b} \code{c}}
#'   \item{\code{sex}}{Sex of the pig; a factor with \code{a} \code{b}
#'     \code{c}. Notice that it is no an error to have three levels; the
#'     third level refers to castrates}
#' }
#'
#' @note carcass: Contains only the variables Fat11, Fat12, Fat13,
#'     Meat11, Meat12, Meat13, LeanMeat
#' @source Busk, H., Olsen, E. V., Brøndum, J. (1999) Determination of
#'     lean meat in pig carcasses with the Autofom classification
#'     system, Meat Science, 52, 307-314
#' @keywords datasets
#' @usage data(carcass)
#' @examples
#' data(carcass)
#' head(carcass)
#' 
"carcass"
"carcassall"

#' Simulated data from the Chest Clinic example
#' 
#' Simulated data from the Chest Clinic example (also known as the Asia
#' example) from Lauritzen and Spiegelhalter, 1988 (see reference below).
#' @concept dataset
#' @name data-chestSim
#'
#' @aliases chestSim500 chestSim1000 chestSim10000
#'     chestSim50000 chestSim100000
#' @docType data
#'
#' @format A data frame with 500 observations on the following 8 variables.
#'   \describe{
#'   \item{\code{asia}}{Recent visit to Asia?; a factor with levels \code{yes} \code{no}}
#'   \item{\code{tub}}{Has tuberculosis?; a factor with levels \code{yes} \code{no}}
#'   \item{\code{smoke}}{Is a smoker?; a factor with levels \code{yes} \code{no}}
#'   \item{\code{lung}}{Has lung cancer?; a factor with levels \code{yes} \code{no}}
#'   \item{\code{bronc}}{Has bronchitis?; a factor with levels \code{yes} \code{no}}
#'   \item{\code{either}}{Either lung cancer or tuberculosis?; a factor with levels \code{yes} \code{no}}
#'   \item{\code{xray}}{Positive x-ray? a factor with levels \code{yes} \code{no}}
#'   \item{\code{dysp}}{Dyspnoea (shortness of breath)?; a factor with levels \code{yes} \code{no}}
#' }
#'
#' @details Notice that the chest clinic example is a contrieved
#'     example; it does not originate from an empirical study.
#' 
#' @references Lauritzen and Spiegelhalter (1988) Local Computations
#'     with Probabilities on Graphical Structures and their
#'     Application to Expert Systems (with
#'     Discussion). J. Roy. Stat. Soc. 50, p. 157-224.
#' 
#' @keywords datasets
#' @usage data(chestSim500)
#' 
#' @examples
#' 
#' data(chestSim500)
#' ## maybe str(chestSim500) ; plot(chestSim500) ...
#' 
"chestSim500"
## "chestSim1000"
## "chestSim10000"
## "chestSim50000"
## "chestSim100000"


#' Growth curves of pigs in a 3x3 factorial experiment
#' 
#' The \code{dietox} data frame has 861 rows and 7 columns.
#' @concept dataset 
#' @name data-dietox
#' 
#' @format This data frame contains the following columns: Weight,
#'     Feed, Time, Pig, Evit, Cu, Litter.
#' @source Lauridsen, C., Højsgaard, S., Sørensen, M.T. C. (1999)
#'     Influence of Dietary Rapeseed Oli, Vitamin E, and Copper on
#'     Performance and Antioxidant and Oxidative Status of
#'     Pigs. J. Anim. Sci.77:906-916
#'
#' @keywords datasets
#' @usage data(dietox)
#' 
#' @examples
#' 
#' data(dietox)
#' 
"dietox"


#' Gastric Dumping
#' 
#' A contingency table relating surgical operation, centre and severity of
#' gastric dumping, a syndrome associated with gastric surgery.
#'
#' Gastric dumping syndrome is a condition where ingested foods bypass the
#' stomach too rapidly and enter the small intestine largely undigested. It is
#' an undesirable side-effect of gastric surgery. The table summarizes the
#' results of a study comparing four different surgical operations on patients
#' with duodenal ulcer, carried out in four centres, as described in Grizzle et
#' al (1969). The four operations were: vagotomy and drainage, vagotomy and
#' antrectomy (removal of 25\% of gastric tissue), vagotomy and hemigastrectomy
#' (removal of 50\% of gastric tissue), and gastric restriction (removal of
#' 75\% of gastric tissue).
#' @concept dataset
#' @name data-dumping
#' @docType data
#' @format A 3x4x4 table of counts cross-classified by Symptom
#' (none/slight/moderate), Operation (Vd/Va/Vh/Gr) and Centre (1:4).
#' @source Grizzle JE, Starmer CF, Koch GG (1969) Analysis of categorical data
#' by linear models. Biometrics 25(3):489-504.
#' @keywords datasets
#' @usage data(dumping)
#' 
#' @examples
#' 
#' data(dumping) 
#' plot(dumping) 
#' 
"dumping"


#' Lizard behaviour
#' 
#' In a study of lizard behaviour, characteristics of 409 lizards were
#' recorded, namely species (S), perch diameter (D) and perch height
#' (H). Perch means preferred place to settle down (a branch on a
#' tree).  The focus of interest is in how the propensities of the
#' lizards to choose perch height and diameter are related, and
#' whether and how these depend on species.
#' @concept dataset
#' @name data-lizard
#' @aliases lizard lizardRAW lizardAGG
#' @docType data
#' @format A 3--dimensional array with factors diam: "<=4" ">4" height: ">4.75"
#' "<=4.75" species: "anoli" "dist"
#' @references Schoener TW (1968) The anolis lizards of bimini: Resource
#' partitioning in a complex fauna. Ecology 49:704-726
#' @keywords datasets
#' @usage data(lizard)
#'
#' @examples
#' 
#' data(lizard)
#' 
#' # Datasets lizardRAW and lizardDF are generated with the following code
#' #lizardAGG <- as.data.frame(lizard)
#' #f   <- lizardAGG$Freq
#' #idx <- unlist(mapply(function(i, n) rep(i, n), 1:8, f))
#' #set.seed(0805)
#' #idx <- sample(idx)
#' #lizardRAW <- as.data.frame(lizardAGG[idx, 1:3])
#' #rownames(lizardRAW) <- 1:NROW(lizardRAW)
#' 
#' 
"lizard"
"lizardRAW"
"lizardAGG"


#' Mathematics marks for students
#' 
#' The \code{mathmark} data frame has 88 rows and 5 columns.
#'
#' @name data-mathmark
#' @concept dataset
#' 
#' @aliases mathmark math
#' @format This data frame contains the following columns: mechanics, vectors,
#' algebra, analysis, statistics.
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
#' @references David Edwards, An Introduction to Graphical Modelling, Second
#' Edition, Springer Verlag, 2000
#' @keywords datasets
#' @usage data(mathmark)
#' 
#' @examples
#' 
#' data(mathmark)
#' 
"mathmark"
"math"


#' Mildew fungus
#' 
#' The data stem from a cross between two isolates of the barley powdery mildew
#' fungus. For each offspring 6 binary characteristics, each corresponding to a
#' single locus, were recorded. The object of the analysis is to determine the
#' order of the loci along the chromosome.
#' @concept dataset
#' @name data-mildew
#' @docType data
#' @format A 6 dimensional array where each variable has levels "1" and "2".
#' The variables are: `la10`, `locc`, `mp58`, `c365`, `p53a` and `a365`.
## ' format i
## '  table [1:2, 1:2, 1:2, 1:2, 1:2, 1:2] 0 0 0 0 3 0 1 0 0 1 ...
## ' - attr(*, "dimnames")=List of 6
## '  ..$ la10: chr [1:2] "1" "2"
## '  ..$ locc: chr [1:2] "1" "2"
## '  ..$ mp58: chr [1:2] "1" "2"
## '  ..$ c365: chr [1:2] "1" "2"
## '  ..$ p53a: chr [1:2] "1" "2"
## '  ..$ a367: chr [1:2] "1" "2"
## #' 
#'
#' @references Christiansen, S.K., Giese, H (1991) Genetic analysis of
#'     obligate barley powdery mildew fungus based on RFLP and
#'     virulence loci. Theor. Appl.  Genet. 79:705-712
#' @keywords datasets
#' @usage data(mildew)
#' 
#' @examples
#' 
#' data(mildew)
#' ## maybe str(mildew) ; plot(mildew) ...
#' 
"mildew"


#' Milk composition data
#' 
#' Data from an experiment on composition of sow milk. Milk composition is
#' measured on four occasions during lactation on a number of sows. The
#' treatments are different types of fat added to the sows feed.
#' 
#' \code{a} is the control, i.e. no fat has been added.
#' 
#' \code{fat} + \code{protein} + \code{lactose} almost add up to \code{dm} (dry
#' matter)
#' @concept dataset
#' @name data-milkcomp
#' @aliases milkcomp milkcomp1
#' @docType data
#' 
#' @format A data frame with 214 observations on the following 7 variables.
#' \describe{
#'   \item{\code{sow}}{a numeric vector}
#'   \item{\code{lactime}}{a numeric vector}
#'   \item{\code{treat}}{a factor with levels \code{a} \code{b} \code{c} \code{d} \code{e} \code{f} \code{g}}
#'   \item{\code{fat}}{a numeric vector}
#'   \item{\code{protein}}{a numeric vector}
#'   \item{\code{dm}}{(dry matter) a numeric vector}
#'   \item{\code{lactose}}{a numeric vector}
#' }
#' 
#' @references Charlotte Lauridsen and Viggo Danielsen (2004):
#'     Lactational dietary fat levels and sources influence milk
#'     composition and performance of sows and their progeny Livestock
#'     Production Science 91 (2004) 95-105
#' @keywords datasets
#' @usage data(milkcomp)
#'
#' @examples
#' 
#' data(milkcomp)
#' ## maybe str(milk) ; plot(milk) ...
#' 
"milkcomp"
"milkcomp1"


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
#' @concept dataset
#' @name data-Nutrimouse
#' @docType data
#' @format A data frame with 40 observations on 143 variables of which two are factors and 141 are numeric.
#'
#' \describe{
#'   \item{\code{genotype}}{a factor with levels \code{wt}
#'   \code{ppar}}
#' \item{\code{diet}}{a factor with levels \code{coc}
#'   \code{fish} \code{lin} \code{ref} \code{sun}}
## #' \item{\code{X36b4}}{a numeric vector}
## #' \item{\code{ACAT1}}{a numeric vector}
## #' \item{\code{ACAT2}}{a numeric vector}
## #' \item{\code{ACBP}}{a numeric vector}
## #' \item{\code{ACC1}}{a numeric vector}
## #' \item{\code{ACC2}}{a numeric vector}
## #' \item{\code{ACOTH}}{a numeric vector}
## #' \item{\code{ADISP}}{a numeric vector}
## #' \item{\code{ADSS1}}{a numeric vector}
## #' \item{\code{ALDH3}}{a numeric vector}
## #' \item{\code{AM2R}}{a numeric vector}
## #' \item{\code{AOX}}{a numeric vector}
## #'   \item{\code{BACT}}{a numeric vector}
## #' \item{\code{BIEN}}{a numeric
## #'   vector}
## #' \item{\code{BSEP}}{a numeric vector}
## #'   \item{\code{Bcl.3}}{a numeric vector}
## #' \item{\code{C16SR}}{a
## #'   numeric vector}
## #' \item{\code{CACP}}{a numeric vector}
## #'   \item{\code{CAR1}}{a numeric vector}
## #' \item{\code{CBS}}{a numeric
## #'   vector}
## #' \item{\code{CIDEA}}{a numeric vector}
## #'   \item{\code{COX1}}{a numeric vector}
## #' \item{\code{COX2}}{a numeric
## #'   vector}
## #' \item{\code{CPT2}}{a numeric vector}
## #'   \item{\code{CYP24}}{a numeric vector}
## #' \item{\code{CYP26}}{a
## #'   numeric vector}
## #' \item{\code{CYP27a1}}{a numeric vector}
## #'   \item{\code{CYP27b1}}{a numeric vector}
## #' \item{\code{CYP2b10}}{a
## #'   numeric vector}
## #' \item{\code{CYP2b13}}{a numeric vector}
## #'   \item{\code{CYP2c29}}{a numeric vector}
## #' \item{\code{CYP3A11}}{a
## #'   numeric vector}
## #' \item{\code{CYP4A10}}{a numeric vector}
## #'   \item{\code{CYP4A14}}{a numeric vector}
## #' \item{\code{CYP7a}}{a
## #'   numeric vector}
## #' \item{\code{CYP8b1}}{a numeric vector}
## #'   \item{\code{FAS}}{a numeric vector}
## #' \item{\code{FAT}}{a numeric
## #'   vector}
## #' \item{\code{FDFT}}{a numeric vector}
## #' \item{\code{FXR}}{a
## #'   numeric vector}
## #' \item{\code{G6PDH}}{a numeric vector}
## #'   \item{\code{G6Pase}}{a numeric vector}
## #' \item{\code{GK}}{a numeric
## #'   vector} \item{\code{GS}}{a numeric vector}
## #' \item{\code{GSTa}}{a
## #'   numeric vector}
## #' \item{\code{GSTmu}}{a numeric vector}
## #'   \item{\code{GSTpi2}}{a numeric vector}
## #' \item{\code{HMGCoAred}}{a
## #'   numeric vector}
## #' \item{\code{HPNCL}}{a numeric vector}
## #'   \item{\code{IL.2}}{a numeric vector}
## #' \item{\code{L.FABP}}{a
## #'   numeric vector}
## #' \item{\code{LCE}}{a numeric vector}
## #'   \item{\code{LDLr}}{a numeric vector}
## #' \item{\code{LPK}}{a numeric
## #'   vector}
## #' \item{\code{LPL}}{a numeric vector}
## #' \item{\code{LXRa}}{a
## #'   numeric vector}
## #' \item{\code{LXRb}}{a numeric vector}
## #'   \item{\code{Lpin}}{a numeric vector}
## #' \item{\code{Lpin1}}{a
## #'   numeric vector}
## #' \item{\code{Lpin2}}{a numeric vector}
## #'   \item{\code{Lpin3}}{a numeric vector}
## #' \item{\code{M.CPT1}}{a
## #'   numeric vector}
## #' \item{\code{MCAD}}{a numeric vector}
## #'   \item{\code{MDR1}}{a numeric vector}
## #' \item{\code{MDR2}}{a numeric
## #'   vector}
## #' \item{\code{MRP6}}{a numeric vector}
## #' \item{\code{MS}}{a
## #'   numeric vector}
## #' \item{\code{MTHFR}}{a numeric vector}
## #'   \item{\code{NGFiB}}{a numeric vector}
## #' \item{\code{NURR1}}{a
## #'   numeric vector}
## #' \item{\code{Ntcp}}{a numeric vector}
## #'   \item{\code{OCTN2}}{a numeric vector}
## #' \item{\code{PAL}}{a numeric
## #'   vector}
## #' \item{\code{PDK4}}{a numeric vector}
## #' \item{\code{PECI}}{a
## #'   numeric vector}
## #' \item{\code{PLTP}}{a numeric vector}
## #'   \item{\code{PMDCI}}{a numeric vector}
## #' \item{\code{PON}}{a numeric
## #'   vector}
## #' \item{\code{PPARa}}{a numeric vector}
## #'   \item{\code{PPARd}}{a numeric vector}
## #' \item{\code{PPARg}}{a
## #'   numeric vector}
## #' \item{\code{PXR}}{a numeric vector}
## #'   \item{\code{Pex11a}}{a numeric vector}
## #' \item{\code{RARa}}{a
## #'   numeric vector}
## #' \item{\code{RARb2}}{a numeric vector}
## #'   \item{\code{RXRa}}{a numeric vector}
## #' \item{\code{RXRb2}}{a
## #'   numeric vector}
## #' \item{\code{RXRg1}}{a numeric vector}
## #'   \item{\code{S14}}{a numeric vector}
## #' \item{\code{SHP1}}{a numeric
## #'   vector}
## #' \item{\code{SIAT4c}}{a numeric vector}
## #'   \item{\code{SPI1.1}}{a numeric vector}
## #' \item{\code{SR.BI}}{a
## #'   numeric vector}
## #' \item{\code{THB}}{a numeric vector}
## #' \item{\code{THIOL}}{a numeric vector}
## #' \item{\code{TRa}}{a numeric
## #'   vector}
## #' \item{\code{TRb}}{a numeric vector}
## #'   \item{\code{Tpalpha}}{a numeric vector}
## #' \item{\code{Tpbeta}}{a
## #'   numeric vector}
## #' \item{\code{UCP2}}{a numeric vector}
## #'   \item{\code{UCP3}}{a numeric vector}
## #' \item{\code{VDR}}{a numeric
## #'   vector}
## #' \item{\code{VLDLr}}{a numeric vector}
## #'   \item{\code{Waf1}}{a numeric vector}
## #' \item{\code{ap2}}{a numeric
## #'   vector}
## #' \item{\code{apoA.I}}{a numeric vector}
## #'   \item{\code{apoB}}{a numeric vector}
## #' \item{\code{apoC3}}{a
## #'   numeric vector}
## #' \item{\code{apoE}}{a numeric vector}
## #'   \item{\code{c.fos}}{a numeric vector}
## #' \item{\code{cHMGCoAS}}{a
## #'   numeric vector}
## #' \item{\code{cMOAT}}{a numeric vector}
## #'   \item{\code{eif2g}}{a numeric vector}
## #' \item{\code{hABC1}}{a
## #'   numeric vector}
## #' \item{\code{i.BABP}}{a numeric vector}
## #'   \item{\code{i.BAT}}{a numeric vector}
## #' \item{\code{i.FABP}}{a
## #'   numeric vector}
## #' \item{\code{i.NOS}}{a numeric vector}
## #'   \item{\code{mABC1}}{a numeric vector}
## #' \item{\code{mHMGCoAS}}{a
## #'   numeric vector}
## #' \item{\code{C14.0}}{a numeric vector}
## #'   \item{\code{C16.0}}{a numeric vector}
## #' \item{\code{C18.0}}{a
## #'   numeric vector} \item{\code{C16.1n.9}}{a numeric vector}
## #'   \item{\code{C16.1n.7}}{a numeric vector}
## #' \item{\code{C18.1n.9}}{a
## #'   numeric vector} \item{\code{C18.1n.7}}{a numeric vector}
## #'   \item{\code{C20.1n.9}}{a numeric vector}
## #' \item{\code{C20.3n.9}}{a
## #'   numeric vector} \item{\code{C18.2n.6}}{a numeric vector}
## #'   \item{\code{C18.3n.6}}{a numeric vector}
## #' \item{\code{C20.2n.6}}{a
## #'   numeric vector} \item{\code{C20.3n.6}}{a numeric vector}
## #'   \item{\code{C20.4n.6}}{a numeric vector}
## #' \item{\code{C22.4n.6}}{a
## #'   numeric vector} \item{\code{C22.5n.6}}{a numeric vector}
## #'   \item{\code{C18.3n.3}}{a numeric vector}
## #' \item{\code{C20.3n.3}}{a
## #'   numeric vector} \item{\code{C20.5n.3}}{a numeric vector}
## #'   \item{\code{C22.5n.3}}{a numeric vector}
## #' \item{\code{C22.6n.3}}{a
## #'   numeric vector} 
#' }
#'
#' @references Martin, P. G. P., Guillou, H., Lasserre, F., D'jean,
#'     S., Lan, A., Pascussi, J.-M., San Cristobal, M., Legrand, P.,
#'     Besse, P. and Pineau, T. (2007). Novel aspects of
#'     PPARa-mediated regulation of lipid and xenobiotic metabolism
#'     revealed through a multrigenomic study. Hepatology 54, 767-777.
#'
#' @source The data were provided by Pascal Martin from the Toxicology
#'     and Pharmacology Laboratory, National Institute for Agronomic
#'     Research, France.
#'
#' @keywords datasets
#' @usage data(Nutrimouse)
#' 
#' @examples
#' 
#' data(Nutrimouse)
#' 
"Nutrimouse"



#' Personality traits
#'
#' The `peronality` dataframe has 240 rows and 32 columns
#' @concept dataset
#' @name data-personality
#'
#' @format This dataframe has recordings on the following 32
#'     variables: distant, talkatv, carelss, hardwrk, anxious,
#'     agreebl, tense, kind, opposng, relaxed, disorgn, outgoin,
#'     approvn, shy, discipl, harsh, persevr, friendl, worryin,
#'     respnsi, contrar, sociabl, lazy, coopera, quiet, organiz,
#'     criticl, lax, laidbck, withdrw, givinup, easygon
#'
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
#' @references Origin unclear
#' @keywords datasets
#' @usage data(personality)
#' @examples
#' data(personality)
#' str(personality)
"personality"


#' Weightloss of rats
#' 
#' An artificial dataset. 24 rats (12 female, 12 male) have been randomized to
#' use one of three drugs (products for loosing weight). The weightloss for
#' each rat is noted after one and two weeks.
#' @concept dataset
#' @name data-rats
#' 
#' @format A dataframe with 4 variables. Sex: "M" (male), "F" (female). Drug:
#' "D1", "D2", "D3" (three types). W1 weightloss, week one. W2 weightloss, week
#' 2.
#' @references Morrison, D.F. (1976). Multivariate Statistical Methods.
#' McGraw-Hill, USA.
#' 
#' Edwards, D. (1995). Introduction to Graphical Modelling, Springer-Verlag.
#' New York.
#' @keywords datasets
#' @usage data(rats)
#' 
"rats"


#' Risk factors for coronary heart disease.
#' 
#' Data collected at the beginning of a 15 year follow-up study of probable
#' risk factors for coronary thrombosis. Data are from all men employed in a
#' car factory.
#' @concept dataset
#' @name data-reinis
#' 
#' @format A table with 6 discrete variables. A: smoking, B: strenous
#'     mental work, D: strenuous physical work, E: systolic blood
#'     pressure, F: ratio of lipoproteins, G: Family anamnesis of
#'     coronary heart disease.
#' @references Edwards and Havranek (1985): A fast procedure for model
#'     search in multidimensional contingency tables. Biometrika, 72:
#'     339-351.
#' 
#' Reinis et al (1981): Prognostic significance of the risk profile in the
#' prevention of coronary heart disease. Bratis. lek. Listy. 76: 137-150.
#' @keywords datasets
#' @usage data(reinis)
#' 
"reinis"


#' Chemical composition of wine
#' 
#' Using chemical analysis determine the origin of wines
#' 
#' Data comes from the UCI Machine Learning Repository. The grape variety
#' \code{Cult} is the class identifier.  
#' @concept dataset
#' @name data-wine
#' @docType data
#' @format A data frame with 178 observations on the following 14 variables.
#'   \describe{
#'   \item{\code{Cult}}{a factor with levels \code{v1} \code{v2}
#'     \code{v3}: 3 different graph varieties}
#'   \item{\code{Alch}}{Alcohol}
#'   \item{\code{Mlca}}{Malic acid}
#'   \item{\code{Ash}}{Ash}
#'   \item{\code{Aloa}}{Alcalinity of ash}
#'   \item{\code{Mgns}}{Magnesium}
#'   \item{\code{Ttlp}}{Total phenols}
#'   \item{\code{Flvn}}{Flavanoids}
#'   \item{\code{Nnfp}}{Nonflavanoid phenols}
#'   \item{\code{Prnt}}{Proanthocyanins}
#'   \item{\code{Clri}}{Color intensity}
#'   \item{\code{Hue}}{Hue}
#'   \item{\code{Oodw}}{OD280/OD315 of diluted wines}
#'   \item{\code{Prln}}{Proline}
#' }
#' 
#' @references See references at
#'   \url{https://archive.ics.uci.edu/ml/datasets/Wine/}
#'
#' @source Frank, A. & Asuncion, A. (2010). UCI Machine Learning
#'     Repository \url{https://archive.ics.uci.edu/ml/}. Irvine, CA:
#'     University of California, School of Information and Computer
#'     Science.
#'
#' @keywords datasets
#' @usage data(wine)
#' @examples
#' 
#' data(wine)
#' ## maybe str(wine) ; plot(wine) ...
#' 
"wine"
