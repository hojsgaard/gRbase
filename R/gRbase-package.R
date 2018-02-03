

#' Old: Class "gModel" - graphical models
#' 
#' The general class \code{gModel} contains a formula object and a
#' \code{gmData} object. Implementations of different specific graphical model
#' classes can inherit from this class and provide methods for parsing the
#' formula. This is illustrated in the implementation of a class for
#' hierarchical log--linear models, \code{\link{hllm}}.
#' 
#' 
#' @aliases gModel gModel-class formula formula.gModel formula<-
#' formula<-.gModel gmData gmData<- gmData.gModel gmData<-.gModel print.gModel
#' @param formula an object of class \code{\link{formula}}.
#' @param gmData an object of class \code{\link{gmData}}.
#' @return \code{gModel} creates an object of class \code{gModel} with the two
#' components \code{formula} and \code{gmData}. These components can be
#' retrieved or replaced using the accessor functions of the same names. Also,
#' a \code{gModel} object may be manipulated using the \code{dynamicGraph}
#' interface.
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}, \cr Claus
#' Dethlefsen, \email{cld@@rn.dk}
#' @seealso \code{\link{gmData}}, \code{\link{gRfit}}, \code{\link{hllm}}.
#' @keywords internal models
#' @examples
#' 
#' data(rats)
#' rats <- as.gmData(rats)
#' 
#' m1 <- gModel(~.^. , rats)
#' m1.form <- formula(m1)
#' m1.data <- gmData(m1)
#' observations(gmData(m1)) <- observations(rats)[1:10,]
#' 






#' Old: Class "gRfit" - fitted graphical models
#' 
#' Objects of class \code{gRfit} are created when the function \code{fit} is
#' applied to a \code{\link{gModel}} object. When adding new types of gModel
#' objects, one must also supply the appropritate \code{fit} function. The
#' \code{gRfit} object contains the output of the fit which can be accessed by
#' \code{getFit}. Separate \code{print} and \code{summary} methods exist for
#' \code{gRfit} objects.
#' 
#' 
#' @aliases gRfit getFit getFit<- getFit<-.gRfit getFit.gRfit print.gRfit
#' summary.gRfit fit
#' @param object An object for which a fit method has been defined
#' @param x an object of class \code{gRfit} as created from \code{fit} applied
#' to a \code{\link{gModel}} object.
#' @param ... Additional arguments
#' @return \code{fit} creates an object of class \code{gRfit}.  \code{getFit}
#' returns the fit information created by the fitting algorithm.
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}, \cr Claus
#' Dethlefsen, \email{cld@@rn.dk}
#' @seealso \code{\link{gModel}}.
#' @keywords internal models






#' Old: Class "gmData" graphical meta data
#' 
#' A common class for representing data. No matter the actual representation of
#' data, the important characteristics are contained in a graphical metadata
#' object.
#' 
#' 
#' If neither nLevels nor valueLabels are given, then all categorical variables
#' are assumed to be binary. If valueLabels are given then nLevels are infered
#' from these. valueLabels / nLevels are recycled if necessary.
#' 
#' @aliases newgmData as.gmData as.gmData.array as.gmData.data.frame
#' as.gmData.table latent latent.gmData latent<- latent<-.gmData nLevels
#' nLevels.gmData nLevels<- nLevels<-.gmData description description.gmData
#' description<- description<-.gmData obs observations observations.gmData
#' observations<- observations<-.gmData print.gmData shortNames
#' shortNames.gmData shortNames<- shortNames<-.gmData summary.gmData
#' valueLabels valueLabels.gmData valueLabels<- valueLabels<-.gmData varNames
#' varNames.gmData varNames<- varNames<-.gmData varTypes varTypes.gmData
#' varTypes<- varTypes<-.gmData dataOrigin dataOrigin.gmData ordinal<-
#' ordinal<-.gmData ordinal ordinal.gmData nominal<- nominal<-.gmData nominal
#' nominal.gmData
#' @param varNames a vector of strings with names of variables.
#' @param varTypes a vector of strings with values from
#' \code{\link{validVarTypes}} giving the types of the variables; typical types
#' are "Discrete","Ordinal","Continuous", but others can be defined. The types
#' can be abbreviated.
#' @param nLevels a numeric vector with integer values for discrete or ordinal
#' variables giving the number of levels.
#' @param latent a vector of strings with names of the latent variables.
#' @param valueLabels a list of vectors of strings with names of the levels for
#' each discrete or ordinal variable.
#' @param observations an object containing the observations, eg. a dataframe
#' or a table.
#' @param description a string describing the origin of the data.
#' @param shortNames a vector of strings giving a short name of each variable.
#' @return An object of class \code{gmData} holds information about the data
#' and can be retrieved and changed by accessor functions.
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{newgmData( varNames, varTypes, nLevels, latent, valueLabels,
#' observations, description)}.
#' 
#' More often, gmData objects will be created from a data.frame or table.
#' 
#' A \code{gmData} object contains the abstraction of data into a meta data
#' object including variable names and types etc. However, the actual data
#' might not be present or may be represented by a reference to data, such as a
#' database file. Also, it may be possible to work without data, which may be
#' valuable if the point of interest is in the model alone. Separating the
#' specification of the variables from data has the benefit, that some
#' properties of a model can be investigated without any reference to data, for
#' example decomposability and collapsibility.
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}, \cr Claus
#' Dethlefsen, \email{dethlef@@math.aau.dk}
#' @seealso \code{demo(gmData)}
#' @keywords internal models
#' @examples
#' 
#' 
#' vn <- c("a","b","c","d")
#' z<-newgmData(vn,varTypes=c("dis","dis","con","con"))
#' summary(z)
#' z<-newgmData(vn,varTypes=c("dis","dis","con","con"),nLevels=c(4,3,NA,NA))
#' summary(z)
#' z<-newgmData(vn,varTypes=c("dis","dis","con","con"),nLevels=c(4,NA,NA,NA))
#' summary(z)
#' z<-newgmData(vn,varTypes=c("dis","dis","ord","con"),valueLabels=list("a"=1:2, "b"=1:4))
#' summary(z)
#' 
#' ccnames <- c("asia", "smoke", "tub", "lung", "bronc", "either", "xray", "dysp")
#' gmd <- newgmData(ccnames,valueLabels=c("yes","no"), description="Chest clinic")
#' summary(gmd)
#' 
#' data(mathmark)
#' as.gmData(mathmark)
#' 
#' data(HairEyeColor)
#' as.gmData(HairEyeColor)
#' 
#' 





#' Old: Hierarchical log-linear models
#' 
#' NOTICE: THIS FUNCTION IS DEFUNCT. PLEASE USE THE gRim PACKAGE FOR
#' HIERARCHICAL LOG LINEAR MODELS.
#' 
#' An implementation of hierarchical log-linear models using the framework of
#' \code{\link{gRbase}}. A model object is defined using \code{hllm}, fitted
#' using \code{fit} (which calls \code{\link[MASS]{loglm}}) and a model search
#' performed using \code{stepwise}. The models may be displayed and manipulated
#' using the \code{\link{gRbase}}.
#' 
#' 
#' @aliases hllm fit.hllm stepwise.hllm hllm-class
#' @param formula an object of class \code{\link{formula}}. The right hand side
#' of the formula is a list of the generators separated by \code{+}. A
#' generator is specified by variable names with separated by \code{*}.
#' Commonly used models have short hand notations: saturated model
#' (\code{~.^.}), main effects (\code{~.^1}), all k'th order interactions
#' (\code{~.^k}).
#' @param gmData an object of class \code{\link{gmData}}.
#' @param marginal an optional argument specifying a subset of the variables
#' from the \code{gmData} object.
#' @param object A hllm object
#' @param engine Definining the fitting engine. For hllm objects only "loglm"
#' is implemented.
#' @param ... Additional arguments
#' @return \code{hllm} returns an object of class \code{hllm}, inheriting from
#' the superclass \code{gModel}.
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}, \cr Claus
#' Dethlefsen, \email{cld@@rn.dk}
#' @seealso \code{\link{gmData}}, \code{\link{gRfit}}, \code{\link{ggm}}
#' @keywords internal models





#' Old: Admissible variable types in gmData objects
#' 
#' The variable types in a \code{\link{gmData}} object must be from a vector
#' predefined types which may be inspected by the command
#' \code{validVarTypes()}. The available types may be extended by the package
#' developers as demonstrated in the example. %The %types of the variables are
#' important for the way they are displayed %using the package
#' \pkg{dynamicGraph}. The type is also important when %the models are fitted
#' to data.
#' 
#' 
#' @aliases validVarTypes
#' @return A character vector with the names of the admissible variable types.
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}, \cr Claus
#' Dethlefsen, \email{cld@@rn.dk}
#' @seealso \code{\link{gmData}}
#' @keywords internal models
#' @examples
#' 
#' oldtypes <- validVarTypes()
#' validVartypes <- function() c(oldtypes, "MyVarType")
#' validVartypes()
#' 










#' Compute table margin or table slice
#' 
#' For a contingency table in array form, compute the sum of table entries for
#' a given index (i.e. a marginal table) or find the slice of the table defined
#' by specific margins being at a specific level.
#' 
#' \code{tableMargin}: tableMargin is analogous to margin.table except that
#' margin can be given both as array indices or as variable names
#' 
#' \code{tableSlice}: If the table x has dimensions Z,U,V where V has levels 1
#' and 2 then tableSlice can extract the slice of x (in this case a 2-way
#' table) defined by e.g. U=2. Setting impose=1000 implies that a 3-way table
#' is returned with the U=2 slice in the right place and the U=1-slice
#' consisting of 1000 in each cell.
#' 
#' \code{tableOp}: If t1 has dimnames A and B and t2 has dimnames B and C then
#' \code{tableOp(t1,t2)} will return a table (an array) with dimnames A, B and
#' C containing the product.
#' 
#' \code{tableMult(t1,t2)} is a wrapper for \code{tableOp(t1,t2, op="*")} and
#' \code{tableMult(t1,t2)} is a wrapper for \code{tableDiv(t1,t2, op="/")}
#' 
#' \code{tablePerm}: A wrapper for aperm, but tablePerm accepts dimnames in
#' addition to indices.
#' 
#' See examples below.
#' 
#' @aliases tableSlice tableSlicePrim tableMargin tableOp tableMult tableDiv
#' tableOp2 tablePerm tableSetSliceValue tableGetSliceIndex
#' @param x,t1,t2 An array (a vector with a dim and a dimnames attribute)
#' @param margin An index, either numerical or character
#' @param keep.class If TRUE the result will be forced to have the same class
#' as the input; otherwise the result will be an array.
#' @param level A value, either numerical or character
#' @param impose Possible value used to fill up a slice to give it full
#' dimension
#' @param complement If TRUE then the entries of the complement of the slice is
#' set to some value.
#' @param value The value to put into the entries given by \code{margin} and
#' \code{level} - or their complement if \code{complement=TRUE}
#' @param op Either "*" or "/"
#' @param perm The subscript permutation vector, which must be a permutation of
#' the integers 1:n, where n is the number of dimensions of a OR a permutation
#' of the dimension names of a. The default is to reverse the order of the
#' dimensions. A permutation of the dimensions of a.
#' @param resize A flag indicating whether the vector should be resized as well
#' as having its elements reordered.
#' @return An array.
#' @author Søren Højsgaard
#' @seealso \link{margin.table}
#' @keywords utilities
#' @examples
#' 
#' 
#' data(HairEyeColor)
#' 
#' tableMargin(HairEyeColor, "Hair")           ## same as:
#' tableMargin(HairEyeColor, 1)
#' tableMargin(HairEyeColor, c("Hair", "Eye")) ## same as:
#' tableMargin(HairEyeColor, c(1, 2))
#' 
#' 
#' tableSlice(HairEyeColor, "Sex", "Male")              ## same as:
#' tableSlice(HairEyeColor, 3, 1)
#' tableSlice(HairEyeColor, "Sex", "Male", impose=1000) ## same as:
#' tableSlice(HairEyeColor, 3, 1, impose=1000)
#' 
#' t1 <- array(1:4, dim=c(2,2), dimnames=list(gender=c('male','female'),
#'                                            answer=c('yes','no')))
#' t2 <- array(5:8, dim=c(2,2), dimnames=list(answer=c('yes','no'),
#'                                            category=c(1,2)))
#' 
#' tableOp(t1, t2, "*")
#' tableOp(t1, t2, "/")
#' 
#' data(reinis)
#' 
#' t1 <- tableMargin(reinis, c(6, 5, 2, 1))
#' t2 <- tableMargin(reinis, c(6, 5, 3, 4))
#' tt1 <- tableOp(t1, t2)
#' 
#' t1 <- tableMargin(reinis, c(6, 5, 2, 4, 1))
#' t2 <- tableMargin(reinis, c(6, 5, 4))
#' tt1 <- tableOp2(t1, t2)
#' 
#' tableSetSliceValue(HairEyeColor, "Eye", "Brown")
#' tableSetSliceValue(HairEyeColor, "Eye", "Brown", complement=TRUE)
#' 
#' tableSetSliceValue(HairEyeColor, c("Eye","Hair"), c("Brown","Black"))
#' tableSetSliceValue(HairEyeColor, c("Eye","Hair"), c("Brown","Black"), complement=TRUE)
#' 






#' Internal arraypack functions
#' 
#' Internal functions called by other functions.
#' 
#' 
#' @aliases allSubsets__ allSubsets0__ cell2entry2_cpp cell2entry_cpp
#' getCellNumberPrim_cpp getCellNumber_cpp nextCellSlicePrim_cpp
#' nextCellSlice_cpp nextCell_cpp permuteCellEntries_cpp slice2entry_cpp
#' @keywords internal
















#' Utility functions for gRbase
#' 
#' Utility functions for gRbase package. Includes 'faster versions' of certain
#' standard R functions.
#' 
#' \code{colwiseProd} multiplies a vector and a matrix columnwise (as opposed
#' to rowwise which is achieved by \code{v*M}). Hence \code{colwiseProd} does
#' the same as \code{t(v*t(M))} - but it does so faster for numeric values.
#' 
#' @aliases colwiseProd is_subsetof__ get_superset__ get_subset__
#' @return A vector or a logical.
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
#' @keywords utilities
#' @examples
#' 
#' 
#' ## colwiseProd
#' M <- matrix(1:16, nrow=4)
#' v <- 1:4
#' 
#' t(v*t(M))
#' colwiseProd(v,M)
#' 
#' system.time(for (ii in 1:100000)  t(v*t(M)))
#' system.time(for (ii in 1:100000)  colwiseProd(v,M))
#' 





#' Set operations
#' 
#' Miscellaneous set operations.
#' 
#' 'setlist' is a list of vectors representing a set of subsets; i.e. V1,...VQ
#' where Vk is a subset of some base set V.
#' 
#' \code{is.insetlist}: Checks if the set x is in one of the Vk's.
#' 
#' \code{removeRedundant}: Returns those Vk which are not contained in other
#' subsets; i.e. gives the maximal sets. If maximal is FALSE then returns the
#' minimal sets; i.e. Vk is returned if Vk is contained in one of the other
#' sets Vl and there are no set Vn contained in Vk.
#' 
#' Notice that the comparisons are made by turning the elements into characters
#' and then comparing these. Hence 1 is identical to "1".
#' 
#' @aliases is.insetlist isin is.subsetof subsetof removeRedundant maximalSets
#' minimalSets
#' @param x,set Vectors representing sets
#' @param setlist List of vectors (representing a set of subsets)
#' @param maximal Logical; see section 'Details' for a description.
#' @param index Logical; should indices (in setlist) be returned or a set of
#' subsets.
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
#' @keywords utilities
#' @examples
#' 
#' 
#' is.subsetof(c(1,2),c(1,2,3))
#' is.subsetof(c(1,2,3), c(1,2))
#' 
#' l <- list(c(1,2),c(1,2,3),c(2,4),c(5,6), 5)
#' 
#' #subsetofList(c(1,2), l)
#' #subsetofList(c(1,2,3,4), l)
#' 
#' removeRedundant(l)
#' removeRedundant(l, maximal=FALSE)
#' 
#' is.insetlist (c(2,4), l)
#' is.insetlist (c(2,8), l)
#' 
#' 



