## ###########################################################
##
#' @title Array operations (2007)
#'
#' @description Array operations; created to facilitate the gRain
#'     package in 2007. Now largely replaceable by other (often
#'     faster) functions implemented in Rcpp.
#'
#' @name array-operations-07
##
## ###########################################################
#'
#' @param tab,tab1,tab2 Arrays with named dimnames.
#' @param perm A permutation; either indices or names.
#' @param resize A flag indicating whether the vector should be
#'     resized as well as having its elements reordered (default
#'     TRUE).
#' @param keep.class Obsolete argument.
#' 
tablePerm <- function(tab, perm, resize=TRUE, keep.class=FALSE){
  # Like aperm() but perm can be dimnames
  if (missing( perm )){
    perm <- integer(0)
    return(aperm.default( tab, perm, resize ))
  }

  if (is.character( perm )){
    perm <- match(perm, names(dimnames( tab )))
    if ( any( is.na( perm )))
      stop("Invalid permutation...")
  }
  ans <- aperm.default( tab, perm, resize )
  if (keep.class){
      class( ans ) <- oldClass( tab )
  }
  ans
}

#' @rdname array-operations-07
tableMult <- function(tab1, tab2){
  tableOp(tab1, tab2, op="*")
}

#' @rdname array-operations-07
tableDiv <- function(tab1, tab2){
  tableOp(tab1, tab2, op="/")
}

#' @rdname array-operations-07
#' @param op The operation; choices are \code{"*"}, \code{"/"}, \code{"+"}, \code{"-"}.
tableOp <- function(tab1, tab2, op="*"){

  if (!is.array(tab1)) {stop("'tab1' is not an array")}
  if (!is.array(tab2)) {stop("'tab2' is not an array")}

  di1 <- dim(tab1)
  di2 <- dim(tab2)
  dn1 <- dimnames(tab1)
  dn2 <- dimnames(tab2)
  vn1 <- names(dn1)
  vn2 <- names(dn2)

  idx <- match(vn2, vn1)    ## location of variables in vn2 in vn1:
  idx.na <- is.na(idx)      ## logical of variables in {vn2\vn1}

  if (any(idx.na)){         ## If there are variables in {vn2 \ vn1}
    aug.vn <- vn2[idx.na]   ## Find those variables
    aug.di <- di2[idx.na]   ## - and their levels
    aug.dn <- dn2[idx.na]   ## - and their dimnames

    ## Create "augmented" table defined over (vn1, vn2\vn1) by repeating tab1.
    pot1      <- rep.int(as.numeric(tab1), prod(aug.di))
    vn.new    <- c(vn1, aug.vn)
    di.new    <- c(di1, aug.di)
    dn.new    <- c(dn1, aug.dn)
    dim(pot1)      <- di.new
    dimnames(pot1) <- dn.new
  } else {
    pot1   <- tab1
    vn.new <- vn1
    di.new <- di1
    dn.new <- dn1
  }

  ## Find indices of vn2 in augmented table (vn1, vn2\vn1)
  vn2.idx    <- match(vn2, vn.new)
  ## Create perumation indices; first variables in vn2; then vn1\vn2
  perm  <-  c(vn2.idx, (1:length(vn.new))[-vn2.idx])

  if (op == "*") {
    pot1 <- as.numeric(aperm.default(pot1, perm, TRUE)) * as.numeric(tab2)
  }
  else {
    pot1 <- as.numeric(aperm.default(pot1, perm, TRUE)) / as.numeric(tab2)
    pot1[!is.finite(pot1)] <- 0
  }
  dim(pot1)      <- di.new[perm]
  dimnames(pot1) <- dn.new[perm]

  pot1
}


.tableOp <- function(tab1, tab2, op="*"){

  if (!is.array(tab1)) {stop("'tab1' is not an array")}
  if (!is.array(tab2)) {stop("'tab2' is not an array")}

  op <- switch(op,
               "*"={`*`},
               "/"={`/`},
               "+"={`+`},
               "-"={`-`})

  di1 <- dim(tab1)
  di2 <- dim(tab2)
  dn1 <- dimnames(tab1)
  dn2 <- dimnames(tab2)
  vn1 <- names(dn1)
  vn2 <- names(dn2)

  idx <- match(vn2, vn1)   ## location of variables in vn2 in vn1:
  idx.na <- is.na(idx)      ## logical of variables in {vn2\vn1}

  if (any(idx.na)){         ## If there are variables in {vn2 \ vn1}
    aug.vn <- vn2[idx.na]   ## Find those variables
    aug.di <- di2[idx.na]   ## - and their levels
    aug.dn <- dn2[idx.na]   ## - and their dimnames

    ## Create "augmented" table defined over (vn1, vn2\vn1) by repeating tab1.
    vn.new    <- c(vn1, aug.vn)
    di.new    <- c(di1, aug.di)
    dn.new    <- c(dn1, aug.dn)
    tab1           <- rep.int(as.numeric(tab1), prod(aug.di))
    dim(tab1)      <- di.new
    dimnames(tab1) <- dn.new
  } else {
    vn.new <- vn1
    di.new <- di1
    dn.new <- dn1
  }

  ## indices of vn2 in augmented table (vn1, vn2\vn1)
  vn2.idx    <- match(vn2, vn.new)
  ## Create perumation indices; first variables in vn2; then vn1\vn2
  perm  <-  c(vn2.idx, (1:length(vn.new))[-vn2.idx])

  ttab1 <- op(aperm.default(tab1, perm, TRUE), as.numeric(tab2))
  if (identical(op, `/`))
    ttab1[!is.finite(ttab1)] <- 0
  dim(ttab1)      <- di.new[perm]
  dimnames(ttab1) <- dn.new[perm]
  ttab1
}

#' @rdname array-operations-07
#' @param restore Not so clear anymore.
tableOp2 <- .tableOp2 <- function (tab1, tab2, op = `*`, restore = FALSE)
{
  if (!is.array(tab1)){
    str( tab1 )
    stop("'tab1' is not an array")
  }
  if (!is.array(tab2)){
    str( tab2 )
    stop("'tab2' is not an array")
  }

  vn1  <- names(dimnames(tab1))
  vn2  <- names(dimnames(tab2))

  ## indices of vn2 in vn1:
  vn2.idx   <- match(vn2, vn1)
  ## Create perumation indices; first variables in vn2; then vn1\vn2
  perm <- c(vn2.idx, (1:length(vn1))[-vn2.idx])

  pot1 <-
    if (restore) {
      zz    <- op(aperm.default(tab1, perm, TRUE), as.numeric(tab2))
      newvn <- c(vn2, vn1[-vn2.idx])
      perm2 <- match(vn1, newvn)
      aperm.default(zz, perm2, TRUE)
    } else {
      op(aperm.default(tab1, perm, TRUE), as.numeric(tab2))
    }
  if (identical(op, `/`))
    pot1[!is.finite(pot1)] <- 0
  pot1
}


#' @rdname array-operations-07
#' @param margin Index or name of margin.
#' @param level Corresponding level of margin.
#' @param impose Value to be imposed. 
tableSlice <-  function (tab, margin, level, impose)
{
    if (is.null(margin))
        return(tab)
    if (is.null(dimnames(tab)))
        stop("'tableSlice' requires a structure with a dimnames attribute")

    dn    <- dimnames(tab)
    vn    <- names(dn)

    if (is.character(margin)){
        mar.idx <- match(margin, vn)
        if (any((z<-is.na(mar.idx))))
            stop("Variable(s): ", margin[z], " do not exist in table...")
    } else {
        mar.idx <- margin
    }

    if (is.character(level)){
        lev.idx  <- rep(NA, length(level))
        for (kk in seq_along(margin)){
            lev.idx[kk] <- match(level[kk], dn[[mar.idx[kk]]])
        }
        if (any((z<-is.na(lev.idx))))
            stop("Level: ", level[z], " do not exist in table...")
    } else {
        lev.idx <- level
    }

    idx          <- vector("list", length(dim(tab)))
    idx[]        <- TRUE
    idx[mar.idx] <- lev.idx
    ans <-do.call("[", c(list(tab), idx))

    if (!missing(impose) && is.numeric(impose)){
        ans[] <- impose
    }
    ans <- array(ans, dim=sapply(dn[-mar.idx], length), dimnames=dn[-mar.idx])
    ans
}


## tableSlicePrim: Works only with margin and level being indices
#' @rdname array-operations-07
#' @param mar.idx Index of margin
#' @param lev.idx Index of level
tableSlicePrim <- function(tab, mar.idx, lev.idx){
  idx         <- vector("list", length(dim(tab)))
  idx[]       <-TRUE
  idx[mar.idx] <- lev.idx
  do.call("[", c(list(tab), idx))
}

#' @rdname array-operations-07
tableMargin <- function (tab, margin, keep.class = FALSE)
{
##   cat("===== tableMargin =====\n")
##   print(as.data.frame.table(tab));   print(margin)

    if (!is.array(tab))
        stop("'tab' is not an array")

    at <- attributes( tab )
    di <- at[['dim']]
    dn <- at[['dimnames']]
    vn <- names( dn )

    if (length(margin)) {
        if( class(margin)=="formula" ){
            margin <- unlist(rhsf2list( margin ), use.names=FALSE)
        }
        if (is.character(margin)) {
            margin <- unique( margin )
            marg.idx <- match(margin, vn)
            if (any(is.na(marg.idx)))
                stop(sprintf("Variable(s): %s not in table ...\n",
                        toString( margin[is.na(marg.idx)] )) )
        }
        else {
            marg.idx <- margin
        }
        rest.idx <- (seq_along(vn))[-marg.idx]
        nr <- prod( di[marg.idx] )
        nc <- prod( di[rest.idx] )

        z <- rowSumsPrim(
            matrix(
                aperm.default(tab, c(rest.idx, marg.idx), TRUE),
                nrow=nr, ncol=nc, byrow=TRUE))
        attributes(z) <- list(dim=di[marg.idx], dimnames=dn[marg.idx])
    } else {
        z <- sum(tab)
    }
    if (keep.class)
        class(z) <- oldClass( tab )
    return(z)
}

#' @rdname array-operations-07
tableGetSliceIndex <- function(tab, margin, level, complement=FALSE){
    di <- dim(tab)
    dn <- dimnames(tab)
    vn <- names(dn)
    nidx <- match(margin, vn)

    if ( any((z<-is.na(nidx))) ){
        stop(sprintf("margin %s not in table\n",
                     toString(margin[z])))
    }

    sidx <- unlist(lapply(seq_along(nidx),
                          function(i) {match(level[i], dn[[ nidx[i] ]])}),
                   use.names = FALSE)

    if (any((z<-is.na(sidx)))){
        stop(sprintf("level %s not in table\n", toString(level[z])))
    }

    out <- slice2entry(sidx, nidx, di)
    if (complement){
        (1:prod(di))[-out]
    } else {
        out
    }
}

#' @rdname array-operations-07
#' @param complement Should values be set for the complement?
#' @param value Which value should be set
tableSetSliceValue <- function(tab, margin, level, complement=FALSE, value=0){
    idx <- tableGetSliceIndex(tab, margin=margin, level=level, complement=complement)
    tab[ idx ] <- value
    tab
}
















