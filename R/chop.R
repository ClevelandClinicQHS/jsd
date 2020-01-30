#' Convert a continuous variable to a discrete variable.
#'
#' @param x A numeric or integer vector.
#' @param factorVars (optional) A character vector of variable names in \code{x} to be directly converted to factors.
#' @param skip (optional) A character vector of variable names in \code{x} to skip processing.
#' @param .m (optional) Default value for the 'm' parameter to \code{Hmisc::cut2()}
#' @param .g (optional) Default value for the 'g' parameter to \code{Hmisc::cut2()}
#' @param ... Additional arguments passed to \code{hist()} or \code{Hmisc::cut2()}
#' @return A factor the same length as \code{x}.
#' @details When \code{x} is numeric, \code{chop()} assumes that any specified values for \code{cuts},
#' \code{g}, or \code{m} are exactly as they are required to be in \code{cut2()}. When \code{x} is a
#' data frame, on the other hand, these parameters (and any other parameters passed to \code{cut2()}
#' or \code{hist()})  are specified through parameters that have names which are common to the names of
#' numeric varibles in \code{x}. (Any such specifications for non-numeric variables in \code{x} are
#' ignored.) See examples for a demonstration of this usage. Note that \code{.m} and \code{.g} are
#' overridden by any explicit values given for specific variables.  See behavior for variable 'drat'
#' in the final example.
#' @seealso \code{\link{hist}} and \code{\link{Hmisc::cut2}}
#' for more details.
#' @examples
#' table(chop(0:100))
#' table(chop(0:100000))
#' table(chop(0:100, breaks = c(-Inf,23,68,Inf))) #breaks passed to hist()
#' table(chop(0:100, cuts   = c(-Inf,23,68,Inf))) #same result, but cuts is passed to cut2()
#' table(chop(0:100, g=4))
#' table(chop(0:100, m=17))
#' summary(chop(mtcars, .m=10, .factorVars = c("cyl","vs","am","gear")))
#' z <-  chop(mtcars,
#'            .factorVars = c("cyl","vs","am","gear"),
#'            .skip = c("mpg"),
#'            .g = 4,
#'            drat = list(g=8),
#'            qsec = list(cuts=c(0,16,18,20,100)))
#' summary(z)
#' table(z$drat)
#'
#' g_chop <- chop(glucose, glucose = list(cuts = c(0,109,125,300)))
#' table(g_chop$glucose, g_chop$cohort)
#' @rdname chop
#' @export
chop <- function(x, ...) {
  UseMethod("chop")
}

#' @rdname chop
#' @export

chop.default <- function(x, ...){
  if(!(is.numeric(x) | is.data.frame(x))) stop("'x' must be numeric vector or data frame.")
}

#' @rdname chop
#' @export

chop.numeric <- function(x, ...) {
  dots <- list(...)
  if(max(c("cuts","m","g") %in% names(dots)) == 0){
    hh <- try(hist(x, plot=FALSE, ...), silent=TRUE)
    if('try-error' %in% class(hh)){
      stop(paste0("Call to 'hist' produced error:\n",as.character(hh)))
    }
    dots$cuts <- hh$breaks
    return(Hmisc::cut2(x, cuts=dots$cuts))
  } else return(Hmisc::cut2(x, ...))
}


#' @rdname chop
#' @export

chop.data.frame <- function(x, .factorVars, .skip, .m, .g, ...){
  dots <- list(...)

  if(!missing(.factorVars)){
    if(!is.character(.factorVars)) stop("Parameter '.factorVars' must be a character vector.")
    if(!all(.factorVars %in% names(x))) stop("Not all variables in '.factorVars' are present in 'x'.")
    for(var in .factorVars) x[,var] <- factor(x[,var], levels=sort(unique(x[,var])))
  }

  varTypes <- sapply(x,class)
  if(any(!varTypes %in% c("numeric","integer","factor"))) {
    stop("All variables in 'x' must be numeric or factor.")
  }
  numVrs <- names(varTypes[which(varTypes != c("factor"))])
  if(!missing(.skip)){
    if(!is.character(.skip)) stop("Parameter '.skip' must be a character vector.")
    if(!all(.skip %in% names(x))) warning("Not all variables in '.skip' are present in 'x'.")
    numVrs <- setdiff(numVrs, .skip)
  }
  for(var in numVrs){
    chopArgs <- list(x = x[[var]])
    if(!is.null(dots[[var]]$m)){
      chopArgs <- c(chopArgs, m = dots[[var]]$m)
    } else if(!missing(.m)) chopArgs <- c(chopArgs, m = .m)
    if(!is.null(dots[[var]]$g)){
      chopArgs <- c(chopArgs, g = dots[[var]]$g)
    } else if(!missing(.g)) chopArgs <- c(chopArgs, g = .g)
    dots[[var]]$m <- dots[[var]]$g <- NULL
    chopArgs <- c(chopArgs, dots[[var]])

    x[,var] <- do.call(chop.numeric, args = chopArgs)
  }
  return(x)
}
