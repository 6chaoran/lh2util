#' @importFrom stats quantile IQR
NULL

#' compute mode number of a numeric array
#' 
#' return the mode number 
#' 
#' @param x input array
#' @param na.rm if remove na
#' @return the mode number
#' @export
#' @examples 
#' util.get.mode(c(rep(1,4), rep(2, 10)))
#' util.get.mode(c(rep('a',4), rep('b', 10)))
util.get.mode <- function(x, na.rm = TRUE){
  if(na.rm) x <- x[!is.na(x)]
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#' generalized aggregation function
#' 
#' generalized aggregation function, handy when use together with summarise_all
#' 
#' @param x input series/array
#' @param agg.func the aggregation function
#' @return the aggregated value
#' @export
#' @examples 
#' util.agg.value(c(rep(1,4), rep(2, 10)), agg.func = mean)
#' util.agg.value(c(rep('a',4), rep('b', 10)), agg.func = util.get.mode)
#' library(dplyr)
#' mtcars %>% summarise_all(function(i) util.agg.value(i, util.get.mode))
util.agg.value <- function(x, agg.func = max){
  if(is.character(x)){
    na <- NA_character_
  } else {
    na <- NA_real_
  }

  x <- x[!is.na(x)]
  if(length(x) > 0){
    agg.func(x)
  } else {
    na
  }
}

#' cap outliers within a certain range
#' 
#' cap outliers within the range of Q1/Q3 +/- 1.5 * IQR
#' 
#' @param x input numeric array
#' @param verbose boolean, TRUE -> show cap limits
#' @return capped numeric array
#' @export
#' @examples 
#' util.cap.outlier(c(rnorm(10), 100), verbose = TRUE)

util.cap.outlier <- function(x, verbose = FALSE){

  x <- as.numeric(x)
  iqr <- IQR(x, na.rm = TRUE)
  q75 <- quantile(x, 0.75, na.rm = TRUE)
  q25 <- quantile(x, 0.25, na.rm = TRUE)
  limit.lo <- q25 - iqr * 1.5
  limit.hi <- q75 + iqr * 1.5
  if(verbose) message(glue('cap range: [{round(limit.lo,1)},{round(limit.hi,1)}]'))
  x[x < limit.lo & !is.na(x)] <- limit.lo
  x[x > limit.hi & !is.na(x)] <- limit.hi
  return(x)
}

#' fill NA with constant value or function
#' 
#' fill NA with constant value or function
#' 
#' @param x input array
#' @param fill.with constant value or function
#' @return array with filled na
#' @export
#' @examples 
#' util.fill.na(c(rnorm(10), NA), fill.with = 100)
#' util.fill.na(c(rnorm(10), NA), fill.with = length)
util.fill.na <- function(x, fill.with = 0){

  # fill.with can be constant or a function

  if(class(fill.with) == 'function'){
    x.without.na <- x[!is.na(x)]
    fill.with.value <- fill.with(x.without.na)
  } else {
    fill.with.value <- fill.with
  }

  ifelse(is.na(x), fill.with.value, x)
}

