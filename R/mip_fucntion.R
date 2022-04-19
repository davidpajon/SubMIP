#' Maximal Intensity Period
#'
#'@description
#'This function is used to extract the maximal intensity output of a variable for a specific duration
#'
#' @param Metric Colum "Metric" of the data frame that wants to be analysed
#' @param dur Duration of the maximum intensity period in seconds
#'
#' @return
#' Numeric value
#' @export
#'
#' @examples mip(data$MetabolicPower,60)
#'
#'
#'
mip <- function(Metric,dur){
  dur <- dur*10
  s <- as.data.table(frollmean(Metric,dur))
  as.numeric(apply(s,2,max,na.rm=T))
}

