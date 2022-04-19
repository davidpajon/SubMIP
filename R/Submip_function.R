#' Submaximal Intensity Periods
#'
#'@description
#'This function is used to extract the submaximal intensity periods that are above a specific MIP threshold
#'
#' @param MIP Reference value of maximum intensity period, either calculated with mip function or previous/maximal player value
#' @param dur Duration of the maximum intensity period in seconds
#' @param Metric Colum "Metric" of the data frame that wants to be analysed
#' @param threshold Threshold for the detection of the submaximal intensity period
#'
#' @return
#' A dataframe with 3 columns, id, duration and intensity of each submaximal intensity period
#' @export
#'
#' @examples
#' submip(19.6,60,data$MetabolicPower,90)

submip <- function(MIP,dur,Metric,threshold){
  durat <- dur*10
  umb <- threshold*0.01
  j <- as.data.frame(frollmean(Metric,durat))
  names(j) <- "roll"
  j %>%
    mutate(x = roll > (umb*MIP), id = rleid(x)) %>%
    mutate(sec=seq(0.1,nrow(j)/10,0.1)) %>%
    na.omit() %>%
    group_by(id) %>%
    mutate(x = x | n() < durat)%>%
    ungroup() %>%
    mutate(id2=rleid(x))%>%
    dplyr::filter(x) %>%
    group_by(id2) %>%
    summarise(event_duration = (last(sec)- first(sec))+dur, intensity = mean(roll)) %>%
    mutate(id=row.names(.)) %>%
    select("id","event_duration","intensity")
}

