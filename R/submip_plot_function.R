#' SubMaximal Intensity Periods Plot
#'
#'@description
#'This function is used to plot the submaximal intensity periods that are above a specific MIP threshold, locate them in the time series and visualize its duration and intensity
#'
#'
#' @param MIP Reference value of maximum intensity period
#' @param dur Duration of the maximum intensity period in seconds
#' @param Metric Colum "Metric" of the data frame that wants to be analysed
#' @param threshold Threshold for the detection of the submaximal intensity period
#'
#' @return plot
#' @export
#'
#' @examples
#' submip_plot(19.6,60,data$MetabolicPower,90)
submip_plot <- function(MIP,dur,Metric,threshold){
  sec <- seq(0.1,length(Metric)/10,0.1)
  dat <-as.data.frame(cbind(Metric,sec))
  durat <- dur*10
  umb <- threshold*0.01
  j <- as.data.frame(frollmean(Metric,durat))
  names(j) <- "roll"
  k <- j %>%
    mutate(x = roll > (umb*MIP), id = rleid(x)) %>%
    mutate(sec=seq(0.1,nrow(j)/10,0.1))
  m <- k %>%
    mutate(tiempocorresp=lag(sec,(durat-1))) %>%
    mutate(tiempocorrespfin=sec) %>%
    na.omit() %>%
    group_by(id) %>%
    mutate(x = x | n() < durat )%>%
    ungroup() %>%
    mutate(id2=rleid(x))%>%
    dplyr::filter(x) %>%
    group_by(id2) %>%
    summarise(event_duration = (last(sec)- first(sec))+dur, int = mean(roll),Timeini=first(tiempocorresp),Timefin=last(tiempocorrespfin),max=max(roll),min=min(roll))

  l <- ggplot()+
    geom_line(data=j,aes(sec,roll*3),color="#2e4057",size=0.6)+
    geom_ribbon(data = j, aes(sec,ymax = pmax(roll*3, 0), ymin =  0), fill = "gray40", alpha = 0.4) +
    geom_line(data=dat,aes(sec,dat[,1]/1.5),alpha=0.3,color="black")+
    geom_hline(aes(yintercept=((umb*MIP)*3),linetype='Threshold'),colour='#d1495b', size=0.5,show.legend = TRUE)+
    scale_linetype_manual(name = element_blank(), values = c(2, 2),
                          guide = guide_legend(override.aes = list(color = c("#d1495b"))))+
    geom_rect(data=m,aes(xmin = Timeini, xmax = Timefin,
                         ymax = int*3,ymin = 0), size=0.1,fill="#ffc425",alpha= 0.7)+
    theme_minimal()+
    scale_y_continuous(sec.axis = sec_axis(~ . /3))+
    labs(x = "Time (s)",
         title = paste0("Submaximal Intensity Periods"),
         subtitle = paste0(threshold," % MIP"))+
    theme(axis.title.y=element_blank(),
          plot.background = element_rect(fill = "gray90"),
          panel.background = element_rect(fill = "gray90" ,
                                          colour = "gray90",
                                          size = 1,
                                          linetype = "solid"),
          panel.grid.major = element_line(size = 0.1,
                                          linetype = 'solid',
                                          colour = "gray90"),
          panel.grid.minor = element_line(size = 0.1,
                                          linetype = 'solid',
                                          colour = "gray90"))
  l

}
