#' Plot Change 14Days Incidence
#'
#' This returns a plot displaying the change in 14days incidence for selected countries
#' @param Dataset,PopulationDataset,ListCountries
#' @keywords
#' @export
#' @examples
#' Plot_Change14DaysIncidence()

Plot_Change14DaysIncidence<-function(Dataset,PopulationDataset,ListCountries){
  PopulationData<-read.csv(PopulationDataset) %>% select(ADM0NAME,UNPOP2019)
  Data<-read.csv(Dataset)
  MaxDate<-as.Date(max(Data$DateReport1))
  TotalCasesToday<-Data %>% filter(DateReport1==MaxDate) %>% select(ADM0NAME,TotalCasesToday=TotalCases)
  TotalCases7DaysAgo<-Data %>% filter(DateReport1==MaxDate-7) %>% select(ADM0NAME,TotalCases7DaysAgo=TotalCases)
  TotalCases14DaysAgo<-Data %>% filter(DateReport1==MaxDate-14) %>% select(ADM0NAME,TotalCases14DaysAgo=TotalCases)
  TotalCases21DaysAgo<-Data %>% filter(DateReport1==MaxDate-21) %>% select(ADM0NAME,TotalCases21DaysAgo=TotalCases)
  Data<-TotalCasesToday %>% left_join(TotalCases7DaysAgo,by='ADM0NAME') %>%
    left_join(TotalCases14DaysAgo,by='ADM0NAME') %>%
    left_join(TotalCases21DaysAgo,by='ADM0NAME') %>%
    left_join(PopulationData,by='ADM0NAME') %>%
    mutate(FrtDaysIncidenceToday=(TotalCasesToday-TotalCases14DaysAgo)/UNPOP2019*100000,
          FrtDaysIncidenceOneWeekAgo=(TotalCases7DaysAgo-TotalCases21DaysAgo)/UNPOP2019*100000,
          Change=if_else(FrtDaysIncidenceToday>FrtDaysIncidenceOneWeekAgo,'Increase','Decrease')) %>%
    arrange(FrtDaysIncidenceToday) %>%
    filter(ADM0NAME %in% ListCountries)
  Data$ADM0NAME<-factor(Data$ADM0NAME,levels=unique(Data$ADM0NAME))
  plot<-ggplot(Data,aes(color=Change))+
    scale_color_manual(breaks=c('Decrease','Increase'),values=c('#0DBF6F','#9E1C1A'))+
    geom_segment(aes(x=FrtDaysIncidenceOneWeekAgo, y=ADM0NAME, xend=FrtDaysIncidenceToday, yend=ADM0NAME),arrow=arrow(length = unit(0.02, "npc")), size=1,show.legend=FALSE)+
    xlab("Change in 14-days cumulative incidence compared to one week ago")+
    ylab('')
  return(plot)
}




