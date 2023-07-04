library(ggpmisc)

summarise_April<-data_weather%>%filter(year==1987|year==1988|year==1989|
                                         year==1990|year==1991|year==1992|
                                         year==1993|year==1994|year==1995|
                                         year==1996|year==2006|year==2007|
                                         year==2008|year==2009|year==2010|
                                         year==2011|year==2012|year==2013|
                                         year==2014|year==2015|year==2016|
                                         year==2017)%>%
  group_by(year)%>%
  filter(month==4)%>%
  summarise(mean_means_April=mean(mean),var_means_April=var(mean),
            min_means_April=min(mean),max_means_April=max(mean),
            mean_mins_April=mean(min),mean_maxs_April=mean(max))%>%
  mutate(range_means_April=max_means_April-min_means_April)

cor(summarise_April[2:8])

grid.arrange(
  ggplot(summarise_April,aes(x=mean_means_April,y=var_means_April))+
    geom_point()+geom_smooth(method="lm")+
    xlab("Mean of daily mean temperatures for April")+
    ylab("Variance of daily mean temperatures for April")+
    stat_poly_eq(formula=y~x,aes(x=mean_means_April,y=var_means_April,
                                 label = paste(..eq.label..,
                                               ..p.value.label..,
                                               ..rr.label..,
                                               sep = "~~~~")),parse = TRUE),
  ggplot(summarise_April,aes(x=mean_means_April,y=range_means_April))+
    geom_point()+geom_smooth(method="lm")+
    xlab("Mean of daily mean temperatures for April")+
    ylab("Range of daily mean temperatures for April")+
    stat_poly_eq(formula=y~x,aes(x=mean_means_April,y=range_means_April,
                                 label = paste(..eq.label..,
                                               ..p.value.label..,
                                               ..rr.label..,
                                               sep = "~~~~")),parse = TRUE),
             ncol=2)

grid.arrange(
  ggplot(summarise_April,aes(x=mean_means_April,y=min_means_April))+
    geom_point()+geom_smooth(method="lm")+
    xlab("Mean of daily mean temperatures for April")+
    ylab("Minimum of daily mean temperatures for April")+
    stat_poly_eq(formula=y~x,aes(x=mean_means_April,y=min_means_April,
                                 label = paste(..eq.label..,
                                               ..p.value.label..,
                                               ..rr.label..,
                                               sep = "~~~~")),parse = TRUE),
  ggplot(summarise_April,aes(x=mean_means_April,y=mean_mins_April))+
    geom_point()+geom_smooth(method="lm")+
    xlab("Mean of daily mean temperatures for April")+
    ylab("Mean of daily minimum temperatures for April")+
    stat_poly_eq(formula=y~x,aes(x=mean_means_April,y=mean_mins_April,
                                 label = paste(..eq.label..,
                                               ..p.value.label..,
                                               ..rr.label..,
                                               sep = "~~~~")),parse = TRUE),
  ggplot(summarise_April,aes(x=mean_means_April,y=max_means_April))+
    geom_point()+geom_smooth(method="lm")+
    xlab("Mean of daily mean temperatures for April")+
    ylab("Maximum of daily mean temperatures for April")+
    stat_poly_eq(formula=y~x,aes(x=mean_means_April,y=max_means_April,
                                 label = paste(..eq.label..,
                                               ..p.value.label..,
                                               ..rr.label..,
                                               sep = "~~~~")),parse = TRUE),
  ggplot(summarise_April,aes(x=mean_means_April,y=mean_maxs_April))+
    geom_point()+geom_smooth(method="lm")+
    xlab("Mean of daily mean temperatures for April")+
    ylab("Mean of daily maximum temperatures for April")+
    stat_poly_eq(formula=y~x,aes(x=mean_means_April,y=mean_maxs_April,
                                 label = paste(..eq.label..,
                                               ..p.value.label..,
                                               ..rr.label..,
                                               sep = "~~~~")),parse = TRUE),
  ncol=2)

n_days_min_below_zero<-data_weather%>%filter(year==1987|year==1988|year==1989|
                                               year==1990|year==1991|year==1992|
                                               year==1993|year==1994|year==1995|
                                               year==1996|year==2006|year==2007|
                                               year==2008|year==2009|year==2010|
                                               year==2011|year==2012|year==2013|
                                               year==2014|year==2015|year==2016|
                                               year==2017)%>%
  group_by(year)%>%
  filter(month==4)%>%
  filter(min<=0)%>%
  summarise(n_days_min_below_zero=n())

ggplot(full_join(summarise_April,n_days_min_below_zero),
                 aes(x=mean_means_April,y=n_days_min_below_zero))+
         geom_point()+geom_smooth(method="lm")+
         xlab("Mean of daily mean temperatures for April")+
         ylab("Number of days when minimum temperature is below zero")+
         stat_poly_eq(formula=y~x,aes(x=mean_means_April,
                                      y=n_days_min_below_zero,
                                      label = paste(..eq.label..,
                                                    ..p.value.label..,
                                                    ..rr.label..,
                                                    sep = "~~~~")),parse = TRUE)
