ggplot()+
  geom_density(data=data_id_flowers_3fl%>%filter(year==1987),
               aes(x=opening_date_v,group=id),color="#E69F00")+
  geom_density(data=data_id_flowers_3fl%>%filter(year==1988),
               aes(x=opening_date_v,group=id),color="#56B4E9")+
  geom_density(data=data_id_flowers_3fl%>%filter(year==1989),
               aes(x=opening_date_v,group=id),color="#009E73")+
  my_theme()+theme(text=element_text(family="serif"))+
  theme(legend.position="top")+
  labs(x="Opening date (number of days from the vernal equinox)",
       y="Individual")
  