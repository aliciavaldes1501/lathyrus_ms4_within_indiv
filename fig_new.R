data_figure2<-data_ids%>%
  dplyr::select(year,avFD_v,var,skew,kurt)%>%
  pivot_longer(cols=avFD_v:kurt,names_to="variable",values_to="value")


fig2<-ggplot(data_ids,aes(x=avFD_v))+
  geom_histogram(color="black",alpha=0.6,position="identity")+
  my_theme()+xlab("Mean")+ylab("Count")+
  ggplot(data_ids,aes(x=avFD_v,fill=year))+
  geom_histogram(color="black",position="identity")+
  scale_fill_manual(values=c("#E69F00","#56B4E9","#009E73"))+
  facet_grid(cols=vars(year))+
  my_theme()+xlab("Mean")+ylab("Count")+
  theme(axis.title.y = element_blank())+
  theme(plot.margin = margin(0, 0, 0, 0, "pt"))+
  ggplot(data_ids,aes(x=var))+
  geom_histogram(color="black",alpha=0.6,position="identity")+
  my_theme()+xlab("Variance")+ylab("Count")+
  theme(plot.margin = margin(0, 0, 0, 0, "pt"))+
  ggplot(data_ids,aes(x=var,fill=year))+
  geom_histogram(color="black",position="identity")+
  scale_fill_manual(values=c("#E69F00","#56B4E9","#009E73"))+
  facet_grid(cols=vars(year))+
  my_theme()+xlab("Variance")+ylab("Count")+
  theme(axis.title.y = element_blank())+
  theme(plot.margin = margin(0, 0, 0, 0, "pt"))+
  ggplot(data_ids,aes(x=skew))+
  geom_histogram(color="black",alpha=0.6,position="identity")+
  my_theme()+xlab("Skewness")+ylab("Count")+
  theme(plot.margin = margin(0, 0, 0, 0, "pt"))+
  ggplot(data_ids,aes(x=skew,fill=year))+
  geom_histogram(color="black",position="identity")+
  scale_fill_manual(values=c("#E69F00","#56B4E9","#009E73"))+
  facet_grid(cols=vars(year))+
  my_theme()+xlab("Skewness")+ylab("Count")+
  theme(axis.title.y = element_blank())+
  theme(plot.margin = margin(0, 0, 0, 0, "pt"))+
  ggplot(data_ids,aes(x=kurt))+
  geom_histogram(color="black",alpha=0.6,position="identity")+
  my_theme()+xlab("Kurtosis")+ylab("Count")+
  theme(plot.margin = margin(0, 0, 0, 0, "pt"))+
  ggplot(data_ids,aes(x=kurt,fill=year))+
  geom_histogram(color="black",position="identity")+
  scale_fill_manual(values=c("#E69F00","#56B4E9","#009E73"))+
  facet_grid(cols=vars(year))+
  my_theme()+xlab("Kurtosis")+ylab("Count")+
  theme(axis.title.y = element_blank())+
  theme(plot.margin = margin(0, 0, 0, 0, "pt"))+
  plot_layout(ncol=2,nrow=4,widths=c(0.3,0.7))

ggsave(fig2,filename="output/figures/fig2.tiff",
       device="tiff",width=20,height=20,units="cm",dpi=300,compression="lzw")




