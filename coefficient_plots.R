# With values
coefplot_87<-grid.arrange(
  plot_models(fr_init_linear_87,fr_mat_linear_87,seed_escpred_linear_87,
              show.values=F,transform=NULL,axis.title=c("Model estimates"),
              axis.labels=c("Kurtosis ","Skewness ","Variance ","Mean "),
              digits=3,dot.size=2)+
    theme_bw()+theme(legend.position="none")+
    theme(text=element_text(family="serif"))+
    geom_hline(yintercept=0,linetype="dotted",color="black",size=0.8)+
    scale_color_manual(values=c("#009E73","#56B4E9","#E69F00"))+
    theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),
          axis.title.x = element_blank())+
    theme(text = element_text(size = 20))+
    scale_y_continuous(limits=c(-0.75,1.25)),
  plot_models(fr_init_nonlinear_87,fr_mat_nonlinear_87,seed_escpred_nonlinear_87,
              show.values=F,transform=NULL,axis.title=c("Model estimates"),
              axis.labels=c(expression("Kurtosis"^2),expression("Skewness"^2),
                            expression("Variance"^2),expression("Mean"^2)),
              digits=3,dot.size=2,
              rm.terms=c("avFD_std","var_std","skew_std","kurt_std"))+
    theme_bw()+theme(legend.position="none")+
    theme(text=element_text(family="serif"))+
    geom_hline(yintercept=0,linetype="dotted",color="black",size=0.8)+
    scale_color_manual(values=c("#009E73","#56B4E9","#E69F00"))+
    theme(text = element_text(size = 20))+
    scale_y_continuous(limits=c(-0.75,1.25)),
  ncol=1,top=textGrob("1987",gp=gpar(fontsize=16,fontfamily="serif"),
                      just="center",hjust=-0.7),
  heights=c(0.9,1))
coefplot_88<-grid.arrange(
  plot_models(fr_init_linear_88,fr_mat_linear_88,seed_escpred_linear_88,
              show.values=F,transform=NULL,axis.title=c("Model estimates"),
              axis.labels=c("Kurtosis ","Skewness ","Variance ","Mean "),
              digits=3,dot.size=2)+
    theme_bw()+theme(legend.position="none")+
    theme(text=element_text(family="serif"))+
    geom_hline(yintercept=0,linetype="dotted",color="black",size=0.8)+
    scale_color_manual(values=c("#009E73","#56B4E9","#E69F00"))+
    theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),
          axis.title.x=element_blank())+scale_x_discrete(labels=NULL)+
    theme(text = element_text(size = 20))+
    scale_y_continuous(limits=c(-0.75,0.75)),
  plot_models(fr_init_nonlinear_88,fr_mat_nonlinear_88,seed_escpred_nonlinear_88,
              show.values=F,transform=NULL,axis.title=c("Model estimates"),
              axis.labels=c(expression("Kurtosis"^2),expression("Skewness"^2),
                            expression("Variance"^2),expression("Mean"^2)),
              digits=3,dot.size=2,
              rm.terms=c("avFD_std","var_std","skew_std","kurt_std"))+
    theme_bw()+theme(legend.position="none")+
    theme(text=element_text(family="serif"))+
    geom_hline(yintercept=0,linetype="dotted",color="black",size=0.8)+
    scale_color_manual(values=c("#009E73","#56B4E9","#E69F00"))+
    scale_x_discrete(labels=NULL)+
    theme(text = element_text(size = 20))+
    scale_y_continuous(limits=c(-0.75,0.75)),
  ncol=1,top=textGrob("1988",gp=gpar(fontsize=16,fontfamily="serif"),
                      just="center"),
  heights=c(0.9,1))
coefplot_89<-grid.arrange(
  plot_models(fr_init_linear_89,fr_mat_linear_89,seed_escpred_linear_89,
              show.values=F,transform=NULL,axis.title=c("Model estimates"),
              axis.labels=c("Kurtosis ","Skewness ","Variance ","Mean "),
              digits=3,dot.size=2)+
    theme_bw()+theme(legend.position="none")+
    theme(text=element_text(family="serif"))+
    geom_hline(yintercept=0,linetype="dotted",color="black",size=0.8)+
    scale_color_manual(values=c("#009E73","#56B4E9","#E69F00"))+
    theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),
          axis.title.x = element_blank())+
    scale_x_discrete(labels=NULL)+
    theme(text = element_text(size = 20))+
    scale_y_continuous(limits=c(-0.75,1)),
  plot_models(fr_init_nonlinear_89,fr_mat_nonlinear_89,seed_escpred_nonlinear_89,
              show.values=F,transform=NULL,axis.title=c("Model estimates"),
              axis.labels=c(expression("Kurtosis"^2),expression("Skewness"^2),
                            expression("Variance"^2),expression("Mean"^2)),
              digits=3,dot.size=2,
              rm.terms=c("avFD_std","var_std","skew_std","kurt_std"))+
    theme_bw()+theme(legend.position="none")+
    theme(text=element_text(family="serif"))+
    geom_hline(yintercept=0,linetype="dotted",color="black",size=0.8)+
    scale_color_manual(values=c("#009E73","#56B4E9","#E69F00"))+
    scale_x_discrete(labels=NULL)+
    theme(text = element_text(size = 20))+
    scale_y_continuous(limits=c(-0.75,1)),
  ncol=1,top=textGrob("1989",gp=gpar(fontsize=16,fontfamily="serif"),
                      just="center"),
  heights=c(0.9,1))

coefplot<-grid.arrange(coefplot_87,coefplot_88,coefplot_89,
                       ncol=3,widths=c(1,0.7,0.7))
legend1<-get_legend(plot_models(fr_init_linear_87,fr_mat_linear_87,seed_escpred_linear_87,
                                show.values=T,transform=NULL,axis.title=c("Model estimates"),
                                axis.labels=c("Kurtosis ","Skewness ","Variance ","Mean "),digits=3)+
                      theme_bw()+theme(legend.position="bottom")+
                      theme(text=element_text(family="serif"))+
                      geom_hline(yintercept=0,linetype="dotted",color="black",size=0.8)+
                      scale_color_manual(labels=c("Seeds escaping predation",
                                                  "Fruit maturation","Fruit initiation"),
                                         values=c("#009E73","#56B4E9","#E69F00"))+
                      theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),
                            axis.title.x = element_blank())+labs(color=NULL)+
                      theme(text = element_text(size = 20)))

coefplot_legend<-grid.arrange(legend1,coefplot,nrow=2,heights=c(0.05,0.95))

ggsave(file="output/figures/coefplot_legend.tiff", plot=coefplot_legend,
       width=8, height=6)
ggsave(file="output/figures/coefplot_legend.svg", plot=coefplot_legend,
       width=8, height=25.3)
