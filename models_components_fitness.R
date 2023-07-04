model87_fitness_init_mat_pred<-glm.nb(fitness~fr_init+fr_mat+prop_seeds_escpred,subset(data_ids,year==1987))
model88_fitness_init_mat_pred<-glm.nb(fitness~fr_init+fr_mat+prop_seeds_escpred,subset(data_ids,year==1988))
model89_fitness_init_mat_pred<-glm.nb(fitness~fr_init+fr_mat+prop_seeds_escpred,subset(data_ids,year==1989))

model87_fitness_init<-glm.nb(fitness~fr_init,subset(data_ids,year==1987))
model87_fitness_mat<-glm.nb(fitness~fr_mat,subset(data_ids,year==1987))
model87_fitness_pred<-glm.nb(fitness~prop_seeds_escpred,subset(data_ids,year==1987))

model87_fitness_init_mat<-glm.nb(fitness~fr_init+fr_mat,subset(data_ids,year==1987))
model87_fitness_init_pred<-glm.nb(fitness~fr_init+prop_seeds_escpred,subset(data_ids,year==1987))
model87_fitness_mat_pred<-glm.nb(fitness~fr_mat+prop_seeds_escpred,subset(data_ids,year==1987))

R87_init_mat_pred<-rsquared(model87_fitness_init_mat_pred,method="nagelkerke")$R.squared
R87_init<-rsquared(model87_fitness_init,method="nagelkerke")$R.squared
R87_mat<-rsquared(model87_fitness_mat,method="nagelkerke")$R.squared
R87_pred<-rsquared(model87_fitness_pred,method="nagelkerke")$R.squared
R87_init_mat<-rsquared(model87_fitness_init_mat,method="nagelkerke")$R.squared
R87_init_pred<-rsquared(model87_fitness_init_pred,method="nagelkerke")$R.squared
R87_mat_pred<-rsquared(model87_fitness_mat_pred,method="nagelkerke")$R.squared

init_87<-R87_init_mat_pred-R87_mat_pred
mat_87<-R87_init_mat_pred-R87_init_pred
pred_87<-R87_init_mat_pred-R87_init_mat
init_mat_87<-R87_init_mat_pred-R87_pred-init_87-mat_87
init_pred_87<-R87_init_mat_pred-R87_mat-init_87-pred_87
mat_pred_87<-R87_init_mat_pred-R87_init-mat_87-pred_87
init_mat_pred_87<-R87_init_mat_pred-init_87-mat_87-pred_87-init_mat_87-init_pred_87-mat_pred_87

(init_87/R87_init_mat_pred)*100
(mat_87/R87_init_mat_pred)*100
(pred_87/R87_init_mat_pred)*100
(init_mat_87/R87_init_mat_pred)*100
(init_pred_87/R87_init_mat_pred)*100
(mat_pred_87/R87_init_mat_pred)*100
(init_mat_pred_87/R87_init_mat_pred)*100

init_87+mat_87+pred_87+init_mat_87+init_pred_87+mat_pred_87+init_mat_pred_87
