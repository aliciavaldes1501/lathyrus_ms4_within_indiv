library(emmeans)
summary(sel_nfl_linear)
         
### Differences among years in the effect of var_std ###
# https://cran.r-project.org/web/packages/emmeans/vignettes/interactions.html

emtrends(sel_nfl_linear, pairwise ~ year, var = "var_std")

emtrends(sel_nfl_linear, pairwise ~ year, var = "avFD_std",adjust="fdr")$contrasts
emtrends(sel_nfl_linear, pairwise ~ year, var = "var_std",adjust="fdr")$contrasts
emtrends(sel_nfl_linear, pairwise ~ year, var = "skew_std",adjust="fdr")$contrasts
emtrends(sel_nfl_linear, pairwise ~ year, var = "kurt_std",adjust="fdr")$contrasts
# all NS


### Bootstrapped emmtrends ###
# Maybe the above is not *correct* because of non-normality of residuasl
# https://stackoverflow.com/questions/76045326/extracting-bootstrapped-emmeans-ci-s-and-the-corresponding-replicates-from-a-l
cont.df<-as.data.frame(emtrends(sel_nfl_linear, pairwise ~ year, var = "var_std")$contrasts)

# create function for bootstrap of contrasts
cont.boot=function(model){
  emtrnds=emtrends(model,pairwise~year,var="var_std",adjust="fdr")
  cont=emtrnds$contrasts
  cont.df=as.data.frame(cont)
  cont.df$estimate
}

# Create function to bootstrap Anova
Anova.boot=function(model){
  data.frame(Anova(model))$Chisq
}

# bootstrap lmm
boots=bootMer(sel_nfl_linear,cont.boot,nsim = 1000,verbose=T)
confint(boots) # CIs do not overlap zero for 1987-1988

# This would show all pairwise diffs
# and show that they are only *significant* for 1987-1988
# and would be a justification for performing yearly tests

Anova_boot=bootMer(sel_nfl_linear,Anova.boot,nsim=100)
round(confint(Anova_boot),3)

# https://journal.r-project.org/articles/RJ-2023-015/
library(lmeresampler)

# Check papers in Zotero folder!
# Which bootstrap method to use?

sel_nfl_linear_boot<-bootstrap(sel_nfl_linear, .f=fixef, type="parametric",
                               B=1000,.refit=T)
summary(sel_nfl_linear_boot)
plot(sel_nfl_linear_boot)
confint(sel_nfl_linear_boot)
sel_nfl_linear_boot_pvalues<-bootstrap_pvals(sel_nfl_linear,type="parametric",
                                             B=1000)
sel_nfl_linear_boot_pvalues
# var_std:year1988 significant with bootstrapped p-value

### Model without interactions with year ###

sel_nfl_linear_noints<-lmer(fitness_rel~avFD_std+var_std+skew_std+kurt_std+n_fl_log_std+
       (1|id),
     # Not including main effect of yr cause fitness rel within yrs
     subset(data_ids,!is.na(avFD_std)&!is.na(var_std)&
              !is.na(skew_std)&!is.na(kurt_std)))
summary(sel_nfl_linear_noints)
sel_nfl_linear_noints_boot_pvalues<-bootstrap_pvals(sel_nfl_linear_noints
                                                    ,type="parametric",
                                                    B=1000) # Selection for var and skew
sel_nfl_linear_noints_boot_pvalues

b_sel_nfl_linear_noints<-bootMer(x=sel_nfl_linear_noints,FUN=fixef,nsim=10000)
save(b_sel_nfl_linear_noints,file="output/b_sel_nfl_linear_noints.RData")

confint(b_sel_nfl_linear_noints,level=0.95, method="boot") # Percentile method
# Selection for var and skew

### New figure selection ###

grid.arrange(
  ggplot()+
    geom_line(data=ggpredict(sel_nfl_linear_noints,terms=c("avFD_std")),
              aes(x=x,y=predicted),
              linewidth=1,color="#CC0000",linetype="dashed")+
    geom_ribbon(data=ggpredict(sel_nfl_linear_noints,terms=c("avFD_std")),
                aes(x=x,y=predicted,ymin=conf.low,ymax=conf.high),
                alpha=0.3,fill="#CC0000")+
    geom_point(data=data_ids,aes(x=avFD_std,y=fitness_rel),
               size=2,alpha=0.3)+
    xlab("Mean")+ylab(NULL)+my_theme(),
  ggplot()+
    geom_line(data=ggpredict(sel_nfl_linear_noints,terms=c("var_std")),
              aes(x=x,y=predicted),
              linewidth=1,color="#CC0000",linetype="solid")+
    geom_ribbon(data=ggpredict(sel_nfl_linear_noints,terms=c("var_std")),
                aes(x=x,y=predicted,ymin=conf.low,ymax=conf.high),
                alpha=0.3,fill="#CC0000")+
    geom_point(data=data_ids,aes(x=var_std,y=fitness_rel),
               size=2,alpha=0.3)+
    xlab("Variance")+ylab(NULL)+my_theme(),
  ggplot()+
    geom_line(data=ggpredict(sel_nfl_linear_noints,terms=c("skew_std")),
              aes(x=x,y=predicted),
              linewidth=1,color="#CC0000",linetype="solid")+
    geom_ribbon(data=ggpredict(sel_nfl_linear_noints,terms=c("skew_std")),
                aes(x=x,y=predicted,ymin=conf.low,ymax=conf.high),
                alpha=0.3,fill="#CC0000")+
    geom_point(data=data_ids,aes(x=skew_std,y=fitness_rel),
               size=2,alpha=0.3)+
    xlab("Skewness")+ylab(NULL)+my_theme(),
  ggplot()+
    geom_line(data=ggpredict(sel_nfl_linear_noints,terms=c("kurt_std")),
              aes(x=x,y=predicted),
              linewidth=1,color="#CC0000",linetype="dashed")+
    geom_ribbon(data=ggpredict(sel_nfl_linear_noints,terms=c("kurt_std")),
                aes(x=x,y=predicted,ymin=conf.low,ymax=conf.high),
                alpha=0.3,fill="#CC0000")+
    geom_point(data=data_ids,aes(x=kurt_std,y=fitness_rel),
               size=2,alpha=0.3)+
    xlab("Kurtosis")+ylab(NULL)+my_theme(),
  ncol=2,left="Relative fitness")

### emmtrends model fruit set ###

emtrends(fr_set_linear, pairwise ~ year, var = "avFD_std",adjust="fdr")$contrasts
emtrends(fr_set_linear, pairwise ~ year, var = "var_std",adjust="fdr")$contrasts
emtrends(fr_set_linear, pairwise ~ year, var = "skew_std",adjust="fdr")$contrasts
emtrends(fr_set_linear, pairwise ~ year, var = "kurt_std",adjust="fdr")$contrasts

### Model fruit set without interactions with year ###

fr_set_linear_noints<-glmer(cbind(fr_set=n_mat_intact_fr,
                           fr_no_set=n_fl-n_mat_intact_fr)~avFD_std+var_std+
                       skew_std+kurt_std+year+ 
                       # Include main effect of year here
                       # but not in selection models!
                       (1|id),
                     data_ids,family="binomial")
summary(fr_set_linear_noints)

### emmtrends model seed pred ###
data_ids$npr_seeds<-with(data_ids,round(n_seed)-round(n_preyed_seed))
data_ids$pr_seeds<-with(data_ids,round(n_preyed_seed))
# Refit model to work with emtrends
seed_escpred_linear<-glmer(cbind(npr_seeds,pr_seeds)~
                             avFD_std+var_std+skew_std+kurt_std+year+ 
                             avFD_std:year+var_std:year+
                             skew_std:year+kurt_std:year+
                             (1|id),
                           subset(data_ids,n_seed>0),family="binomial",
                           glmerControl(optimizer="bobyqa"))

emtrends(seed_escpred_linear, pairwise ~ year, var = "avFD_std",adjust="fdr")$contrasts
emtrends(seed_escpred_linear, pairwise ~ year, var = "var_std",adjust="fdr")$contrasts
emtrends(seed_escpred_linear, pairwise ~ year, var = "skew_std",adjust="fdr")$contrasts
emtrends(seed_escpred_linear, pairwise ~ year, var = "kurt_std",adjust="fdr")$contrasts



### BELOW NOT USED SO FAR ###

### Model fruit set ###
library(DHARMa)
plot(simulateResiduals(fr_set_linear)) # OK

# Parametric bootstrap
fr_set_linear_boot<-lmeresampler::bootstrap(fr_set_linear, .f=fixef, type="residual",
                              B=100,.refit=T) 
fr_set_linear_boot_pvalues<-bootstrap_pvals(fr_set_linear,type="residual",
                                             B=100)

fr_set_linear_boot<-bootstrap(fr_set_linear, .f=fixef, type="case",resample=c(F,T),
          B=100,.refit=T) # For some reason, type="parametric" does not work
# residual also works
# See which one to use!
seed_escpred_linear_boot<-bootstrap(seed_escpred_linear, .f=fixef,type="parametric",
                              B=1000,.refit=T)

fr_set_linear_boot_pvalues<-bootstrap_pvals(fr_set_linear,
                                                    type="parametric",B=1000)

# changig ref level: different results! - NOT USE!

data_ids$year_relevel<-relevel(data_ids$year,ref="1989")

sel_nfl_linear<-lmer(fitness_rel~avFD_std+var_std+skew_std+kurt_std+n_fl_log_std+
                       avFD_std:year+var_std:year+skew_std:year+kurt_std:year+
                       (1|id),
                     # Not including main effect of yr cause fitness rel within yrs
                     subset(data_ids,!is.na(avFD_std)&!is.na(var_std)&
                              !is.na(skew_std)&!is.na(kurt_std)))
summary(sel_nfl_linear)

sel_nfl_linear_relevel<-lmer(fitness_rel~avFD_std+var_std+skew_std+kurt_std+n_fl_log_std+
                       avFD_std:year_relevel+var_std:year_relevel+skew_std:year_relevel+kurt_std:year_relevel+
                       (1|id),
                     # Not including main effect of yr cause fitness rel within yrs
                     subset(data_ids,!is.na(avFD_std)&!is.na(var_std)&
                              !is.na(skew_std)&!is.na(kurt_std)))
summary(sel_nfl_linear_relevel)

