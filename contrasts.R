

pairs(emtrends(sel_nfl_linear, "year", var = "avFD_std"))
pairs(emtrends(sel_nfl_linear, "year", var = "var_std"))
pairs(emtrends(sel_nfl_linear, "year", var = "skew_std"))
pairs(emtrends(sel_nfl_linear, "year", var = "kurt_std"))


pairs(emtrends(sel_nfl_linear, "year", var = "var_std"))

contrast(emtrends(sel_nfl_linear, "year", var = "var_std"), "pairwise", by = NULL)

library(parallel)
(nc <- detectCores()) # 16 in my case
cl <- makeCluster(rep("localhost", nc))

boot1<-mixed(sel_nfl_linear,data_ids,method="PB",
             args.test = list(nsim = 1000, cl = cl, details = 2))
emtrends(boot1, "year", var = "var_std")
emtrends(boot1, "year", var = "skew_std")
pairs(emtrends(boot1, "year", var = "var_std"))
pairs(emtrends(boot1, "year", var = "skew_std"))



sel_nfl_linear_brms<-brm(fitness_rel~avFD_std+var_std+skew_std+kurt_std+
                           n_fl_log_std+avFD_std:year+var_std:year+
                           skew_std:year+kurt_std:year+(1|id),
             data = subset(data_ids,!is.na(avFD_std)&!is.na(var_std)&
                             !is.na(skew_std)&!is.na(kurt_std)),
             family = gaussian(),
             chains = 4, cores = 4, iter = 10000, warmup = 3000, thin = 5)

summary(sel_nfl_linear_brms)
emtrends(sel_nfl_linear_brms, "year", var = "skew_std")

pairs(emtrends(sel_nfl_linear_brms, "year", var = "var_std"))
pairs(emtrends(sel_nfl_linear_brms, "year", var = "skew_std"))

pairs(emtrends(sel_nfl_linear, "year", var = "var_std"),adjust="mvt")
