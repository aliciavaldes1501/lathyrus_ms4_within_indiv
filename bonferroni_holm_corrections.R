p_values_1<-c(summary(fr_set_linear_87)$coefficients[2:5,4],
              summary(fr_set_linear_88)$coefficients[2:5,4],
              summary(fr_set_linear_89)$coefficients[2:5,4],
              summary(seed_escpred_linear_87)$coefficients[2:5,4],
              summary(seed_escpred_linear_88)$coefficients[2:5,4],
              summary(seed_escpred_linear_89)$coefficients[2:5,4])
table_p_values<-cbind(
  variable=c(rep("fr_set",12),rep("seeds_escpred",12)),
  year=as.integer(c(rep(1987,4),rep(1988,4),rep(1989,4))),
  tibble(enframe(round(p_values_1,3))%>%rename(p_value=value)),
  tibble(p_value_bonf=round(p.adjust(p_values_1,method="bonferroni"),3)),
  tibble(p_value_holm=round(p.adjust(p_values_1,method="holm"),3)),
  tibble(p_value_hochberg=round(p.adjust(p_values_1,method="hochberg"),3)),
  tibble(p_value_hommel=round(p.adjust(p_values_1,method="hommel"),3)),
  tibble(p_value_BH=round(p.adjust(p_values_1,method="BH"),3)),
  tibble(p_value_BY=round(p.adjust(p_values_1,method="BY"),3)))

print(xtable(table_p_values,digits=3), type="html", file="example.html")

