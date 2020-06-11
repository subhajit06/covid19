library(tidyverse)
library(reshape2)
library(ggplot2)
library(grid)
library(gridExtra)
library(ggsignif)


MIN_COUNT = 5

cap_str_age = paste0("H_0: Proportion of an age group that is positive is same as the proportion of positive in the entire population (H_1: greater)")
cap_str_race = paste0("H_0: Proportion of a race group that is positive is same as the proportion of positive in the entire population (H_1: greater)")

stat_plot_obj = list()

#date_arr = c("04-16-20","04-21-20","04-23-20", "04-28-20", "04-30-20","05-05-20",
#               "05-07-20","05-12-20", "05-14-20", '05-19-20','05-21-20')

date_arr = c('05-26-20','05-28-20', '06-02-20', '06-04-20' )



for(ii in 1:length(date_arr)){
  #fileName = sprintf('./HIT_data/covid-%s.csv', date_arr[i])
  #date_str = str_split(str_split(basename(fileName),'.csv')[[1]][1],'covid-')[[1]][2]
  date_str = date_arr[ii]
  file1 = sprintf("./out/df_covid_ghi_%s.Rds", date_str)
  
  df_covid_ghi_loc_sel = readRDS(file=file1)
  
  df_tmp = as.data.frame(table(df_covid_ghi_loc_sel$result_txt))
  tot_covid_pos = df_tmp$Freq[which(df_tmp$Var1  == "COVID-19 Positive")]
  tot_covid_neg = df_tmp$Freq[which(df_tmp$Var1  == "COVID-19 Negative")] 
  tot_covid_tested = tot_covid_pos + tot_covid_neg
  #tbl_age_count = table(df_covid_ghi_loc_sel$AGE_BRACKET, df_covid_ghi_loc_sel$RESULT)
  tbl_age_count = table(df_covid_ghi_loc_sel$AGE_BRACKET, df_covid_ghi_loc_sel$result_txt)
  L = dim(tbl_age_count)[1]
  
  
  df_tmp1 = NULL
  df_tmp1$age_group = row.names(tbl_age_count)
  df_tmp1$covid_19_neg = as.vector(tbl_age_count[,1])
  df_tmp1$covid_19_pos = as.vector(tbl_age_count[,2])
  
  age_group_pval = NULL
  
  for(i in 1:L){
    ##Age group i
    ### Hypothesis 1
    k1 = tbl_age_count[as.character(i+1),"COVID-19 Positive"]
    k2 = tbl_age_count[as.character(i+1),"COVID-19 Negative"]
    
    age_group_pval$p1[i]= (prop.test(x=c(k1, k2), n=c(tot_covid_pos, tot_covid_neg), 
                                     alternative = 'two.sided'))$p.value
    
    ### Hypothesis 2
    age_group_pval$p2[i] = (prop.test(x=c(k1, tot_covid_pos), n=c(k1+k2, tot_covid_tested), 
                                      alternative = 'greater'))$p.value
    
  }
  df_tmp1$pval1 = age_group_pval$p1
  df_tmp1$pval2 = age_group_pval$p2
  
  data = as.data.frame(table(df_covid_ghi_loc_sel$AGE_BRACKET, df_covid_ghi_loc_sel$result_txt))
  data$type = rep(2, length(data$Freq))
  g0 = ggplot(data=data, aes(x=Var1, y=Freq, fill=Var2)) +
    geom_bar(color="black",stat="identity", position=position_dodge())+
    geom_text(aes(label=Freq), vjust=1.6, color="black",
              position = position_dodge(0.9), size=5) +
    theme_classic(base_size = 16) +
    xlab("Age-group") + ylab("Count") +scale_fill_discrete("")+
    xlim(-5, 12) + ylim(-20, max(tbl_age_count)+10)+
    scale_x_discrete(labels=c("2" = "20-29","3" = "30-39", "4" = "40-49",
                              "5" = "50-59","6" = "60-69", "7" = "70-79", "8" = "80+"))+
    theme(legend.position="bottom")
  
  h1 = max(tbl_age_count)
  h2 = -12
  
  g1 = g0
  text_for_grob = list()
  MULTI_P_VAL_CORR = 0.05/L
  ##pval1 and pval2
  for(j in 1:L){
    X1 = 0.5*(2*j-1)
    str_tmp1 = ""
    str_tmp2 = ""
    if(tbl_age_count[j,2] >= MIN_COUNT & age_group_pval$p2[j] <= 0.05){
      str_tmp1 = sprintf("P-val: %0.5f", age_group_pval$p1[j])
      str_tmp2 = sprintf("P-val: %0.5f", age_group_pval$p2[j])
    }
    if(age_group_pval$p1[j] < MULTI_P_VAL_CORR){
      str_tmp1 = sprintf("%s [*]",str_tmp1)
    }
    if(age_group_pval$p2[j] < MULTI_P_VAL_CORR){
      str_tmp2 = sprintf("%s [*]",str_tmp2)
    }
    hk = max(tbl_age_count[j,])+12
    
    #g1 = g1 + geom_signif(annotation=str_tmp1,
    #                      y_position=df_tmp1$covid_19_neg[j]+8, xmin=X1, xmax=X1+1, 
    #                      tip_length = c(0.04, 0.04), color='blue')
    
    text_for_grob[[j]] <- textGrob(str_tmp2, gp = gpar(col = 'blue', fontsize = 15))
    g1 = g1 + annotation_custom(grob = text_for_grob[[j]],  
                                xmin = j-1, xmax = j+1, ymin <- hk, ymax = ymin)+
      labs(caption = cap_str_age)+
      theme(plot.caption = element_text(hjust=0.5, size = 14, color = 'blue'))
    
  }
  
  
  stat_plot_obj[[1]] = g1
  file1 = sprintf("./out/age_pvals_%s.pdf",date_str)
  pdf(file=file1, width = 10, height = 8)
  plot(g1)
  dev.off()
  
  #####################
  
  #tbl_race_count = table(df_covid_ghi_loc_sel$race_source_value, df_covid_ghi_loc_sel$RESULT)
  tbl_race_count = table(df_covid_ghi_loc_sel$race, df_covid_ghi_loc_sel$result_txt)

  L = dim(tbl_race_count)[1]
  
  df_tmp2 = NULL
  df_tmp2$race_group = row.names(tbl_race_count)
  df_tmp2$covid_19_neg = as.vector(tbl_race_count[,1])
  df_tmp2$covid_19_pos = as.vector(tbl_race_count[,2])
  
  
  race_group_pval = NULL
  for(i in 1:L){
    ##Race group i
    ### Hypothesis 1
    k1 = tbl_race_count[i,"COVID-19 Positive"]
    k2 = tbl_race_count[i,"COVID-19 Negative"]
    
    race_group_pval$p1[i]= (prop.test(x=c(k1, k2), n=c(tot_covid_pos, tot_covid_neg), 
                                      alternative = 'two.sided'))$p.value
    
    ### Hypothesis 2
    race_group_pval$p2[i] = (prop.test(x=c(k1, tot_covid_pos), n=c(k1+k2, tot_covid_tested), 
                                       alternative = 'greater'))$p.value
    
  }
  
  
  h1 = max(tbl_race_count)+10
  h2 = -30
  
  df_tmp2$pval1 = race_group_pval$p1
  df_tmp2$pval2 = race_group_pval$p2
  df_tmp2=as.data.frame(df_tmp2)
  
  data = as.data.frame(table(df_covid_ghi_loc_sel$race, df_covid_ghi_loc_sel$result_txt))
  data$type = rep(3, length(data$Freq))
  g0 = ggplot(data=data, aes(x=Var1, y=Freq, fill=Var2)) +
    geom_bar(color="black",stat="identity", position=position_dodge())+
    geom_text(aes(label=Freq), vjust=1.6, color="black",
              position = position_dodge(0.9), size=5) +
    theme_classic(base_size = 16)+
    xlab("Race") + ylab("Count") + scale_fill_discrete("") +
    theme(axis.text.x = element_text(angle = 90, hjust = 0.8, vjust = 0.8))+
    ylim(h2, h1)+theme(legend.position="bottom")
  
  
  
  # g0 = ggplot(data=df_tmp2_melt, aes(x=race_group, y=value, fill=variable)) +
  #   geom_bar(color="black",stat="identity", position=position_dodge())+
  #   geom_text(aes(label=value), vjust=1.6, color="black",
  #             position = position_dodge(0.9), size=5) +
  #   theme_classic(base_size = 13) +
  #   xlab("Race") + ylab("Count") + 
  #   theme(axis.text.x = element_text(angle = 90, hjust = 0.8, vjust = 0.8))+
  #   scale_fill_discrete("Status") +
  #   ylim(h2, h1)
  
  g1 = g0
  text_for_grob = list()
  MULTI_P_VAL_CORR = 0.05/L
  ##pval1 and pval2
  for(j in 1:L){
    X1 = 0.5*(2*j-1)
    str_tmp1 = ""
    str_tmp2 = ""
    if(tbl_race_count[j,2] >= MIN_COUNT & race_group_pval$p2[j] <= 0.05){
      str_tmp1 = sprintf("P-val: %0.5f", race_group_pval$p1[j])
      str_tmp2 = sprintf("P-val: %0.5f", race_group_pval$p2[j])
    }
    if(race_group_pval$p1[j] < MULTI_P_VAL_CORR){
      str_tmp1 = sprintf("%s [*]",str_tmp1)
    }
    if(race_group_pval$p2[j] < MULTI_P_VAL_CORR){
      str_tmp2 = sprintf("%s [*]",str_tmp2)
    }
    
    hk = max(tbl_race_count[j,])+40
    hk
    #g1 = g1 + geom_signif(annotation=str_tmp1,
    #                      y_position=df_tmp2$covid_19_neg[j]+8, xmin=X1, xmax=X1+1, 
    #                      tip_length = c(0.04, 0.04), color='blue')
    
    text_for_grob[[j]] <- textGrob(str_tmp2, gp = gpar(col = 'blue', fontsize = 15))
    g1 = g1 + annotation_custom(grob = text_for_grob[[j]],  
                                xmin = j-1, xmax = j+1, ymin <- hk, ymax = ymin)+
    
              labs(caption = cap_str_race)+
               theme(plot.caption = element_text(hjust=0.5, size = 14, color = 'blue'))
    
  }
  
  
  stat_plot_obj[[2]] = g1
  
  file1 = sprintf("./out/race_pvals_%s.pdf", date_str)
  pdf(file1, width = 10, height = 8)
  plot(g1)
  dev.off()

  file1 = sprintf("./out/stat_plot_obj_%s.Rds", date_str)
  saveRDS(stat_plot_obj,file=file1)
}

