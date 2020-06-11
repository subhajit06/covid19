#!/usr/bin/env Rscript

library(data.table)
library(ggplot2)
library(ggrepel)
library(forestplot)
library(stringr)

######################################### used to make demo data only

entryCount = 1
if(entryCount == 1){
df_cond = readRDS("../EHR_data_integration/concept_hierarchy/OMOP_SQL/new_rds_files/concept_OMOP_table.rds") 
df_cond = df_cond %>% 
  filter(domain_id == "Condition" & standard_concept == "S") %>% 
  select(concept_id, concept_name) %>% select(concept_id, concept_name)
entryCount = entryCount+1
}

# # function for random strings
# myFun <- function(n = 5000) {
#   a <- do.call(paste0, replicate(5, sample(LETTERS, n, TRUE), FALSE))
#   paste0(a, sprintf("%04d", sample(9999, n, TRUE)), sample(LETTERS, n, TRUE))
# }
# 
# # demo table (health records: hr, covid status-cov, # conds-cnd, # people-ppl)
# set.seed(1)
# cnd = 2000
# ppl = 500
# hr = data.table(condition=c("covid_pos", "covid_hos", "covid_vent", myFun(cnd)), 
#                 matrix(rbinom(ppl * (cnd + 3), 1, 0.2), ncol=ppl, nrow=cnd + 3))
# setnames(hr, c("condition", str_pad(23 * (1:ppl), 6, pad="0")))
#########################################

###FUNCTIONS
# function generate Fisher stats 
# given data.table identity matrix with rows=conditions cols=individuals
# first column is "condition"
get_tbl = function(c, i, j) {
  cols = names(c)[2:length(c)]
  a = fisher.test(x=as.vector(unlist(c[ condition == i, ..cols ])),
                  y=as.vector(unlist(c[ condition == j, ..cols ])))
  return(list(a$p.value, a$estimate, a$conf.int[1], a$conf.int[2]))
}

# manhattan function (pass result tbl to function, returns plot obj)
manhat = function(res, outname, date_str) {
  ggman = ggplot(res, aes(x=condition, y=-log10(P), label=condition)) + 
    geom_point(alpha=0.75) + labs(x="Condition", y="-log10(P)") + 
    ggtitle(paste("Association of OMOP ConceptIDs with", outname)) +
    geom_text_repel(data=subset(res, P < 0.01)) + 
    geom_hline(yintercept=-log10(0.05/nrow(res)), color="grey40", linetype="dashed") +
    scale_y_continuous(expand=c(0,0), limits=c(0, abs(floor(log10(min(res$P)))) + 2)) +
    scale_x_discrete() + theme_classic(base_size = 15) + 
    theme( 
      legend.position = "none",
      plot.title = element_text(hjust = 0.5),
      panel.border = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.text.x = element_blank(),#element_text(angle = 90, size = 8, vjust = 0.5)
      axis.ticks = element_blank())
    
  
  file1 = sprintf("./out/%s_test_man_%s.pdf", outname, date_str)
  pdf(file=file1, onefile=T, paper="USr", width=11, height=8.5)
  plot(ggman)
  dev.off()
  return(ggman)
}

# OR plot function (pass result tbl to function, returns plot obj)
plotOR = function(res, outname, date_str) {
  or = res[ P < 0.01 ][order(condition)] 
  file1 = sprintf("./out/%s_test_or_%s.pdf", outname, date_str)
  pdf(file=file1, onefile=T, paper="USr", width=11, height=8.5)
  forestplot(or[, .(condition, P)], mean=or[, .(OR, CIlow, CIhigh)], graph.pos=3,
             title=paste("Odds Ratio for comorbidities of", outname), new_page=F,
             txt_gp=fpTxtGp(label=gpar(cex=1.25),
                            ticks=gpar(cex=1.1),
                            xlab=gpar(cex = 1.2),
                            title=gpar(cex = 1.2)),
             col=fpColors(box="black", lines="black", zero="gray50"),
             zero=1, cex=0.9, lineheight="auto", boxsize=0.5,
             lwd.ci=2, ci.vertices=TRUE)
  dev.off()
  return(forestplot)
}

plotOR_gg = function(res, outname, date_str) {
  
  names(df_cond)[1] = 'condition'
  df_cond$condition = as.character(df_cond$condition)
  
  or = res[ P < 0.01 ][order(condition)] 
  
  df_cond_name_or =  inner_join(or, df_cond, by = "condition")
  
  df_cond_name_or$concept_name_with_P = sprintf("%s (%0.5f)", 
                                                df_cond_name_or$concept_name, df_cond_name_or$P)
  df_cond_name_or = df_cond_name_or[order(-df_cond_name_or$OR),]
  
  tStr = sprintf("Enrichment of top conditions")
  cap_str = sprintf("Among %d OMOP conditions",dim(res)[1])
  
  if(dim(df_cond_name_or)[1] > 20){
    df_cond_name_or = df_cond_name_or[1:20,]
  }
  
  p_or = ggplot(df_cond_name_or,aes(OR, concept_name)) +
    geom_point(size=3, shape=15) +
    geom_errorbarh(aes(xmax = CIhigh, xmin = CIlow), height = 0.15) +
    geom_vline(xintercept = 1, linetype = "longdash") +
    #scale_x_continuous(breaks = seq(-5,30,1), labels = seq(-5,30,1)) +
    scale_y_discrete(limits = as.character(df_cond_name_or$concept_name[order(df_cond_name_or$OR)]))+
    xlab("Odds Ratio") + ylab("") + #xlim(c(0,40)) +
    theme(text = element_text(size=16, hjust=0.5)) +
    ggtitle(tStr)+
    labs(caption = cap_str)+
    theme(plot.caption = element_text(hjust=0.5, size = 13, color = 'blue'))
  
  
  file1 = sprintf("./out/%s_test_or_gg_%s.pdf", outname, date_str)
  pdf(file=file1, onefile=T, paper="USr", width=11, height=8.5)
  plot(p_or)
  dev.off()
  return(p_or)
}

# function to test for each of the covid conditions (in filtered 1/0 matrix, 
# return list of plots for manhattan, OR; write plots and stats to file)
run_test = function(hr_filt, covnam, date_str) {
  # generate stats
  a = copy(hr_filt)
  a[, c("P", "OR", "CIlow", "CIhigh") := get_tbl(a, covnam, condition), 
     by=seq(nrow(a)) ]
  result = a[ condition != covnam, .(condition, P, OR, CIlow, CIhigh) ]
  # plot functions
  b = manhat(result, covnam, date_str)
  #c = plotOR(result, covnam, date_str)
  d = plotOR_gg(result, covnam, date_str)
  # write table
  file1 = sprintf("./out/%s_test_enrich_stats_%s.txt",covnam,date_str)
  fwrite(result, file=file1, sep="\t", quote=F)
  return(list(b,c,d))
}


###MAIN
#data loading
#date_arr = c("04-16-20","04-21-20","04-23-20","04-28-20", "04-30-20",
#              "05-05-20","05-07-20","05-12-20", "05-14-20", '05-19-20','05-21-20')

date_arr = c('05-26-20','05-28-20', '06-02-20', '06-04-20' )
for(ii in 1:length(date_arr)){
  #fileName = sprintf('./HIT_data/covid-%s.csv', date_arr[i])
  #date_str = str_split(str_split(basename(fileName),'.csv')[[1]][1],'covid-')[[1]][2]
  date_str = date_arr[ii]
  file1 = sprintf("./out/cov_cond_person_%s.Rds",date_str)
  hr = readRDS(file=file1)
  ##############################
  
  # run Fisher for all rows except cov, At least 5 count of condition
  #covnams = c("covid_pos", "covid_hos", "covid_vent")
  #covnams = c("cov")
  covnam = "cov"
  # filter columns with too low count
  hrcols = names(hr)[2:length(hr)]
  hr_filt = hr[hr[, rowSums(.SD) > 5 & rowSums(.SD) < length(hrcols), .SDcols=hrcols]]
  # run them
  #for (i in seq(covnams)) {
    #plot_obj = run_test(hr_filt, covnams[[i]], date_str)
  #}
  plot_obj = run_test(hr_filt, covnam, date_str)
  cond_plot_obj = list()
  cond_plot_obj[[1]] = plot_obj[[1]]
  cond_plot_obj[[2]] = plot_obj[[3]]
  
  file1 = sprintf("./out/cond_plot_obj_%s.Rds", date_str)
  saveRDS(cond_plot_obj, file=file1)
}