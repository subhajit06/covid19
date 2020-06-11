library(data.table)
library(ggplot2)
library(reshape2)
library(tidyverse)
library(rgdal)
library(rgeos)
library(leaflet)
library(stringr)
library(tigris) 

entryCount = 1

add_age_bracket <- function(df_covid_ghi_loc_sel){
  ### age_bracket = < 9 = 1; 10-19 = 1; 20-29 = 2; 30-39 = 3; 
  ###             40-49 = 4; 50-59 = 5; 60-69 = 6; 70-79 = 7; 80+ = 8
  ### 
  df_covid_ghi_loc_sel$AGE_BRACKET = df_covid_ghi_loc_sel$AGE %/% 10
  df_covid_ghi_loc_sel$AGE_BRACKET[which(df_covid_ghi_loc_sel$AGE_BRACKET > 8)] = 8 
}

change_date_format <-function(orig_date_str){
    tmp_str_only_date = str_split(orig_date_str," ")[[1]][1]
    t1 = str_split(tmp_str_only_date,"/")[[1]]
    new_date_str = sprintf("%s-%s-%s",t1[3], t1[1], t1[2])
    date_str = as.Date(new_date_str)
  
  return(date_str)
}

make_date_ordered <- function(date_str, min_date_str){
  n_days = date_str - min_date_str
  return(n_days)
}

setwd("~/Desktop/ToDo/CPG/covid-19/")
if(entryCount == 1){
  df_cond = readRDS("../EHR_data_integration/concept_hierarchy/OMOP_SQL/new_rds_files/concept_OMOP_table.rds")
  df_cond = df_cond %>% 
            filter(domain_id == "Condition" & standard_concept == "S") %>% 
            select(concept_id, concept_name) %>% select(concept_id, concept_name)

  df_measure = readRDS("../EHR_data_integration/concept_hierarchy/OMOP_SQL/new_rds_files/measurement_OMOP_table.rds")
  entryCount = entryCount+1
}
### for file 04_16_20 .. use this code
#fileName = './HIT_data/covid-04-16-20.csv'
#fileName = './HIT_data/covid-04-21-20.csv'
#fileName = './HIT_data/covid-04-23-20.csv'
#fileName = './HIT_data/covid-04-28-20.csv'
#fileName = './HIT_data/covid-04-30-20.csv'
#fileName = './HIT_data/covid-05-05-20.csv'
#fileName = './HIT_data/covid-05-07-20.csv'

#freeze_arr = c('04-16-20','04-21-20','04-23-20', '04-28-20', '04-30-20', '05-05-20', 
#                 '05-07-20', '05-12-20', '05-14-20','05-19-20','05-21-20')
freeze_arr = c('05-26-20','05-28-20', '06-02-20', '06-04-20' )
for(i in 1:length(freeze_arr)){

  fileName = sprintf('./HIT_data/covid-%s.csv', freeze_arr[i])
  date_str = str_split(str_split(basename(fileName),'.csv')[[1]][1],'covid-')[[1]][2]
  
  df_covid = fread(fileName)
  
  col_new_names = c('ghi_rid','person_id','age','gender','race','ethnicity',
                        'zip_code','result','hospitalization','death','vented','status',
                        'order_date','result_date','ever_vented','smoking','diabetes')
  
  names(df_covid) = col_new_names[1:dim(df_covid)[2]]
  
  ###### biospecimen Yes/No
  df_bio_rid = fread("./biosamples_list/GHI_RID_BIOSAMPLES_22May_2020.csv")
  df_covid$biosample = as.integer(df_covid$ghi_rid %in% df_bio_rid$`Research ID`)
  
  pos_person_id = df_covid %>% filter(result == '1') %>% select(person_id)
  neg_person_id = df_covid %>% filter(result == '0') %>% select(person_id)
  
  #df_person = readRDS("../EHR_data_integration/concept_hierarchy/OMOP_SQL/new_rds_files/person_OMOP_table.rds")
  #df_measure = readRDS("../EHR_data_integration/concept_hierarchy/OMOP_SQL/new_rds_files/measurement_OMOP_table.rds")
  
  
  df_measure_wt = df_measure %>% 
    filter(measurement_source_value == "WEIGHT" & !is.na(value_source_value)) %>% 
    select(person_id, measurement_date, value_source_value)
  
  df_measure_wt_max_date = df_measure_wt %>%
    group_by(person_id) %>% 
    summarize(latest_wt = value_source_value[which(measurement_date == max(measurement_date))[1]])
  
  df_measure_bmi = df_measure %>% 
    filter(measurement_source_value == "BMI" & !is.na(value_source_value)) %>% 
    select(person_id, measurement_date, value_source_value)
  
  df_measure_bmi_max_date = df_measure_bmi %>%
    group_by(person_id) %>% 
    summarize(latest_bmi = value_source_value[which(measurement_date == max(measurement_date))[1]])
  
  df_measure_bp_d = df_measure %>% 
    filter(measurement_source_value == "BP_DIASTOLIC" & !is.na(value_source_value)) %>% 
    select(person_id, measurement_date, value_source_value)
  df_measure_bp_d_max_date = df_measure_bp_d %>%
    group_by(person_id) %>% 
    summarize(latest_bp_d = value_source_value[which(measurement_date == max(measurement_date))[1]])
  
  df_measure_bp_s = df_measure %>% 
    filter(measurement_source_value == "BP_SYSTOLIC" & !is.na(value_source_value)) %>% 
    select(person_id, measurement_date, value_source_value)
  df_measure_bp_s_max_date = df_measure_bp_s %>%
    group_by(person_id) %>% 
    summarize(latest_bp_s = value_source_value[which(measurement_date == max(measurement_date))[1]])
  
  df_covid_ghi = left_join(df_covid, df_measure_bmi_max_date, by='person_id') 
  df_covid_ghi = left_join(df_covid_ghi, df_measure_bp_d_max_date, by='person_id')  
  df_covid_ghi = left_join(df_covid_ghi, df_measure_bp_s_max_date, by='person_id')  
  df_covid_ghi = left_join(df_covid_ghi, df_measure_wt_max_date, by='person_id') 
  
  df_covid_ghi = df_covid_ghi %>% mutate(AGE_BRACKET = age %/% 10)
  
  date_arr = unlist(lapply(df_covid_ghi$order_date, function(x){change_date_format(x)}))
  min_date_str = min(date_arr)
  df_covid_ghi$order_day = unlist(lapply(date_arr, function(x){make_date_ordered(x,min_date_str)}))
  
  #date_arr = unlist(lapply(df_covid_ghi$result_date, function(x){change_date_format(x)}))
  #min_date_str = min(date_arr)
  #df_covid_ghi$result_day = unlist(lapply(date_arr, function(x){make_date_ordered(x,min_date_str)}))
  
  
  df_covid_ghi$result_txt = rep("", dim(df_covid_ghi)[1])
  df_covid_ghi$result_txt[which(df_covid_ghi$result == "1")] = "COVID-19 Positive"
  df_covid_ghi$result_txt[which(df_covid_ghi$result == "0")] = "COVID-19 Negative"
  df_covid_ghi$zip_code = as.integer(df_covid_ghi$zip_code)
  df_covid_ghi$AGE_BRACKET[which(df_covid_ghi$AGE_BRACKET > 8)] = 8
  
  outfile = sprintf("./out/df_covid_ghi_%s.Rds",date_str)
  saveRDS(df_covid_ghi,file=outfile)
  
  ###############
  
  #df_cond = readRDS("../EHR_data_integration/concept_hierarchy/OMOP_SQL/new_rds_files/concept_OMOP_table.rds")
  df_person_with_cond_ancestor = readRDS("../EHR_data_integration/concept_hierarchy/OMOP_SQL/hierarchy/person_cond_arr_with_ancestor.rds")
  
  pos_id =  unlist(lapply(pos_person_id$person_id, function(x) {
    which(df_person_with_cond_ancestor$person_id == x)}))
  neg_id =  unlist(lapply(neg_person_id$person_id, function(x) {
    which(df_person_with_cond_ancestor$person_id == x)}))
  
  pos_cond_set = vector()
  for(i in pos_id){
    pos_cond_set = c(pos_cond_set, str_split(
      df_person_with_cond_ancestor$condition_concept_id_with_ancestor_arr[i],",")[[1]])
  }
  
  df_pos_cond_count = as.data.frame(sort(table(pos_cond_set), decreasing = TRUE))
  colnames(df_pos_cond_count) = c("concept_id","freq")
  df_pos_cond_count$concept_id = as.numeric(as.character(df_pos_cond_count$concept_id))
  df_pos_cond_count_with_name = inner_join(df_cond, df_pos_cond_count, by="concept_id") %>% 
    select("concept_id","concept_name", "freq")
  df_pos_cond_count_with_name = df_pos_cond_count_with_name[order(df_pos_cond_count_with_name$freq, 
                                                                  decreasing = TRUE),]
  neg_cond_set = vector()
  for(i in neg_id){
    neg_cond_set = c(neg_cond_set, str_split(
      df_person_with_cond_ancestor$condition_concept_id_with_ancestor_arr[i],",")[[1]])
  }
  df_neg_cond_count = as.data.frame(sort(table(neg_cond_set), decreasing = TRUE))
  colnames(df_neg_cond_count) = c("concept_id","freq")
  df_neg_cond_count$concept_id = as.numeric(as.character(df_neg_cond_count$concept_id))
  df_neg_cond_count_with_name = inner_join(df_cond, df_neg_cond_count, by="concept_id") %>% 
    select("concept_id","concept_name", "freq")
  df_neg_cond_count_with_name = df_neg_cond_count_with_name[order(df_neg_cond_count_with_name$freq, 
                                                                  decreasing = TRUE),]
  
  outfile = sprintf('./out/df_pos_cond_count_with_name_%s.tsv',date_str)
  fwrite(df_pos_cond_count_with_name,file=outfile, sep="\t")
  outfile = sprintf('./out/df_neg_cond_count_with_name_%s.tsv',date_str)
  fwrite(df_neg_cond_count_with_name,file=outfile, sep="\t")
  
  #### in the process of making a 0/1 table for all tested participants 
  #### with all the conditions that only available for them
  
  df_person_covid_with_cond_ancestor = df_person_with_cond_ancestor %>%
    filter(person_id %in% df_covid_ghi$person_id) %>%
    select(person_id, condition_concept_id_with_ancestor_arr)  
  
  L = dim(df_person_with_cond_ancestor)[1] ### in the whole set
  cond_set = NULL
  for(i in 1:L){
    set_tmp = unlist(lapply(df_person_with_cond_ancestor$condition_concept_id_with_ancestor_arr[i], 
                            function(x){str_split(x,",")[[1]]}))
    cond_set = union(cond_set, set_tmp)
  }
  
  cond_set_sorted =  sort(as.integer(cond_set))
  
  N_particpants = length(df_person_covid_with_cond_ancestor$person_id)
  N_conds = length(cond_set_sorted)
  
  mat_all_cond_covid = matrix(rep(0, N_particpants*N_conds), nrow = N_particpants)
  
  colnames(mat_all_cond_covid) = as.character(cond_set_sorted)
  rownames(mat_all_cond_covid) = as.character(df_person_covid_with_cond_ancestor$person_id)
  
  for(i in 1:N_particpants){
    set_tmp = sort(as.integer(unlist(lapply(df_person_covid_with_cond_ancestor$condition_concept_id_with_ancestor_arr[i], 
                                            function(x){str_split(x,",")[[1]]}))))
    mat_all_cond_covid[i,] = as.integer(cond_set_sorted %in% set_tmp)
  }
  
  outfile = sprintf("./out/mat_all_cond_covid_%s.Rds",date_str)
  saveRDS(mat_all_cond_covid,file=outfile)
  
  mat_all_cond_covid_tr = t(mat_all_cond_covid)
  
  covid_conds = sort(union(df_neg_cond_count_with_name$concept_id, 
                           df_pos_cond_count_with_name$concept_id))
  
  id_list = which(rownames(mat_all_cond_covid_tr) %in% as.character(covid_conds) == TRUE)
  
  #### in Rob's format
  N_conds_covid = length(covid_conds)
  mat_all_cond_covid_tr_add_cov = matrix(0, nrow = N_conds_covid+1 , ncol = N_particpants)
  ### cov row
  mat_all_cond_covid_tr_add_cov [1,] = as.integer(colnames(mat_all_cond_covid_tr) %in% 
                                                    as.character(pos_person_id$person_id))
  
  mat_all_cond_covid_tr_add_cov[2:(N_conds_covid+1),] = mat_all_cond_covid_tr[id_list,]
  
  cond_covid = NULL
  cond_covid$concept_id = as.integer(rownames(mat_all_cond_covid_tr[id_list,]))
  cond_covid = as.data.frame(cond_covid)
  cond_name_covid = inner_join(cond_covid, df_cond, by="concept_id") %>% select(concept_id, concept_name)
  
  cov_cond_person = data.table(condition=c('cov', cond_name_covid$concept_id), 
                               mat_all_cond_covid_tr_add_cov)
  
  setnames(cov_cond_person, c("condition", colnames(mat_all_cond_covid_tr)))
  
  outfile = sprintf("./out/cov_cond_person_%s.Rds",date_str)
  saveRDS(cov_cond_person,file=outfile)
  
  ####################
  
  #date_str = "04-16-20"
  ### map part
  
  ### county
  file1 = sprintf("./out/df_covid_ghi_%s.Rds", date_str)
  df_covid_ghi_loc_sel = readRDS(file=file1)
  
  county_map = readRDS("./GHI_county_map.rds") #### tri state county map
  df_zip_info = fread("./GHI_zip_AQI_summary_output.csv")
  df_covid_ghi_loc_sel_ext = inner_join(df_covid_ghi_loc_sel, df_zip_info, by="zip_code") 
  t1 = addmargins(table(df_covid_ghi_loc_sel_ext$result_txt, df_covid_ghi_loc_sel_ext$state_county), 1)
  attr(t1, "dimnames")[[1]][3] = 'Total tested'
  df_tab = as.data.frame(t1)
  colnames(df_tab) = c('Status','state_county','Count')
  df_tab$state_county = as.character(df_tab$state_county)
  df_covid_ghi_county_count = inner_join(df_zip_info, df_tab, by="state_county") 
  
  state_county_list = unique(df_tab$state_county)
  county_map_filtered = county_map[county_map$state_county %in% state_county_list,]
  data_filtered = df_tab %>% filter(Status == "Total tested")
  #leafmap = merge(county_map_filtered, data_filtered, by='state_county',all=FALSE, no.dups=TRUE)
  df_covid_ghi_county_map = geo_join(county_map_filtered, data_filtered, by='state_county')
  
  df_covid_ghi_county_map$positive = unlist(lapply(df_covid_ghi_county_map$state_county,function(x){
    df_tab$Count[which(df_tab$state_county == x & 
                         df_tab$Status == "COVID-19 Positive")[1]]}))
  
  df_covid_ghi_county_map$negative = unlist(lapply(df_covid_ghi_county_map$state_county,function(x){
    df_tab$Count[which(df_tab$state_county == x & 
                         df_tab$Status == "COVID-19 Negative")[1]]}))
  
  file1 = sprintf("./out/df_covid_ghi_county_map_%s.Rds",date_str)
  saveRDS(df_covid_ghi_county_map, file=file1)
  
  
  #file1 = sprintf("./out/df_covid_ghi_%s.Rds",date_str)
  #df_covid_ghi_loc_sel = readRDS(file=file1)
  
  df_covid_ghi = df_covid_ghi_loc_sel
  df_covid_ghi_loc = as.data.frame(table(df_covid_ghi$result_txt, 
                                         df_covid_ghi$zip_code))
  colnames(df_covid_ghi_loc) = c('status','zipcode','count')
  df_covid_ghi_zip_demelt = reshape2::dcast(df_covid_ghi_loc, zipcode ~ status, value.var = 'count')
  colnames(df_covid_ghi_zip_demelt) = c('zipcode','covid_19_neg_count','covid_19_pos_count')
  df_covid_ghi_zip = df_covid_ghi_zip_demelt %>% mutate(total_tested = covid_19_pos_count +
                                                          covid_19_neg_count)
  # cache zip boundaries that are download via tigris package
  options(tigris_use_cache = TRUE)
  
  zip_3_digit_str = unique(unlist(lapply(df_covid_ghi_zip$zipcode, function(x){str_sub(x,1,3)})))
  # get zip boundaries that start with first 3 digits
  char_zips <- zctas(cb = TRUE, starts_with = zip_3_digit_str)
  
  # join zip boundaries and covid data 
  df_covid_ghi_zip_map <- geo_join(char_zips, 
                                   df_covid_ghi_zip, 
                                   by_sp = "GEOID10", 
                                   by_df = "zipcode",
                                   how = "inner")
  
  file1 = sprintf("./out/df_covid_ghi_zip_map_%s.Rds",date_str)
  saveRDS(df_covid_ghi_zip_map, file=file1)

}
