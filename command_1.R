#!/usr/bin/env Rscript

library(data.table)

get_sequenced = function (path, outname, date_str) {
  b = fread(path)
  # write
  if (nrow(a[ Research_ID %in% b$V1 ]) > 0) {
    fwrite(a[ Research_ID %in% b$V1 ], sep="\t", quote=F,
           file=paste0("./out/",date_str, "_", outname, "_covid.txt"))
  }
}

exome = './Ambry_RID_1003.txt'
array = './Array_RID_5911.txt'

#freeze_arr = c('04-16-20','04-21-20','04-23-20', '04-28-20', '04-30-20', 
#               '05-05-20', '05-07-20', '05-12-20','05-14-20','05-19-20','05-21-20')

freeze_arr = c('05-26-20','05-28-20', '06-02-20', '06-04-20' )

for(i in 1:length(freeze_arr)){
  
  # paths to files
  date_str = freeze_arr[i]
  query = sprintf('./HIT_data/covid-%s.csv', date_str)
  
  #setwd('/data/software/covid_temp_dir')
  #today = format(Sys.Date(), '%Y%m%d')

  # issue with csv (\r endings)
  # system("sed \'s/\\r/\\n/g\' /data/sengupts/ICEGO/COVID_19/covid-rid.csv > covid-rid.csv")

  ##FUNCTIONS

  # function for counts
  

  ##MAIN
  # read in
  a = fread(query)
  setnames(a, names(a), gsub(" ", "_", names(a)))
  # a[, GHI_RID := gsub(",", "", GHI_RID) ]
  get_sequenced(array, "array", date_str)
  get_sequenced(exome, "exome", date_str)

}
