## Only run this example in interactive R sessions
library(shiny)
library(readr)
library(magrittr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(data.table)
library(leaflet)
library(grid)
library(gridExtra)
library(tigris) 
library(stringr)
library(shinydashboard)
library(ggpubr)
library(ggplotify)
library(leafsync)
library(gg.gap)
library(png)

##############################
hospitalIcon <- makeIcon(
  iconUrl = "./hospital_icon.png",
  iconWidth = 20, iconHeight = 20,
  #iconAnchorX = 2, iconAnchorY = 94,
  #shadowUrl = "",
  #shadowWidth = 50, shadowHeight = 64,
  #shadowAnchorX = 4, shadowAnchorY = 62
)
###### NS hospitals locations
hospital_arr = NULL
hospital_arr$lng = c(-87.6848701,-87.7426636,-87.8570432,-87.8097423)  
hospital_arr$lat = c(42.0655769, 42.0566691,42.0923938,42.1908336)

hospital_arr$pop_ns_hos = c("NS Evanston Hospital", "NS Skokie Hospital",
                            "NS Glenbrook Hospital", "NS Highland Park Hospital")

hospital_arr = as.data.frame(hospital_arr)

##############
date_arr = c("04-16-20","04-21-20","04-23-20", "04-28-20","04-30-20",
             "05-05-20","05-07-20", "05-12-20", "05-14-20", "05-19-20","05-21-20")# "05-26-20","05-28-20")
             #"06-02-20", "06-04-20" )
#date_arr = c("06-02-20")
  
temp_time_list = vector()
L = length(date_arr)
fName = vector()
data_ov_list = list()
for(i in 1:L){
  fName[i] = sprintf('./out/df_covid_ghi_%s.Rds', date_arr[i])
  df = readRDS(fName[i])
  tmp1 = as.data.frame(table(df$result_txt))
  tmp2 = NULL
  tmp2$Var1[2] = as.character(tmp1$Var1[2])
  tmp2$Freq[2] = tmp1$Freq[2]
  tmp2$Var1[3] = as.character(tmp1$Var1[1])
  tmp2$Freq[3] = tmp1$Freq[1]
  tmp2$Var1[1] = "Total Tested"
  tmp2$Freq[1] = sum(tmp1$Freq)
  tmp2 = as.data.frame(tmp2)
  colnames(tmp2) = c("Status", "Count")
  data_ov_list[[i]] = tmp2
  temp_time_list[i] = sprintf("T%d",i)
}

df_ov_data = as.data.frame(data_ov_list)
id1 = which(str_detect(names(df_ov_data),'Count') == TRUE)
names(df_ov_data)[id1] = temp_time_list
#unlist(lapply(date_arr, function(x){str_replace_all(x,'-','_')}))
df_ov_data = df_ov_data[, c(1,id1)]

if(length(id1) != L){
  stop("Problem in dataset !!")
}

file1 = sprintf("./out/df_zip_count_time_pos_tbl_loc.Rds")
df_zip_count_time_pos_tbl_loc = readRDS(file=file1)

custom_data = NULL
custom_data$lng = -88.4 
custom_data$lat = 42.1
custom_data = as.data.frame(custom_data)

###################
## comparison with NS
#NS_TOT = 20576; NS_POS = 5460 ## May 1, 2020
#NS_TOT = 22251; NS_POS = 6036 ## May 5, 2020
#NS_TOT = 23362; NS_POS = 6389 ## May 7, 2020
#NS_TOT = 25249; NS_POS = 6821  ## May 11, 2020
#NS_TOT = 25759; NS_POS = 6962  ## May 12, 2020
#NS_TOT = 26764; NS_POS = 7193  ## May 14, 2020
#NS_TOT = 28409; NS_POS = 7553  ## May 19, 2020
NS_TOT = 29950; NS_POS = 7856  ## May 21, 2020

k=dim(df_ov_data)[2]
GHI_TOT = df_ov_data[1,k]; GHI_POS = df_ov_data[2, k]
x1 = matrix(c(NS_TOT, GHI_TOT, NS_POS, GHI_POS), nrow = 2,
            dimnames = list(c('NS', 'GHI'), c('Tot', 'Pos')))
ft = fisher.test(x1, alternative='two.sided', conf.int=TRUE, conf.level=0.95)

###################
header_covid <- dashboardHeader(title = "COVID-19 GHI Pilot Data",
                                titleWidth = "1200px")

# sidebar_covid <- dashboardSidebar(
#   sidebarMenu(sliderInput("ts", "Timepoint", min = 1, max = L, value = L, step=1),
#               sliderInput("dayId", "Animation", 1, length(unique(df_zip_count_time_pos_tbl_loc$day)), 
#                                           value = 1, step = 1, animate = TRUE)))

sidebar_covid1 <- dashboardSidebar(
  conditionalPanel("input.tabset1 == 'tab1_val'",
                       sliderInput("dayId", "Animation", 1, length(unique(df_zip_count_time_pos_tbl_loc$day)), 
                                   value = 1, step = 1, animate=animationOptions(interval = 550))),
  conditionalPanel("input.tabset1 != 'tab1_val'",
                       sliderInput("ts", "Data-Freeze Timepoints", min = 1, max = L, value = L, step=1)),
  #conditionalPanel("(input.tabset1 == 'tab8_val') | (input.tabset1 == 'tab9_val')",
  #                 sliderInput("ts", "Data-Freeze Timepoints", min = 1, max = L, value = L, step=1),
  #                 img(src="test.jpg")
  #                 )
  conditionalPanel("(input.tabset1 == 'tab1_val') | (input.tabset1 == 'tab5_val')",
                   radioButtons("rb",
                                label = "Masked Location",
                                choices = list("Yes", "No"),
                                selected = "Yes"))
                   
  )

body_covid <- dashboardBody(
  
  tags$head(tags$style(HTML('
      .main-header .logo {
        font-family: "Georgia";
        font-weight: bold;
        font-size: 24px;
      }
    '))),
  
  tags$head(tags$style(HTML('
    .nav-tabs-custom>.nav-tabs>li.active>a {
        font-weight: bold;
        font-size: 15px;
      }
    '))),
  
  fluidRow(
    tabBox(
      title = "",
      # The id lets us use input$tabset1 on the server to find the current tab
      id = "tabset1", height = "700px", width = 12,
      
      #tabPanel("Overview", fluidRow(plotOutput("overviewPlot", width="50%", height = "600px"))),
      #tabPanel("Spread-Map",h4(textOutput("day")), leafletOutput("map_u", width = "70%", height = "600px")),
      tabPanel("Overview", id="tab2",value='tab2_val', fluidRow(plotOutput("summaryPlot", height = "550px"))),
      #tabPanel("Orders", fluidRow(plotOutput("orderCountPlot",height = "600px"))),
      tabPanel("Gender/Age", id="tab3",value='tab3_val', fluidRow(plotOutput("demogPlot1", height = "600px"))),
      tabPanel("Ancestry", id="tab4",value='tab4_val',fluidRow(plotOutput("demogPlot2", height = "600px"))),
      tabPanel("Location", id="tab5",value='tab5_val',h3(textOutput('t1'), align = "center"), 
               fluidRow(splitLayout(cellWidths = c("48%", "48%"),
                                       leafletOutput("map_c", width="78%", height="580px"), 
                                       leafletOutput("map_z", width="78%", height="580px")))),
      tabPanel("Timeline", id="tab1",value='tab1_val',h2(textOutput('t2'), align = "center"), 
               fluidRow(splitLayout(cellWidths = c("50%", "50%"),
                        plotOutput("timeLinePlot", width="98%", height = "580px"),
                        #h4(textOutput("day")),
                        leafletOutput("map_u", width = "82%", height = "580px")))),
      tabPanel("Trait", id="tab6",value='tab6_val', fluidRow(plotOutput("traitPlot", height = "650px"))),
      tabPanel("Condition", id="tab7",value='tab7_val', fluidRow(plotOutput("condPlot", width='98%',height = "500px"))),
      tabPanel("Hypothesis-test Age", id="tab8",value='tab8_val', fluidRow(plotOutput("statPlot1",width='90%',
                                                                                  height = "600px"))),
      tabPanel("Hypothesis-test Race", id="tab9",value='tab9_val', fluidRow(plotOutput("statPlot2",width='90%',
                                                                                    height = "650px")))
    )
  )
)

covid_app = shinyApp(
  ui = dashboardPage(skin = "blue", header_covid, sidebar_covid1, body_covid),
  
  server <- function(input,output, session){
    
    output$t1 <- renderText({ paste0("County and Zipcode based information") })
    output$t2 <- renderText({ paste0("Spread of Covid-19 Positive Cases") })

    output$timeLinePlot <- renderPlot({
      
      df_ov_data_melted = reshape2::melt(df_ov_data, id.vars = "Status")
      
      p1 = ggplot(data=df_ov_data_melted, aes(x=variable, y=value, group=Status)) +
        geom_line(aes(color=Status), size=2)+
        geom_point(aes(color=Status))+
        xlab("Timepoints (data freeze)") + ylab("Count")+
        scale_color_manual("",values=c("#F8766D", "#00BFC4", "#7CAE00"), labels = c("Pos", "Neg", "Tot"))+
        #scale_y_continuous(breaks = seq(0, 950, 100))+
        geom_text(aes(label=value), color = 'black', hjust=0.3, vjust=-1.0, size=5)+
        theme_bw(base_size = 16)+theme(legend.position="bottom")+
        geom_label(aes(x = 'T7', y = 100, label = "Pos"), size = 5)+
        geom_label(aes(x = 'T7', y = 780, label = "Neg"), size = 5)+
        geom_label(aes(x = 'T7', y = 1240, label = "Total"), size = 5)
      
      p1_1 = gg.gap(plot=p1, segments=list(c(220, 580)), ylim=c(0, 1750))
      
      # img <- readPNG(source = "plot1.png")
      # g <- rasterGrob(img, interpolate=TRUE)
      # 
      # p2 = ggplot(data=NULL, geom="blank") +
      #   annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
      #   theme_void()
      # 
      # p3 = textGrob("", gp=gpar(cex=1.3), just = 'center')
      # 
      # grid.arrange(p1_1, p3, 
      #             nrow = 1, ncol = 2, 
      #             widths=c(1,1.5))
      p1_1
      
    })
    
    datasetInput <- reactive({
      file1 = sprintf("./out/df_covid_ghi_%s.Rds",date_arr[input$ts])
      df_covid_ghi_loc_sel = readRDS(file=file1)
      return(df_covid_ghi_loc_sel)
    })
    
    output$summaryPlot <- renderPlot({
      
      df_covid_ghi_loc_sel = datasetInput()
      data_tmp = as.data.frame(table(df_covid_ghi_loc_sel$result_txt))
      data = NULL
      data$Var1[2] = as.character(data_tmp$Var1[2])
      data$Freq[2] = data_tmp$Freq[2]
      data$Var1[3] = as.character(data_tmp$Var1[1])
      data$Freq[3] = data_tmp$Freq[1]
      data$Var1[1] = "Total Tested"
      data$Freq[1] = sum(data_tmp$Freq)
      
      data = as.data.frame(data)
      colnames(data) = c("Status", "Count")
      data$type = rep(0, length(data$Count))
      
      fname_1 = sprintf("./out/%s_exome_covid.txt",date_arr[input$ts])
      df_exome = fread(fname_1)
      count_WES = sum(df_exome$`Results:_1=Positive`)
      fname_2 = sprintf("./out/%s_array_covid.txt",date_arr[input$ts])
      df_array = fread(fname_2)
      count_ARR = sum(df_array$`Results:_1=Positive`)
      count_GEN = count_WES+count_ARR
      cap_str = sprintf("Note [1]: Currently entire NorthShore has %0.2f%% positives\nNote [2]: %d positives have Biosamples and %d of them\n   have genotype data (%d WES and %d Array)", 
                      NS_POS*100/NS_TOT, sum(df_covid_ghi_loc_sel$biosample==1 & df_covid_ghi_loc_sel$result==1),count_GEN, count_WES, count_ARR)
      
      p1 = ggplot(data=data, aes(x=Status, y=Count, fill=Status)) +
        geom_bar(color="black",stat="identity", width = 0.5) +
        geom_text(aes(label=Count), vjust=1.6, color="black",size=6)+
        theme_classic(base_size = 16)+
        scale_fill_manual(values=c("#F8766D", "#00BFC4", "#7CAE00"))+
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(), legend.position="bottom", legend.title=element_blank())+
        labs(caption  = cap_str)+
        theme(plot.caption = element_text(hjust=0, size = 15, face = 'bold.italic', color = 'blue'))
      
      ############
      data = as.data.frame(table(df_covid_ghi_loc_sel$status, df_covid_ghi_loc_sel$result_txt))
      
      p2 = ggplot(data=data, aes(x=Var1, y=Freq, fill=Var2)) +
        geom_bar(stat="identity", position=position_dodge())+
        geom_text(aes(label=Freq), vjust=1.6, color="black",
                  position = position_dodge(0.9), size=4.5) +
        theme_classic(base_size = 16) +
        xlab("Status") + ylab("Count") + scale_fill_discrete("", guide=FALSE)
      
      ########
      ### positive and active 
      ########
      
      df_pos_active = df_covid_ghi_loc_sel %>% filter(result == '1' & status == 'Active')
      ### how many active are hospitalized and not vented 
      ### how many active are hospitalized and vented 
      df_pos_active_hos = as.data.frame(table(df_pos_active$hospitalization))
      if(length(unique(df_pos_active_hos$Var1)) == 2){
        df_pos_active_hos$Var1 = c('Not hospitalized', 'hospitalized')
      }else{
        df_pos_active_hos$Var1 = c('Not hospitalized')
      }
      
      p3 = ggplot(data=df_pos_active_hos, aes(x=Var1, y=Freq)) +
        geom_bar(stat="identity", position=position_dodge(),  color="black", fill="#80cdc1")+
        geom_text(aes(label=Freq), vjust=1.6, color="black",
                  position = position_dodge(0.9), size=4.5) +
        theme_classic(base_size = 16) +
        xlab("Active-patient Status") + ylab("Count") + scale_fill_discrete("")
      
      df_hos = df_pos_active %>% filter(hospitalization == '1') 
      df_hos_vent = as.data.frame(table(df_hos$vented))
      if(dim(df_hos)[1]>0){
        if(length(unique(df_hos_vent$Var1)) == 2){
          df_hos_vent$Var1 = c('Not vented', 'vented')
        }else{
          df_hos_vent$Var1 = c('Not vented')
        }
      
        p4 = ggplot(data=df_hos_vent, aes(x=Var1, y=Freq)) +
          geom_bar(stat="identity", position=position_dodge(), color="black", fill="#018571")+
          geom_text(aes(label=Freq), vjust=1.6, color="white",
                    position = position_dodge(0.9), size=4.5) +
          theme_classic(base_size = 16) +
          xlab("Hospitalization Status") + ylab("Count") + scale_fill_discrete("")
        
      }else{
        p4 = textGrob("")
      }
      
      p5 = textGrob("Status of Active patients ========>", gp=gpar(cex=2), just = 'center')
      
      grid.arrange(p1, p2, p5, p3, p4, nrow = 2, ncol = 3,
                   layout_matrix = rbind(c(1,2,2), c(3,4,5)), 
                   widths = c(3.2, 1.7, 2), heights = c(5,2))
      
    })
    
    
    output$orderCountPlot <- renderPlot({
      df_covid_ghi_loc_sel = datasetInput()
      data = as.data.frame(table(df_covid_ghi_loc_sel$order_day, df_covid_ghi_loc_sel$result_txt))
      labels_str = unlist(lapply(df_covid_ghi_loc_sel$order_day, function(x){sprintf('day %d',x)}))
      
      ggplot(data, aes(x=Var1, y=Freq, fill = Var2)) +
        geom_col(color="black") +
        geom_text(aes(label = stat(y), group = Var1), 
                  stat = 'summary', fun = sum, vjust = -1)+
        theme_classic(base_size = 16) +
        ylab("Count") + scale_fill_discrete("")+
        scale_x_discrete(name="Day", breaks=df_covid_ghi_loc_sel$order_day, labels = labels_str)+
        theme(axis.text.x = element_text(angle = 90, hjust = 0.8, vjust = 0.2),
              axis.line = element_blank(),
              axis.ticks.x = element_blank()) 
      
    })
    
    output$demogPlot1 <- renderPlot({
      df_covid_ghi_loc_sel = datasetInput()
      data = as.data.frame(table(df_covid_ghi_loc_sel$gender, df_covid_ghi_loc_sel$result_txt))
      data$type = rep(1, length(data$Freq))
      
      p1 = ggplot(data=data, aes(x=Var1, y=Freq, fill=Var2)) +
        geom_bar(color="black",stat="identity", position=position_dodge())+
        geom_text(aes(label=Freq), vjust=1.6, color="black",
                  position = position_dodge(0.9), size=4.5) +
        theme_classic(base_size = 16) +
        xlab("Gender") + ylab("Count") + scale_fill_discrete("")
    
      data = as.data.frame(table(df_covid_ghi_loc_sel$AGE_BRACKET, df_covid_ghi_loc_sel$result_txt))
      data$type = rep(2, length(data$Freq))
      p2 = ggplot(data=data, aes(x=Var1, y=Freq, fill=Var2)) +
        geom_bar(color="black",stat="identity", position=position_dodge())+
        geom_text(aes(label=Freq), vjust=1.6, color="black",
                  position = position_dodge(0.9), size=5) +
        theme_classic(base_size = 14) +
        xlab("Age-group") + ylab("Count") + scale_fill_discrete("",guide = FALSE) +
        scale_x_discrete(labels=c("2" = "20-29","3" = "30-39", "4" = "40-49",
                                  "5" = "50-59","6" = "60-69", "7" = "70-79", "8" = "80+"))
      #theme(axis.text.x = element_text(angle = 90, hjust = 0.8, vjust = 0.2),
      #      axis.line = element_blank(),
      #      axis.ticks.x = element_blank()) 
  
      grid.arrange(p1, p2, nrow = 1, ncol = 2, widths = c(1,1))
    
    })
    
    output$demogPlot2 <- renderPlot({
      df_covid_ghi_loc_sel = datasetInput()
      data = as.data.frame(table(df_covid_ghi_loc_sel$race, df_covid_ghi_loc_sel$result_txt))
      data$type = rep(3, length(data$Freq))
      p3 = ggplot(data=data, aes(x=Var1, y=Freq, fill=Var2)) +
        geom_bar(color="black",stat="identity", position=position_dodge())+
        geom_text(aes(label=Freq), vjust=1.6, color="black",
                  position = position_dodge(0.9), size=5) +
        theme_classic(base_size = 14)+
        xlab("Race") + ylab("Count") + scale_fill_discrete("") + 
      theme(axis.text.x = element_text(angle = 90, hjust = 0.8, vjust = 0.2))
      
      data = as.data.frame(table(df_covid_ghi_loc_sel$ethnicity, df_covid_ghi_loc_sel$result_txt))
      data$type = rep(3, length(data$Freq))
      p4 = ggplot(data=data, aes(x=Var1, y=Freq, fill=Var2)) +
        geom_bar(color="black",stat="identity", position=position_dodge())+
        geom_text(aes(label=Freq), vjust=1.6, color="black",
                  position = position_dodge(0.9), size=5) +
        theme_classic(base_size = 14)+
        xlab("Ethnicity") + ylab("Count") + scale_fill_discrete("",guide = FALSE) 
      #theme(axis.text.x = element_text(angle = 90, hjust = 0.8, vjust = 0.2),
      #      axis.line = element_blank(),
      #      axis.ticks.x = element_blank()) 
      
      grid.arrange(p3, p4, nrow = 1, ncol = 2, widths = c(1.5,1))
    })
    
    output$map_c <- renderLeaflet({
      
      df_covid_ghi_loc_sel = datasetInput()
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
      leafmap = geo_join(county_map_filtered, data_filtered, by='state_county')
      
      leafmap$positive = unlist(lapply(leafmap$state_county,function(x){
        df_tab$Count[which(df_tab$state_county == x & 
                             df_tab$Status == "COVID-19 Positive")[1]]}))
      
      leafmap$negative = unlist(lapply(leafmap$state_county,function(x){
        df_tab$Count[which(df_tab$state_county == x & 
                             df_tab$Status == "COVID-19 Negative")[1]]}))
      
      if(input$rb == "No"){
        popup_county = paste0("County: ", "<b>", leafmap$state_county,"</b>","<br>",
                            "Total tested: ","<b>", leafmap$Count,"</b>","<br>",
                            "&nbsp;&nbsp;&nbsp;Positive: ","<b>", leafmap$positive,"</b>","<br>",
                            "&nbsp;&nbsp;&nbsp;Negative: ","<b>", leafmap$negative,"</b>","<br>") %>%
        lapply(htmltools::HTML)
      }else{
        popup_county = paste0("Total tested: ","<b>", leafmap$Count,"</b>","<br>",
                              "&nbsp;&nbsp;&nbsp;Positive: ","<b>", leafmap$positive,"</b>","<br>",
                              "&nbsp;&nbsp;&nbsp;Negative: ","<b>", leafmap$negative,"</b>","<br>") %>%
          lapply(htmltools::HTML)
      }
      
      variable_to_plot = leafmap$Count
      tStr = "Total tested by County"
      
      #bin_county = c(0, 100, 200, 300, 400, 500)
      bin_county = seq(0, max(variable_to_plot)+100, by=130)
      pal_county = colorBin("Oranges", domain = variable_to_plot, bins = bin_county)
      
      p1 = df_covid_ghi_county_count %>%
        leaflet() %>%
        #addTiles() %>%
        addProviderTiles("Stamen.Terrain") %>%
        addPolygons(data = leafmap,
                    fillColor = ~pal_county(variable_to_plot),
                    weight = 1,
                    opacity = 3,
                    color = "black",
                    dashArray = "3",
                    fillOpacity = 0.7,
                    label=~popup_county,
                    #popupOptions = popupOptions(closeOnClick = TRUE)
                    ) %>%
        addLegend(pal = pal_county, values = ~variable_to_plot, 
                  opacity = 0.5, title = tStr, 
                  position = "bottomleft") %>%
        setView(-88.2, 42.05, zoom = 8)
      
    })
    
    output$map_z <- renderLeaflet({
      
      # join zip boundaries and covid data 
      #df_covid_ghi_zip_map <- geo_join(char_zips, 
      #                                 df_covid_ghi_zip, 
      #                                 by_sp = "GEOID10", 
      #                                 by_df = "zipcode",
      #                                 how = "inner")
      file1 = sprintf("./out/df_covid_ghi_zip_map_%s.Rds",date_arr[input$ts])
      #saveRDS(df_covid_ghi_zip_map, file=file1)
      df_covid_ghi_zip_map = readRDS(file=file1)
      # create color palette 
      #pal_zip <- colorNumeric(
      #            palette = "Oranges",
      #            domain = df_covid_ghi_zip_map@data$covid_19_pos_count)
      
      #bin_zip = c(0, 5, 10, 15, 20, 25, 30, 35, 40)
      bin_zip = seq(0, max(df_covid_ghi_zip_map@data$covid_19_pos_count)+5, by=5)
      pal_zip = colorBin("Oranges", domain = df_covid_ghi_zip_map@data$covid_19_pos_count, 
                         bins = bin_zip)
      
      
      tStr = "#-of positives by Zip"
      # create labels for zipcodes
      if(input$rb == "No"){
        popup_zip <- paste0(
          "Zipcode: ","<b>",
          df_covid_ghi_zip_map@data$GEOID10,"</b>", "<br/>",
          "Total tested: ","<b>",
          df_covid_ghi_zip_map@data$total_tested,"</b>","<br/>",
          "&nbsp;&nbsp;&nbsp;Positives: ","<b>",
          df_covid_ghi_zip_map@data$covid_19_pos_count,"</b>","<br/>",
          "&nbsp;&nbsp;&nbsp;Negatives: ","<b>",
          df_covid_ghi_zip_map@data$covid_19_neg_count,"</b>") %>%
        lapply(htmltools::HTML)
      }else{
        popup_zip <- paste0(
          "Total tested: ","<b>",
          df_covid_ghi_zip_map@data$total_tested,"</b>","<br/>",
          "&nbsp;&nbsp;&nbsp;Positives: ","<b>",
          df_covid_ghi_zip_map@data$covid_19_pos_count,"</b>","<br/>",
          "&nbsp;&nbsp;&nbsp;Negatives: ","<b>",
          df_covid_ghi_zip_map@data$covid_19_neg_count,"</b>") %>%
          lapply(htmltools::HTML)
        }
      
      p2 = df_covid_ghi_zip_map %>% 
        leaflet() %>% 
        # add base map
        addProviderTiles("Stamen.Terrain") %>% 
        # add zip codes
        addPolygons(fillColor = ~pal_zip(covid_19_pos_count),
                    weight = 2,
                    opacity = 2,
                    color = "black",
                    dashArray = "2",
                    fillOpacity = 0.7,
                    #popup=~popup_zip,
                    #popupOptions = popupOptions(closeOnClick = TRUE),
                    label = popup_zip,
                    highlight = highlightOptions(weight = 2,
                                                 color = "#666",
                                                 dashArray = "",
                                                 fillOpacity = 0.7,
                                                 bringToFront = TRUE)) %>%
        # add legend
        addLegend(pal = pal_zip, values = ~covid_19_pos_count, 
                  opacity = 0.5, title = tStr, 
                  position = "bottomleft") %>%
        setView(-88.0, 42.17, zoom = 9)
        
    })
    
    output$traitPlot <- renderPlot({
      
      df_covid_ghi_loc_sel = datasetInput()
      if('smoking' %in% names(df_covid_ghi_loc_sel) == FALSE){
        p1 = textGrob("Not smoking data available", gp=gpar(cex=2), just = 'center')
        p2 = textGrob("Not diabetes data available", gp=gpar(cex=2), just = 'center')
      }else{
        data = as.data.frame(table(df_covid_ghi_loc_sel$smoking, df_covid_ghi_loc_sel$result_txt))
        colnames(data) = c("smoker","covid_status","count")
        data$smoker = unlist(lapply(data$smoker, function(x){ifelse(x == '0', 'non-smoker', 'smoker')}))
        p1 = ggplot(data=data, aes(x=smoker, y=count, fill=covid_status)) +
          geom_bar(stat="identity", position=position_dodge(),  color="black")+
          geom_text(aes(label=count), vjust=1.6, color="black",
                    position = position_dodge(0.9), size=4.5) +
          theme_classic(base_size = 16) +
          xlab("Smoking Status") + ylab("Count") + scale_fill_discrete("")
        
        data = as.data.frame(table(df_covid_ghi_loc_sel$diabetes, df_covid_ghi_loc_sel$result_txt))
        colnames(data) = c("diabetic","covid_status","count")
        data$diabetic = unlist(lapply(data$diabetic, function(x){ifelse(x == '0', 'non-diabetic', 'diabetic')}))
        p2 = ggplot(data=data, aes(x=diabetic, y=count, fill=covid_status)) +
          geom_bar(stat="identity", position=position_dodge(),  color="black")+
          geom_text(aes(label=count), vjust=1.6, color="black",
                    position = position_dodge(0.9), size=4.5) +
          theme_classic(base_size = 16) +
          xlab("Diabetes Status") + ylab("Count") + scale_fill_discrete("")
        
      }
      
      data = df_covid_ghi_loc_sel %>% filter(!is.na(latest_bmi)) 
      data$latest_bmi = as.numeric(data$latest_bmi)
      p3 = ggplot(data=data, aes(x=latest_bmi, color=result_txt)) +
        geom_histogram(aes(fill=result_txt),color="white",alpha=0.75, position="identity", bins = 30)+
        xlab("BMI (latest)") + scale_fill_discrete("") + theme_classic(base_size = 16)
      
      data = df_covid_ghi_loc_sel %>% filter(!is.na(latest_bp_d) & !is.na(latest_bp_s)) 
      data$latest_bp_d = as.numeric(data$latest_bp_d)
      data$latest_bp_s = as.numeric(data$latest_bp_s)
      p4 = ggplot(data=data, aes(x = latest_bp_s, y = latest_bp_d) ) + 
        geom_point(aes(color=result_txt)) +
        stat_density_2d(aes(fill = result_txt, alpha = ..level..), geom = "polygon", show.legend = F) +
        xlab("BP systolic") + ylab("BP diastolic")+ 
        theme_classic(base_size = 16) + scale_color_discrete("")
      
      data = df_covid_ghi_loc_sel %>% filter(!is.na(latest_wt)) 
      data$latest_wt = as.numeric(data$latest_wt)*0.0625
      p5 = ggplot(data=data, aes(x=latest_wt, color=result_txt)) +
        geom_histogram(aes(fill=result_txt),color="white",alpha=0.75, position="identity", bins = 30)+
        xlab("Weight (latest) [in lb]") + scale_fill_discrete("") + theme_classic(base_size = 16)
      
      
      #grid.arrange(p1, p2, p3, p4, ncol = 2)
      grid.arrange(p1, p5, p3, p4, ncol = 2)
      
    })
    
    output$condPlot <- renderPlot({
      
      cond_count = NULL
      cond_count$condition = c("Disorder of cardiovascular system",
                               "Heart disease",
                               "Diabetes mellitus",
                               "Chronic disease of respiratory system",
                               "Hypertensive disorder",
                               "Cancer")  ### Malignant neoplastic disease
      
      cond_count$condition = factor(cond_count$condition, levels = cond_count$condition)
      file1 = sprintf("./out/df_pos_cond_count_with_name_%s.tsv",date_arr[input$ts])
      df_pos_cond_count_with_name = fread(file = file1)
      
      count_vec = unlist(lapply(c("Disorder of cardiovascular system","Heart disease","Diabetes mellitus",
                                  "Chronic disease of respiratory system","Hypertensive disorder",
                                  "Malignant neoplastic disease"), function(x){
                                    df_pos_cond_count_with_name$freq[which(
                                      df_pos_cond_count_with_name$concept_name==x)]}))
      
      
      cond_count$count = count_vec
      #cond_count$count = c(37, 18, 10, 20, 26, 10)
      
      data = as.data.frame(cond_count)
      
      data = data[order(-data$count),]
      
      str1 = sprintf("Count (Out of %d Positives)", max(df_pos_cond_count_with_name$freq) )
      
      cap_str = sprintf("[*] Ref: Epidemiological characteristics of new coronavirus pneumonia. 
                        Chinese Journal of Epidemiology, 2020,41 (2020-02-17).")
      
      p1 = ggplot(data=data, aes(x=condition, y=count)) +
        geom_bar(color="black",stat="identity", fill="#bd0026")+
        geom_text(aes(label=count), vjust=0.5, hjust = 1.2, color="white", size=6) +
        theme_classic(base_size = 16)+
        xlab("") + ylab(str1) + scale_x_discrete(limits = as.character(data$condition[order(data$count)]))+
        coord_flip()+
        ggtitle("Predefined common conditions* in COVID-19 Positives")+
        theme(plot.title = element_text(hjust= 1.0, size = 17), 
              plot.caption = element_text(hjust = 1, colour = 'blue'))+
        labs(caption = cap_str)
      #theme(axis.text.x = element_text(angle = 90, hjust = 0.8, vjust = 0.2),
      #      axis.line = element_blank(),
      #      axis.ticks.x = element_blank()) 
      
      file2 = sprintf("./out/cond_plot_obj_%s.Rds",date_arr[input$ts])
      cond_plot_obj = readRDS(file2)
      p2 = cond_plot_obj[[2]] ## OR
      p3 = cond_plot_obj[[1]] ## manhattan
      
      
      #grid.arrange(p1, p2, p3, ncol = 2, nrow = 2, 
      #             layout_matrix = rbind(c(1,2), c(3,3)),
      #             heights=c(3,2))
      grid.arrange(p1, p2, ncol = 2, nrow = 1, widths=c(0.87,1))
      
    })
    
    output$statPlot1 <- renderPlot({
      file1 = sprintf("./out/stat_plot_obj_%s.Rds",date_arr[input$ts])
      stat_plot_obj = readRDS(file1)
    
      #text_str.p1 <- ggparagraph(text_str, size = 13.5)
      
      #p2 <- textGrob("")
      
      #img <- readPNG(source = "plot2.png")
      #g <- rasterGrob(img, interpolate=TRUE)
      
      #p4 = ggplot(data=NULL, geom="blank") +
      #  annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
      #  theme_void()
      
      
      #grid.arrange(stat_plot_obj[[1]], stat_plot_obj[[2]], p4, p2, 
      #             nrow = 2,ncol = 2, 
      #             layout_matrix = rbind(c(1,3), c(2,4)), widths=c(2,0.9), heights=c(2,3))
      #grid.arrange(stat_plot_obj[[1]], stat_plot_obj[[2]], 
      #             nrow = 1,ncol = 2, widths=c(1,1.1))
      stat_plot_obj[[1]]
    })
    
    output$statPlot2 <- renderPlot({
      file1 = sprintf("./out/stat_plot_obj_%s.Rds",date_arr[input$ts])
      stat_plot_obj = readRDS(file1)
      stat_plot_obj[[2]]
      
    })
    
    
    output$day <- renderText({
      
      file1 = sprintf("./out/df_zip_count_time_pos_tbl_loc.Rds")
      df_zip_count_time_pos_tbl_loc = readRDS(file=file1)
      
      day_list = as.integer(as.character(unique(df_zip_count_time_pos_tbl_loc$day)))
      sprintf("Day %d",day_list[input$dayId]) 
    })
    
    output$map_u <- renderLeaflet({
      
      file1 = sprintf("./out/df_covid_ghi_zip_map_%s.Rds", date_arr[input$ts])
      df_covid_ghi_zip_map_0 = readRDS(file = file1)
      df_covid_ghi_zip_map = df_covid_ghi_zip_map_0 %>% subset(covid_19_pos_count>0)
      
      day_list = as.integer(as.character(unique(df_zip_count_time_pos_tbl_loc$day)))

      data1 <- df_zip_count_time_pos_tbl_loc %>%
        filter(day <= day_list[1] & count > 0) %>%  #can not be input$dayId as that would cause flickering
        group_by(zip_code, lat, lng) %>%
        summarize(tot_count = sum(count))

      if(input$rb == "No"){
        popup_zip <- paste0("Zipcode: ","<b>",data1$zip_code,"</b>", "<br/>",
                          "&emsp;","Positives: ","<b>", data1$tot_count,"</b>","<br/>") %>%
                    lapply(htmltools::HTML)
      }else{
        popup_zip <- paste0("Positives: ","<b>", data1$tot_count,"</b>","<br/>") %>%
          lapply(htmltools::HTML)
      }
      
      tStr = paste0("Day ", day_list[1]) %>% lapply(htmltools::HTML)
      
      basemap = df_covid_ghi_zip_map %>%
        leaflet() %>%
        addProviderTiles("Stamen.Terrain") %>%
        addPolygons(fillColor = NA,
                    fill = NA,
                    weight = 1,
                    opacity = 2,
                    color = "#164781",
                    dashArray = "2",
                    fillOpacity = 0.7) %>%
        setView(-87.9, 42.02, zoom = 9) %>%
        addCircleMarkers(lng=data1$lng, lat=data1$lat, fillColor = "red", fillOpacity = 0.3,
                         color = "red", radius = 2*data1$tot_count, layerId = data1$zip_code,
                         label = popup_zip) %>%
        addLabelOnlyMarkers(lng=custom_data$lng, lat=custom_data$lat, label=tStr, 
                            labelOptions = labelOptions(noHide = T, textsize='25px')) %>%
        addMarkers(data = hospital_arr, lng = ~lng, lat = ~lat, popup = ~pop_ns_hos, 
                   icon = hospitalIcon)
      
      
    })
    
    observeEvent(input$dayId, {
     
       day_list = as.integer(as.character(unique(df_zip_count_time_pos_tbl_loc$day)))
       
       data1 <- df_zip_count_time_pos_tbl_loc %>%
         filter(day <= day_list[input$dayId] & count > 0) %>%
         group_by(zip_code, lat, lng) %>%
         summarize(tot_count = sum(count))
     
       if(input$rb == "No"){
         popup_zip <- paste0("Zipcode: ","<b>",data1$zip_code,"</b>", "<br/>",
                             "&emsp;","Positives: ","<b>", data1$tot_count,"</b>","<br/>") %>%
           lapply(htmltools::HTML)
       }else{
        popup_zip <- paste0("Positives: ","<b>", data1$tot_count,"</b>","<br/>") %>%
         lapply(htmltools::HTML)
       }
       
       
       tStr = paste0("Day ", day_list[input$dayId]) %>% lapply(htmltools::HTML)
       
       leafletProxy("map_u", session) %>%
         clearMarkers() %>%
         addCircleMarkers(lng=data1$lng, lat=data1$lat, fillColor = "red", fillOpacity = 0.3,
                          color = "red", radius = 2*data1$tot_count, layerId = data1$zip_code,
                          label = popup_zip) %>%
         addLabelOnlyMarkers(lng=custom_data$lng, lat=custom_data$lat, label=tStr, 
                             labelOptions = labelOptions(noHide = T, textsize='24px')) %>%
         addMarkers(data = hospital_arr, lng = ~lng, lat = ~lat, popup = ~pop_ns_hos, 
                    icon = hospitalIcon)
     })
     
  } ### server
  
) ##shinyApp


###################  

#runApp(covid_app)
runApp(covid_app, host="127.0.0.1", port = 29579, launch.browser = FALSE)


