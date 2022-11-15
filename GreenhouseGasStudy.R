#install.packages('fontawesome')
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(readxl)
library(readr)
library(dplyr)
library(ggplot2)
library(scales)
library(RColorBrewer)
library(ggiraph)
library(maps)
library(plyr)
library(dplyr)
library(Rmisc)
library(BSDA)
library(PASWR)
library(ggpubr)
library(MASS)
library(hash)
library(tidyverse)
library(broom)
library(corrplot) 

data=read_xlsx("last20yearswithname.xlsx")
world_data <- read_excel("world_data.xlsx")
world_data <- fortify(world_data)

AnnexI_2019 <- read_excel("AnnexI_2019.xlsx")
AnnexI_2010 <- read_excel("AnnexI_2010.xlsx")
AnnexI_2000 <- read_excel("AnnexI_2000.xlsx")
NonAnnexI_2019 <- read_excel("NonAnnexI_2019.xlsx")
NonAnnexI_2010 <- read_excel("NonAnnexI_2010.xlsx")
NonAnnexI_2000 <- read_excel("NonAnnexI_2000.xlsx")
Combined_2019 <- read_excel("Combined_2019.xlsx")
Combined_2010 <- read_excel("Combined_2010.xlsx")
Combined_2000 <- read_excel("Combined_2000.xlsx")
ANOVA_2019 <- read_excel("ANOVA_2019.xlsx")
ANOVA_2010 <- read_excel("ANOVA_2010.xlsx")
ANOVA_2000 <- read_excel("ANOVA_2000.xlsx")

df <- read_excel("MLR.xlsx")
top_emitters_1 <- c("China","United States")
top_emitters_2 <- c("Japan","Brazil","India","Indonesia",
                  "Russian Federation","Germany","Canada","Iran, Islamic Rep.")
df1 <- df %>%
  filter(Country_Name %in% top_emitters_1)
df2 <- df %>%
  filter(Country_Name %in% top_emitters_2)

MultipleLinearReg1 <- function() {
  df_cor <- df1 %>%
    dplyr::select(Renewable_Energy,Population,Export,Import,GDP_Per_Capita)
  dfcor = cor(df_cor)
  corrplot(dfcor,addCoef.col = 2,tl.cex = 1,cl.cex = 1,number.cex = 1, title = "Correlation Matrix Plot",mar=c(0,0,2,0))
  y <- df1$Total_Greenhouse_Emissions
  Population <- df1$Population
  Export <- df1$Export
  GDP_Per_Capita <- df1$GDP_Per_Capita
  IncomeGroup <- df1$IncomeGroup
  Region <- df1$Region
  fit1 <- lm(y~Population+GDP_Per_Capita+Region, data = df1)
  step <- stepAIC(fit1, direction ="both")
  summary(step)
  par(mfrow = c(2, 2))
  #plot(fit1)
}

MultipleLinearReg2 <- function() {  
  df_cor <- df2 %>%
    dplyr::select(Renewable_Energy,Population,Export,Import,GDP_Per_Capita)
  dfcor = cor(df_cor)
  corrplot(dfcor,addCoef.col = 2,tl.cex = 0.6,cl.cex = 0.6,number.cex = 1,title= "Correlation Matrix Plot",mar=c(0,0,2,0))

  y <- df2$Total_Greenhouse_Emissions
  Population <- df2$Population
  Renewable_Energy <- df2$Renewable_Energy
  Export <- df2$Export
  GDP_Per_Capita <- df2$GDP_Per_Capita
  IncomeGroup <- df2$IncomeGroup
  Region <- df2$Region
  fit2 <- lm(y~Population+Renewable_Energy+Export+GDP_Per_Capita+IncomeGroup+Region, data = df2)
  step <- stepAIC(fit2, direction ="both")
  par(mfrow = c(2, 2))
  #plot(fit2)
  
  summary(step)
}

h <- hash()
h[["CO2_emissions"]] <- "Metric Tonne Per Capita"
h[["Methane_emissions"]] <- "Metric Tonne Per Capita"
h[["Nitrous_Oxide"]] <- "Metric Tonne Per Capita"
h[["Renewable_Energy(%)"]] <- "% of Total Energy Consumption"
h[["GDP_Per_Capita"]] <- "US$"
h[["Military_Expenditures(%)"]] <- "% of Total Government Expenditures"
h[["Export(%)"]] <- "% of GDP"
h[["Import(%)"]] <- "% of GDP"
h[["Total_CO2_emissions"]] <- "Kilotonnes"
h[["Total_Methane_emissions"]] <- "Kilotonnes"
h[["Total_Nitrous_Oxide_emissions"]] <- "Kilotonnes"
h[["Total_Greenhouse_Emissions"]] <- "Kilotonnes"

worldMaps <- function(last20years, world_data, data_type, year){
  
  # Function for setting the aesthetics of the plot
  my_theme <- function () { 
    theme_bw() + theme(axis.text = element_text(size = 12),
                       axis.title = element_text(size = 12),
                       strip.text = element_text(size = 12),
                       panel.grid.major = element_blank(), 
                       panel.grid.minor = element_blank(),
                       panel.background = element_blank(), 
                       legend.position = "bottom",
                       legend.text = element_text(hjust=1,size = 7, angle = 60),
                       panel.border = element_blank(), 
                       strip.background = element_rect(fill = 'white', colour = 'white'))
  }
  
  plotdf <- last20years[last20years$Year == year,][c("Country_Name","Country_Code","Year",data_type)]
  
  map_title=''
  if(data_type== "Total_CO2_emissions")
    map_title="Total CO2 Emissions" 
  else if("Total_Methane_emissions"==data_type)
    map_title="Total Methane Emissions"
  else if("Total_Nitrous_Oxide_emissions"==data_type)
    map_title="Total Nitrous Oxide Emissions"
  else if("Total_Greenhouse_Emissions"==data_type)
    map_title="Total Greenhouse Emissions"
  else if("Renewable_Energy(%)"==data_type)
    map_title="Renewable Energy(%)"
  else if("Military_Expenditures(%)"==data_type)
    map_title="Military Expenditures(%)"
  else if("CO2_emissions"==data_type)
    map_title="CO2 Emissions Per Capita"
  else if("Methane_emissions"==data_type)
    map_title="Methane Emissions Per Capita"
  else if("Nitrous_Oxide"==data_type)
    map_title="Nitrous Oxide Emissions Per Capita"
  
  world_data['Year'] <- rep(year, nrow(world_data))
  
  world_data['Value'] <- plotdf[[data_type]][match(world_data$Country_Code,plotdf$Country_Code)]
  capt <- paste0("Source: ", ifelse(data_type == "Data", "United Nations" , "World Bank"))
  
  g <- ggplot() + 
    geom_polygon_interactive(data = world_data, color = 'gray70', size = 0.2,
                             aes(x = long, y = lat, fill = Value, group = group, 
                                 tooltip = sprintf("%s<br/>%s", Country_Code, Value))) + 
    scale_fill_gradientn(colours = brewer.pal(6, "Reds"), na.value = 'white') + 
    scale_y_continuous(limits = c(-60, 90), breaks = c()) + 
    scale_x_continuous(breaks = c()) + 
    labs(fill = h[[data_type]],color = data_type, title = map_title, x = NULL, y = NULL, caption = capt) + 
    my_theme()
  
  return(g)
  
  
}



GHGEmissionPlot <- function(year){
  names(data)[names(data) == 'Country_Name']<-'Percentage_of_Total_GHG_Emissions'
  
  green=data[which(data$Year==year),c('Percentage_of_Total_GHG_Emissions','Total_Greenhouse_Emissions')]
  green$Total_Greenhouse_Emissions[is.na(green$Total_Greenhouse_Emissions)]=mean(green$Total_Greenhouse_Emissions,na.rm=T)
  green['percent']=green['Total_Greenhouse_Emissions']/sum(green['Total_Greenhouse_Emissions'])
  
  green=green[order(green$percent,decreasing = T),]
  green['order']=c(1:nrow(green))
  
  top_10=green[which(green$order<=10),]
  last_100=green[which(green$order>=nrow(green)-99),]
  mid=green[which(green$order>10 & green$order<nrow(green)-99),]
  
  mid_sum=data.frame(Percentage_of_Total_GHG_Emissions='Else',percent=sum(mid$percent))
  last_sum=data.frame(Percentage_of_Total_GHG_Emissions='Bottom 100 emitters',percent=sum(last_100$percent))
  top_sum=data.frame(Percentage_of_Total_GHG_Emissions='Top 10 emitters',percent=sum(top_10$percent))
  
  sd=rbind(top_sum,mid_sum,last_sum)
  sd$Percentage_of_Total_GHG_Emissions=paste(sd$Percentage_of_Total_GHG_Emissions,as.character(round(sd$percent*100,2)),'%')
  sd$ymax=cumsum(sd$percent)
  sd$ymin=c(0,head(sd$ymax,n=-1))
  p=ggplot(sd,aes(ymax=ymax,ymin=ymin,
                  xmax=4,xmin=3))+
    geom_rect(aes(fill=Percentage_of_Total_GHG_Emissions))+
    theme(plot.title = element_text(hjust = 0.5))+
    theme_bw()+
    xlim(2,4)+
    coord_polar(theta = 'y')+
    theme_void()+
    labs(title = 'Greenhouse Gas Emission Distribution', subtitle = paste('in', year)) +
    theme(plot.title = element_text(color = "Black", size = 18,hjust = 0.5,family='STXihei'), plot.subtitle = element_text(color = "DarkGrey", size = 14,hjust = 0.5))
  print(p)
}

Annex1v2<- function(choice) {
  
  meanEm=read_xlsx("Means.xlsx",sheet='MeanGHGEmission')
  meanRE=read_xlsx("Means.xlsx",sheet='MeanRenewableEnergy')
  meanImp=read_xlsx("Means.xlsx",sheet='MeanImport')
  meanExp=read_xlsx("Means.xlsx", sheet="MeanExport")
  meanME=read_xlsx("Means.xlsx",sheet='MeanMilitaryExpenditure')
  meanGDP=read_xlsx("Means.xlsx",sheet='MeanGDP')
  meanElec=read_xlsx("Means.xlsx",sheet='MeanElectricityConsumption')
  
  coeff<- 10000
  
  annexI_label <- "#2452B8"
  annexII_label <- "#38761d"
  annexI_ghg_label <- "#9fc5e8"
  annexII_ghg_label <- "#6aa84f"
  
  annexI_ghg<- meanEm %>%
    filter(Annex_I=="Yes")
  annexI_ghg
  
  annexII_ghg<- meanEm %>%
    filter(Annex_I=="No")
  annexII_ghg
  
  if(choice=='Average Emissions')
  {
    
    p<- ggplot(data = meanEm,aes(x=Year,y=Mean_Total_Emission,group = Annex_I,color=Annex_I))+
      labs(title = "Average Total GHG Emissions (Annex I vs Non-Annex I)")+geom_line()+ geom_point()+
      xlab('Year')+ylab('Average Total GHG Emission')+theme_bw()+
      theme(panel.grid.major=element_line(colour=NA),
            plot.title = element_text(size=18, family='STXihei',hjust = 0.5),
            text = element_text(family = "STXihei"),
            legend.box.background = element_rect(color="black")) + scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-06))
    
  } 
  else if (choice=='Renewable Energy')
  {
    annexI_RE<- meanRE %>%
      filter(Annex_I=="Yes")
    
    annexII_RE<- meanRE %>%
      filter(Annex_I=="No")
    
    p<- ggplot(NULL, aes(x=Year))+
      geom_line(data=annexI_RE, aes(y=Mean_Renewable_Energy,  color = annexI_label))+geom_point(data=annexI_RE, aes(y=Mean_Renewable_Energy,  color = annexI_label))+
      geom_line(data=annexII_RE, aes(y=Mean_Renewable_Energy,  color = annexII_label))+geom_point(data=annexII_RE, aes(y=Mean_Renewable_Energy,  color = annexII_label))+
      geom_line(data = annexI_ghg, aes(x=Year,y=Mean_Total_Emission/coeff,color=annexI_ghg_label))+geom_point(data = annexI_ghg, aes(x=Year,y=Mean_Total_Emission/coeff,color=annexI_ghg_label))+
      geom_line(data = annexII_ghg, aes(x=Year,y=Mean_Total_Emission/coeff,color=annexII_ghg_label))+geom_point(data = annexII_ghg, aes(x=Year,y=Mean_Total_Emission/coeff,color=annexII_ghg_label))+
      theme_bw()+labs(title = "Average Renewables (as % of total energy consumption) vs Average Total GHG Emissions")+theme(plot.title = element_text(size=18, hjust = 0.5))+
      scale_color_manual("Legend",labels=c('Renewable Energy of Annex I', 'Renewable Energy of Non Annex I','GHG Emissions of Non-Annex I','GHG Emissions of Annex I'), values=c(annexI_ghg_label,annexII_ghg_label, annexI_label, annexII_label))+
      scale_y_continuous(name = "Average Renewable Energy(% of total energy consumption)", sec.axis = sec_axis(~.*coeff, name = "Average Total GHG Emission", labels = unit_format(unit = "M", scale = 1e-06)))
    
  }
  else if (choice=='Import')
  {
    annexI_import<- meanImp %>%
      filter(Annex_I=="Yes")
    
    annexII_import<- meanImp %>%
      filter(Annex_I=="No")
    
    p<- ggplot(NULL, aes(x=Year))+
      geom_line(data=annexI_import, aes(y=Mean_Import,  color = annexI_label))+geom_point(data=annexI_import, aes(y=Mean_Import,  color = annexI_label))+
      geom_line(data=annexII_import, aes(y=Mean_Import,  color = annexII_label))+geom_point(data=annexII_import, aes(y=Mean_Import,  color = annexII_label))+
      geom_line(data = annexI_ghg, aes(x=Year,y=Mean_Total_Emission/coeff,color=annexI_ghg_label))+geom_point(data = annexI_ghg, aes(x=Year,y=Mean_Total_Emission/coeff,color=annexI_ghg_label))+
      geom_line(data = annexII_ghg, aes(x=Year,y=Mean_Total_Emission/coeff,color=annexII_ghg_label))+geom_point(data = annexII_ghg, aes(x=Year,y=Mean_Total_Emission/coeff,color=annexII_ghg_label))+
      theme_bw()+labs(title = "Average Imports (as % of GDP) vs Average Total GHG Emissions")+theme(plot.title = element_text(size=18, hjust = 0.5))+
      scale_color_manual("Legend",labels=c('Imports of Annex I', 'Imports of Non Annex I', 'GHG Emissions of Non-Annex I','GHG Emissions of Annex I'), values=c(annexI_ghg_label,annexII_ghg_label, annexI_label, annexII_label))+
      scale_y_continuous(name = "Imports (as % of GDP)", sec.axis = sec_axis(~.*coeff, name = "Average Total GHG Emission", labels = unit_format(unit = "M", scale = 1e-06)))
    
  }
  else if (choice=='Export')
  {
    annexI_export<- meanExp %>%
      filter(Annex_I=="Yes")
    
    annexII_export<- meanExp %>%
      filter(Annex_I=="No")
    
    p<- ggplot(NULL, aes(x=Year))+
      geom_line(data=annexI_export, aes(y=Mean_Export,  color = annexI_label))+geom_point(data=annexI_export, aes(y=Mean_Export,  color = annexI_label))+
      geom_line(data=annexII_export, aes(y=Mean_Export,  color = annexII_label))+geom_point(data=annexII_export, aes(y=Mean_Export,  color = annexII_label))+
      geom_line(data = annexI_ghg, aes(x=Year,y=Mean_Total_Emission/coeff,color=annexI_ghg_label))+geom_point(data = annexI_ghg, aes(x=Year,y=Mean_Total_Emission/coeff,color=annexI_ghg_label))+
      geom_line(data = annexII_ghg, aes(x=Year,y=Mean_Total_Emission/coeff,color=annexII_ghg_label))+geom_point(data = annexII_ghg, aes(x=Year,y=Mean_Total_Emission/coeff,color=annexII_ghg_label))+
      theme_bw()+labs(title = "Average Exports (as % of GDP) vs Average Total GHG Emissions")+theme(plot.title = element_text(size=18, hjust = 0.5))+
      scale_color_manual("Legend",labels=c('Exports of Annex I', 'Exports of Non Annex I', 'GHG Emissions of Non-Annex I','GHG Emissions of Annex I'), values=c(annexI_ghg_label,annexII_ghg_label, annexI_label, annexII_label))+
      scale_y_continuous(name = "Exports (% of GDP)", sec.axis = sec_axis(~.*coeff, name = "Average Total GHG Emission", labels = unit_format(unit = "M", scale = 1e-06)))
    
  }
  else if (choice=='Military Expenditure')
  {
    coeff2<- 100000
    annexI_ME<- meanME %>%
      filter(Annex_I=="Yes")
    
    annexII_ME<- meanME %>%
      filter(Annex_I=="No")
    
    p<- ggplot(NULL, aes(x=Year))+
      geom_line(data=annexI_ME, aes(y=Mean_Military_Expenditure,  color = annexI_label))+geom_point(data=annexI_ME, aes(y=Mean_Military_Expenditure,  color = annexI_label))+
      geom_line(data=annexII_ME, aes(y=Mean_Military_Expenditure,  color = annexII_label))+geom_point(data=annexII_ME, aes(y=Mean_Military_Expenditure,  color = annexII_label))+
      geom_line(data = annexI_ghg, aes(x=Year,y=Mean_Total_Emission/coeff2,color=annexI_ghg_label))+geom_point(data = annexI_ghg, aes(x=Year,y=Mean_Total_Emission/coeff2,color=annexI_ghg_label))+
      geom_line(data = annexII_ghg, aes(x=Year,y=Mean_Total_Emission/coeff2,color=annexII_ghg_label))+geom_point(data = annexII_ghg, aes(x=Year,y=Mean_Total_Emission/coeff2,color=annexII_ghg_label))+
      theme_bw()+labs(title = "Average Military Expenditure (as % government expenditure) vs Average Total GHG Emissions")+theme(plot.title = element_text(size=18, hjust = 0.5))+
      scale_color_manual("Legend",labels=c('Military Expenditure of Annex I', 'Military Expenditure of Non Annex I', 'GHG Emissions of Non-Annex I','GHG Emissions of Annex I'), values=c(annexI_ghg_label,annexII_ghg_label, annexI_label, annexII_label))+
      scale_y_continuous(name = "Average Military Expenditure (% of government expenditure)", sec.axis = sec_axis(~.*coeff2, name = "Average Total GHG Emission", labels = unit_format(unit = "M", scale = 1e-07)))
    
  }
  else if (choice=='GDP Per Capita')
  {
    coeff3 <- 10
    annexI_GDP<- meanGDP %>%
      filter(Annex_I=="Yes")
    
    annexII_GDP<- meanGDP %>%
      filter(Annex_I=="No")
    
    p<- ggplot(NULL, aes(x=Year))+
      geom_line(data=annexI_GDP, aes(y=Mean_GDP_Per_Capita,  color = annexI_label))+geom_point(data=annexI_GDP, aes(y=Mean_GDP_Per_Capita,  color = annexI_label))+
      geom_line(data=annexII_GDP, aes(y=Mean_GDP_Per_Capita,  color = annexII_label))+geom_point(data=annexII_GDP, aes(y=Mean_GDP_Per_Capita,  color = annexII_label))+
      geom_line(data = annexI_ghg, aes(x=Year,y=Mean_Total_Emission/coeff3,color=annexI_ghg_label))+geom_point(data = annexI_ghg, aes(x=Year,y=Mean_Total_Emission/coeff3,color=annexI_ghg_label))+
      geom_line(data = annexII_ghg, aes(x=Year,y=Mean_Total_Emission/coeff3,color=annexII_ghg_label))+geom_point(data = annexII_ghg, aes(x=Year,y=Mean_Total_Emission/coeff3,color=annexII_ghg_label))+
      theme_bw()+labs(title = "Average GDP per capita vs Average Total GHG Emissions")+theme(plot.title = element_text(size=18, hjust = 0.5))+
      scale_color_manual("Legend",labels=c('GDP Per Capita of Annex I', 'GDP Per Capita of Non Annex I','GHG Emissions of Non-Annex I', 'GHG Emissions of Annex I'), values=c(annexI_ghg_label,annexII_ghg_label, annexI_label, annexII_label))+
      scale_y_continuous(name = "Average GDP Per Capita", sec.axis = sec_axis(~.*coeff3, name = "Average Total GHG Emission", labels = unit_format(unit = "", scale = 1e-01)))
  }
  else if (choice=='Electricity Consumption')
  {
    coeff4<- 10
    annexI_Elec<- meanElec %>%
      filter(Annex_I=="Yes"& Year < 2015)
    
    annexII_Elec<- meanElec %>%
      filter(Annex_I=="No"& Year < 2015)
    annexII_Elec
    
    annexI_ghg_elec <-  meanEm %>%
      filter(Annex_I=="Yes" & Year < 2015)
    annexI_ghg_elec
    
    annexII_ghg_elec <-  meanEm %>%
      filter(Annex_I=="No" & Year < 2015)
    annexII_ghg_elec
    
    p<- ggplot(NULL, aes(x=Year))+
      geom_line(data=annexI_Elec, aes(y=Mean_KWH_Per_Capita,  color = annexI_label))+geom_point(data=annexI_Elec, aes(y=Mean_KWH_Per_Capita,  color = annexI_label))+
      geom_line(data=annexII_Elec, aes(y=Mean_KWH_Per_Capita,  color = annexII_label))+geom_point(data=annexII_Elec, aes(y=Mean_KWH_Per_Capita,  color = annexII_label))+
      geom_line(data = annexI_ghg_elec, aes(x=Year,y=Mean_Total_Emission/coeff4,color=annexI_ghg_label))+geom_point(data = annexI_ghg_elec, aes(x=Year,y=Mean_Total_Emission/coeff4,color=annexI_ghg_label))+
      geom_line(data = annexII_ghg_elec, aes(x=Year,y=Mean_Total_Emission/coeff4,color=annexII_ghg_label))+geom_point(data = annexII_ghg_elec, aes(x=Year,y=Mean_Total_Emission/coeff4,color=annexII_ghg_label))+
      theme_bw()+labs(title = "Average Electricity Consumption vs Average Total GHG Emission")+theme(plot.title = element_text(size=18, hjust = 0.5))+
      scale_color_manual("Legend",labels=c('Electricity Consumption of Annex I', 'Electricity Consumption of Non-Annex I','GHG Emissions of Non-Annex I', 'GHG Emissions of Annex I'), values=c(annexI_ghg_label,annexII_ghg_label, annexI_label, annexII_label))+
      scale_y_continuous(name = "kWh Per Capita", sec.axis = sec_axis(~.*coeff4, name = "Average Total GHG Emission", labels = unit_format(unit = "M", scale = 1e-06)))
    
  }
  
  print(p)
  
}



ui <- dashboardPage(skin = 'green',
                    dashboardHeader(title = 'GREENHOUSE GAS EMISSIONS STUDY', titleWidth = 400),
                    dashboardSidebar(width = 400,
                                     sidebarMenu(id = 'sidemenu',
                                                 menuItem('Introduction', tabName = 'Intro', icon=icon('tree')),
                                                 menuItem('Descriptive Analysis', tabName = 'Desc', icon=icon('chart-bar')),
                                                 menuItem('Inferential Analysis', tabName = 'Inf', icon=icon('search'))
                                     )
                    ),
                    dashboardBody(
                      tags$head(
                        tags$style(HTML(".main-sidebar { font-size: 20px; }")),
                        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
                      tabItems(
                        tabItem(tabName = 'Desc',
                                fluidPage(
                                  titlePanel("DESCRIPTIVE ANALYSIS"),
                                  fluidRow(
                                    column(width = 12,
                                           tabsetPanel(
                                             tabPanel("Greenhouse Gas Emission Distribution",
                                                      tags$head(
                                                        tags$style(HTML(".main-sidebar { font-size: 20px; }")),
                                                        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
                                                      box(
                                                        width = 12,
                                                        height = 100,
                                                        selectInput(
                                                          inputId = "year_piechart", 
                                                          label = "Select year:", 
                                                          choices = 2000:2019)
                                                      ),
                                                      box(
                                                        width = 12,
                                                        height = 600,
                                                        solidHeader = FALSE,
                                                        collapsible = FALSE,
                                                        collapsed = FALSE,
                                                        plotOutput('GHGEmission', height = 550)
                                                      )
                                             ),
                                             tabPanel("Annex I vs Non-Annex I",
                                                      box(
                                                        width = 12,
                                                        height = 100,
                                                        radioButtons('AnnexChoice',
                                                                     label = tags$strong('Analyse Annex I and Non-Annex I Based on:'),
                                                                     choices = c('Average Emissions',
                                                                                 'Renewable Energy',
                                                                                 'Electricity Consumption',
                                                                                 'GDP Per Capita',
                                                                                 'Military Expenditure',
                                                                                 'Export',
                                                                                 'Import'),
                                                                     inline = TRUE)
                                                      ),
                                                      box(
                                                        width = 12,
                                                        height = 600,
                                                        solidHeader = TRUE,
                                                        collapsible = FALSE,
                                                        collapsed = FALSE,
                                                        plotOutput('AnnexIvsII', height = 550)
                                                      ),
                                                      box(
                                                        width = 12,
                                                        height = 100,
                                                        footer=("Annex I countries, comprising mainly developed countries, have been legally bound by the Kyoto Protocol to reduce GHG emissions since 2005. Non-Annex I countries comprise developing countries with lower and less binding reduction targets.")
                                                      )
                                             )
                                             
                                           )
                                    )
                                    
                                  ))), #end of "Descriptive"
                        tabItem(
                          tabName="Intro",
                          fluidPage(
                            titlePanel("GREENHOUSE GAS EMISSIONS STUDY"),
                            fluidRow(
                              column(
                                width=12,
                                box(
                                  width=3,
                                  height = 250,
                                  selectInput(inputId = "data_type",
                                              label = "Choose the type of data you are interested in:",
                                              choices = list("Total CO2 Emissions" = "Total_CO2_emissions", 
                                                             "Total Methane Emissions" = "Total_Methane_emissions",
                                                             "Total Nitrous Oxide Emissions"="Total_Nitrous_Oxide_emissions",
                                                             "Total Greenhouse Emissions"="Total_Greenhouse_Emissions",
                                                             "Renewable Energy(%)"="Renewable_Energy(%)",
                                                             "Military Expenditures(%)"="Military_Expenditures(%)",
                                                             "CO2 Emissions Per Capita"="CO2_emissions",
                                                             "Methane Emissions Per Capita"="Methane_emissions",
                                                             "Nitrous Oxide Emissions Per Capita"="Nitrous_Oxide")),
                                  sliderInput(inputId="Year_intro", 
                                              label="Choose the year you are interested in:", 
                                              min=2000, max=2019, value=2010, 
                                              step = 1)),
                                box(
                                  align="center",
                                  width=9,
                                  girafeOutput("worldmap"))
                                
                                
                              )
                            )
                          )
                        ),#End of Intro
                        tabItem(tabName = 'Inf',
                                fluidPage(
                                  titlePanel("INFERENTIAL ANALYSIS"),
                                  fluidRow(
                                    column(width = 12,
                                           tabsetPanel(
                                             tabPanel("Two Means",
                                                      titlePanel("Comparing Non-Annex I to Annex I Emissions"),
                                                      box(
                                                        width=3,
                                                        height=200,
                                                        selectInput("variable_twomeans", "Emissions Category:",
                                                                    c("Carbon Dioxide" = "CO2_emissions",
                                                                      "Methane" = "Methane_emissions",
                                                                      "Nitrous Oxide" = "Nitrous_Oxide",
                                                                      "Total GHG Emissions" = "Total_GHG_percapita")
                                                        ),
                                                        selectInput("year_twomeans", "Year",
                                                                    c("2019" = "2019",
                                                                      "2010" = "2010",
                                                                      "2000" = "2000")
                                                        )
                                                      ),
                                                      box(
                                                        width=9,
                                                        height=750,
                                                        textOutput("emissionscategory"),
                                                        tags$head(tags$style("#emissionscategory{color: midnightblue; font-size: 20px; font-style: bold;}")),
                                                        plotOutput("boxplot_twomeans"),
                                                        verbatimTextOutput("ztest"),
                                                        textOutput("conclusion"),
                                                        tags$head(tags$style("#conclusion{color: midnightblue; font-size: 20px; font-style: bold;}"))
                                                      )
                                             ),
                                             tabPanel("ANOVA",
                                                      titlePanel("Comparing Emissions Across Main Geographical Regions"),
                                                      box(
                                                        width=3,
                                                        height=200,
                                                        selectInput("variable_ANOVA", "Emissions Category:",
                                                                    c("Carbon Dioxide" = "CO2_emissions",
                                                                      "Methane" = "Methane_emissions",
                                                                      "Nitrous Oxide" = "Nitrous_Oxide",
                                                                      "Total GHG Emissions" = "Total_GHG_percapita")
                                                        ),
                                                        selectInput("year_ANOVA", "Year",
                                                                    c("2019" = "2019",
                                                                      "2010" = "2010",
                                                                      "2000" = "2000"))),
                                                      box(width=9,
                                                          height=950,
                                                          textOutput("emissionscategory2"),
                                                          tags$head(tags$style("#emissionscategory2{color: midnightblue; font-size: 20px; font-style: bold;}")),
                                                          plotOutput("boxplot_ANOVA"),
                                                          verbatimTextOutput("ANOVA"))),
                                             tabPanel("Regression",
                                                      titlePanel(""),
                                                      box(
                                                        width=12,
                                                        height=1800,
                                                        textOutput("multLinRegText"),
                                                        tags$head(tags$style("#multLinRegText{color: midnightblue; font-size: 20px; font-style: bold;}")),
                                                        plotOutput("multLinRegPlot1"),
                                                        br(),
                                                        br(),
                                                        verbatimTextOutput("multLinRegResult")
   
                                                      ))
                                           )
                                    )
                                  )
                                )
                        ) #end of inferential
                      )))

server <- function(input,output, session) {
  output$GHGEmission <- renderPlot({
    year <- input$year_piechart
    GHGEmissionPlot(year)
    
  })
  
  output$AnnexIvsII <- renderPlot({
    Annex1v2(input$AnnexChoice)
  })
  
  output$worldmap <- renderGirafe({
    ggiraph(print(worldMaps(data, world_data,input$data_type, input$Year_intro)))
    
  })
  
  output$multLinRegPlot1 <- renderPlot({
    MultipleLinearReg2()
  })
  
  output$multLinRegResult <- renderPrint({
    MultipleLinearReg2()
  })
  
  output$multLinRegText <- renderText({
    paste("Multiple Linear Regression Analysis for Top GHG Emitting Countries")
  })
  
  output$emissionscategory <- renderText({
    if (input$variable_twomeans == "CO2_emissions") {
      return(paste("Claim: Non-Annex I countries emitted less Carbon Dioxide per capita compared to Annex I countries in ", input$year_twomeans))
    } else if (input$variable_twomeans == "Methane_emissions"){
      return(paste("Claim: Non-Annex I countries emit less Methane per capita compared to Annex I countries in ", input$year_twomeans))
    } else if (input$variable_twomeans == "Nitrous_Oxide"){
      return(paste("Claim: Non-Annex I countries emit less Nitrous Oxide per capita compared to Annex I countries in ", input$year_twomeans))
    } else {
      return(paste("Claim: Non-Annex I countries emit less Greenhouse Gases per capita compared to Annex I countries in ", input$year_twomeans))
    }
  }
  )
  
  output$boxplot_twomeans <- renderPlot({
    if (input$year_twomeans == "2019") {
      if (input$variable_twomeans == "CO2_emissions") {
        ggboxplot(Combined_2019, x ="Status", y = "CO2_emissions",
                  color = "Status", palette = c("darkblue", "darkcyan"),
                  order = c("Annex I", "Non-Annex I"),
                  ylab = "CO2 Emissions per capita (kilotonnes)", xlab = "Status")
      } else if (input$variable_twomeans == "Methane_emissions") {
        ggboxplot(Combined_2019, x ="Status", y = "Methane_emissions",
                  color = "Status", palette = c("darkblue", "darkcyan"),
                  order = c("Annex I", "Non-Annex I"),
                  ylab = "CH4 per capita (kilotonnes)", xlab = "Status")
      } else if (input$variable_twomeans == "Nitrous_Oxide") {
        ggboxplot(Combined_2019, x ="Status", y = "Nitrous_Oxide",
                  color = "Status", palette = c("darkblue", "darkcyan"),
                  order = c("Annex I", "Non-Annex I"),
                  ylab = "N2O Emissions per capita (kilotonnes)", xlab = "Status")
      } else {
        ggboxplot(Combined_2019, x ="Status", y = "Total_GHG_percapita",
                  color = "Status", palette = c("darkblue", "darkcyan"),
                  order = c("Annex I", "Non-Annex I"),
                  ylab = "GHG Emissions per capita (kilotonnes)", xlab = "Status")
      }
      
    } else if (input$year_twomeans == "2010") {
      if (input$variable_twomeans == "CO2_emissions") {
        ggboxplot(Combined_2010, x ="Status", y = "CO2_emissions",
                  color = "Status", palette = c("darkblue", "darkcyan"),
                  order = c("Annex I", "Non-Annex I"),
                  ylab = "CO2 Emissions per capita (kilotonnes)", xlab = "Status")
      } else if (input$variable_twomeans == "Methane_emissions") {
        ggboxplot(Combined_2010, x ="Status", y = "Methane_emissions",
                  color = "Status", palette = c("darkblue", "darkcyan"),
                  order = c("Annex I", "Non-Annex I"),
                  ylab = "CH4 Emissions per capita (kilotonnes)", xlab = "Status")
      } else if (input$variable_twomeans == "Nitrous_Oxide") {
        ggboxplot(Combined_2010, x ="Status", y = "Nitrous_Oxide",
                  color = "Status", palette = c("darkblue", "darkcyan"),
                  order = c("Annex I", "Non-Annex I"),
                  ylab = "N2O Emissions per capita (kilotonnes)", xlab = "Status")
      } else {
        ggboxplot(Combined_2010, x ="Status", y = "Total_GHG_percapita",
                  color = "Status", palette = c("darkblue", "darkcyan"),
                  order = c("Annex I", "Non-Annex I"),
                  ylab = "GHG Emissions per capita (kilotonnes)", xlab = "Status")
      }
    } else {
      if (input$variable_twomeans == "CO2_emissions") {
        ggboxplot(Combined_2000, x ="Status", y = "CO2_emissions",
                  color = "Status", palette = c("darkblue", "darkcyan"),
                  order = c("Annex I", "Non-Annex I"),
                  ylab = "CO2 Emissions per capita (kilotonnes)", xlab = "Status")
      } else if (input$variable_twomeans == "Methane_emissions") {
        ggboxplot(Combined_2000, x ="Status", y = "Methane_emissions",
                  color = "Status", palette = c("darkblue", "darkcyan"),
                  order = c("Annex I", "Non-Annex I"),
                  ylab = "CH4 Emissions per capita (kilotonnes)", xlab = "Status")
      } else if (input$variable_twomeans == "Nitrous_Oxide") {
        ggboxplot(Combined_2000, x ="Status", y = "Nitrous_Oxide",
                  color = "Status", palette = c("darkblue", "darkcyan"),
                  order = c("Annex I", "Non-Annex I"),
                  ylab = "N2O Emissions per capita (kilotonnes)", xlab = "Status")
      } else {
        ggboxplot(Combined_2000, x ="Status", y = "Total_GHG_percapita",
                  color = "Status", palette = c("darkblue", "darkcyan"),
                  order = c("Annex I", "Non-Annex I"),
                  ylab = "GHG Emissions per capita (kilotonnes)", xlab = "Status")
      }
    }
  }
  )
  
  output$ztest <- renderPrint({
    if (input$year_twomeans == "2019") {
      if (input$variable_twomeans == "CO2_emissions") {
        ztest <- z.test(AnnexI_2019[["CO2_emissions"]], NonAnnexI_2019[["CO2_emissions"]], alternative = "greater",
                        sigma.x = sd(AnnexI_2019[["CO2_emissions"]]), sigma.y = sd(NonAnnexI_2019[["CO2_emissions"]]),
                        conf.level=0.95)
        print(ztest)
      } else if (input$variable_twomeans == "Methane_emissions") {
        ztest <- z.test(AnnexI_2019[["Methane_emissions"]], NonAnnexI_2019[["Methane_emissions"]], alternative = "greater",
                        sigma.x = sd(AnnexI_2019[["Methane_emissions"]]), sigma.y = sd(NonAnnexI_2019[["Methane_emissions"]]),
                        conf.level=0.95)
        print(ztest)
      } else if (input$variable_twomeans == "Nitrous_Oxide") {
        ztest <- z.test(AnnexI_2019[["Nitrous_Oxide"]], NonAnnexI_2019[["Nitrous_Oxide"]], alternative = "greater",
                        sigma.x = sd(AnnexI_2019[["Nitrous_Oxide"]]), sigma.y = sd(NonAnnexI_2019[["Nitrous_Oxide"]]),
                        conf.level=0.95)
        print(ztest)
      } else {
        ztest <- z.test(AnnexI_2019[["Total_GHG_percapita"]], NonAnnexI_2019[["Total_GHG_percapita"]], alternative = "greater",
                        sigma.x = sd(AnnexI_2019[["Total_GHG_percapita"]]), sigma.y = sd(NonAnnexI_2019[["Total_GHG_percapita"]]),
                        conf.level=0.95)
        print(ztest)
      }
    } else if (input$year_twomeans == "2010") {
      if (input$variable_twomeans == "CO2_emissions") {
        ztest <- z.test(AnnexI_2010[["CO2_emissions"]], NonAnnexI_2010[["CO2_emissions"]], alternative = "greater",
                        sigma.x = sd(AnnexI_2010[["CO2_emissions"]]), sigma.y = sd(NonAnnexI_2010[["CO2_emissions"]]),
                        conf.level=0.95)
        print(ztest)
      } else if (input$variable_twomeans == "Methane_emissions") {
        ztest <- z.test(AnnexI_2010[["Methane_emissions"]], NonAnnexI_2010[["Methane_emissions"]], alternative = "greater",
                        sigma.x = sd(AnnexI_2010[["Methane_emissions"]]), sigma.y = sd(NonAnnexI_2010[["Methane_emissions"]]),
                        conf.level=0.95)
        print(ztest)
      } else if (input$variable_twomeans == "Nitrous_Oxide") {
        ztest <- z.test(AnnexI_2010[["Nitrous_Oxide"]], NonAnnexI_2010[["Nitrous_Oxide"]], alternative = "greater",
                        sigma.x = sd(AnnexI_2010[["Nitrous_Oxide"]]), sigma.y = sd(NonAnnexI_2010[["Nitrous_Oxide"]]),
                        conf.level=0.95)
        print(ztest)
      } else {
        ztest <- z.test(AnnexI_2010[["Total_GHG_percapita"]], NonAnnexI_2010[["Total_GHG_percapita"]], alternative = "greater",
                        sigma.x = sd(AnnexI_2010[["Total_GHG_percapita"]]), sigma.y = sd(NonAnnexI_2010[["Total_GHG_percapita"]]),
                        conf.level=0.95)
        print(ztest)
      }
    } else {
      if (input$variable_twomeans == "CO2_emissions") {
        ztest <- z.test(AnnexI_2000[["CO2_emissions"]], NonAnnexI_2000[["CO2_emissions"]], alternative = "greater",
                        sigma.x = sd(AnnexI_2000[["CO2_emissions"]]), sigma.y = sd(NonAnnexI_2000[["CO2_emissions"]]),
                        conf.level=0.95)
        print(ztest)
      } else if (input$variable_twomeans == "Methane_emissions") {
        ztest <- z.test(AnnexI_2000[["Methane_emissions"]], NonAnnexI_2000[["Methane_emissions"]], alternative = "greater",
                        sigma.x = sd(AnnexI_2000[["Methane_emissions"]]), sigma.y = sd(NonAnnexI_2000[["Methane_emissions"]]),
                        conf.level=0.95)
        print(ztest)
      } else if (input$variable_twomeans == "Nitrous_Oxide") {
        ztest <- z.test(AnnexI_2000[["Nitrous_Oxide"]], NonAnnexI_2000[["Nitrous_Oxide"]], alternative = "greater",
                        sigma.x = sd(AnnexI_2000[["Nitrous_Oxide"]]), sigma.y = sd(NonAnnexI_2000[["Nitrous_Oxide"]]),
                        conf.level=0.95)
        print(ztest)
      } else {
        ztest <- z.test(AnnexI_2000[["Total_GHG_percapita"]], NonAnnexI_2000[["Total_GHG_percapita"]], alternative = "greater",
                        sigma.x = sd(AnnexI_2000[["Total_GHG_percapita"]]), sigma.y = sd(NonAnnexI_2000[["Total_GHG_percapita"]]),
                        conf.level=0.95)
        print(ztest)
      }
    }
    
    
  })
  
  output$conclusion <- renderText({
    if (input$variable_twomeans == "Methane_emissions") {
      return(paste("P-value is more than 0.05. There is insufficient statistical evidence to reject the null hypothesis."))
    } else {
      return(paste("P-value is less than 0.05. There is sufficient statistical evidence to reject the null hypothesis."))
    }
  }
  )
  output$emissionscategory2 <- renderText({
    if (input$variable_ANOVA == "CO2_emissions") {
      return(paste("Claim: There was significant difference in regional CO2 emissions in ", input$year_ANOVA))
    } else if (input$variable_ANOVA == "Methane_emissions"){
      return(paste("Claim: There was significant difference in regional CH4 emissions in ", input$year_ANOVA))
    } else if (input$variable_ANOVA == "Nitrous_Oxide"){
      return(paste("Claim: There was significant difference in regional NO2 emissions in ", input$year_ANOVA))
    } else {
      return(paste("Claim: There was significant difference in regional GHG emissions in ", input$year_ANOVA))
    }
  }
  )
  
  output$boxplot_ANOVA <- renderPlot({
    if (input$year_ANOVA == "2019") {
      if (input$variable_ANOVA == "CO2_emissions") {
        ggboxplot(ANOVA_2019, x = "Region", y = "CO2_emissions",
                  color = "Region", palette = c("darkred", "darkorange", "darkmagenta", "darkgreen", "darkblue"),
                  order = c("Asia-Pacific",
                            "South Asia, Middle East & North Africa",
                            "Sub-Saharan Africa",
                            "Europe & Central Asia",
                            "Latin America & Caribbean"),
                  ylab = "CO2 Emissions per capita (kilotonnes)",
                  xlab = "Region")
      } else if (input$variable_ANOVA == "Methane_emissions") {
        ggboxplot(ANOVA_2019, x = "Region", y = "Methane_emissions",
                  color = "Region", palette = c("darkred", "darkorange", "darkmagenta", "darkgreen", "darkblue"),
                  order = c("Asia-Pacific",
                            "South Asia, Middle East & North Africa",
                            "Sub-Saharan Africa",
                            "Europe & Central Asia",
                            "Latin America & Caribbean"),
                  ylab = "CH4 Emissions per capita (kilotonnes)",
                  xlab = "Region")
      } else if (input$variable_ANOVA == "Nitrous_Oxide") {
        ggboxplot(ANOVA_2019, x = "Region", y = "Nitrous_Oxide",
                  color = "Region", palette = c("darkred", "darkorange", "darkmagenta", "darkgreen", "darkblue"),
                  order = c("Asia-Pacific",
                            "South Asia, Middle East & North Africa",
                            "Sub-Saharan Africa",
                            "Europe & Central Asia",
                            "Latin America & Caribbean"),
                  ylab = "N2O Emissions per capita (kilotonnes)",
                  xlab = "Region")
      } else {
        ggboxplot(ANOVA_2019, x = "Region", y = "Total_GHG_percapita",
                  color = "Region", palette = c("darkred", "darkorange", "darkmagenta", "darkgreen", "darkblue"),
                  order = c("Asia-Pacific",
                            "South Asia, Middle East & North Africa",
                            "Sub-Saharan Africa",
                            "Europe & Central Asia",
                            "Latin America & Caribbean"),
                  ylab = "GHG Emissions per capita (kilotonnes)",
                  xlab = "Region")
      }
      
    } else if (input$year_ANOVA == "2010") {
      if (input$variable_ANOVA == "CO2_emissions") {
        ggboxplot(ANOVA_2010, x = "Region", y = "CO2_emissions",
                  color = "Region", palette = c("darkred", "darkorange", "darkmagenta", "darkgreen", "darkblue"),
                  order = c("Asia-Pacific",
                            "South Asia, Middle East & North Africa",
                            "Sub-Saharan Africa",
                            "Europe & Central Asia",
                            "Latin America & Caribbean"),
                  ylab = "CO2 Emissions per capita (kilotonnes)",
                  xlab = "Region")
      } else if (input$variable_ANOVA == "Methane_emissions") {
        ggboxplot(ANOVA_2010, x = "Region", y = "Methane_emissions",
                  color = "Region", palette = c("darkred", "darkorange", "darkmagenta", "darkgreen", "darkblue"),
                  order = c("Asia-Pacific",
                            "South Asia, Middle East & North Africa",
                            "Sub-Saharan Africa",
                            "Europe & Central Asia",
                            "Latin America & Caribbean"),
                  ylab = "CH4 Emissions per capita (kilotonnes)",
                  xlab = "Region")
      } else if (input$variable_ANOVA == "Nitrous_Oxide") {
        ggboxplot(ANOVA_2010, x = "Region", y = "Nitrous_Oxide",
                  color = "Region", palette = c("darkred", "darkorange", "darkmagenta", "darkgreen", "darkblue"),
                  order = c("Asia-Pacific",
                            "South Asia, Middle East & North Africa",
                            "Sub-Saharan Africa",
                            "Europe & Central Asia",
                            "Latin America & Caribbean"),
                  ylab = "N2O Emissions per capita (kilotonnes)",
                  xlab = "Region")
      } else {
        ggboxplot(ANOVA_2010, x = "Region", y = "Total_GHG_percapita",
                  color = "Region", palette = c("darkred", "darkorange", "darkmagenta", "darkgreen", "darkblue"),
                  order = c("Asia-Pacific",
                            "South Asia, Middle East & North Africa",
                            "Sub-Saharan Africa",
                            "Europe & Central Asia",
                            "Latin America & Caribbean"),
                  ylab = "GHG Emissions per capita (kilotonnes)",
                  xlab = "Region")
      }
    } else {
      if (input$variable_ANOVA == "CO2_emissions") {
        ggboxplot(ANOVA_2000, x = "Region", y = "CO2_emissions",
                  color = "Region", palette = c("darkred", "darkorange", "darkmagenta", "darkgreen", "darkblue"),
                  order = c("Asia-Pacific",
                            "South Asia, Middle East & North Africa",
                            "Sub-Saharan Africa",
                            "Europe & Central Asia",
                            "Latin America & Caribbean"),
                  ylab = "CO2 Emissions per capita (kilotonnes)",
                  xlab = "Region")
      } else if (input$variable_ANOVA == "Methane_emissions") {
        ggboxplot(ANOVA_2000, x = "Region", y = "Methane_emissions",
                  color = "Region", palette = c("darkred", "darkorange", "darkmagenta", "darkgreen", "darkblue"),
                  order = c("Asia-Pacific",
                            "South Asia, Middle East & North Africa",
                            "Sub-Saharan Africa",
                            "Europe & Central Asia",
                            "Latin America & Caribbean"),
                  ylab = " Emissions per capita (kilotonnes)",
                  xlab = "Region")
      } else if (input$variable_ANOVA == "Nitrous_Oxide") {
        ggboxplot(ANOVA_2000, x = "Region", y = "Nitrous_Oxide",
                  color = "Region", palette = c("darkred", "darkorange", "darkmagenta", "darkgreen", "darkblue"),
                  order = c("Asia-Pacific",
                            "South Asia, Middle East & North Africa",
                            "Sub-Saharan Africa",
                            "Europe & Central Asia",
                            "Latin America & Caribbean"),
                  ylab = "N2O Emissions per capita (kilotonnes)",
                  xlab = "Region")
      } else {
        ggboxplot(ANOVA_2000, x = "Region", y = "Total_GHG_percapita",
                  color = "Region", palette = c("darkred", "darkorange", "darkmagenta", "darkgreen", "darkblue"),
                  order = c("Asia-Pacific",
                            "South Asia, Middle East & North Africa",
                            "Sub-Saharan Africa",
                            "Europe & Central Asia",
                            "Latin America & Caribbean"),
                  ylab = "GHG Emissions per capita (kilotonnes)",
                  xlab = "Region")
      }
    }
  }
  )
  
  output$ANOVA <- renderPrint({
    if (input$year_ANOVA == "2019") {
      if (input$variable_ANOVA == "CO2_emissions") {
        res.aov <- aov(CO2_emissions ~ Region, data=ANOVA_2019)
        summary(res.aov)
        print(summary(res.aov))
        TukeyHSD(res.aov)
        print(TukeyHSD(res.aov))
      } else if (input$variable_ANOVA == "Methane_emissions") {
        res.aov <- aov(Methane_emissions ~ Region, data=ANOVA_2019)
        summary(res.aov)
        print(summary(res.aov))
        TukeyHSD(res.aov)
        print(TukeyHSD(res.aov))
      } else if (input$variable_ANOVA == "Nitrous_Oxide") {
        res.aov <- aov(Nitrous_Oxide ~ Region, data=ANOVA_2019)
        summary(res.aov)
        print(summary(res.aov))
        TukeyHSD(res.aov)
        print(TukeyHSD(res.aov))
      } else {
        res.aov <- aov(Total_GHG_percapita ~ Region, data=ANOVA_2019)
        summary(res.aov)
        print(summary(res.aov))
        TukeyHSD(res.aov)
        print(TukeyHSD(res.aov))
      }
    } else if (input$year_ANOVA == "2010"){
      if (input$variable_ANOVA == "CO2_emissions") {
        res.aov <- aov(CO2_emissions ~ Region, data=ANOVA_2010)
        summary(res.aov)
        print(summary(res.aov))
        TukeyHSD(res.aov)
        print(TukeyHSD(res.aov))
      } else if (input$variable_ANOVA == "Methane_emissions") {
        res.aov <- aov(Methane_emissions ~ Region, data=ANOVA_2010)
        summary(res.aov)
        print(summary(res.aov))
        TukeyHSD(res.aov)
        print(TukeyHSD(res.aov))
      } else if (input$variable_ANOVA == "Nitrous_Oxide") {
        res.aov <- aov(Nitrous_Oxide ~ Region, data=ANOVA_2010)
        summary(res.aov)
        print(summary(res.aov))
        TukeyHSD(res.aov)
        print(TukeyHSD(res.aov))
      } else {
        res.aov <- aov(Total_GHG_percapita ~ Region, data=ANOVA_2010)
        summary(res.aov)
        print(summary(res.aov))
        TukeyHSD(res.aov)
        print(TukeyHSD(res.aov))
      }
    } else {
      if (input$variable_ANOVA == "CO2_emissions") {
        res.aov <- aov(CO2_emissions ~ Region, data=ANOVA_2000)
        summary(res.aov)
        print(summary(res.aov))
        TukeyHSD(res.aov)
        print(TukeyHSD(res.aov))
      } else if (input$variable_ANOVA == "Methane_emissions") {
        res.aov <- aov(Methane_emissions ~ Region, data=ANOVA_2000)
        summary(res.aov)
        print(summary(res.aov))
        TukeyHSD(res.aov)
        print(TukeyHSD(res.aov))
      } else if (input$variable_ANOVA == "Nitrous_Oxide") {
        res.aov <- aov(Nitrous_Oxide ~ Region, data=ANOVA_2000)
        summary(res.aov)
        print(summary(res.aov))
        TukeyHSD(res.aov)
        print(TukeyHSD(res.aov))
      } else {
        res.aov <- aov(Total_GHG_percapita ~ Region, data=ANOVA_2000)
        summary(res.aov)
        print(summary(res.aov))
        TukeyHSD(res.aov)
        print(TukeyHSD(res.aov))
      }
    }
  }
  )
  
}


shinyApp(ui, server)