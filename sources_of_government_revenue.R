###Sources of Government Revenue###

#Clear working environment####
rm(list=ls())
gc()

#general set-up
using<-function(...,prompt=TRUE){
  libs<-sapply(substitute(list(...))[-1],deparse)
  req<-unlist(lapply(libs,require,character.only=TRUE))
  need<-libs[req==FALSE]
  n<-length(need)
  installAndRequire<-function(){
    install.packages(need)
    lapply(need,require,character.only=TRUE)
  }
  if(n>0){
    libsmsg<-if(n>2) paste(paste(need[1:(n-1)],collapse=", "),",",sep="") else need[1]
    if(n>1){
      libsmsg<-paste(libsmsg," and ", need[n],sep="")
    }
    libsmsg<-paste("The following packages count not be found: ",libsmsg,"n\r\n\rInstall missing packages?",collapse="")
    if(prompt==FALSE){
      installAndRequire()
    }else if(winDialog(type=c("yesno"),libsmsg)=="YES"){
      installAndRequire()
    }
  }
}

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

using(gtools)
using(plyr)
using(reshape2)
using(OECD)
using(readxl)
using(rvest)
using(htmltab)
using(tidyverse)
using(stringr)
using(dplyr)
using(naniar)
using(magicfor)


oecd_countries<-c("AUS",
                  "AUT",
                  "BEL",
                  "CAN",
                  "CHL",
                  "COL",
                  "CRI",
                  "CZE",
                  "DNK",
                  "EST",
                  "FIN",
                  "FRA",
                  "DEU",
                  "GRC",
                  "HUN",
                  "ISL",
                  "IRL",
                  "ISR",
                  "ITA",
                  "JPN",
                  "KOR",
                  "LVA",
                  "LUX",
                  "LTU",
                  "MEX",
                  "NLD",
                  "NZL",
                  "NOR",
                  "POL",
                  "PRT",
                  "SVK",
                  "SVN",
                  "ESP",
                  "SWE",
                  "CHE",
                  "TUR",
                  "GBR",
                  "USA")

#Reading in and cleaning OECD's global tax revenue statistics dataset####
#dataset_list <- get_datasets()
#search_dataset("Global Revenue", data= dataset_list)

dataset <- ("REV")

dstruc <- get_data_structure(dataset)
str(dstruc, max.level = 1)
#dstruc$GOV
#dstruc$TAX
#dstruc$VAR
#dstruc$YEA

all_data <- get_dataset("REV",filter=list(c("NES"),c("1100","1200","1300","2000","3000","4000","5000","6000",
                                                     "CUS"),c("TAXUSD"),c(oecd_countries)), start_time = 2008)

#Drop redundant columns
all_data <- subset(all_data, select=-c(TIME_FORMAT, GOV, VAR, UNIT, POWERCODE))

#Rename columns
colnames(all_data)[colnames(all_data)=="COU"] <- "iso_3"
colnames(all_data)[colnames(all_data)=="TAX"] <- "category"
colnames(all_data)[colnames(all_data)=="obsTime"] <- "year"
colnames(all_data)[colnames(all_data)=="obsValue"] <- "value"



#Import and match country names with ISO-3 codes###

#Read in country name file
country_names <- read.csv("source-data/country_codes.csv")

#Keep and rename selected columns
country_names <- subset(country_names, select = c(official_name_en, ISO3166.1.Alpha.2, ISO3166.1.Alpha.3, Continent))

colnames(country_names)[colnames(country_names)=="official_name_en"] <- "country"
colnames(country_names)[colnames(country_names)=="ISO3166.1.Alpha.2"] <- "iso_2"
colnames(country_names)[colnames(country_names)=="ISO3166.1.Alpha.3"] <- "iso_3"
colnames(country_names)[colnames(country_names)=="Continent"] <- "continent"

#Replace continent abbreviation 'NA' (North America) to 'NO' (R does not recognize 'NA' as a character)
country_names$continent <- as.character(country_names$continent)
country_names$continent <- if_else(is.na(country_names$continent),"NO",country_names$continent)

#Add country names and continents to all_data, and add variable signaling OECD countries###
all_data <- merge(all_data, country_names, by='iso_3')

all_data$oecd <- ifelse(all_data$iso_3 == "AUS"
                        | all_data$iso_3 == "AUT"
                        | all_data$iso_3 == "BEL"
                        | all_data$iso_3 == "CAN"
                        | all_data$iso_3 == "CHL"
                        | all_data$iso_3 == "COL"
                        | all_data$iso_3 == "CRI"
                        | all_data$iso_3 == "CZE"
                        | all_data$iso_3 == "DNK"
                        | all_data$iso_3 == "EST"
                        | all_data$iso_3 == "FIN"
                        | all_data$iso_3 == "FRA"
                        | all_data$iso_3 == "DEU"
                        | all_data$iso_3 == "GRC"
                        | all_data$iso_3 == "HUN"
                        | all_data$iso_3 == "ISL"
                        | all_data$iso_3 == "IRL"
                        | all_data$iso_3 == "ISR"
                        | all_data$iso_3 == "ITA"
                        | all_data$iso_3 == "JPN"
                        | all_data$iso_3 == "KOR"
                        | all_data$iso_3 == "LTU"
                        | all_data$iso_3 == "LUX"
                        | all_data$iso_3 == "LVA"
                        | all_data$iso_3 == "MEX"
                        | all_data$iso_3 == "NLD"
                        | all_data$iso_3 == "NZL"
                        | all_data$iso_3 == "NOR"
                        | all_data$iso_3 == "POL"
                        | all_data$iso_3 == "PRT"
                        | all_data$iso_3 == "SVK"
                        | all_data$iso_3 == "SVN"
                        | all_data$iso_3 == "ESP"
                        | all_data$iso_3 == "SWE"
                        | all_data$iso_3 == "CHE"
                        | all_data$iso_3 == "TUR"
                        | all_data$iso_3 == "GBR"
                        | all_data$iso_3 == "USA"
                        ,1,0)

#Adjust the order of the columns
all_data <- all_data[c("iso_2", "iso_3", "country", "continent", "oecd", "year", "category", "value")]

write.csv(all_data, "intermediate-outputs/data_preliminary.csv",row.names=F)

#Inflation data####

#dataset_list <- get_datasets()
#search_dataset("price", data= dataset_list)

dataset <- ("PRICES_CPI")

#dstruc <- get_data_structure(dataset)
#str(dstruc, max.level = 1)
#List of 11
#$ VAR_DESC       :'data.frame':	11 obs. of  2 variables:
#  $ LOCATION       :'data.frame':	53 obs. of  2 variables:
#  $ SUBJECT        :'data.frame':	104 obs. of  2 variables:
#  $ MEASURE        :'data.frame':	7 obs. of  2 variables:
#  $ FREQUENCY      :'data.frame':	3 obs. of  2 variables:
#  $ TIME           :'data.frame':	1610 obs. of  2 variables:
#  $ OBS_STATUS     :'data.frame':	15 obs. of  2 variables:
#  $ UNIT           :'data.frame':	316 obs. of  2 variables:
#  $ POWERCODE      :'data.frame':	32 obs. of  2 variables:
#  $ REFERENCEPERIOD:'data.frame':	96 obs. of  2 variables:
#  $ TIME_FORMAT    :'data.frame':	5 obs. of  2 variables:

#dstruc$VAR_DESC
#dstruc$LOCATION
#dstruc$SUBJECT
#dstruc$MEASURE
#dstruc$FREQUENCY
#dstruc$TIME
#dstruc$OBS_STATUS
#dstruc$MEASURE

inflation <- get_dataset("PRICES_CPI",filter=list(c("OECD"),c("CPALTT01"),
                                                  c("IXOB"),c("A")),
                         start_time = 2008)
inflation<- subset(inflation, select=c(obsTime, obsValue))

colnames(inflation)<-c("year","inflation")

#Calculate average OECD tax revenue sources####

#Categories
categories<-c("individual_1100",
              "corporate_1200",
              "social_2000",
              "property_4000",
              "consumption_5000",
              "other")
years<-print(unique(all_data$year))

all_data$category_broad<-if_else(all_data$category==1100,"individual_1100",
                                 if_else(all_data$category==1200,"corporate_1200",
                                         if_else(all_data$category==2000,"social_2000",
                                                 if_else(all_data$category==4000,"property_4000",
                                                         if_else(all_data$category==5000,"consumption_5000","other")))))


magic_for(silent = TRUE)

for(year in years){
  individual_1100<-sum(all_data$value[all_data$category_broad=="individual_1100"&
                                        all_data$year==year])
  corporate_1200<-sum(all_data$value[all_data$category_broad=="corporate_1200"&
                                       all_data$year==year])
  social_2000<-sum(all_data$value[all_data$category_broad=="social_2000"&
                                    all_data$year==year])
  property_4000<-sum(all_data$value[all_data$category_broad=="property_4000"&
                                      all_data$year==year])
  consumption_5000<-sum(all_data$value[all_data$category_broad=="consumption_5000"&
                                         all_data$year==year])
  other<-sum(all_data$value[all_data$category_broad=="other"&
                              all_data$year==year])
  put(individual_1100,corporate_1200,social_2000,property_4000,consumption_5000,other)
}
total_revenues<-magic_result_as_dataframe() 

total_revenues$total<-rowSums(total_revenues[2:7])
write.csv(total_revenues, "intermediate-outputs/total_revenues.csv",row.names=F)

#Inflation Adjustment####
total_revenues<-merge(inflation,total_revenues,by=c("year"))
total_revenues$inflation<-total_revenues$inflation/100

total_revenues$individual_1100<-total_revenues$individual_1100/total_revenues$inflation
total_revenues$corporate_1200<-total_revenues$corporate_1200/total_revenues$inflation
total_revenues$social_2000<-total_revenues$social_2000/total_revenues$inflation
total_revenues$property_4000<-total_revenues$property_4000/total_revenues$inflation
total_revenues$consumption_5000<-total_revenues$consumption_5000/total_revenues$inflation
total_revenues$other<-total_revenues$other/total_revenues$inflation
total_revenues$total<-total_revenues$total/total_revenues$inflation





#Indexing to 2008####
total_revenues$ind_2008<-total_revenues$individual_1100[total_revenues$year==2008]
total_revenues$corp_2008<-total_revenues$corporate_1200[total_revenues$year==2008]
total_revenues$soc_2008<-total_revenues$social_2000[total_revenues$year==2008]
total_revenues$prop_2008<-total_revenues$property_4000[total_revenues$year==2008]
total_revenues$con_2008<-total_revenues$consumption_5000[total_revenues$year==2008]
total_revenues$oth_2008<-total_revenues$other[total_revenues$year==2008]
total_revenues$total_2008<-total_revenues$total[total_revenues$year==2008]

total_revenues$individual<-total_revenues$individual_1100/total_revenues$ind_2008
total_revenues$corporate<-total_revenues$corporate_1200/total_revenues$corp_2008
total_revenues$social<-total_revenues$social_2000/total_revenues$soc_2008
total_revenues$property<-total_revenues$property_4000/total_revenues$prop_2008
total_revenues$consumption<-total_revenues$consumption_5000/total_revenues$con_2008
total_revenues$other<-total_revenues$other/total_revenues$oth_2008
total_revenues$total2<-total_revenues$total/total_revenues$total_2008


index<- subset(total_revenues, select=c(year, individual, corporate,social,property,consumption,other,total2))

write.csv(index, "intermediate-outputs/index.csv",row.names=F)