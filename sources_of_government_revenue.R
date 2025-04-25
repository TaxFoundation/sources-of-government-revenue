####Sources of Government Revenue####

#Clear working environment#####
rm(list=ls())
gc()

#Directory Variables####
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source_data<-"C:/Users/acer/Documents/GitHub/sources_of_government_revenue/source_data/"
intermediate_outputs<-"C:/Users/acer/Documents/GitHub/sources_of_government_revenue/intermediate_outputs/"
final_outputs<-"C:/Users/acer/Documents/GitHub/sources_of_government_revenue/final_outputs/"

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

#Reading and cleaning 1990 and 2023 OECD countries from Comparative tables of Revenue Statistics in OECD member countries

url = "https://sdmx.oecd.org/public/rest/data/OECD.CTP.TPS,DSD_REV_COMP_OECD@DF_RSOECD,1.1/AUS+AUT+BEL+CAN+CHL+COL+CRI+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+ISL+IRL+ISR+ITA+KOR+LVA+LTU+LUX+MEX+NLD+NZL+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+TUR+GBR+USA..S13.T_2000+T_1100+T_1200+T_1300+T_3000+T_4000+T_5000+T_6000..PT_OTR_REV_CAT.A?startPeriod=2023&dimensionAtObservation=AllDimensions&format=csvfilewithlabels"
all_data_OECD<-read.csv(url)

url = "https://sdmx.oecd.org/public/rest/data/OECD.CTP.TPS,DSD_REV_COMP_OECD@DF_RSOECD,1.1/AUS+AUT+BEL+CAN+CHL+COL+CRI+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+ISL+IRL+ISR+ITA+JPN+KOR+LVA+LTU+LUX+MEX+NLD+NZL+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+TUR+GBR+USA..S13.T_2000+T_1100+T_1200+T_1300+T_3000+T_4000+T_5000+T_6000..PT_OTR_REV_CAT.A?startPeriod=1990&endPeriod=1990&dimensionAtObservation=AllDimensions&format=csvfilewithlabels"
all_OECD_1990<-read.csv(url)


#Reading and cleaning 2022 non-OECD countries + Australia+ Greece+Japan (since 2023 data is not available or incomplete in Comparative tables of Revenue Statistics in OECD member countries) from Comparative tables of countries in the global database
url = "https://sdmx.oecd.org/public/rest/data/OECD.CTP.TPS,DSD_REV_COMP_GLOBAL@DF_RSGLOBAL,/JPN+GRC+ATG+ARM+AZE+BGD+BWA+BGR+BFA+KHM+CMR+TCD+CHN+COG+COK+HRV+COD+EGY+GNQ+SWZ+FJI+GAB+GEO+GHA+GIN+HKG+IDN+KAZ+KEN+KIR+LAO+LSO+LIE+MDG+MWI+MDV+MLI+MLT+MHL+MRT+MNG+MAR+MOZ+NAM+NRU+NIC+NER+NGA+PAK+PNG+PHL+ROU+RWA+LCA+WSM+SEN+SYC+SLE+SGP+SLB+SOM+LKA+THA+TLS+TGO+TKL+TUN+UGA+UKR+VUT+VNM+ZMB+AUS+ARG+BHS+BRB+BLZ+BTN+BOL+BRA+CPV+CIV+CUB+DOM+ECU+SLV+GTM+GUY+HND+JAM+KGZ+MYS+MUS+PAN+PRY+PER+ZAF+TTO+URY..S13.T_5000+T_4000+T_3000+T_2000+T_1300+T_1200+T_1100+T_6000..PT_OTR_REV_CAT.A?startPeriod=2022&endPeriod=2022&dimensionAtObservation=AllDimensions&format=csvfilewithlabels"
non_OECD<-read.csv(url)

#url = "https://sdmx.oecd.org/public/rest/data/OECD.CTP.TPS,DSD_REV_COMP_GLOBAL@DF_RSGLOBAL,/ATG+ARM+AZE+BGD+BWA+BGR+BFA+KHM+CMR+TCD+CHN+COG+COK+HRV+COD+EGY+GNQ+SWZ+FJI+GAB+GEO+GHA+GIN+HKG+IDN+KAZ+KEN+KIR+LAO+LSO+LIE+MDG+MWI+MDV+MLI+MLT+MHL+MRT+MNG+MAR+MOZ+NAM+NRU+NIC+NER+NGA+PAK+PNG+PHL+ROU+RWA+LCA+WSM+SEN+SYC+SLE+SGP+SLB+SOM+LKA+THA+TLS+TGO+TKL+TUN+UGA+UKR+VUT+VNM+ZMB+ARG+BHS+BRB+BLZ+BTN+BOL+BRA+CPV+CIV+CUB+DOM+ECU+SLV+GTM+GUY+HND+JAM+KGZ+MYS+MUS+PAN+PRY+PER+ZAF+TTO+URY..S13.T_5000+T_4000+T_3000+T_2000+T_1300+T_1200+T_1100+T_6000..PT_OTR_REV_CAT.A?startPeriod=1990&endPeriod=1990&dimensionAtObservation=AllDimensions&format=csvfilewithlabels"
#non_OECD_1990<-read.csv(url)

#Combine OECD and non_OECD data for 1990 with 2022 and 2023 in one dataset called all_data###
all_data <- rbind (all_data_OECD, all_OECD_1990,non_OECD)

#Drop redundant columns
all_data <- subset(all_data, select=c(REF_AREA,TIME_PERIOD, OBS_VALUE,REVENUE_CODE))

#Rename columns
colnames(all_data)[colnames(all_data)=="REF_AREA"] <- "iso_3"
colnames(all_data)[colnames(all_data)=="REVENUE_CODE"] <- "category"
colnames(all_data)[colnames(all_data)=="TIME_PERIOD"] <- "year"
colnames(all_data)[colnames(all_data)=="OBS_VALUE"] <- "share"

#Import and match country names with ISO-3 codes####

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

#Add country names and continents to all_data, and add variable signaling OECD countries####
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
all_data <- all_data[c("iso_2", "iso_3", "country", "continent", "oecd", "year", "category", "share")]

#Create dataset with only OECD countries
oecd_data <- all_data
oecd_data <- subset(oecd_data, subset = oecd == 1)

write.csv(oecd_data, "intermediate-outputs/oecd_data_preliminary.csv")

#Fix countries for which 2023 data is not available (unless otherwise noted, 2022 data is used for these cases)####

#Greece: for the categories 1100, 1200, and 1300 data is not available; it is for 1000. -> use 2022 data breakdown between 1100,1200 and 1300 to distribute 2023 data for category 1000.
missing_greece <- all_data
missing_greece <- subset(missing_greece, subset = iso_3 == "GRC" & year == "2022" & category < 2000)

#use 2022 distribution for 2023 data for 1000 category# 
missing_greece$"share"<-ifelse(missing_greece$category== "1200", missing_greece$"share"*23.980145/20.506, missing_greece$"share")
missing_greece$"share"<-ifelse(missing_greece$category== "1100", missing_greece$"share"*23.980145/20.506, missing_greece$"share")
missing_greece$"share"<-ifelse(missing_greece$category== "1300", missing_greece$"share"*23.980145/20.506, missing_greece$"share")
missing_greece[missing_greece$year == 2022, "year"] <- 2023

#Delete from the all_data 2023 data for Greece when data is missing  (category < 2000)
all_data <- all_data %>% filter(!(iso_3 == "GRC" & year==2023 & category < 2000))

#Australia: 2023 data not available -> use 2022 data
missing_australia <- all_data
missing_australia <- subset(missing_australia, subset = iso_3 == "AUS" & year == "2022")
missing_australia[missing_australia$year == 2022, "year"] <- 2023

#Japan: 2023 data not available -> use 2022 data
missing_japan <- all_data
missing_japan <- subset(missing_japan, subset = iso_3 == "JPN" & year == "2022")
missing_japan[missing_japan$year == 2022, "year"] <- 2023

#Combine data
all_data <- rbind(all_data,missing_greece,missing_australia, missing_japan)

#Sort dataset
all_data <- all_data[order(all_data$country, all_data$category, all_data$year),]

#Calculate average OECD tax revenue sources####

#Limit data to OECD countries and 2023
oecd_data_2023 <- all_data
oecd_data_2023 <- subset(oecd_data_2023, subset = year == 2023)
oecd_data_2023 <- subset(oecd_data_2023, subset = oecd == 1)
oecd_data_2023$share<-as.numeric(oecd_data_2023$share)

#Calculate averages for 1100 (individual taxes)
individual_1100 <- subset(oecd_data_2023, category==1100)
individual_1100_mean <- mean(individual_1100$share, na.rm = TRUE)

#Calculate averages for 1200 (corporate taxes)
corporate_1200 <- subset(oecd_data_2023, category==1200)
corporate_1200_mean <- mean(corporate_1200$share, na.rm = TRUE)

#Calculate averages for 2000 (social insurance taxes)
social_2000 <- subset(oecd_data_2023, category==2000)
social_2000_mean <- mean(social_2000$share, na.rm = TRUE)

#Calculate averages for 4000 (property taxes)
property_4000 <- subset(oecd_data_2023, category==4000)
property_4000_mean <- mean(property_4000$share, na.rm = TRUE)

#Calculate averages for 5000 (consumption taxes)
consumption_5000 <- subset(oecd_data_2023, category==5000)
consumption_5000_mean <- mean(consumption_5000$share, na.rm = TRUE)

#Calculate averages for 1300 + 3000 + 6000 (other)
other <- subset(oecd_data_2023, category == 1300 | category == 3000 | category == 6000)
other <- subset(other, select = -c(continent, oecd, year))

other_wide <- reshape(other, 
             timevar = "category",
             idvar = c("iso_2","iso_3","country"),
             direction = "wide")

colnames(other_wide)[colnames(other_wide)=="share.1300"] <- "1300"
colnames(other_wide)[colnames(other_wide)=="share.3000"] <- "3000"
colnames(other_wide)[colnames(other_wide)=="share.6000"] <- "6000"

other_wide[is.na(other_wide)] <- 0

other_wide$sum <- rowSums(other_wide[,c("1300", "3000", "6000")])

other_mean <- mean(other_wide$sum, na.rm = TRUE)

#Compile averages into one dataframe
tax_categories <- c("Individual Taxes","Corporate Taxes","Social Insurance Taxes","Property Taxes","Consumption Taxes","Other")
average_oecd <- c(individual_1100_mean, corporate_1200_mean, social_2000_mean, property_4000_mean, consumption_5000_mean, other_mean)

columns<-c("year","iso_2","iso_3","Country","Individual Taxes","Corporate Taxes",
          "Social Insurance Taxes","Property Taxes","Consumption Taxes","Other")

oecd_averages <- data.frame(tax_categories, average_oecd)

oecd_averages$average_oecd <- round(oecd_averages$average_oecd, digits = 1)

colnames(oecd_averages)[colnames(oecd_averages)=="tax_categories"] <- "Tax Category"
colnames(oecd_averages)[colnames(oecd_averages)=="average_oecd"] <- "Average Share"

write.csv(oecd_averages, "final-outputs/oecd_averages.csv", row.names = FALSE)


#Graph comparing OECD tax revenue shares in 1990 with 2023####

#Limit data to OECD countries and 1990
oecd_data_1990 <- all_data
oecd_data_1990 <- subset(oecd_data_1990, subset = year == 1990)
oecd_data_1990 <- subset(oecd_data_1990, subset = oecd == 1)
oecd_data_1990$share<-as.numeric(oecd_data_1990$share)

#Drop countries for which 1990 data is available but that were not part of the OECD in 1990 including Colombia and Costa Rica
oecd_data_1990 <- subset(oecd_data_1990, oecd_data_1990$iso_3 != "CZE" & oecd_data_1990$iso_3 != "EST" & oecd_data_1990$iso_3 != "HUN" & oecd_data_1990$iso_3 != "ISL" & oecd_data_1990$iso_3 != "ISR" & oecd_data_1990$iso_3 != "CHL" & oecd_data_1990$iso_3 != "LVA" & oecd_data_1990$iso_3 != "LTU" & oecd_data_1990$iso_3 != "SVK" & oecd_data_1990$iso_3 != "SVN" & oecd_data_1990$iso_3 != "POL"& oecd_data_1990$iso_3 != "MEX" & oecd_data_1990$iso_3 != "CHL"& oecd_data_1990$iso_3 != "KOR" & oecd_data_1990$iso_3 != "COL" & oecd_data_1990$iso_3 != "CRI")
                        
#Calculate averages for 1100 (individual taxes) for 1990 data
individual_1100_90 <- subset(oecd_data_1990, category==1100)
individual_1100_mean_90 <- mean(individual_1100_90$share, na.rm = TRUE)

#Calculate averages for 1200 (corporate taxes) for 1990 data
corporate_1200_90 <- subset(oecd_data_1990, category==1200)
corporate_1200_mean_90 <- mean(corporate_1200_90$share, na.rm = TRUE)

#Calculate averages for 2000 (social insurance taxes) for 1990 data
social_2000_90 <- subset(oecd_data_1990, category==2000)
social_2000_mean_90 <- mean(social_2000_90$share, na.rm = TRUE)

#Calculate averages for 4000 (property taxes) for 1990 data
property_4000_90 <- subset(oecd_data_1990, category==4000)
property_4000_mean_90 <- mean(property_4000_90$share, na.rm = TRUE)

#Calculate averages for 5000 (consumption taxes) for 1990 data
consumption_5000_90 <- subset(oecd_data_1990, category==5000)
consumption_5000_mean_90 <- mean(consumption_5000_90$share, na.rm = TRUE)

#Calculate averages for 1300 + 3000 + 6000 (other taxes) for 1990 data
other_90 <- subset(oecd_data_1990, category == 1300 | category == 3000 | category == 6000)
other_90 <- subset(other_90, select = -c(continent, oecd, year))

other_wide_90 <- reshape(other_90, 
                      timevar = "category",
                      idvar = c("iso_2","iso_3","country"),
                      direction = "wide")

colnames(other_wide_90)[colnames(other_wide_90)=="share.1300"] <- "1300"
colnames(other_wide_90)[colnames(other_wide_90)=="share.3000"] <- "3000"
colnames(other_wide_90)[colnames(other_wide_90)=="share.6000"] <- "6000"

other_wide_90[is.na(other_wide_90)] <- 0

other_wide_90$sum <- rowSums(other_wide_90[,c("1300", "3000", "6000")])

other_mean_90 <- mean(other_wide_90$sum, na.rm = TRUE)

#Compile averages into one dataframe
average_oecd_90 <- c(individual_1100_mean_90, corporate_1200_mean_90, social_2000_mean_90, property_4000_mean_90, consumption_5000_mean_90, other_mean_90)

oecd_averages_90 <- data.frame(tax_categories, average_oecd, average_oecd_90)

oecd_averages_90$average_oecd_90 <- round(oecd_averages_90$average_oecd_90, digits = 1)
oecd_averages_90$average_oecd <- round(oecd_averages_90$average_oecd, digits = 1)

colnames(oecd_averages_90)[colnames(oecd_averages_90)=="tax_categories"] <- "Tax Category"
colnames(oecd_averages_90)[colnames(oecd_averages_90)=="average_oecd"] <- "Average Share 2023"
colnames(oecd_averages_90)[colnames(oecd_averages_90)=="average_oecd_90"] <- "Average Share 1990"

write.csv(oecd_averages_90, "final-outputs/oecd_averages_1990.csv")

#Create table showing tax revenue shares for each OECD country####

oecd_data_2023_wide <- subset(oecd_data_2023, select = -c(continent, oecd, year, iso_2, iso_3))

oecd_data_2023_wide <- reshape(oecd_data_2023_wide, 
                               timevar = "category",
                               idvar = c("country"),
                               direction = "wide")

oecd_data_2023_wide <- subset(oecd_data_2023_wide, select = -c(share.1300, share.3000, share.6000))

oecd_data_2023_wide$Other <- other_wide$sum

oecd_data_2023_wide <- merge(oecd_data_2023_wide, country_names, by='country')
oecd_data_2023_wide <- subset(oecd_data_2023_wide, select = -c(continent))

colnames(oecd_data_2023_wide)[colnames(oecd_data_2023_wide)=="country"] <- "Country"
colnames(oecd_data_2023_wide)[colnames(oecd_data_2023_wide)=="share.1100"] <- "Individual Taxes"
colnames(oecd_data_2023_wide)[colnames(oecd_data_2023_wide)=="share.1200"] <- "Corporate Taxes"
colnames(oecd_data_2023_wide)[colnames(oecd_data_2023_wide)=="share.2000"] <- "Social Insurance Taxes"
colnames(oecd_data_2023_wide)[colnames(oecd_data_2023_wide)=="share.4000"] <- "Property Taxes"
colnames(oecd_data_2023_wide)[colnames(oecd_data_2023_wide)=="share.5000"] <- "Consumption Taxes"

oecd_data_2023_wide[,c('Individual Taxes', 'Corporate Taxes', 'Social Insurance Taxes', 'Property Taxes', 'Consumption Taxes', 'Other')] <- round(oecd_data_2023_wide[,c('Individual Taxes', 'Corporate Taxes', 'Social Insurance Taxes', 'Property Taxes', 'Consumption Taxes', 'Other')], digits = 1)
oecd_data_2023_wide <- oecd_data_2023_wide[c("iso_2", "iso_3", "Country", "Individual Taxes", "Corporate Taxes", "Social Insurance Taxes", "Property Taxes", "Consumption Taxes", "Other")]

#Add OECD Average to table
oecd_average<-c("NA","NA","OECD Average",round(individual_1100_mean, digits = 1)
                , round(corporate_1200_mean, digits = 1), round(social_2000_mean, digits = 1)
                , round(property_4000_mean, digits = 1), round(consumption_5000_mean, digits = 1), 
                round(other_mean, digits = 1))
oecd_data_2023_wide<-rbind(oecd_data_2023_wide,oecd_average)

write.csv(oecd_data_2023_wide, "final-outputs/oecd_by_country.csv", row.names = FALSE)

#Chile other taxes is is -1 and the total of the rest is 101*** 

###STOP IF JUST UPDATING OECD COUNTRIES***

#Graph comparing tax revenue shares by region####

#Get non-OECD data for 2019 (2020 data not available for non-OECD countries as of January 2022)

#Reading in and cleaning OECD's Global Revenue Statistics dataset to get data for NoN-OECD Countries

#dataset <- ("RS_GBL")

#dstruc <- get_data_structure(dataset)
#str(dstruc, max.level = 1)
#dstruc$VAR
#dstruc$TAX
#dstruc$GOV
#dstruc$YEA


#all_data_NON_OECD <- get_dataset("RS_GBL", filter= list(c(),c("NES"),c(taxes),c("TAXPER")),start_time = 2018)

#Drop redundant columns
#all_data_NON_OECD <- subset(all_data_NON_OECD, select=-c(GOV,VAR,TIME_FORMAT))

#Rename columns
#colnames(all_data_NON_OECD)[colnames(all_data_NON_OECD)=="COU"] <- "iso_3"
#colnames(all_data_NON_OECD)[colnames(all_data_NON_OECD)=="TAX"] <- "category"
#colnames(all_data_NON_OECD)[colnames(all_data_NON_OECD)=="obsTime"] <- "year"
#colnames(all_data_NON_OECD)[colnames(all_data_NON_OECD)=="obsValue"] <- "share"

#Match country names with ISO-3 codes

#Add country names and continents to all_data_NON_OECD, and add variable signaling OECD countries, including Colombia
#all_data_NON_OECD <- merge(all_data_NON_OECD, country_names, by='iso_3')

#all_data_NON_OECD$oecd <- ifelse(all_data_NON_OECD$iso_3 == "AUS"
#                       | all_data_NON_OECD$iso_3 == "AUT"
#                        | all_data_NON_OECD$iso_3 == "BEL"
#                        | all_data_NON_OECD$iso_3 == "CAN"
#                        | all_data_NON_OECD$iso_3 == "CHL"
#                        |all_data_NON_OECD$iso_3 == "COL"
#                        |all_data_NON_OECD$iso_3 == "CRI"
#                       | all_data_NON_OECD$iso_3 == "CZE"
#                        | all_data_NON_OECD$iso_3 == "DNK"
#                        | all_data_NON_OECD$iso_3 == "EST"
#                       | all_data_NON_OECD$iso_3 == "FIN"
#                        | all_data_NON_OECD$iso_3 == "FRA"
#                        | all_data_NON_OECD$iso_3 == "DEU"
#                        | all_data_NON_OECD$iso_3 == "GRC"
#                        | all_data_NON_OECD$iso_3 == "HUN"
#                        | all_data_NON_OECD$iso_3 == "ISL"
#                        | all_data_NON_OECD$iso_3 == "IRL"
#                        | all_data_NON_OECD$iso_3 == "ISR"
#                        | all_data_NON_OECD$iso_3 == "ITA"
#                        | all_data_NON_OECD$iso_3 == "JPN"
#                        | all_data_NON_OECD$iso_3 == "KOR"
#                        | all_data_NON_OECD$iso_3 == "LTU"
#                        | all_data_NON_OECD$iso_3 == "LUX"
#                        | all_data_NON_OECD$iso_3 == "LVA"
#                        | all_data_NON_OECD$iso_3 == "MEX"
#                        | all_data_NON_OECD$iso_3 == "NLD"
#                        | all_data_NON_OECD$iso_3 == "NZL"
#                        | all_data_NON_OECD$iso_3 == "NOR"
#                        | all_data_NON_OECD$iso_3 == "POL"
#                        | all_data_NON_OECD$iso_3 == "PRT"
#                        | all_data_NON_OECD$iso_3 == "SVK"
#                        | all_data_NON_OECD$iso_3 == "SVN"
#                        | all_data_NON_OECD$iso_3 == "ESP"
#                        | all_data_NON_OECD$iso_3 == "SWE"
#                        | all_data_NON_OECD$iso_3 == "CHE"
#                        | all_data_NON_OECD$iso_3 == "TUR"
#                        | all_data_NON_OECD$iso_3 == "GBR"
#                        | all_data_NON_OECD$iso_3 == "USA"
#                        ,1,0)

#Adjust the order of the columns
#all_data_NON_OECD <- all_data_NON_OECD[c("iso_2", "iso_3", "country", "continent", "oecd", "year", "category", "share")]

#Select only NON-OECD countries from all_data (2023 is not available only 2022)####
non_oecd_data <- subset(all_data, subset = oecd == 0)

#Fix country name that was read in incorrectly
non_oecd_data$country <- as.character(non_oecd_data$country)
non_oecd_data[non_oecd_data$iso_3 == "CIV", "country"] <- "Cote d'Ivoire"

write.csv(non_oecd_data, "intermediate-outputs/non_oecd_data_preliminary.csv")

#Fix non-OECD countries for which some 2022 data is missing

#Ecuador: The OECD dataset provides the tax revenue shares for the categories 1100, 1200, and 1300 only in currency values (as opposed to as a share of total revenue). Thus, shares had to be calculated
missing_ecuador <- data.frame(iso_2 = c("EC"), iso_3 = c("ECU"), country = c("Ecuador"), continent = c("SA"), oecd = c(0), year = c(2022), category = c(1100, 1200, 1300), share = c(0.7538, 5.6066, 15.7212))

#For the following countries, the OECD does not provide data for some tax categories. However, the sum of the categories that do contain data equals the total amount of taxes raised. As a result, the categories with missing data are set to zero.

#3000:Armenia
missing_armenia <- data.frame(iso_2 = c("AM"), iso_3 = c("ARM"), country = c("Armenia"), continent = c("AS"), oecd = c(0), year = c(2022), category = c(3000), share = c(0))

#1100 y 1200: Botswana
missing_botswana <- data.frame(iso_2 = c("BW"), iso_3 = c("BWA"), country = c("Botswana"), continent = c("AF"), oecd = c(0), year = c(2022), category = c(1100), share = c(0))
missing_botswana_02 <- data.frame(iso_2 = c("BW"), iso_3 = c("BWA"), country = c("Botswana"), continent = c("AF"), oecd = c(0), year = c(2022), category = c(1200), share = c(0))

#2000: Cambodia, Chad
missing_cambodia <- data.frame(iso_2 = c("KH"), iso_3 = c("KHM"), country = c("Cambodia"), continent = c("AS"), oecd = c(0), year = c(2022), category = c(2000), share = c(0))
missing_chad <- data.frame(iso_2 = c("TD"), iso_3 = c("TCD"), country = c("Chad"), continent = c("AF"), oecd = c(0), year = c(2022), category = c(2000), share = c(0))

#2000: Guinea, Kyrgyzstan, Lao People's Democratic Republic
missing_guinea <- data.frame(iso_2 = c("GN"), iso_3 = c("GNI"), country = c("Guinea"), continent = c("AF"), oecd = c(0), year = c(2022), category = c(2000), share = c(0))
missing_kyrgyzstan <- data.frame(iso_2 = c("KG"), iso_3 = c("KGZ"), country = c("Kyrgyzstan"), continent = c("AS"), oecd = c(0), year = c(2022), category = c(2000), share = c(0))
missing_lao <- data.frame(iso_2 = c("LA"), iso_3 = c("LAO"), country = c("Lao People's Democratic Republic"), continent = c("AS"), oecd = c(0), year = c(2022), category = c(2000), share = c(0))

#4000: Lesotho
missing_lesotho <- data.frame(iso_2 = c("LS"), iso_3 = c("LSO"), country = c("Lesotho"), continent = c("AF"), oecd = c(0), year = c(2022), category = c(4000), share = c(0))

#3000: Liechtenstein
missing_liechtenstein <- data.frame(iso_2 = c("LI"), iso_3 = c("LIE"), country = c("Liechtenstein"), continent = c("EU"), oecd = c(0), year = c(2022), category = c(3000), share = c(0))

#4000: Malawi
missing_malawi <- data.frame(iso_2 = c("MW"), iso_3 = c("MWI"), country = c("Malawi"), continent = c("AF"), oecd = c(0), year = c(2022), category = c(4000), share = c(0))

#3000: Maldives
missing_maldives <- data.frame(iso_2 = c("MV"), iso_3 = c("MDV"), country = c("Maldives"), continent = c("AS"), oecd = c(0), year = c(2022), category = c(3000), share = c(0))

#1100 y 1200: Pakistan
missing_pakistan<- data.frame(iso_2 = c("PK"), iso_3 = c("PAK"), country = c("Pakistan"), continent = c("AS"), oecd = c(0), year = c(2022), category = c(1100), share = c(0))
missing_pakistan_02<- data.frame(iso_2 = c("PK"), iso_3 = c("PAK"), country = c("Pakistan"), continent = c("AS"), oecd = c(0), year = c(2022), category = c(1200), share = c(0))

#4000: Sierra Leone
missing_sierra_leone <- data.frame(iso_2 = c("SL"), iso_3 = c("SLE"), country = c("Sierra Leone"), continent = c("AF"), oecd = c(0), year = c(2022), category = c(4000), share = c(0))	

#3000:Thailand
missing_thailand <- data.frame(iso_2 = c("TH"), iso_3 = c("THA"), country = c("Thailand"), continent = c("AS"), oecd = c(0), year = c(2022), category = c(3000), share = c(0))

#category :2000:Togo, Uganda, Zambia
missing_togo <- data.frame(iso_2 = c("TG"), iso_3 = c("TGO"), country = c("Togo"), continent = c("AF"), oecd = c(0), year = c(2022), category = c(2000), share = c(0))
missing_uganda <- data.frame(iso_2 = c("UG"), iso_3 = c("UGA"), country = c("Uganda"), continent = c("AF"), oecd = c(0), year = c(2022), category = c(2000), share = c(0))
missing_zambia <- data.frame(iso_2 = c("ZM"), iso_3 = c("ZMB"), country = c("Zambia"), continent = c("AF"), oecd = c(0), year = c(2022), category = c(2000), share = c(0))

#Jamaica: The OECD dataset does not provide data for the category 1300. It was calculated as a residual (total revenue minus all other shares).
#missing_jamaica <- data.frame(iso_2 = c("JM"), iso_3 = c("JAM"), country = c("Jamaica"), continent = c("NO"), oecd = c(0), year = c(2018), category = c(1300), share = c(8.5300))

#Nicaragua: The OECD dataset provides the tax revenue for the category 1300 only in currency values (as opposed to as a share of total revenue). Thus, shares had to be calculated.
#missing_nicaragua <- data.frame(iso_2 = c("NI"), iso_3 = c("NIC"), country = c("Nicaragua"), continent = c("NO"), oecd = c(0), year = c(2018), category = c(1300), share = c(30.7441733))

#Liechtenstein
#missing_liechtenstein <- data.frame(iso_2 = c("LI"), iso_3 = c("LIE"), country = c("Liechtenstein"), continent = c("EU"), oecd = c(0), year = c(2018), category = c(3000), share = c(0))

#Philippines
#missing_philippines <- data.frame(iso_2 = c("PH"), iso_3 = c("PHL"), country = c("Philippines"), continent = c("As"), oecd = c(0), year = c(2018), category = c(3000), share = c(0))

#Put all rows into one dataframe
non_oecd_data <- rbind(non_oecd_data, missing_ecuador,missing_armenia,missing_botswana,missing_botswana_02, missing_cambodia, missing_chad, missing_guinea,missing_kyrgyzstan,missing_lao,missing_lesotho,missing_liechtenstein,missing_malawi,missing_maldives,missing_pakistan,missing_pakistan_02,missing_sierra_leone,missing_thailand,missing_togo,missing_uganda,missing_zambia )

#Combine non-OECD and OECD countries into one dataframe
oecd_and_non_oecd <- rbind(oecd_data_2023, non_oecd_data)

#Change the continent assigned to Turkey from Asia to Europe
oecd_and_non_oecd[oecd_and_non_oecd$country == "Turkey", "continent"] <- "EU"

#Calculate regional averages

#Africa

#Calculate averages for 1100 (individual taxes)
individual_1100_af <- subset(oecd_and_non_oecd, category==1100 & continent == "AF")
individual_1100_af_mean <- mean(individual_1100_af$share, na.rm = TRUE)

#Calculate averages for 1200 (corporate taxes)
corporate_1200_af <- subset(oecd_and_non_oecd, category==1200 & continent == "AF")
corporate_1200_af_mean <- mean(corporate_1200_af$share, na.rm = TRUE)

#Calculate averages for 2000 (social insurance taxes)
social_2000_af <- subset(oecd_and_non_oecd, category==2000 & continent == "AF")
social_2000_af_mean <- mean(social_2000_af$share, na.rm = TRUE)

#Calculate averages for 4000 (property taxes)
property_4000_af <- subset(oecd_and_non_oecd, category==4000 & continent == "AF")
property_4000_af_mean <- mean(property_4000_af$share, na.rm = TRUE)

#Calculate averages for 5000 (consumption taxes)
consumption_5000_af <- subset(oecd_and_non_oecd, category==5000 & continent == "AF")
consumption_5000_af_mean <- mean(consumption_5000_af$share, na.rm = TRUE)

#Calculate averages for 1300 + 3000 + 6000 (Other)
other_af <- subset(oecd_and_non_oecd, category == 1300 | category == 3000 | category == 6000)
other_af <- subset(other_af, continent == "AF")
other_af <- subset(other_af, select = -c(continent, oecd, year))

other_wide_af <- reshape(other_af, 
                      timevar = "category",
                      idvar = c("iso_2","iso_3","country"),
                      direction = "wide")

colnames(other_wide_af)[colnames(other_wide_af)=="share.1300"] <- "1300"
colnames(other_wide_af)[colnames(other_wide_af)=="share.3000"] <- "3000"
colnames(other_wide_af)[colnames(other_wide_af)=="share.6000"] <- "6000"

other_wide_af[is.na(other_wide_af)] <- 0

other_wide_af$sum <- rowSums(other_wide_af[,c("1300", "3000", "6000")])

other_af_mean <- mean(other_wide_af$sum, na.rm = TRUE)

#Compile averages into one dataframe
average_africa <- c(individual_1100_af_mean, corporate_1200_af_mean, social_2000_af_mean, property_4000_af_mean, consumption_5000_af_mean, other_af_mean)

africa_averages <- data.frame(tax_categories, average_africa)

africa_averages$average_africa <- round(africa_averages$average_africa, digits = 1)

colnames(africa_averages)[colnames(africa_averages)=="tax_categories"] <- "Tax Category"
colnames(africa_averages)[colnames(africa_averages)=="average_africa"] <- "Average Share Africa"

#Asia

#Calculate averages for 1100 (individual taxes)
individual_1100_as <- subset(oecd_and_non_oecd, category==1100 & continent == "AS")
individual_1100_as_mean <- mean(individual_1100_as$share, na.rm = TRUE)

#Calculate averages for 1200 (corporate taxes)
corporate_1200_as <- subset(oecd_and_non_oecd, category==1200 & continent == "AS")
corporate_1200_as_mean <- mean(corporate_1200_as$share, na.rm = TRUE)

#Calculate averages for 2000 (social insurance taxes)
social_2000_as <- subset(oecd_and_non_oecd, category==2000 & continent == "AS")
social_2000_as_mean <- mean(social_2000_as$share, na.rm = TRUE)

#Calculate averages for 4000 (property taxes)
property_4000_as <- subset(oecd_and_non_oecd, category==4000 & continent == "AS")
property_4000_as_mean <- mean(property_4000_as$share, na.rm = TRUE)

#Calculate averages for 5000 (consumption taxes)
consumption_5000_as <- subset(oecd_and_non_oecd, category==5000 & continent == "AS")
consumption_5000_as_mean <- mean(consumption_5000_as$share, na.rm = TRUE)

#Calculate averages for 1300 + 3000 + 6000 (Other)
other_as <- subset(oecd_and_non_oecd, category == 1300 | category == 3000 | category == 6000)
other_as <- subset(other_as, continent == "AS")
other_as <- subset(other_as, select = -c(continent, oecd, year))

other_wide_as <- reshape(other_as, 
                         timevar = "category",
                         idvar = c("iso_2","iso_3","country"),
                         direction = "wide")

colnames(other_wide_as)[colnames(other_wide_as)=="share.1300"] <- "1300"
colnames(other_wide_as)[colnames(other_wide_as)=="share.3000"] <- "3000"
colnames(other_wide_as)[colnames(other_wide_as)=="share.6000"] <- "6000"


other_wide_as[is.na(other_wide_as)] <- 0

other_wide_as$sum <- rowSums(other_wide_as[,c("1300", "3000", "6000")])

other_as_mean <- mean(other_wide_as$sum, na.rm = TRUE)

#Compile averages into one dataframe
average_asia <- c(individual_1100_as_mean, corporate_1200_as_mean, social_2000_as_mean, property_4000_as_mean, consumption_5000_as_mean, other_as_mean)

asia_averages <- data.frame(tax_categories, average_asia)

asia_averages$average_asia <- round(asia_averages$average_asia, digits = 1)

colnames(asia_averages)[colnames(asia_averages)=="tax_categories"] <- "Tax Category"
colnames(asia_averages)[colnames(asia_averages)=="average_asia"] <- "Average Share Asia"

#Europe

#Calculate averages for 1100 (individual taxes)
individual_1100_eu <- subset(oecd_and_non_oecd, category==1100 & continent == "EU")
individual_1100_eu_mean <- mean(individual_1100_eu$share, na.rm = TRUE)

#Calculate averages for 1200 (corporate taxes)
corporate_1200_eu <- subset(oecd_and_non_oecd, category==1200 & continent == "EU")
corporate_1200_eu_mean <- mean(corporate_1200_eu$share, na.rm = TRUE)

#Calculate averages for 2000 (social insurance taxes)
social_2000_eu <- subset(oecd_and_non_oecd, category==2000 & continent == "EU")
social_2000_eu_mean <- mean(social_2000_eu$share, na.rm = TRUE)

#Calculate averages for 4000 (property taxes)
property_4000_eu <- subset(oecd_and_non_oecd, category==4000 & continent == "EU")
property_4000_eu_mean <- mean(property_4000_eu$share, na.rm = TRUE)

#Calculate averages for 5000 (consumption taxes)
consumption_5000_eu <- subset(oecd_and_non_oecd, category==5000 & continent == "EU")
consumption_5000_eu_mean <- mean(consumption_5000_eu$share, na.rm = TRUE)

#Calculate averages for 1300 + 3000 + 6000(Other)
other_eu <- subset(oecd_and_non_oecd, category == 1300 | category == 3000 | category == 6000)
other_eu <- subset(other_eu, continent == "EU")
other_eu <- subset(other_eu, select = -c(continent, oecd, year))

other_wide_eu <- reshape(other_eu, 
                         timevar = "category",
                         idvar = c("iso_2","iso_3","country"),
                         direction = "wide")

colnames(other_wide_eu)[colnames(other_wide_eu)=="share.1300"] <- "1300"
colnames(other_wide_eu)[colnames(other_wide_eu)=="share.3000"] <- "3000"
colnames(other_wide_eu)[colnames(other_wide_eu)=="share.6000"] <- "6000"

other_wide_eu[is.na(other_wide_eu)] <- 0

other_wide_eu$sum <- rowSums(other_wide_eu[,c("1300", "3000", "6000")])

other_eu_mean <- mean(other_wide_eu$sum, na.rm = TRUE)

#Compile averages into one dataframe
average_europe <- c(individual_1100_eu_mean, corporate_1200_eu_mean, social_2000_eu_mean, property_4000_eu_mean, consumption_5000_eu_mean, other_eu_mean)

europe_averages <- data.frame(tax_categories, average_europe)

europe_averages$average_europe <- round(europe_averages$average_europe, digits = 1)

colnames(europe_averages)[colnames(europe_averages)=="tax_categories"] <- "Tax Category"
colnames(europe_averages)[colnames(europe_averages)=="average_europe"] <- "Average Share Europe"

#North America

#Calculate averages for 1100 (individual taxes)
individual_1100_no <- subset(oecd_and_non_oecd, category==1100 & continent == "NO")
individual_1100_no_mean <- mean(individual_1100_no$share, na.rm = TRUE)

#Calculate averages for 1200 (corporate taxes)
corporate_1200_no <- subset(oecd_and_non_oecd, category==1200 & continent == "NO")
corporate_1200_no_mean <- mean(corporate_1200_no$share, na.rm = TRUE)

#Calculate averages for 2000 (social insurance taxes)
social_2000_no <- subset(oecd_and_non_oecd, category==2000 & continent == "NO")
social_2000_no_mean <- mean(social_2000_no$share, na.rm = TRUE)

#Calculate averages for 4000 (property taxes)
property_4000_no <- subset(oecd_and_non_oecd, category==4000 & continent == "NO")
property_4000_no_mean <- mean(property_4000_no$share, na.rm = TRUE)

#Calculate averages for 5000 (consumption taxes)
consumption_5000_no <- subset(oecd_and_non_oecd, category==5000 & continent == "NO")
consumption_5000_no_mean <- mean(consumption_5000_no$share, na.rm = TRUE)

#Calculate averages for 1300 + 3000 + 6000 (Other)
other_no <- subset(oecd_and_non_oecd, category == 1300 | category == 3000 | category == 6000)
other_no <- subset(other_no, continent == "NO")
other_no <- subset(other_no, select = -c(continent, oecd, year))

other_wide_no <- reshape(other_no, 
                         timevar = "category",
                         idvar = c("iso_2","iso_3","country"),
                         direction = "wide")

colnames(other_wide_no)[colnames(other_wide_no)=="share.1300"] <- "1300"
colnames(other_wide_no)[colnames(other_wide_no)=="share.3000"] <- "3000"
colnames(other_wide_no)[colnames(other_wide_no)=="share.6000"] <- "6000"

other_wide_no[is.na(other_wide_no)] <- 0

other_wide_no$sum <- rowSums(other_wide_no[,c("1300", "3000", "6000")])

other_no_mean <- mean(other_wide_no$sum, na.rm = TRUE)

#Compile averages into one dataframe
average_namerica <- c(individual_1100_no_mean, corporate_1200_no_mean, social_2000_no_mean, property_4000_no_mean, consumption_5000_no_mean, other_no_mean)

namerica_averages <- data.frame(tax_categories, average_namerica)

namerica_averages$average_namerica <- round(namerica_averages$average_namerica, digits = 1)

colnames(namerica_averages)[colnames(namerica_averages)=="tax_categories"] <- "Tax Category"
colnames(namerica_averages)[colnames(namerica_averages)=="average_namerica"] <- "Average Share North America"

#Oceania

#Calculate averages for 1100 (individual taxes)
individual_1100_oc <- subset(oecd_and_non_oecd, category==1100 & continent == "OC")
individual_1100_oc_mean <- mean(individual_1100_oc$share, na.rm = TRUE)

#Calculate averages for 1200 (corporate taxes)
corporate_1200_oc <- subset(oecd_and_non_oecd, category==1200 & continent == "OC")
corporate_1200_oc_mean <- mean(corporate_1200_oc$share, na.rm = TRUE)

#Calculate averages for 2000 (social insurance taxes)
social_2000_oc <- subset(oecd_and_non_oecd, category==2000 & continent == "OC")
social_2000_oc_mean <- mean(social_2000_oc$share, na.rm = TRUE)

#Calculate averages for 4000 (property taxes)
property_4000_oc <- subset(oecd_and_non_oecd, category==4000 & continent == "OC")
property_4000_oc_mean <- mean(property_4000_oc$share, na.rm = TRUE)

#Calculate averages for 5000 (consumption taxes)
consumption_5000_oc <- subset(oecd_and_non_oecd, category==5000 & continent == "OC")
consumption_5000_oc_mean <- mean(consumption_5000_oc$share, na.rm = TRUE)

#Calculate averages for 1300 + 3000 + 6000 (Other)
other_oc <- subset(oecd_and_non_oecd, category == 1300 | category == 3000 | category == 6000)
other_oc <- subset(other_oc, continent == "OC")
other_oc <- subset(other_oc, select = -c(continent, oecd, year))

other_wide_oc <- reshape(other_oc, 
                         timevar = "category",
                         idvar = c("iso_2","iso_3","country"),
                         direction = "wide")

colnames(other_wide_oc)[colnames(other_wide_oc)=="share.1300"] <- "1300"
colnames(other_wide_oc)[colnames(other_wide_oc)=="share.3000"] <- "3000"
colnames(other_wide_oc)[colnames(other_wide_oc)=="share.6000"] <- "6000"

other_wide_oc[is.na(other_wide_oc)] <- 0

other_wide_oc$sum <- rowSums(other_wide_oc[,c("1300", "3000", "6000")])

other_oc_mean <- mean(other_wide_oc$sum, na.rm = TRUE)

#Compile averages into one dataframe
average_oceania <- c(individual_1100_oc_mean, corporate_1200_oc_mean, social_2000_oc_mean, property_4000_oc_mean, consumption_5000_oc_mean, other_oc_mean)

oceania_averages <- data.frame(tax_categories, average_oceania)

oceania_averages$average_oceania <- round(oceania_averages$average_oceania, digits = 1)

colnames(oceania_averages)[colnames(oceania_averages)=="tax_categories"] <- "Tax Category"
colnames(oceania_averages)[colnames(oceania_averages)=="average_oceania"] <- "Average Share Oceania"

#South America

#Calculate averages for 1100 (individual taxes)
individual_1100_sa <- subset(oecd_and_non_oecd, category==1100 & continent == "SA")
individual_1100_sa_mean <- mean(individual_1100_sa$share, na.rm = TRUE)

#Calculate averages for 1200 (corporate taxes)
corporate_1200_sa <- subset(oecd_and_non_oecd, category==1200 & continent == "SA")
corporate_1200_sa_mean <- mean(corporate_1200_sa$share, na.rm = TRUE)

#Calculate averages for 2000 (social insurance taxes)
social_2000_sa <- subset(oecd_and_non_oecd, category==2000 & continent == "SA")
social_2000_sa_mean <- mean(social_2000_sa$share, na.rm = TRUE)

#Calculate averages for 4000 (property taxes)
property_4000_sa <- subset(oecd_and_non_oecd, category==4000 & continent == "SA")
property_4000_sa_mean <- mean(property_4000_sa$share, na.rm = TRUE)

#Calculate averages for 5000 (consumption taxes)
consumption_5000_sa <- subset(oecd_and_non_oecd, category==5000 & continent == "SA")
consumption_5000_sa_mean <- mean(consumption_5000_sa$share, na.rm = TRUE)

#Calculate averages for 1300 + 3000 + 6000(Other)
other_sa <- subset(oecd_and_non_oecd, category == 1300 | category == 3000 | category == 6000)
other_sa <- subset(other_sa, continent == "SA")
other_sa <- subset(other_sa, select = -c(continent, oecd, year))

other_wide_sa <- reshape(other_sa, 
                         timevar = "category",
                         idvar = c("iso_2","iso_3","country"),
                         direction = "wide")

colnames(other_wide_sa)[colnames(other_wide_sa)=="share.1300"] <- "1300"
colnames(other_wide_sa)[colnames(other_wide_sa)=="share.3000"] <- "3000"
colnames(other_wide_sa)[colnames(other_wide_sa)=="share.6000"] <- "6000"

other_wide_sa[is.na(other_wide_sa)] <- 0

other_wide_sa$sum <- rowSums(other_wide_sa[,c("1300", "3000", "6000")])

other_sa_mean <- mean(other_wide_sa$sum, na.rm = TRUE)

#Compile averages into one dataframe
average_samerica <- c(individual_1100_sa_mean, corporate_1200_sa_mean, social_2000_sa_mean, property_4000_sa_mean, consumption_5000_sa_mean, other_sa_mean)

samerica_averages <- data.frame(tax_categories, average_samerica)

samerica_averages$average_samerica <- round(samerica_averages$average_samerica, digits = 1)

colnames(samerica_averages)[colnames(samerica_averages)=="tax_categories"] <- "Tax Category"
colnames(samerica_averages)[colnames(samerica_averages)=="average_samerica"] <- "Average Share South America"

#Combine regional averages into one dataframe
regional_averages <- data.frame(tax_categories, africa_averages$`Average Share Africa`, asia_averages$`Average Share Asia`, europe_averages$`Average Share Europe`, namerica_averages$`Average Share North America`, oceania_averages$`Average Share Oceania`, samerica_averages$`Average Share South America`, average_oecd)

regional_averages$average_oecd <- round(regional_averages$average_oecd, digits = 1)

colnames(regional_averages)[colnames(regional_averages)=="tax_categories"] <- "Tax Category"
colnames(regional_averages)[colnames(regional_averages)=="africa_averages..Average.Share.Africa."] <- "Africa"
colnames(regional_averages)[colnames(regional_averages)=="asia_averages..Average.Share.Asia."] <- "Asia"
colnames(regional_averages)[colnames(regional_averages)=="europe_averages..Average.Share.Europe."] <- "Europe"
colnames(regional_averages)[colnames(regional_averages)=="namerica_averages..Average.Share.North.America."] <- "North America"
colnames(regional_averages)[colnames(regional_averages)=="oceania_averages..Average.Share.Oceania."] <- "Oceania"
colnames(regional_averages)[colnames(regional_averages)=="samerica_averages..Average.Share.South.America."] <- "South America"
colnames(regional_averages)[colnames(regional_averages)=="average_oecd"] <- "OECD"

write.csv(regional_averages, "final-outputs/regional_averages.csv")

#This part has not been updated in 2025#########

#Reading in and cleaning OECD's Revenue Statistics - OECD countries by level of government####
#dataset_list <- get_datasets()
#search_dataset("Revenue Statistics - OECD countries: Comparative tables", data= dataset_list)
dataset <- ("REV")
#dstruc <- get_data_structure(dataset)
#str(dstruc, max.level = 1)
#dstruc$VAR
#dstruc$TAX
#dstruc$GOV
#dstruc$YEA

url = "https://sdmx.oecd.org/public/rest/data/OECD.CTP.TPS,DSD_REV_COMP_OECD@DF_RSOECD,1.1/AUS+AUT+BEL+CAN+CHL+COL+CRI+CZE+DNK+EST+FIN+FRA+DEU+HUN+ISL+IRL+ISR+ITA+JPN+KOR+LVA+LTU+LUX+MEX+NLD+NZL+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+TUR+GBR+USA+GRC..S1315+S1314+S1313+S1312+S1311+S13._T..XDC.A?startPeriod=2022&endPeriod=2023&dimensionAtObservation=AllDimensions&format=csvfilewithlabels"
lev_OECD<-read.csv(url)

#levgov<-c("SUPRA","FED","STATE","LOCAL","SOCSEC","NES")
#lev_OECD <- get_dataset("REV", filter= list(c(levgov),c("TOTALTAX"),c("TAXLOG")),start_time = 2018)

#Drop redundant columns
lev_OECD <- subset(lev_OECD, select=c(5,9,10,19,21,25))

#Calculate value according to UNIT_MULT
lev_OECD$value<- lev_OECD$OBS_VALUE*10^lev_OECD$UNIT_MULT

#Rename columns
colnames(lev_OECD)[colnames(lev_OECD)=="REF_AREA"] <- "iso_3"
colnames(lev_OECD)[colnames(lev_OECD)=="SECTOR"] <- "gov_sec"
colnames(lev_OECD)[colnames(lev_OECD)=="Institutional.sector"] <- "government"
colnames(lev_OECD)[colnames(lev_OECD)=="TIME_PERIOD"] <- "year"
colnames(lev_OECD)[colnames(lev_OECD)=="value"] <- "amount_national_cur"
#colnames(lev_OECD)[colnames(lev_OECD)=="obsValue"] <- "percenage"


#Add country names and continents to lev_OECD data
lev_OECD <- merge(lev_OECD, country_names, by='iso_3')

#Adjust the order of the columns
lev_OECD <- subset(lev_OECD, select=c(1,2,3,4,7,8,9,10))
lev_OECD <- lev_OECD[c("iso_2", "iso_3", "country", "continent", "year", "government", "gov_sec","amount_national_cur")]

lev_OECD <- subset(lev_OECD, iso_3 == "AUS" | iso_3 == "MEX" | iso_3 == "COL" | iso_3 == "AUT"| iso_3 == "BEL"| iso_3 == "CAN"| iso_3 == "ESP"| iso_3 == "DEU"| iso_3 == "CHE"| iso_3 == "USA")


#Australia: 2023 data not available -> use 2022 data
lev_australia <- lev_OECD
lev_australia <- subset(lev_australia, subset = iso_3 == "AUS" & year == "2022")
lev_australia[lev_australia$year == 2022, "year"] <- 2023

#Eliminate 2022 data for Australia from the dataset
lev_OECD <- subset(lev_OECD, !(iso_3 == "AUS" & year == "2022"))

lev_OECD <- rbind(lev_OECD, lev_australia)

#Limit data to 2023
lev_OECD <- subset(lev_OECD, subset = year == 2023)

lev_OECD_wide <- subset(lev_OECD, select = -c(continent, year, iso_2, iso_3, government))
lev_OECD_wide <-reshape(lev_OECD_wide,
                        timevar = "gov_sec",
                        idvar = c("country"),
                        direction = "wide")

lev_OECD_wide$LOCAL <- lev_OECD_wide$amount_national_cur.S1313/lev_OECD_wide$amount_national_cur.S13

lev_OECD_wide$SOSSEC <- lev_OECD_wide$amount_national_cur.S1314/lev_OECD_wide$amount_national_cur.S13*100
lev_OECD_wide$FED <- lev_OECD_wide$amount_national_cur.S1311/lev_OECD_wide$amount_national_cur.S13*100
lev_OECD_wide$STATE <- lev_OECD_wide$amount_national_cur.S1312/lev_OECD_wide$amount_national_cur.S13*100
lev_OECD_wide$SUPRA <- lev_OECD_wide$amount_national_cur.S1315/lev_OECD_wide$amount_national_cur.S13*100

#Supranational level of government to zero for non EU countries

lev_OECD_wide <- lev_OECD_wide %>%  mutate(SUPRA = ifelse(is.na(SUPRA), 0, SUPRA))

lev_OECD_wide<- subset(lev_OECD_wide, select = c(1,8,9,10,11,12))

#Convert from long to wide format

lev_OECD_long <- lev_OECD_wide %>%
  pivot_longer(cols = c("LOCAL", "SOSSEC", "FED", "STATE","SUPRA"),
               names_to = "government",
               values_to = "percentage")

#Calculate average for the 10 countries####

#Calculate averages for FED (Central Government)
lev_FED <- subset(lev_OECD_long, government=="FED")
lev_FED_mean <- mean(lev_FED$percentage, na.rm = TRUE)

#Calculate averages for STATE (State or Regional Government)
lev_STATE <- subset(lev_OECD_long, government=="STATE")
lev_STATE_mean <- mean(lev_STATE$percentage, na.rm = TRUE)

#Calculate averages for LOCAL (Local Government)
lev_LOCAL <- subset(lev_OECD_long, government=="LOCAL")
lev_LOCAL_mean <- mean(lev_LOCAL$percentage, na.rm = TRUE)

#Calculate averages for SOSSEC (Social Security Funds)
lev_SOSSEC <- subset(lev_OECD_long, government=="SOSSEC")
lev_SOSSEC_mean <- mean(lev_SOSSEC$percentage, na.rm = TRUE)

#Calculate averages for SUPRA (Supranational)
lev_SUPRA <- subset(lev_OECD_long, government=="SUPRA")
lev_SUPRA_mean <- mean(lev_SUPRA$percentage, na.rm = TRUE)

#Create table showing tax revenue by level of Government####

lev_OECD_means <- lev_OECD_wide


lev_OECD_means <- merge(lev_OECD_means, country_names, by='country')

lev_OECD_means <- subset(lev_OECD_means, select = -c(continent))

colnames(lev_OECD_means)[colnames(lev_OECD_means)=="country"] <- "Country"
colnames(lev_OECD_means)[colnames(lev_OECD_means)=="FED"] <- "Central Government"
colnames(lev_OECD_means)[colnames(lev_OECD_means)=="LOCAL"] <- "Local Government"
colnames(lev_OECD_means)[colnames(lev_OECD_means)=="SOSSEC"] <- "Social Security Funds"
colnames(lev_OECD_means)[colnames(lev_OECD_means)=="STATE"] <- "State or Regional Government"
colnames(lev_OECD_means)[colnames(lev_OECD_means)=="SUPRA"] <- "Supranational"

lev_OECD_means[,c("Central Government","State or Regional Government","Social Security Funds","Local Government","Supranational")] <- round(lev_OECD_means[,c("Central Government","State or Regional Government","Social Security Funds","Local Government","Supranational")], digits = 1)
lev_OECD_means <- lev_OECD_means[c("iso_2", "iso_3", "Country","Central Government","State or Regional Government","Social Security Funds","Local Government","Supranational")]

#Add countries average to table
lev_gov_average<-c("NA","NA","Average", round (lev_FED_mean, digits = 1), round (lev_STATE_mean, digits = 1), round (lev_SOSSEC_mean, digits = 1), round (lev_LOCAL_mean, digits = 1), round (lev_SUPRA_mean, digits = 1))
lev_OECD_means<-rbind(lev_OECD_means,lev_gov_average)

write.csv(lev_OECD_means, "final-outputs/level_of_government_oecd.csv", row.names = FALSE)
