####Sources of Government Revenue####

#Clear working environment#####
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



#Reading in and cleaning OECD's Revenue Statistics - OECD countries comparative tables dataset since 2019 data is not available in the OECD's Global Revenue Statistics dataset (as of February 2021)####
dataset_list <- get_datasets()
search_dataset("Revenue Statistics - OECD countries: Comparative tables", data= dataset_list)
dataset <- ("REV")
dstruc <- get_data_structure(dataset)
str(dstruc, max.level = 1)
dstruc$VAR
dstruc$TAX
dstruc$GOV
dstruc$YEA
taxes<-c("1100","1200","1300","2000","3000","4000","5000","6000")
all_data_OECD <- get_dataset("REV", filter= list(c("NES"),c(taxes),c("TAXPER")),start_time = 2018)
all_data_1990 <- get_dataset("REV", filter= list(c("NES"),c(taxes),c("TAXPER")),start_time = 1990, end_time = 1990)

#Drop redundant columns
all_data_OECD <- subset(all_data_OECD, select=-c(UNIT, POWERCODE))
all_data_1990 <- subset(all_data_1990, select=-c(UNIT, POWERCODE))

#Combine OECD data for 1990 with 2018 and 2019 in one dataset called all_data###
all_data <- rbind (all_data_OECD, all_data_1990)

#Drop redundant columns
all_data <- subset(all_data, select=-c(TIME_FORMAT, GOV, VAR))

#Rename columns
colnames(all_data)[colnames(all_data)=="COU"] <- "iso_3"
colnames(all_data)[colnames(all_data)=="TAX"] <- "category"
colnames(all_data)[colnames(all_data)=="obsTime"] <- "year"
colnames(all_data)[colnames(all_data)=="obsValue"] <- "share"


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
                                        |all_data$iso_3 == "COL"
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


write.csv(all_data, "intermediate-outputs/oecd_data_preliminary.csv")



#Fix countries for which 2019 data is not available (unless otherwise noted, 2018 data is used for these cases)####

#Greece: Greece doesn't have data for the categories 1100, 1200, and 1300 (only for 1000); so we take the average share of these categories of the three years prior (2016-2018) to weigh the current year
missing_greece <- data.frame(iso_2 = c("GR", "GR", "GR"), iso_3 = c("GRC","GRC", "GRC"), country = c("Greece","Greece", "Greece"), continent = c("EU", "EU", "EU"), oecd = c(1, 1, 1), year = c(2019, 2019, 2019), category = c(1100, 1200, 1300), share = c(14.976108977859, 5.38929911485138, 1.18927826140175))

# Sweden: change the data in "categorry== 6000" (other taxes) to 100% - (1100+1200+1300+2000+4000+5000) since the sum of all categories is not 100%
missing_sweden <- data.frame(iso_2 = c("SE", "SE", "SE"), iso_3 = c("SWE","SWE", "SWE"), country = c("Sweden","Sweden", "Sweden"), continent = c("EU", "EU", "EU"), oecd = c(1, 1, 1), year = c(1990, 2018, 2019), category = c(6000, 6000, 6000), share = c(0.576, 0.398, 0.396))

#Australia: 2019 data not available -> use 2018 data
missing_australia <- all_data
missing_australia <- subset(missing_australia, subset = iso_3 == "AUS" & year == "2018")
missing_australia[missing_australia$year == 2018, "year"] <- 2019

#Japan: 2019 data not available -> use 2018 data
missing_japan <- all_data
missing_japan <- subset(missing_japan, subset = iso_3 == "JPN" & year == "2018")
missing_japan[missing_japan$year == 2018, "year"] <- 2019

#Mexico: 2019 data not available -> use 2018 data
missing_mexico <- all_data
missing_mexico <- subset(missing_mexico, subset = iso_3 == "MEX" & year == "2018")
missing_mexico[missing_mexico$year == 2018, "year"] <- 2019


#Delete from the dataset the data for Greece and Sweden for the catagories that were recalculated
all_data <- subset(all_data, !(iso_3 == "GRC" & year == "2019" & category == 1100),)
all_data <- subset(all_data, !(iso_3 == "GRC" & year == "2019" & category == 1200),)
all_data <- subset(all_data, !(iso_3 == "GRC" & year == "2019" & category == 1300),)
all_data <- subset(all_data, !(iso_3 == "SWE" & category == 6000),)

#Combine data
all_data <- rbind(all_data, missing_greece, missing_australia, missing_japan, missing_mexico, missing_sweden)

#Sort dataset
all_data <- all_data[order(all_data$country, all_data$category, all_data$year),]


#Calculate average OECD tax revenue sources####

#Limit data to OECD countries and 2019
oecd_data_2019 <- all_data
oecd_data_2019 <- subset(oecd_data_2019, subset = year == 2019)
oecd_data_2019 <- subset(oecd_data_2019, subset = oecd == 1)

#Calculate averages for 1100 (individual taxes)
individual_1100 <- subset(oecd_data_2019, category==1100)
individual_1100_mean <- mean(individual_1100$share, na.rm = TRUE)

#Calculate averages for 1200 (corporate taxes)
corporate_1200 <- subset(oecd_data_2019, category==1200)
corporate_1200_mean <- mean(corporate_1200$share, na.rm = TRUE)

#Calculate averages for 2000 (social insurance taxes)
social_2000 <- subset(oecd_data_2019, category==2000)
social_2000_mean <- mean(social_2000$share, na.rm = TRUE)

#Calculate averages for 4000 (property taxes)
property_4000 <- subset(oecd_data_2019, category==4000)
property_4000_mean <- mean(property_4000$share, na.rm = TRUE)

#Calculate averages for 5000 (consumption taxes)
consumption_5000 <- subset(oecd_data_2019, category==5000)
consumption_5000_mean <- mean(consumption_5000$share, na.rm = TRUE)

#Calculate averages for 1300 + 3000 + 6000 (other)
other <- subset(oecd_data_2019, category == 1300 | category == 3000 | category == 6000)
other <- subset(other, select = -c(continent, oecd, year))

other_long <- reshape(other, 
             timevar = "category",
             idvar = c("iso_2","iso_3","country"),
             direction = "wide")

colnames(other_long)[colnames(other_long)=="share.1300"] <- "1300"
colnames(other_long)[colnames(other_long)=="share.3000"] <- "3000"
colnames(other_long)[colnames(other_long)=="share.6000"] <- "6000"

other_long[is.na(other_long)] <- 0

other_long$sum <- rowSums(other_long[,c("1300", "3000", "6000")])

other_mean <- mean(other_long$sum, na.rm = TRUE)

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



#Graph comparing OECD tax revenue shares in 1990 with 2019####

#Limit data to OECD countries and 1990
oecd_data_1990 <- all_data
oecd_data_1990 <- subset(oecd_data_1990, subset = year == 1990)
oecd_data_1990 <- subset(oecd_data_1990, subset = oecd == 1)

#Drop countries for which 1990 data is available but that were not part of the OECD in 1990 inlcuding Colombia
oecd_data_1990 <- subset(oecd_data_1990, oecd_data_1990$iso_3 != "CHL" & oecd_data_1990$iso_3 != "KOR" & oecd_data_1990$iso_3 != "MEX" & oecd_data_1990$iso_3 != "COL")
                         
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

other_long_90 <- reshape(other_90, 
                      timevar = "category",
                      idvar = c("iso_2","iso_3","country"),
                      direction = "wide")

colnames(other_long_90)[colnames(other_long_90)=="share.1300"] <- "1300"
colnames(other_long_90)[colnames(other_long_90)=="share.3000"] <- "3000"
colnames(other_long_90)[colnames(other_long_90)=="share.6000"] <- "6000"

other_long_90[is.na(other_long_90)] <- 0

other_long_90$sum <- rowSums(other_long_90[,c("1300", "3000", "6000")])

other_mean_90 <- mean(other_long_90$sum, na.rm = TRUE)

#Compile averages into one dataframe
average_oecd_90 <- c(individual_1100_mean_90, corporate_1200_mean_90, social_2000_mean_90, property_4000_mean_90, consumption_5000_mean_90, other_mean_90)

oecd_averages_90 <- data.frame(tax_categories, average_oecd, average_oecd_90)

oecd_averages_90$average_oecd_90 <- round(oecd_averages_90$average_oecd_90, digits = 1)
oecd_averages_90$average_oecd <- round(oecd_averages_90$average_oecd, digits = 1)

colnames(oecd_averages_90)[colnames(oecd_averages_90)=="tax_categories"] <- "Tax Category"
colnames(oecd_averages_90)[colnames(oecd_averages_90)=="average_oecd"] <- "Average Share 2019"
colnames(oecd_averages_90)[colnames(oecd_averages_90)=="average_oecd_90"] <- "Average Share 1990"

write.csv(oecd_averages_90, "final-outputs/oecd_averages_1990.csv")


#Create table showing tax revenue shares for each OECD country####

oecd_data_2019_long <- subset(oecd_data_2019, select = -c(continent, oecd, year, iso_2, iso_3))

oecd_data_2019_long <- reshape(oecd_data_2019_long, 
                               timevar = "category",
                               idvar = c("country"),
                               direction = "wide")

oecd_data_2019_long <- subset(oecd_data_2019_long, select = -c(share.1300, share.3000, share.6000))

oecd_data_2019_long$Other <- other_long$sum

oecd_data_2019_long <- merge(oecd_data_2019_long, country_names, by='country')
oecd_data_2019_long <- subset(oecd_data_2019_long, select = -c(continent))

colnames(oecd_data_2019_long)[colnames(oecd_data_2019_long)=="country"] <- "Country"
colnames(oecd_data_2019_long)[colnames(oecd_data_2019_long)=="share.1100"] <- "Individual Taxes"
colnames(oecd_data_2019_long)[colnames(oecd_data_2019_long)=="share.1200"] <- "Corporate Taxes"
colnames(oecd_data_2019_long)[colnames(oecd_data_2019_long)=="share.2000"] <- "Social Insurance Taxes"
colnames(oecd_data_2019_long)[colnames(oecd_data_2019_long)=="share.4000"] <- "Property Taxes"
colnames(oecd_data_2019_long)[colnames(oecd_data_2019_long)=="share.5000"] <- "Consumption Taxes"

oecd_data_2019_long[,c('Individual Taxes', 'Corporate Taxes', 'Social Insurance Taxes', 'Property Taxes', 'Consumption Taxes', 'Other')] <- round(oecd_data_2019_long[,c('Individual Taxes', 'Corporate Taxes', 'Social Insurance Taxes', 'Property Taxes', 'Consumption Taxes', 'Other')], digits = 1)
oecd_data_2019_long <- oecd_data_2019_long[c("iso_2", "iso_3", "Country", "Individual Taxes", "Corporate Taxes", "Social Insurance Taxes", "Property Taxes", "Consumption Taxes", "Other")]


#Add OECD Average to table
oecd_average<-c("NA","NA","OECD Average",round(individual_1100_mean, digits = 1)
                , round(corporate_1200_mean, digits = 1), round(social_2000_mean, digits = 1)
                , round(property_4000_mean, digits = 1), round(consumption_5000_mean, digits = 1), 
                round(other_mean, digits = 1))
oecd_data_2019_long<-rbind(oecd_data_2019_long,oecd_average)


write.csv(oecd_data_2019_long, "final-outputs/oecd_by_country.csv", row.names = FALSE)


#Graph comparing tax revenue shares by region####

#Get non-OECD data for 2018 (2019 data not available for non-OECD countries as of February 2021)

#Reading in and cleaning OECD's Global Revenue Statistics dataset to get data for NoN-OECD Countries

dataset <- ("RS_GBL")

dstruc <- get_data_structure(dataset)
str(dstruc, max.level = 1)
dstruc$VAR
dstruc$TAX
dstruc$GOV
dstruc$YEA


all_data_NON_OECD <- get_dataset("RS_GBL", filter= list(c(),c("NES"),c(taxes),c("TAXPER")),start_time = 2018)

#Drop redundant columns
all_data_NON_OECD <- subset(all_data_NON_OECD, select=-c(GOV,VAR,TIME_FORMAT))

#Rename columns
colnames(all_data_NON_OECD)[colnames(all_data_NON_OECD)=="COU"] <- "iso_3"
colnames(all_data_NON_OECD)[colnames(all_data_NON_OECD)=="TAX"] <- "category"
colnames(all_data_NON_OECD)[colnames(all_data_NON_OECD)=="obsTime"] <- "year"
colnames(all_data_NON_OECD)[colnames(all_data_NON_OECD)=="obsValue"] <- "share"

#Match country names with ISO-3 codes

#Add country names and continents to all_data_NON_OECD, and add variable signaling OECD countries, including Colombia
all_data_NON_OECD <- merge(all_data_NON_OECD, country_names, by='iso_3')

all_data_NON_OECD$oecd <- ifelse(all_data_NON_OECD$iso_3 == "AUS"
                        | all_data_NON_OECD$iso_3 == "AUT"
                        | all_data_NON_OECD$iso_3 == "BEL"
                        | all_data_NON_OECD$iso_3 == "CAN"
                        | all_data_NON_OECD$iso_3 == "CHL"
                        |all_data_NON_OECD$iso_3 == "COL"
                        | all_data_NON_OECD$iso_3 == "CZE"
                        | all_data_NON_OECD$iso_3 == "DNK"
                        | all_data_NON_OECD$iso_3 == "EST"
                        | all_data_NON_OECD$iso_3 == "FIN"
                        | all_data_NON_OECD$iso_3 == "FRA"
                        | all_data_NON_OECD$iso_3 == "DEU"
                        | all_data_NON_OECD$iso_3 == "GRC"
                        | all_data_NON_OECD$iso_3 == "HUN"
                        | all_data_NON_OECD$iso_3 == "ISL"
                        | all_data_NON_OECD$iso_3 == "IRL"
                        | all_data_NON_OECD$iso_3 == "ISR"
                        | all_data_NON_OECD$iso_3 == "ITA"
                        | all_data_NON_OECD$iso_3 == "JPN"
                        | all_data_NON_OECD$iso_3 == "KOR"
                        | all_data_NON_OECD$iso_3 == "LTU"
                        | all_data_NON_OECD$iso_3 == "LUX"
                        | all_data_NON_OECD$iso_3 == "LVA"
                        | all_data_NON_OECD$iso_3 == "MEX"
                        | all_data_NON_OECD$iso_3 == "NLD"
                        | all_data_NON_OECD$iso_3 == "NZL"
                        | all_data_NON_OECD$iso_3 == "NOR"
                        | all_data_NON_OECD$iso_3 == "POL"
                        | all_data_NON_OECD$iso_3 == "PRT"
                        | all_data_NON_OECD$iso_3 == "SVK"
                        | all_data_NON_OECD$iso_3 == "SVN"
                        | all_data_NON_OECD$iso_3 == "ESP"
                        | all_data_NON_OECD$iso_3 == "SWE"
                        | all_data_NON_OECD$iso_3 == "CHE"
                        | all_data_NON_OECD$iso_3 == "TUR"
                        | all_data_NON_OECD$iso_3 == "GBR"
                        | all_data_NON_OECD$iso_3 == "USA"
                        ,1,0)

#Adjust the order of the columns
all_data_NON_OECD <- all_data_NON_OECD[c("iso_2", "iso_3", "country", "continent", "oecd", "year", "category", "share")]

#Fix country name that was read in incorrectly
all_data_NON_OECD$country <- as.character(all_data_NON_OECD$country)
all_data_NON_OECD[all_data_NON_OECD$iso_3 == "CIV", "country"] <- "Cote d'Ivoire"

#Select only NON-OECD countries

non_oecd_data <- subset(all_data_NON_OECD, subset = oecd == 0)

write.csv(non_oecd_data, "intermediate-outputs/non_oecd_data_preliminary.csv")


#Fix non-OECD countries for which some 2018 data is missing

#Ecuador: The OECD dataset provides the tax revenue shares for the categories 1100, 1200, and 1300 only in currency values (as opposed to as a share of total revenue). Thus, shares had to be calculated.
missing_ecuador <- data.frame(iso_2 = c("EC"), iso_3 = c("ECU"), country = c("Ecuador"), continent = c("SA"), oecd = c(0), year = c(2018), category = c(1100, 1200, 1300), share = c(0.8664, 8.5020, 14.1357))

#Jamaica: The OECD dataset does not provide data for the category 1300. It was calculated as a residual (total revenue minus all other shares).
missing_jamaica <- data.frame(iso_2 = c("JM"), iso_3 = c("JAM"), country = c("Jamaica"), continent = c("NO"), oecd = c(0), year = c(2018), category = c(1300), share = c(8.5300))

#Nicaragua: The OECD dataset provides the tax revenue for the category 1300 only in currency values (as opposed to as a share of total revenue). Thus, shares had to be calculated.
missing_nicaragua <- data.frame(iso_2 = c("NI"), iso_3 = c("NIC"), country = c("Nicaragua"), continent = c("NO"), oecd = c(0), year = c(2018), category = c(1300), share = c(30.7441733))

#For the following countries, the OECD does not provide data for some tax categories. However, the sum of the categories that do contain data equals the total amount of taxes raised. As a result, the categories with missing data are set to zero.

#Liechtenstein
missing_liechtenstein <- data.frame(iso_2 = c("LI"), iso_3 = c("LIE"), country = c("Liechtenstein"), continent = c("EU"), oecd = c(0), year = c(2018), category = c(3000), share = c(0))

#Philippines
missing_philippines <- data.frame(iso_2 = c("PH"), iso_3 = c("PHL"), country = c("Philippines"), continent = c("As"), oecd = c(0), year = c(2018), category = c(3000), share = c(0))


#Put all rows into one dataframe
non_oecd_data <- rbind(non_oecd_data, missing_ecuador, missing_jamaica, missing_nicaragua, missing_liechtenstein, missing_philippines)

#Combine non-OECD and OECD countries into one dataframe
oecd_and_non_oecd <- rbind(oecd_data_2019, non_oecd_data)

#Change the continent assigned to Turkey from Asia to Europe (that's how it is done in the publication)
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

other_long_af <- reshape(other_af, 
                      timevar = "category",
                      idvar = c("iso_2","iso_3","country"),
                      direction = "wide")

colnames(other_long_af)[colnames(other_long_af)=="share.1300"] <- "1300"
colnames(other_long_af)[colnames(other_long_af)=="share.3000"] <- "3000"
colnames(other_long_af)[colnames(other_long_af)=="share.6000"] <- "6000"

other_long_af[is.na(other_long_af)] <- 0

other_long_af$sum <- rowSums(other_long_af[,c("1300", "3000", "6000")])

other_af_mean <- mean(other_long_af$sum, na.rm = TRUE)

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

other_long_as <- reshape(other_as, 
                         timevar = "category",
                         idvar = c("iso_2","iso_3","country"),
                         direction = "wide")

colnames(other_long_as)[colnames(other_long_as)=="share.1300"] <- "1300"
colnames(other_long_as)[colnames(other_long_as)=="share.3000"] <- "3000"
colnames(other_long_as)[colnames(other_long_as)=="share.6000"] <- "6000"


other_long_as[is.na(other_long_as)] <- 0

other_long_as$sum <- rowSums(other_long_as[,c("1300", "3000", "6000")])

other_as_mean <- mean(other_long_as$sum, na.rm = TRUE)

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

other_long_eu <- reshape(other_eu, 
                         timevar = "category",
                         idvar = c("iso_2","iso_3","country"),
                         direction = "wide")

colnames(other_long_eu)[colnames(other_long_eu)=="share.1300"] <- "1300"
colnames(other_long_eu)[colnames(other_long_eu)=="share.3000"] <- "3000"
colnames(other_long_eu)[colnames(other_long_eu)=="share.6000"] <- "6000"

other_long_eu[is.na(other_long_eu)] <- 0

other_long_eu$sum <- rowSums(other_long_eu[,c("1300", "3000", "6000")])

other_eu_mean <- mean(other_long_eu$sum, na.rm = TRUE)

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

other_long_no <- reshape(other_no, 
                         timevar = "category",
                         idvar = c("iso_2","iso_3","country"),
                         direction = "wide")

colnames(other_long_no)[colnames(other_long_no)=="share.1300"] <- "1300"
colnames(other_long_no)[colnames(other_long_no)=="share.3000"] <- "3000"
colnames(other_long_no)[colnames(other_long_no)=="share.6000"] <- "6000"

other_long_no[is.na(other_long_no)] <- 0

other_long_no$sum <- rowSums(other_long_no[,c("1300", "3000", "6000")])

other_no_mean <- mean(other_long_no$sum, na.rm = TRUE)

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

other_long_oc <- reshape(other_oc, 
                         timevar = "category",
                         idvar = c("iso_2","iso_3","country"),
                         direction = "wide")

colnames(other_long_oc)[colnames(other_long_oc)=="share.1300"] <- "1300"
colnames(other_long_oc)[colnames(other_long_oc)=="share.3000"] <- "3000"
colnames(other_long_oc)[colnames(other_long_oc)=="share.6000"] <- "6000"

other_long_oc[is.na(other_long_oc)] <- 0

other_long_oc$sum <- rowSums(other_long_oc[,c("1300", "3000", "6000")])

other_oc_mean <- mean(other_long_oc$sum, na.rm = TRUE)

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

other_long_sa <- reshape(other_sa, 
                         timevar = "category",
                         idvar = c("iso_2","iso_3","country"),
                         direction = "wide")

colnames(other_long_sa)[colnames(other_long_sa)=="share.1300"] <- "1300"
colnames(other_long_sa)[colnames(other_long_sa)=="share.3000"] <- "3000"
colnames(other_long_sa)[colnames(other_long_sa)=="share.6000"] <- "6000"

other_long_sa[is.na(other_long_sa)] <- 0

other_long_sa$sum <- rowSums(other_long_sa[,c("1300", "3000", "6000")])

other_sa_mean <- mean(other_long_sa$sum, na.rm = TRUE)

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


#Reading in and cleaning OECD's Revenue Statistics - OECD countries by level of government####
dataset_list <- get_datasets()
search_dataset("Revenue Statistics - OECD countries: Comparative tables", data= dataset_list)
dataset <- ("REV")
dstruc <- get_data_structure(dataset)
str(dstruc, max.level = 1)
dstruc$VAR
dstruc$TAX
dstruc$GOV
dstruc$YEA

levgov<-c("SUPRA","FED","STATE","LOCAL","SOCSEC","NES")
lev_OECD <- get_dataset("REV", filter= list(c(levgov),c("TOTALTAX"),c("TAXLOG")),start_time = 2018)

#Drop redundant columns
lev_OECD <- subset(lev_OECD, select=-c(UNIT,POWERCODE,TAX,TIME_FORMAT,VAR))

#Rename columns
colnames(lev_OECD)[colnames(lev_OECD)=="COU"] <- "iso_3"
colnames(lev_OECD)[colnames(lev_OECD)=="GOV"] <- "government"
colnames(lev_OECD)[colnames(lev_OECD)=="obsTime"] <- "year"
colnames(lev_OECD)[colnames(lev_OECD)=="obsValue"] <- "percentage"

#Add country names and continents to lev_OECD data
lev_OECD <- merge(lev_OECD, country_names, by='iso_3')

#Adjust the order of the columns
lev_OECD <- lev_OECD[c("iso_2", "iso_3", "country", "continent", "year", "government", "percentage")]

lev_OECD <- subset(lev_OECD, iso_3 == "AUS" | iso_3 == "MEX" | iso_3 == "COL" | iso_3 == "AUT"| iso_3 == "BEL"| iso_3 == "CAN"| iso_3 == "ESP"| iso_3 == "DEU"| iso_3 == "CHE"| iso_3 == "USA")

#Australia: 2019 data not available -> use 2018 data
lev_australia <- lev_OECD
lev_australia <- subset(lev_australia, subset = iso_3 == "AUS" & year == "2018")
lev_australia[lev_australia$year == 2018, "year"] <- 2019

#Mexico: 2019 data not available -> use 2018 data
lev_mexico <- lev_OECD
lev_mexico <- subset(lev_mexico, subset = iso_3 == "MEX" & year == "2018")
lev_mexico[lev_mexico$year == 2018, "year"] <- 2019

#Eliminate 2018 data for Mexico and Australia from the dataset
lev_OECD <- subset(lev_OECD, !(iso_3 == "AUS" & year == "2018"))
lev_OECD <- subset(lev_OECD, !(iso_3 == "MEX" & year == "2018"))

#For the following countries set Supranational level of government to zero

#Australia
lev_australia_supra <- data.frame(iso_2 = c("AU"), iso_3 = c("AUS"), country = c("Australia"), continent = c("OC"), year = c(2019), government = c("SUPRA"), percentage = c(0))

#Mexico
lev_mexico_supra <- data.frame(iso_2 = c("MX"), iso_3 = c("MEX"), country = c("Mexico"), continent = c("NO"), year = c(2019), government = c("SUPRA"), percentage = c(0))

#Switzerland
lev_switzerland_supra <- data.frame(iso_2 = c("CH"), iso_3 = c("CHE"), country = c("Switzerland"), continent = c("EU"), year = c(2019), government = c("SUPRA"), percentage = c(0))

#United States of America
lev_usa_supra <- data.frame(iso_2 = c("US"), iso_3 = c("USA"), country = c("United States of America"), continent = c("NO"), year = c(2019), government = c("SUPRA"), percentage = c(0))

#Canada
lev_canada_supra <- data.frame(iso_2 = c("CA"), iso_3 = c("CAN"), country = c("Canada"), continent = c("NO"), year = c(2019), government = c("SUPRA"), percentage = c(0))

#Colombia
lev_colombia_supra <- data.frame(iso_2 = c("CO"), iso_3 = c("COL"), country = c("Colombia"), continent = c("SA"), year = c(2019), government = c("SUPRA"), percentage = c(0))

#Combine data
lev_OECD <- rbind(lev_OECD, lev_australia, lev_mexico,lev_australia_supra, lev_mexico_supra, lev_switzerland_supra, lev_usa_supra, lev_canada_supra, lev_colombia_supra )

#Sort dataset
lev_OECD <- lev_OECD[order(lev_OECD$country, lev_OECD$government, lev_OECD$year),]

#Calculate average for the 10 countries####

#Limit data to 2019
lev_OECD <- subset(lev_OECD, subset = year == 2019)

#Calculate averages for FED (Central Government)
lev_FED <- subset(lev_OECD, government=="FED")
lev_FED_mean <- mean(lev_FED$percentage, na.rm = TRUE)

#Calculate averages for STATE (State or Regional Government)
lev_STATE <- subset(lev_OECD, government=="STATE")
lev_STATE_mean <- mean(lev_STATE$percentage, na.rm = TRUE)

#Calculate averages for LOCAL (Local Government)
lev_LOCAL <- subset(lev_OECD, government=="LOCAL")
lev_LOCAL_mean <- mean(lev_LOCAL$percentage, na.rm = TRUE)

#Calculate averages for SOCSEC (Social Security Funds)
lev_SOCSEC <- subset(lev_OECD, government=="SOCSEC")
lev_SOCSEC_mean <- mean(lev_SOCSEC$percentage, na.rm = TRUE)

#Calculate averages for SUPRA (Supranational)
lev_SUPRA <- subset(lev_OECD, government=="SUPRA")
lev_SUPRA_mean <- mean(lev_SUPRA$percentage, na.rm = TRUE)

#Create table showing tax revenue by level of Government####

lev_OECD_long <- subset(lev_OECD, select = -c(continent, year, iso_2, iso_3))
lev_OECD_long <-reshape(lev_OECD_long,
                       timevar = "government",
                       idvar = c("country"),
                       direction = "wide")
lev_OECD_long <- subset(lev_OECD_long, select= -c(percentage.NES))


lev_OECD_long <- merge(lev_OECD_long, country_names, by='country')

lev_OECD_long <- subset(lev_OECD_long, select = -c(continent))

colnames(lev_OECD_long)[colnames(lev_OECD_long)=="country"] <- "Country"
colnames(lev_OECD_long)[colnames(lev_OECD_long)=="percentage.FED"] <- "Central Government"
colnames(lev_OECD_long)[colnames(lev_OECD_long)=="percentage.LOCAL"] <- "Local Government"
colnames(lev_OECD_long)[colnames(lev_OECD_long)=="percentage.SOCSEC"] <- "Social Security Funds"
colnames(lev_OECD_long)[colnames(lev_OECD_long)=="percentage.STATE"] <- "State or Regional Government"
colnames(lev_OECD_long)[colnames(lev_OECD_long)=="percentage.SUPRA"] <- "Supranational"

lev_OECD_long[,c("Central Government","State or Regional Government","Social Security Funds","Local Government","Supranational")] <- round(lev_OECD_long[,c("Central Government","State or Regional Government","Social Security Funds","Local Government","Supranational")], digits = 1)
lev_OECD_long <- lev_OECD_long[c("iso_2", "iso_3", "Country","Central Government","State or Regional Government","Social Security Funds","Local Government","Supranational")]

#Add countries average to table
lev_gov_average<-c("NA","NA","Average", round (lev_FED_mean, digits = 1), round (lev_STATE_mean, digits = 1), round (lev_SOCSEC_mean, digits = 1), round (lev_LOCAL_mean, digits = 1), round (lev_SUPRA_mean, digits = 1))
lev_OECD_long<-rbind(lev_OECD_long,lev_gov_average)

write.csv(lev_OECD_long, "final-outputs/level_of_government_oecd.csv", row.names = FALSE)

