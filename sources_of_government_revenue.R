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



#Reading in and cleaning OECD's global tax revenue statistics dataset####
dataset_list <- get_datasets()
search_dataset("Global Revenue", data= dataset_list)

dataset <- ("RS_GBL")

dstruc <- get_data_structure(dataset)
str(dstruc, max.level = 1)
#dstruc$VAR
#dstruc$TAX
#dstruc$GOV
#dstruc$YEA
taxes<-c("1100","1200","1300","2000","3000","4000","5000","6000","CUS")

all_data <- get_dataset("RS_GBL", filter= list(c(),c("NES"),c(taxes),c("TAXPER")),start_time = 2017)

all_data_1990 <- get_dataset("RS_GBL", filter= list(c(),c("NES"),c(taxes),c("TAXPER")),start_time = 1990, end_time = 1990)




#Only keep data that shows tax revenue as a share of total revenue (drop other indicators)
#all_data <- subset(all_data, all_data$VAR == "TAXPER")
#all_data <- subset(all_data, all_data$GOV == "NES")

#Only keep the tax revenue categories used in the publication (drop the remaining ones)
#all_data <- subset(all_data, subset = TAX == "1100" | TAX == "1200" | TAX == "1300" | TAX == "2000" | TAX == "3000" | TAX == "4000" | TAX == "5000" | TAX == "6000" | TAX == "CUS")

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

#Fix country name that was read in incorrectly
all_data$country <- as.character(all_data$country)
all_data[all_data$iso_3 == "CIV", "country"] <- "Cote d'Ivoire"

write.csv(all_data, "intermediate-outputs/data_preliminary.csv")



#Fix countries for which 2018 data is not available (unless otherwise noted, 2017 data is used for these cases)####

#Greece: Greece doesn't have data for the categories 1100, 1200, and 1300 (only for 1000); so we take the average share of these categories of the three years prior (2015-2017) to weigh the current year
missing_greece <- data.frame(iso_2 = c("GR", "GR", "GR"), iso_3 = c("GRC","GRC", "GRC"), country = c("Greece","Greece", "Greece"), continent = c("EU", "EU", "EU"), oecd = c(1, 1, 1), year = c(2018, 2018, 2018), category = c(1100, 1200, 1300), share = c(15.58660311, 5.765385917, 1.666333732))

#Australia: 2018 data not available -> use 2017 data
missing_australia <- all_data
missing_australia <- subset(missing_australia, subset = iso_3 == "AUS" & year == "2017")
missing_australia[missing_australia$year == 2017, "year"] <- 2018

#Japan: 2018 data not available -> use 2017 data
missing_japan <- all_data
missing_japan <- subset(missing_japan, subset = iso_3 == "JPN" & year == "2017")
missing_japan[missing_japan$year == 2017, "year"] <- 2018

#Mexico: 2018 data not available -> use 2017 data
missing_mexico <- all_data
missing_mexico <- subset(missing_mexico, subset = iso_3 == "MEX" & year == "2017")
missing_mexico[missing_mexico$year == 2017, "year"] <- 2018

#Combine data
all_data <- rbind(all_data, missing_greece, missing_australia, missing_japan, missing_mexico)

#Sort dataset
all_data <- all_data[order(all_data$country, all_data$category, all_data$year),]



#Calculate average OECD tax revenue sources####

#Limit data to OECD countries and 2018
oecd_data_2018 <- all_data
oecd_data_2018 <- subset(oecd_data_2018, subset = year == 2018)
oecd_data_2018 <- subset(oecd_data_2018, subset = oecd == 1)

#Calculate averages for 1100 (individual taxes)
individual_1100 <- subset(oecd_data_2018, category==1100)
individual_1100_mean <- mean(individual_1100$share, na.rm = TRUE)

#Calculate averages for 1200 (corporate taxes)
corporate_1200 <- subset(oecd_data_2018, category==1200)
corporate_1200_mean <- mean(corporate_1200$share, na.rm = TRUE)

#Calculate averages for 2000 (social insurance taxes)
social_2000 <- subset(oecd_data_2018, category==2000)
social_2000_mean <- mean(social_2000$share, na.rm = TRUE)

#Calculate averages for 4000 (property taxes)
property_4000 <- subset(oecd_data_2018, category==4000)
property_4000_mean <- mean(property_4000$share, na.rm = TRUE)

#Calculate averages for 5000 (consumption taxes)
consumption_5000 <- subset(oecd_data_2018, category==5000)
consumption_5000_mean <- mean(consumption_5000$share, na.rm = TRUE)

#Calculate averages for 1300 + 3000 + 6000 + CUS (Other)
other <- subset(oecd_data_2018, category == 1300 | category == 3000 | category == 6000 | category == "CUS")
other <- subset(other, select = -c(continent, oecd, year))

other_long <- reshape(other, 
             timevar = "category",
             idvar = c("iso_2","iso_3","country"),
             direction = "wide")

colnames(other_long)[colnames(other_long)=="share.1300"] <- "1300"
colnames(other_long)[colnames(other_long)=="share.3000"] <- "3000"
colnames(other_long)[colnames(other_long)=="share.6000"] <- "6000"
colnames(other_long)[colnames(other_long)=="share.CUS"] <- "CUS"

other_long[is.na(other_long)] <- 0

other_long$sum <- rowSums(other_long[,c("1300", "3000", "6000", "CUS")])

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



#Graph comparing OECD tax revenue shares in 1990 with 2018####

#Limit data to OECD countries and 1990
oecd_data_1990 <- all_data
oecd_data_1990 <- subset(oecd_data_1990, subset = year == 1990)
oecd_data_1990 <- subset(oecd_data_1990, subset = oecd == 1)

#Drop countries for which 1990 data is available but that were not part of the OECD in 1990
oecd_data_1990 <- subset(oecd_data_1990, oecd_data_1990$iso_3 != "CHL" & oecd_data_1990$iso_3 != "KOR" & oecd_data_1990$iso_3 != "MEX")
                         
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

#Calculate averages for 1300 + 3000 + 6000 + CUS (Other) for 1990 data
other_90 <- subset(oecd_data_1990, category == 1300 | category == 3000 | category == 6000 | category == "CUS")
other_90 <- subset(other_90, select = -c(continent, oecd, year))

other_long_90 <- reshape(other_90, 
                      timevar = "category",
                      idvar = c("iso_2","iso_3","country"),
                      direction = "wide")

colnames(other_long_90)[colnames(other_long_90)=="share.1300"] <- "1300"
colnames(other_long_90)[colnames(other_long_90)=="share.3000"] <- "3000"
colnames(other_long_90)[colnames(other_long_90)=="share.6000"] <- "6000"
colnames(other_long_90)[colnames(other_long_90)=="share.CUS"] <- "CUS"

other_long_90[is.na(other_long_90)] <- 0

other_long_90$sum <- rowSums(other_long_90[,c("1300", "3000", "6000", "CUS")])

other_mean_90 <- mean(other_long_90$sum, na.rm = TRUE)

#Compile averages into one dataframe
average_oecd_90 <- c(individual_1100_mean_90, corporate_1200_mean_90, social_2000_mean_90, property_4000_mean_90, consumption_5000_mean_90, other_mean_90)

oecd_averages_90 <- data.frame(tax_categories, average_oecd, average_oecd_90)

oecd_averages_90$average_oecd_90 <- round(oecd_averages_90$average_oecd_90, digits = 1)
oecd_averages_90$average_oecd <- round(oecd_averages_90$average_oecd, digits = 1)

colnames(oecd_averages_90)[colnames(oecd_averages_90)=="tax_categories"] <- "Tax Category"
colnames(oecd_averages_90)[colnames(oecd_averages_90)=="average_oecd"] <- "Average Share 2018"
colnames(oecd_averages_90)[colnames(oecd_averages_90)=="average_oecd_90"] <- "Average Share 1990"

write.csv(oecd_averages_90, "final-outputs/oecd_averages_1990.csv")



#Selected country comparison: Austria####
aut_data_2018 <- all_data
aut_data_2018 <- subset(aut_data_2018, subset = iso_3 == "AUT")
aut_data_2018 <- subset(aut_data_2018, subset = year == "2018")

aut_data_2018 <- subset(aut_data_2018, select = -c(continent, oecd, year))

#Calculating share of category "Other"
aut_data_other <- subset(aut_data_2018, category == 1300 | category == 3000 | category == 6000 | category == "CUS")

aut_data_other_v <- sum(aut_data_other$share, na.rm = TRUE)

aut_other_sum <- data.frame(iso_2 = c("AT"), iso_3 = c("AUT"), country = c("Austria"), category = c("Other"), share = aut_data_other_v)

aut_data_2018 <- rbind(aut_data_2018, aut_other_sum)

aut_data_2018 <- subset(aut_data_2018, category == 1100 | category == 1200 | category == 2000 | category == 4000 | category == 5000 | category == "Other")

#Compile averages into one dataframe
aut_data_2018$share <- round(aut_data_2018$share, digits = 1)

aut_oecd_averages <- data.frame(tax_categories, aut_data_2018, average_oecd)

aut_oecd_averages <- subset(aut_oecd_averages, select = -c(iso_2, iso_3, country, category))

aut_oecd_averages$average_oecd <- round(aut_oecd_averages$average_oecd, digits = 1)

colnames(aut_oecd_averages)[colnames(aut_oecd_averages)=="tax_categories"] <- "Tax Category"
colnames(aut_oecd_averages)[colnames(aut_oecd_averages)=="share"] <- "Average Share Austria"
colnames(aut_oecd_averages)[colnames(aut_oecd_averages)=="average_oecd"] <- "Average Share OECD"

write.csv(aut_oecd_averages, "final-outputs/aut_oecd_averages.csv")



#Selected country comparison: Greece####
grc_data_2018 <- all_data
grc_data_2018 <- subset(grc_data_2018, subset = iso_3 == "GRC")
grc_data_2018 <- subset(grc_data_2018, subset = year == "2018")

grc_data_2018 <- subset(grc_data_2018, select = -c(continent, oecd, year))

#Calculating share of category "Other"
grc_data_other <- subset(grc_data_2018, category == 1300 | category == 3000 | category == 6000 | category == "CUS")

grc_data_other_v <- sum(grc_data_other$share, na.rm = TRUE)

grc_other_sum <- data.frame(iso_2 = c("GR"), iso_3 = c("GRC"), country = c("Greece"), category = c("Other"), share = grc_data_other_v)

grc_data_2018 <- rbind(grc_data_2018, grc_other_sum)

grc_data_2018 <- subset(grc_data_2018, category == 1100 | category == 1200 | category == 2000 | category == 4000 | category == 5000 | category == "Other")

#Compile averages into one dataframe
grc_data_2018$share <- round(grc_data_2018$share, digits = 1)

grc_oecd_averages <- data.frame(tax_categories, grc_data_2018, average_oecd)

grc_oecd_averages <- subset(grc_oecd_averages, select = -c(iso_2, iso_3, country, category))

grc_oecd_averages$average_oecd <- round(grc_oecd_averages$average_oecd, digits = 1)

colnames(grc_oecd_averages)[colnames(grc_oecd_averages)=="tax_categories"] <- "Tax Category"
colnames(grc_oecd_averages)[colnames(grc_oecd_averages)=="share"] <- "Average Share Greece"
colnames(grc_oecd_averages)[colnames(grc_oecd_averages)=="average_oecd"] <- "Average Share OECD"

write.csv(grc_oecd_averages, "final-outputs/grc_oecd_averages.csv")



#Selected country comparison: United Kingdom####
gbr_data_2018 <- all_data
gbr_data_2018 <- subset(gbr_data_2018, subset = iso_3 == "GBR")
gbr_data_2018 <- subset(gbr_data_2018, subset = year == "2018")

gbr_data_2018 <- subset(gbr_data_2018, select = -c(continent, oecd, year))

#Calculating share of category "Other"
gbr_data_other <- subset(gbr_data_2018, category == 1300 | category == 3000 | category == 6000 | category == "CUS")

gbr_data_other_v <- sum(gbr_data_other$share, na.rm = TRUE)

gbr_other_sum <- data.frame(iso_2 = c("GB"), iso_3 = c("GBR"), country = c("United Kingdom of Great Britain and Northern Ireland"), category = c("Other"), share = gbr_data_other_v)

gbr_data_2018 <- rbind(gbr_data_2018, gbr_other_sum)

gbr_data_2018 <- subset(gbr_data_2018, category == 1100 | category == 1200 | category == 2000 | category == 4000 | category == 5000 | category == "Other")

#Compile averages into one dataframe
gbr_data_2018$share <- round(gbr_data_2018$share, digits = 1)

gbr_oecd_averages <- data.frame(tax_categories, gbr_data_2018, average_oecd)

gbr_oecd_averages <- subset(gbr_oecd_averages, select = -c(iso_2, iso_3, country, category))

gbr_oecd_averages$average_oecd <- round(gbr_oecd_averages$average_oecd, digits = 1)

colnames(gbr_oecd_averages)[colnames(gbr_oecd_averages)=="tax_categories"] <- "Tax Category"
colnames(gbr_oecd_averages)[colnames(gbr_oecd_averages)=="share"] <- "Average Share United Kingdom"
colnames(gbr_oecd_averages)[colnames(gbr_oecd_averages)=="average_oecd"] <- "Average Share OECD"

write.csv(gbr_oecd_averages, "final-outputs/gbr_oecd_averages.csv")



#Graph comparing tax revenue shares by region####

#Get non-OECD data for 2017 (2018 data not available for non-OECD countries as of February 2020)
non_oecd_data <- subset(all_data, subset = oecd == 0)
non_oecd_data <- subset(non_oecd_data, subset = year == "2017")

#Fix non-OECD countries for which some 2017 data is missing

#Ecuador: The OECD dataset provides the tax revenue shares for the categories 1100, 1200, and 1300 only in currency values (as opposed to as a share of total revenue). Thus, shares had to be calculated.
missing_ecuador <- data.frame(iso_2 = c("EC"), iso_3 = c("ECU"), country = c("Ecuador"), continent = c("SA"), oecd = c(0), year = c(2017), category = c(1100, 1200, 1300), share = c(0.8665, 5.1699, 13.8635))

#Jamaica: The OECD dataset does not provide data for the category 1300. It was calculated as a residual (total revenue minus all other shares).
missing_jamaica <- data.frame(iso_2 = c("JM"), iso_3 = c("JAM"), country = c("Jamaica"), continent = c("NO"), oecd = c(0), year = c(2017), category = c(1300), share = c(6.5918))

#Nicaragua: The OECD dataset provides the tax revenue shares for the categories 1100, 1200, and 1300 only in currency values (as opposed to as a share of total revenue). Thus, shares had to be calculated.
missing_nicaragua <- data.frame(iso_2 = c("NI"), iso_3 = c("NIC"), country = c("Nicaragua"), continent = c("NO"), oecd = c(0), year = c(2017), category = c(1100, 1200, 1300), share = c(0, 0, 28.90296853))

#For the following countries, the OECD does not provide data for some tax categories. However, the sum of the categories that do contain data equals the total amount of taxes raised. As a result, the categories with missing data are set to zero.

#Botswana
missing_botswana <- data.frame(iso_2 = c("BW"), iso_3 = c("BWA"), country = c("Botswana"), continent = c("AF"), oecd = c(0), year = c(2017), category = c(1100, 1200), share = c(0, 0))

#Congo
missing_congo <- data.frame(iso_2 = c("CG"), iso_3 = c("COG"), country = c("Congo"), continent = c("AF"), oecd = c(0), year = c(2017), category = c(2000), share = c(0))

#Democratic Republic of the Congo
missing_drc <- data.frame(iso_2 = c("CD"), iso_3 = c("COD"), country = c("Democratic Republic of the Congo"), continent = c("AF"), oecd = c(0), year = c(2017), category = c(1300), share = c(0))

#Equatorial Guinea
missing_eqguinea <- data.frame(iso_2 = c("GQ"), iso_3 = c("GNQ"), country = c("Equatorial Guinea"), continent = c("AF"), oecd = c(0), year = c(2017), category = c(2000), share = c(0))

#Ghana
missing_ghana <- data.frame(iso_2 = c("GH"), iso_3 = c("GHA"), country = c("Ghana"), continent = c("AF"), oecd = c(0), year = c(2017), category = c(4000), share = c(0))

#Nigeria
missing_nigeria <- data.frame(iso_2 = c("NG"), iso_3 = c("NGA"), country = c("Nigeria"), continent = c("AF"), oecd = c(0), year = c(2017), category = c(4000), share = c(0))

#Togo
missing_togo <- data.frame(iso_2 = c("TG"), iso_3 = c("TGO"), country = c("Togo"), continent = c("AF"), oecd = c(0), year = c(2017), category = c(2000), share = c(0))

#Uganda
missing_uganda <- data.frame(iso_2 = c("UG"), iso_3 = c("UGA"), country = c("Uganda"), continent = c("AF"), oecd = c(0), year = c(2017), category = c(2000, 4000), share = c(0,0))

#Vanuatu
missing_vanuatu <- data.frame(iso_2 = c("VU"), iso_3 = c("VUT"), country = c("Vanuatu"), continent = c("OC"), oecd = c(0), year = c(2017), category = c(1100, 1200, 1300), share = c(0,0,0))

#Put all rows into one dataframe
non_oecd_data <- rbind(non_oecd_data, missing_ecuador, missing_jamaica, missing_nicaragua, missing_botswana, missing_congo, missing_drc, missing_eqguinea, missing_ghana, missing_nigeria, missing_togo, missing_uganda, missing_vanuatu)

#Combine non-OECD and OECD countries into one dataframe
oecd_and_non_oecd <- rbind(oecd_data_2018, non_oecd_data)

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

#Calculate averages for 1300 + 3000 + 6000 + CUS (Other)
other_af <- subset(oecd_and_non_oecd, category == 1300 | category == 3000 | category == 6000 | category == "CUS")
other_af <- subset(other_af, continent == "AF")
other_af <- subset(other_af, select = -c(continent, oecd, year))

other_long_af <- reshape(other_af, 
                      timevar = "category",
                      idvar = c("iso_2","iso_3","country"),
                      direction = "wide")

colnames(other_long_af)[colnames(other_long_af)=="share.1300"] <- "1300"
colnames(other_long_af)[colnames(other_long_af)=="share.3000"] <- "3000"
colnames(other_long_af)[colnames(other_long_af)=="share.6000"] <- "6000"
colnames(other_long_af)[colnames(other_long_af)=="share.CUS"] <- "CUS"

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

#Calculate averages for 1300 + 3000 + 6000 + CUS (Other)
other_as <- subset(oecd_and_non_oecd, category == 1300 | category == 3000 | category == 6000 | category == "CUS")
other_as <- subset(other_as, continent == "AS")
other_as <- subset(other_as, select = -c(continent, oecd, year))

other_long_as <- reshape(other_as, 
                         timevar = "category",
                         idvar = c("iso_2","iso_3","country"),
                         direction = "wide")

colnames(other_long_as)[colnames(other_long_as)=="share.1300"] <- "1300"
colnames(other_long_as)[colnames(other_long_as)=="share.3000"] <- "3000"
colnames(other_long_as)[colnames(other_long_as)=="share.6000"] <- "6000"
colnames(other_long_as)[colnames(other_long_as)=="share.CUS"] <- "CUS"

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

#Calculate averages for 1300 + 3000 + 6000 + CUS (Other)
other_eu <- subset(oecd_and_non_oecd, category == 1300 | category == 3000 | category == 6000 | category == "CUS")
other_eu <- subset(other_eu, continent == "EU")
other_eu <- subset(other_eu, select = -c(continent, oecd, year))

other_long_eu <- reshape(other_eu, 
                         timevar = "category",
                         idvar = c("iso_2","iso_3","country"),
                         direction = "wide")

colnames(other_long_eu)[colnames(other_long_eu)=="share.1300"] <- "1300"
colnames(other_long_eu)[colnames(other_long_eu)=="share.3000"] <- "3000"
colnames(other_long_eu)[colnames(other_long_eu)=="share.6000"] <- "6000"
colnames(other_long_eu)[colnames(other_long_eu)=="share.CUS"] <- "CUS"

other_long_eu[is.na(other_long_eu)] <- 0

other_long_eu$sum <- rowSums(other_long_eu[,c("1300", "3000", "6000", "CUS")])

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

#Calculate averages for 1300 + 3000 + 6000 + CUS (Other)
other_no <- subset(oecd_and_non_oecd, category == 1300 | category == 3000 | category == 6000 | category == "CUS")
other_no <- subset(other_no, continent == "NO")
other_no <- subset(other_no, select = -c(continent, oecd, year))

other_long_no <- reshape(other_no, 
                         timevar = "category",
                         idvar = c("iso_2","iso_3","country"),
                         direction = "wide")

colnames(other_long_no)[colnames(other_long_no)=="share.1300"] <- "1300"
colnames(other_long_no)[colnames(other_long_no)=="share.3000"] <- "3000"
colnames(other_long_no)[colnames(other_long_no)=="share.6000"] <- "6000"
colnames(other_long_no)[colnames(other_long_no)=="share.CUS"] <- "CUS"

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

#Calculate averages for 1300 + 3000 + 6000 + CUS (Other)
other_oc <- subset(oecd_and_non_oecd, category == 1300 | category == 3000 | category == 6000 | category == "CUS")
other_oc <- subset(other_oc, continent == "OC")
other_oc <- subset(other_oc, select = -c(continent, oecd, year))

other_long_oc <- reshape(other_oc, 
                         timevar = "category",
                         idvar = c("iso_2","iso_3","country"),
                         direction = "wide")

colnames(other_long_oc)[colnames(other_long_oc)=="share.1300"] <- "1300"
colnames(other_long_oc)[colnames(other_long_oc)=="share.3000"] <- "3000"
colnames(other_long_oc)[colnames(other_long_oc)=="share.6000"] <- "6000"
colnames(other_long_oc)[colnames(other_long_oc)=="share.CUS"] <- "CUS"

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

#Calculate averages for 1300 + 3000 + 6000 + CUS (Other)
other_sa <- subset(oecd_and_non_oecd, category == 1300 | category == 3000 | category == 6000 | category == "CUS")
other_sa <- subset(other_sa, continent == "SA")
other_sa <- subset(other_sa, select = -c(continent, oecd, year))

other_long_sa <- reshape(other_sa, 
                         timevar = "category",
                         idvar = c("iso_2","iso_3","country"),
                         direction = "wide")

colnames(other_long_sa)[colnames(other_long_sa)=="share.1300"] <- "1300"
colnames(other_long_sa)[colnames(other_long_sa)=="share.3000"] <- "3000"
colnames(other_long_sa)[colnames(other_long_sa)=="share.6000"] <- "6000"
colnames(other_long_sa)[colnames(other_long_sa)=="share.CUS"] <- "CUS"

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



#Create table showing tax revenue shares for each OECD country####

oecd_data_2018_long <- subset(oecd_data_2018, select = -c(continent, oecd, year, iso_2, iso_3))

oecd_data_2018_long <- reshape(oecd_data_2018_long, 
                               timevar = "category",
                               idvar = c("country"),
                               direction = "wide")

oecd_data_2018_long <- subset(oecd_data_2018_long, select = -c(share.1300, share.3000, share.6000, share.CUS))

oecd_data_2018_long$Other <- other_long$sum

oecd_data_2018_long <- merge(oecd_data_2018_long, country_names, by='country')
oecd_data_2018_long <- subset(oecd_data_2018_long, select = -c(continent))

colnames(oecd_data_2018_long)[colnames(oecd_data_2018_long)=="country"] <- "Country"
colnames(oecd_data_2018_long)[colnames(oecd_data_2018_long)=="share.1100"] <- "Individual Taxes"
colnames(oecd_data_2018_long)[colnames(oecd_data_2018_long)=="share.1200"] <- "Corporate Taxes"
colnames(oecd_data_2018_long)[colnames(oecd_data_2018_long)=="share.2000"] <- "Social Insurance Taxes"
colnames(oecd_data_2018_long)[colnames(oecd_data_2018_long)=="share.4000"] <- "Property Taxes"
colnames(oecd_data_2018_long)[colnames(oecd_data_2018_long)=="share.5000"] <- "Consumption Taxes"

oecd_data_2018_long[,c('Individual Taxes', 'Corporate Taxes', 'Social Insurance Taxes', 'Property Taxes', 'Consumption Taxes', 'Other')] <- round(oecd_data_2018_long[,c('Individual Taxes', 'Corporate Taxes', 'Social Insurance Taxes', 'Property Taxes', 'Consumption Taxes', 'Other')], digits = 1)
oecd_data_2018_long <- oecd_data_2018_long[c("iso_2", "iso_3", "Country", "Individual Taxes", "Corporate Taxes", "Social Insurance Taxes", "Property Taxes", "Consumption Taxes", "Other")]


#Add OECD Average to table
oecd_average<-c("NA","NA","OECD Average",round(individual_1100_mean, digits = 1)
                , round(corporate_1200_mean, digits = 1), round(social_2000_mean, digits = 1)
                , round(property_4000_mean, digits = 1), round(consumption_5000_mean, digits = 1), 
                round(other_mean, digits = 1))
oecd_data_2018_long<-rbind(oecd_data_2018_long,oecd_average)


write.csv(oecd_data_2018_long, "final-outputs/oecd_by_country.csv", row.names = FALSE)
