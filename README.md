# Sources of Government Revenue

The Tax Foundation’s publication [Sources of Government Revenue in the OECD](https://taxfoundation.org/publications/sources-of-government-revenue-in-the-oecd/) shows to what extent OECD and non-OECD countries rely on different tax revenue sources.

## The Dataset and its Tax Revenue Categories and Scope

### The Dataset
All the revenue data used in this publication comes from the OECD's Global Revenue Statistics Database (https://stats.oecd.org/Index.aspx?DataSetCode=RS_GBL). The dataset includes annual revenue data for almost 100 countries worldwide, dating back to 1965 for many OECD countries and 1990 for a number of non-OECD countries. As of February 2020, the most recent data is 2018 for OECD countries and 2017 for non-OECD countries.

Details regarding the OECD dataset can be found in "Revenue Statistics 1965-2017: Interpretative Guide," http://www.oecd.org/tax/tax-policy/oecd-classification-taxes-interpretative-guide.pdf.

### Revenue Categories
The OECD's Global Revenue Statistics Database provides various categories for the different tax revenue sources. For our report, we chose and combined these categories as follows:

* **Individual Taxes:** Covers the OECD category *1100 Taxes on income, profits and capital gains of individuals*.

* **Corporate Taxes:** Covers the OECD category *1200 Taxes on income, profits and capital gains of corporates*.

* **Social Insurance Taxes:** Covers the OECD category *2000 Social security contributions (SSC)*.

* **Property Taxes:** Covers the OECD category *4000 Taxes on property*.

* **Consumption Taxes:** Covers the OECD category *5000 Taxes on goods and services*.

* **Other:** Covers the OECD categories *1300 Unallocable between 1100 and 1200*, *3000 Taxes on payroll and workforce,* *6000 Taxes other than 1000, 2000, 3000, 4000 and 5000*, and *Custom duties collected for the EU*.

### Scope: Countries Covered by Region

* **Africa:** Botswana, Burkina Faso, Cameroon, Cabo Verde, Congo, Democratic Republic of the Congo, Côte d'Ivoire, Egypt, Equatorial Guinea, Ghana, Kenya, Madagascar, Mali, Mauritania, Mauritius, Morocco, Niger, Nigeria, Rwanda, Senegal, Seychelles, South Africa, Eswatini, Togo, Tunisia, and Uganda.

* **Asia:** Israel, Japan, Korea, Indonesia, Kazakhstan, Malaysia, Philippines, Singapore, and Thailand.

* **Europe:** Austria, Belgium, Czech Republic, Denmark, Estonia, Finland, France, Germany, Greece, Hungary, Iceland, Ireland, Italy, Latvia, Lithuania, Luxembourg, Netherlands, Norway, Poland, Portugal, Slovak Republic, Slovenia, Spain, Sweden, Switzerland, Turkey, and United Kingdom.

* **North America:** Canada, Mexico, United States, Bahamas, Barbados, Belize, Costa Rica, Cuba, Dominican Republic, El Salvador, Guatemala, Honduras, Jamaica, Nicaragua, Panama, and Trinidad and Tobago.

* **Oceania:** Australia, New Zealand, Cook Islands, Fiji, Papua New Guinea, Samoa, Solomon Islands, Tokelau, and Vanuatu.

* **South America:** Chile, Argentina, Bolivia, Brazil, Colombia, Ecuador, Guyana, Paraguay, Peru, and Uruguay.



## Explanation of Files in Repository

### /main directory

Location of the R code, the source documentation, and this README.

The R code reads in and downloads the necessary data, cleans the data, adds missing data manually, merges datasets, and produces intermediate and final output datasets and tables.

The source documentation cites all the sources used.

### /source-data

Location of **input** files to .R code file including:

- `country_codes.csv` Dataset that includes all 249 sovereign states and dependent territories that have been assigned a country code by the International Organization for Standardization (ISO). Includes official country names in various languages, ISO country codes, continents, and further geographical information.

### /intermediate-outputs

Location of **intermediate output** files of .R code file including:

- `data_preliminary.csv` Tax revenue shares for all countries for the years 1990, 2017, and 2018 (note: 2018 data is not available for all non-OECD countries including Australia, Japan, and Mexico).

### /final-outputs
Location of **output tables** that are included in the publication.

- `aut_oecd_averages.csv` Table comparing Austria's tax revenue shares with the OECD average shares.

- `gbr_oecd_averages.csv` Table comparing the UK's tax revenue shares with the OECD average shares.

- `grc_oecd_averages.csv` Table comparing Greece's tax revenue shares with the OECD average shares.

- `oecd_averages.csv` Table showing the OECD average tax revenue shares (for the year 2018).

- `oecd_averages_1990.csv` Table comparing the 2018 OECD average tax revenue shares with the corresponding 1990 shares.

- `oecd_by_country.csv` Table showing the tax revenue shares for each OECD country, Table 1 in the publication.

- `regional_averages.csv` Table showing the average tax revenue shares by continents, Table 2 in the publication.