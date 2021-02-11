# Sources of Government Revenue

The Tax Foundation’s publication [Sources of Government Revenue in the OECD](https://taxfoundation.org/publications/sources-of-government-revenue-in-the-oecd/) shows to what extent OECD and non-OECD countries rely on different tax revenue sources.

## The Dataset and its Tax Revenue Categories and Scope

### The Dataset
All the revenue data used in this publication comes from the OECD's Revenue Statistics Database (https://stats.oecd.org/Index.aspx?DataSetCode=RS_GBL) and Revenue Statistics – OECD countries: comparative tables (https://stats.oecd.org/Index.aspx?DataSetCode=RevGlobal). The dataset includes annual revenue data for almost 100 countries worldwide, dating to 1965 for many OECD countries and 1990 for a number of non-OECD countries. As of February 2021, the most recent data is 2019 for OECD countries and 2018 for non-OECD countries.

Details regarding the OECD dataset can be found in Annex A of the “Revenue Statistics 2020,” https://www.oecd-ilibrary.org/sites/8625f8e5-en/1/4/1/index.html?itemId=/content/publication/8625f8e5-en&_csp_=10eecbf76519867b3d14d98d90e8aff0&itemIGO=oecd&itemContentType=book#.

### Revenue Categories
The OECD's Global Revenue Statistics Database provides various categories for the different tax revenue sources. For our report, we chose and combined these categories as follows:

* **Individual Taxes:** Covers the OECD category *1100 Taxes on income, profits and capital gains of individuals*.

* **Corporate Taxes:** Covers the OECD category *1200 Taxes on income, profits and capital gains of corporates*.

* **Social Insurance Taxes:** Covers the OECD category *2000 Social security contributions (SSC)*.

* **Property Taxes:** Covers the OECD category *4000 Taxes on property*.

* **Consumption Taxes:** Covers the OECD category *5000 Taxes on goods and services*.

* **Other:** Covers the OECD categories *1300 Unallocable between 1100 and 1200*; *3000 Taxes on payroll and workforce*; *6000 Taxes other than 1000, 2000, 3000, 4000 and 5000*.

### Scope: Countries Covered by Region

* **Africa:** Botswana, Burkina Faso, Cameroon, Cabo Verde, Chad, Congo, Democratic Republic of the Congo, Côte d'Ivoire, Egypt, Equatorial Guinea, Ghana, Kenya, Lesotho, Madagascar, Malawi, Mali, Mauritania, Mauritius, Morocco, Namibia, Niger, Nigeria, Rwanda, Senegal, Seychelles, South Africa, Eswatini, Togo, Tunisia, and Uganda.

* **Asia:** Bhutan, Israel, Japan, Korea, Indonesia, Kazakhstan, Malaysia, Mongolia, Philippines, Singapore, and Thailand.

* **Europe:** Austria, Belgium, Bulgaria, Czech Republic, Denmark, Estonia, Finland, France, Germany, Greece, Hungary, Iceland, Ireland, Italy, Latvia, Liechtenstein, Lithuania, Luxembourg, Netherlands, Norway, Poland, Portugal, Slovak Republic, Slovenia, Spain, Sweden, Switzerland, Turkey, and United Kingdom.

* **North America:** CCanada, Mexico, United States, Bahamas, Barbados, Belize, Costa Rica, Cuba, Dominican Republic, El Salvador, Guatemala, Honduras, Jamaica, Nicaragua, Panama, Saint Lucia, and Trinidad and Tobago.

* **Oceania:** Australia, New Zealand, Cook Islands, Fiji, Nauru, Papua New Guinea, Samoa, Solomon Islands, Tokelau, and Vanuatu.

* **South America:**  Chile, Argentina, Bolivia, Brazil, Colombia, Ecuador, Guyana, Paraguay, Peru, and Uruguay.



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

- `oecd_data_preliminary.csv` Tax revenue shares for OECD countries for the years 1990, 2018, and 2019 (note: 2019 data is not available for Australia, Japan, and Mexico).
- `non_oecd_data_preliminary.csv` Tax revenue shares for non-OECD countries for the year 2018 (note: 2019 data is not available for non-OECD countries).


### /final-outputs
Location of **output tables** that are included in the publication.

- `oecd_averages.csv` Table showing the OECD average tax revenue shares (for the year 2019).

- `oecd_averages_1990.csv` Table comparing the 2019 OECD average tax revenue shares with the corresponding 1990 shares.

- `oecd_by_country.csv` Table showing the tax revenue shares for each OECD country, Table 1 in the publication.

- `regional_averages.csv` Table showing the average tax revenue shares by continents, Table 2 in the publication.

- `level_of_government_oecd.csv` Table showing the tax revenue shares by level of government for 10 countries.