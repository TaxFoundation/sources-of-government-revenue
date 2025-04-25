# Sources of Government Revenue

The Tax Foundation’s publication [Sources of Government Revenue in the OECD](https://taxfoundation.org/publications/sources-of-government-revenue-in-the-oecd/) shows to what extent OECD and non-OECD countries rely on different tax revenue sources.

## The Dataset and its Tax Revenue Categories and Scope

### The Dataset
All the revenue data used in this publication comees from the OECD's Comparative Tables of Revenue Statistics in OECD member countries (https://data-explorer.oecd.org/vis?lc=en&fs[0]=Topic%2C1%7CTaxation%23TAX%23%7CGlobal%20tax%20revenues%23TAX_GTR%23&pg=0&fc=Topic&bp=true&snb=153&df[ds]=dsDisseminateFinalDMZ&df[id]=DSD_REV_COMP_OECD%40DF_RSOECD&df[ag]=OECD.CTP.TPS&df[vs]=1.1&dq=..S13._T..PT_B1GQ.A&lom=LASTNPERIODS&lo=10&to[TIME_PERIOD]=false) and Comparative Tables of countries in the global database (https://data-explorer.oecd.org/vis?df[ds]=dsDisseminateFinalDMZ&df[id]=DSD_REV_COMP_GLOBAL%40DF_RSGLOBAL&df[ag]=OECD.CTP.TPS&df[vs]=1.1&dq=..S13._T..PT_B1GQ.A&lom=LASTNPERIODS&lo=10&to[TIME_PERIOD]=false). The dataset includes annual revenue data for almost 100 countries worldwide, dating to 1965 for many OECD countries and 1990 for a number of non-OECD countries. As of April 2025, the most recent data is 2023 for OECD countries and 2022 for non-OECD countries.

Details regarding the OECD dataset can be found in Annex A of the “Revenue Statistics 2024,” https://www.oecd.org/en/publications/revenue-statistics-2024_c87a3da5-en/full-report/component-9.html#annex-d1e746315-fc012931d9.

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

- `oecd_data_preliminary.csv` Tax revenue shares for OECD countries for the years 1990, 2022, and 2023 (note: 2023 data is not available for Australia, and Japan).
- `non_oecd_data_preliminary.csv` Tax revenue shares for non-OECD countries for the year 2022 (note: 2023 data is not available for non-OECD countries).


### /final-outputs
Location of **output tables** that are included in the publication.

- `oecd_averages.csv` Table showing the OECD average tax revenue shares (for the year 2023).

- `oecd_averages_1990.csv` Table comparing the 2023 OECD average tax revenue shares with the corresponding 1990 shares.

- `oecd_by_country.csv` Table showing the tax revenue shares for each OECD country, Table 1 in the publication.

- `regional_averages.csv` Table showing the average tax revenue shares by continents, Table 2 in the publication.

- `level_of_government_oecd.csv` Table showing the tax revenue shares by level of government for 10 countries.