# Sources of Government Revenue

The Tax Foundation’s publication [Sources of Government Revenue](https://taxfoundation.org/publications/sources-of-government-revenue-in-the-oecd/) shows to what extent OECD and non-OECD countries rely on different tax revenue sources.

## The Dataset and its Tax Revenue Categories

### The Dataset
All the revenue data used in this publication comes from the OECD's Global Revenue Statistics Database (https://stats.oecd.org/Index.aspx?DataSetCode=RS_GBL). The dataset includes annual revenue data for almost 100 countries worldwide, dating back to 1965 for many OECD countries and to 1990 for a number of non-OECD countries. As of February 2020, the most recent data is 2018 for OECD countries and 2017 for non-OECD countries.

Details regarding the dataset can be found in "Revenue Statistics 1965-2017: Interpretative Guide," http://www.oecd.org/tax/tax-policy/oecd-classification-taxes-interpretative-guide.pdf.

### Revenue Categories
The OECD's Global Revenue Statistics Database provides various tax categories for the different tax revenue sources. For our report, we chose and combined these categories as follows:

* **Individual Taxes:** Covers the OECD category *1100 Taxes on income, profits and capital gains of individuals*.

* **Corporate Taxes:** Covers the OECD category *1200 Taxes on income, profits and capital gains of corporates*.

* **Social Insurance Taxes:** Covers the OECD category *2000 Social security contributions (SSC)*.

* **Property Taxes:** Covers the OECD category *4000 Taxes on property*.

* **Consumption Taxes:** Covers the OECD category *5000 Taxes on goods and services*.

* **Other:** Covers the OECD categories *1300 Unallocable between 1100 and 1200*, *3000 Taxes on payroll and workforce,* *6000 Taxes other than 1000, 2000, 3000, 4000 and 5000*, and *Custom duties collected for the EU*.



## Explanation of Files in Repository

### /main directory

Location of the R code, the source documentation, and this README.

The R code reads in and downloads the necessary data, cleans the data, adds missing data manually, merges datasets, and produces intermediate and final output datasets and tables.

The source documentation cites all the sources used.

### /source-data

Location of **input** files to .R code file including:

- `country_codes.csv` Dataset that includes all 249 sovereign states and dependent territories that have been assigned a country code by the International Organization for Standardization (ISO). Includes official country names in various languages, ISO country codes, continents, and further geographical information.

- `data_rates_1980_2018.csv` Tax Foundation's dataset of statutory corporate income tax rates for the years 1980 to 2018. This dataset has been built in stages since 2015.

- `gdp_historical.xlsx` U.S. Department of Agriculture's dataset of historical real Gross Domestic Product (GDP) and growth rates of GDP for 176 countries and various regions (in billions of 2010 dollars) for the years 1980 to 2017.

- `gdp_projected.xlsx` U.S. Department of Agriculture's dataset of projected real Gross Domestic Product (GDP) and growth rates of GDP for 176 countries and various regions (in billions of 2010 dollars) for the years 2010 to 2030.

- `kpmg_dataset.xlsx` KPMG's dataset of statutory corporate income tax rates for 171 jurisdictions for the years 2003 to 2019.

### /intermediate-outputs

Location of **intermediate output** files of .R code file including:

- `gdp_iso.csv` GDP data paired with ISO country codes for the years 1980 to 2019.

- `rates_final.csv` Statutory corporate income tax rates for the years 1980 to 2019. Includes rates of all countries for which data was available in 2019 (data from OECD, KPMG, and researched individually).

- `rates_preliminary.csv` Statutory corporate income tax rates for the years 1980 to 2019. Includes rates of countries for which OECD and KPMG data was available for the year 2019. Does not include countries for which the rate was researched and added individually.

### /final-data
Location of **final output** files of .R code file including

- `final_data_long.csv` Statutory corporate income tax rates and GDP levels of all countries paired with ISO country codes, continents, and country groups for the years 1980 to 2019. Includes all countries that have an ISO country code, including the ones for which corporate income tax rates and/or GDP data was not available. In long format.

- `final_data_2019.csv` Statutory corporate income tax rates and GDP levels of countries paired with ISO country codes, continents, and country groups for the year 2019. Only includes countries for which both the corporate income tax rates and GDP data were available.

- `final_data_2019_gdp_incomplete.csv` Statutory corporate income tax rates and GDP levels of countries paired with ISO country codes, continents, and country groups for the year 2019. Includes all countries for which we have data for the corporate income tax rate, including countries for which we do not have GDP data.

### /final-outputs
Location of **output tables** that are included in the publication.

- `bottom_rates.csv` Table of the 21 countries with the lowest corporate income tax rates in the world in 2019 (excluding countries without a corporate tax).

- `distribution_1980.csv` Table showing the distribution of corporate income tax rates in 1980.

- `distribution_1990.csv` Table showing the distribution of corporate income tax rates in 1990.

- `distribution_2000.csv` Table showing the distribution of corporate income tax rates in 2000.

- `distribution_2010.csv` Table showing the distribution of corporate income tax rates in 2010.

- `distribution_2019.csv` Table showing the distribution of corporate income tax rates in 2019.

- `rate_changes.csv` Table showing by how much the corporate income tax rates changed between 2000 and 2019 by country.

- `rate_time_series.csv` Table showing the weighted and unweighted worldwide average of corporate income tax rates by year between 1980 and 2019.

- `rates_regional.csv` Table showing the weighted and unweighted averages of corporate income tax rates by continent and country groups for the year 2019.

- `regional_all_data.csv` Table showing the weighted and unweighted averages of corporate income tax rates by continent and country groups for the years 1980, 1990, 2000, 2010, and 2019.

- `top_rates.csv` Table of the 21 countries with the highest corporate income tax rates in the world in 2019.

- `zero_rates.csv` Table of countries without a corporate income tax in 2019.