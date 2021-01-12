The R scripts in this repository can be used to replicate the results in the paper
# "Climate Policy and Transition Risk in the Housing Market"
by
# Konstantinos Ferentinos & Alex Gibberd & Benjamin Guin
that is to be published in the Bank of England Staff Working Paper Series.

## Data Sources

There are five open data sources that need to be accessed in order to replicate the results of the paper.

### 1. Data on Property Transactions

The first open data source consists of Price Paid Data collected across England and Wales, that are sourced from the HM Land Registry website: <https://www.gov.uk/government/statistical-data-sets/price-paid-data-downloads#yearly-file> and <https://data.gov.uk/dataset/314f77b3-e702-4545-8bcb-9ef8262ea0fd/archived-price-paid-data-1995-to-2017>. 

The specific CSV files that need to be downloaded from <https://data.gov.uk/dataset/314f77b3-e702-4545-8bcb-9ef8262ea0fd/archived-price-paid-data-1995-to-2017> are:
* Price Paid Data: 2015 part 1 — **pp-2015-part1.csv**
* Price Paid Data: 2015 part 2 — **pp-2015-part2.csv**
* Price Paid Data: 2016 part 1 — **pp-2016-part1.csv**
* Price Paid Data: 2016 part 2 — **pp-2016-part2.csv**
* Price Paid Data: 2017 part 1 — **pp-2017-part1.csv**
* Price Paid Data: 2017 part 2 — **pp-2017-part2.csv**

The specific CSV files that need to be downloaded from <https://www.gov.uk/government/statistical-data-sets/price-paid-data-downloads#yearly-file> are:
* part 1 of the 2018 yearly data as a CSV file (part 1) — **pp-2018-part1.csv**
* part 2 of the 2018 yearly data as a CSV file (part 2) — **pp-2018-part2.csv**
* the complete 2019 yearly data as a CSV file (complete) — **pp-2019.csv**

At the time of the analysis, I used the **pp-2015.csv** and **pp-2016.csv** complete yearly CSV files instead of the evenly split CSV files **pp-2015-part1.csv**, **pp-2015-part2.csv**, **pp-2016-part1.csv**, and **pp-2016-part2.csv**, that are mentioned above. If there are discrepancies in the data sourcing process due to updates to the CSV files from the HM Land Registry, please do not hesitate to contact me so as to share the specific CSV files I used in the analysis.

> Contains HM Land Registry data © Crown copyright and database right 2020. This data is licensed under the Open Government Licence v3.0.

### 2. Data on Energy Performance Certificates (EPC)

The second open data source consists of the Energy Performance of Buildings Registers that are the official source for all EPCs issued for all domestic buildings and building units in England and Wales that have been constructed, sold or let since 2008, and are sourced from the Ministry of Housing, Communities & Local Government: <https://epc.opendatacommunities.org/>. 

After signing up, an email will be sent with a link to access the data. The main download option is that of the full register, that consists of 340 data files, one for each local authority district to which the postcodes of the registered properties have been assigned, containing all the up to date EPC data from 2008 to the current date. Hence, the zip file that contains the full register needs to be downloaded and extracted to a folder named **EPC**.

### 3. Output Area Classification (December 2011) in the United Kingdom

The third open data source contains the Output Area Classifications (OACs) in their December 2011 format, that partition these areas into distinct groups, and are sourced from the Open Geography Portal: <https://geoportal.statistics.gov.uk/datasets/be648a095a9745998f6961e5ba54e01c_0>.

### 4. ONS Postcode Directory (May 2020)

The fourth open data source is the ONS Postcode Directory (ONSPD) for the United Kingdom in its May 2020 format, which relates both current and terminated UK postcodes to a range of current area geographies, among them the 2011 OAC codes, and is sourced from the Open Geography Portal: <https://geoportal.statistics.gov.uk/datasets/ons-postcode-directory-may-2020>, in the form of a zip file that should be downloaded and extracted to a folder named **ONSPD**.

The CSV files that are used in the R scripts are: 
* **ONSPD_MAY_2020_UK.csv** which can be found in the Data folder of the **ONSPD** folder
* **LAU218_LAU118_NUTS318_NUTS218_NUTS118_UK_LU.csv** which can be found in the Documents folder of the **ONSPD** folder

### 5. NUTS Level 1 (January 2018) Full Clipped Boundaries in the United Kingdom

The fifth open data source contains the digital vector boundaries for Nomenclature of Territorial Units for Statistics (NUTS) Level 1, in the United Kingdom, as at January 2018, and are sourced from the Open Geography Portal: <https://geoportal.statistics.gov.uk/datasets/01fd6b2d7600446d8af768005992f76a_0>, in the form of the shapefile **NUTS_Level_1__January_2018__Boundaries-shp.zip** that should be downloaded and extracted to a folder named **NUTS Level 1 shapefiles**.

## Description of R Scripts

### Main Analysis Folder

The **Main Analysis** folder holds the R scripts required to replicate the results of the main analysis of the report.
1. **data_upload.R** contains the code that uploads the property transactions and EPC datasets
2. **data_merging.R** contains the code that sets up a panel dataset, combining price transactions with EPC data
3. **data_processing.R** contains the code that replicates the feature engineering process
4. **descriptive_statistics.R** contains the code that replicates the descriptive analyses
5. **prices_map.R** contains the code that constructs a heat map of property prices by regions
6. **property_map.R** contains the code that constructs a heat map of the number of properties by regions
7. **propensity_score_matching.R** contains the code that implements Propensity Score Matching to the pre-intervention treated and non-treated properties,
in order to maximize their comparability
8. **gam.R** contains the code that presents a novel application of GAMs to assess the assumption that the outcomes in treatment and control group follow the same time trend in the absence of the intervention
9. **did_analysis.R** contains the code that implements the Difference-in-Difference regression model

### Comparison of Samples Folder

The **Comparison of Samples** folder holds the R scripts required to replicate the comparison of a random sample of the population of all properties and the sample of repeated property sales employed in the main analysis.
1. **sample_comp_data_upload.R** contains the code that creates a random sample of the population of all properties
2. **sample_comp_data_processing.R** contains the code that pre-processes both the random sample of the population of all properties and the sample of repeated property sales employed in the main analysis
3. **sample_comp_table.R** contains the code that compares a random sample of the population of all properties and the sample of repeated property sales employed in the main analysis

### Sensitivity Analysis Folder

The **Sensitivity Analysis** folder holds the R scripts required to replicate the results of the sensitivity analyses of the report.
1. **log_prices_analysis.R** contains the code that replicates the sensitivity analysis considering a logarithmic transformation of the prices in the PSM matched dataset
2. **5pc_analysis_data_processing.R** contains the code that replicates the feature engineering process for the sensitivity analysis that trims at the 5th and 95th percentile of prices
3. **5pc_analysis_psm.R** contains the code that replicates the PSM analysis for the sensitivity analysis that trims at the 5th and 95th percentile of prices
4. **5pc_analysis_gam.R** contains the code that replicates the GAM analysis for the sensitivity analysis that trims at the 5th and 95th percentile of prices
5. **5pc_analysis_did.R** contains the code that implements the Difference-in-Difference regression model for the sensitivity analysis that trims at the 5th and 95th percentile of prices
