## Data informations

### Context of data creation

SoDUCo ANR Project: Social Dynamics in Urban Context. Open tools, models and data - Paris and its suburbs, 1789-1950. ANR-18-CE38-0013, 2019-2023. Website: [https://soduco.geohistoricaldata.org/](https://soduco.geohistoricaldata.org/)

### City directories

The "data_extraction_with_population.gpkg" is the final data-set used for analysis. It consists of an extraction of "directories-ListNoms.gpkg" (see _R/1_extracting_specific_dates_listeNoms.R_ and note bellow), then a manual cleaning of the data and an enrichment of the entries of Paris' directories according to activity categories inspired by NAICS 2022 of the U.S. Census Bureau (see _R/3_manual_editing.R_), and an addin of population censuses information (in _R/4_data_critics_and_output_version.R_). An output in .xlsx format is also provided for users not used to processing spatial data formats.

**Note:** "directories-ListNoms.gpkg" data-set (version 3) was created in SoDUCo ANR Project and is open on Nakala platform [doi: 10.34847/nkl.98eem49t.v3](https://doi.org/10.34847/nkl.98eem49t). See documentation and presentations in .pdf format on Nakala platform for precise information and licence to know how to use the data. The initial data (4.9 Go) must be downloaded and included in _data/init_datasets_ folder to exactly replicate the _R/1_extracting_specific_dates_listeNoms.R_ code. The data for the years extracted from the initial global data-set and studied here are, however, directly included in the _data/intermediary_datasets/studied_data_2.rds_ file.

### Population data

Population data (named in _data/init_datasets_ "pop_apres_1860.xlsx" and "pop_avt_1860.xlsx") were created in SoDUCo ANR Project and are open on Nakala platform [doi: 10.34847/nkl.e173c93p](https://doi.org/10.34847/nkl.e173c93p). See documentation in .pdf format on Nakala platform for precise information and licence to know how to use the data.

## Other informations

### Data analyses
All analyses are created from _R/5_data_analysis.R_ code. Final figures and tables are contained in _ouputs_ folder.

### R session info
All informations are given in _R/R_infos.html_ file.
