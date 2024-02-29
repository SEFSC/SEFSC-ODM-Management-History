# ODM-MH-Data_log code

The ODM-MH-Data_log folder within the SEFSC - ODM - Management History repository includes code related to processing the raw Management History dataset to create a MH Data Log RDS file. The MH Data Log focuses on filing in the start and end dates for regulations within management type, FMP, species, region, sector, and subsector.

The **main_MH_prep.R** script sources the data as well as all other standard scripts in this folder. Detailed descriptions of that process can be found [here](https://docs.google.com/document/d/1FXv0kkcQRFgHativf3xW3v7-eQLyTDvb/edit#heading=h.gjdgxs).

The data folder is organized into the following subfolders:

### raw/

This is the raw Management History dataset downloaded as a csv file from the Online Dataset Manager (ODM). This folder also contains two look up tables for cleaning up the data for inconsistencies in spatial areas (zone name) and categories management actions to distinguish the most important regulations for analyses.

### interim/

This folder contains look up tables for expanding regulations pertaining to a group or aggregate of species so each species within a group has its own line in the dataset. This folder also contains files essential for generation unique identifiers to a series of regulations within the same management type, FMP, species, region, sector, and subsector.

### results/

This folder contains the final result of the standard MH Data Log processing code saved as a RDS file. This file will be used in the MH-Analysis-Ready scripts. Detailed descriptions of all variables contained in the MH Data Log can be found [here](https://docs.google.com/document/d/1JNZg1o5J5JwiWJxduRhGlIIynqRP7bjBtY2f46YxoYA/edit#heading=h.9iooo3tfvvi9).

This folder also contained an R data file that contains all data frames and objects created from the processing code to generate the RDS Data Log file. 

