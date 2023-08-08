# Repository of the SEFSC Management History Processing Project

This SEFSC - ODM - Management History repository provides a method for generating an analysis-ready version of the Management History Database. In their original format, the records in this database represent changes in management actions affecting federally managed species throughout the Gulf of Mexico, South Atlantic, and U.S. Caribbean regions. The data files and code contained in this repository define and execute logic for grouping and creating streamlined time series of related management events. This process is actively being updated. Draft metadata can be found [here](https://docs.google.com/document/d/1P229OLxEG_iIS31VEbKlKiW2jmiFPKfn5N9Vr9xG4dM/edit#heading=h.gjdgxs).

## code/
This folder contains the standard processing code for generating an analysis-ready dataset. The metadata for this code can be found [here](https://docs.google.com/document/d/1FXv0kkcQRFgHativf3xW3v7-eQLyTDvb/edit#).

## data/
This folder contains the raw Management History data downloaded from the Online Database Manager (ODM). Additional csv files for grouping the data and cleaning up reported zone names are also found here.

## examples/
This folder contains example outputs that replicate tables and datasets used in the SouthEast Data and Assessment and Review ([SEDAR](https://sedarweb.org/)) process.

## Products
The records contained the Management History dataset have been used to create SEDAR working papers to assist with the assessment process. These working papers are outlined below for reference.

- SEDAR 74 Gulf of Mexico Red Snapper - [SEDAR74-DW-25](https://sedarweb.org/documents/sedar-74-dw-25-summary-of-management-actions-for-red-snapper-lutjanus-campechanus-from-the-gulf-of-mexico-1984-2022-as-documented-within-the-management-history-database/)
- SEDAR 80 Caribbean Queen Triggerfish - [SEDAR80-WP-11](https://sedarweb.org/documents/sedar-80-wp-11-summary-of-closure-management-actions-for-the-reef-fish-fishery-of-puerto-rico-and-the-u-s-virgin-islands-as-documented-within-the-management-history-database/)
- SEDAR 81 Gulf of Mexico Spanish Mackerel - [SEDAR81-WP-01](https://sedarweb.org/documents/sedar-81-wp-01-summary-of-management-actions-for-spanish-mackerel-scomberomorus-maculatus-from-the-gulf-of-mexico-as-documented-within-the-management-history-database%ef%bf%bc/)
- SEDAR 82 South Atlantic Gray Triggerfish - [SEDAR82-DW02](https://sedarweb.org/documents/sedar-82-dw02-summary-of-management-actions-for-gray-triggerfish-balistes-capriscus-from-the-south-atlantic-as-documented-within-the-management-history-database/)

# Disclamer:

The Management History database is for general information purposes only. Any discrepancies between this information and the regulations as published in official sources of information, such as the Federal Register and Code of Federal Regulations will be resolved in favor of the official sources.

This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project code is provided on an ‘as is’ basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.
