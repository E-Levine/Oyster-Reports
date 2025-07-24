# Oyster-Reports
PROJECT DESCRIPTION: <br>
Project for housing code for generating FWRI Oyster reports.  RMarkdown scripts exist for each Report type and funding agency, but their method of use varies based on current data source and compilation method. Please refer to the "File Structure" and "File Use" sections. SQL code for data requests are housed in /Data_requests.
Data managers are currently the only FWRI staff who will be able to execute these scripts. Code is still being written and revised.


### FOLDER SCHEMA <br>
- /CERP - RMarkdown scripts for Comprehensive Everglades Restoration Plan project - Current versions: Monthly report, Quarterly figures and summary, Annual figures and summary.<br>
- /Data - Summary data Excel files. Files can be read by anyone but can only be modified by specified users. If write access is needed, please request access from E-Levine. <br>
- /Data_requests - SQL code for pulling and formatting data requested by grant/project managers.
- /DBHYDRO - R code for selecting, pulling, and cleaning DBHYDRO data. Cleaned data is maintained in the repo as designated within code. Folders are maintained to help distinguish between data saved locally vs in the repo. Currently tested and released for FLOW data. May require edits for other data types.<br>
- /DMFM - RMarkdown scripts for sharing with the Division of Marine Fisheries Management  - Current versions: AS NEEDED.<br>
- /FLTIG - RMarkdown scripts for the Florida Trustees Implementation Group Data Gaps project  - Current versions: Annually.<br>
- /NFWF2 - RMarkdown scripts for the National Fish and Wildlife Foundation 2.0 project - Current versions: Quarterly..<br>
- /Presentations - RMarkdown scripts for Presentations given to other groups. As needed. <br>
- /PBC - RMarkdown scripts for Plam Beach County project - Current versions: Monthly, Quarterly.<br>


### FILE USE <br>
- /CERP - Compiling monthly reports from Excel data files into Word document outputs. Compile quarterly summary data into html outputs and figures into Word document output. Compile summary data into annual figures in Word document output.<br>
- /Data - Output of monthly summary data for CERP and PBC projects.<br>
- /Data_requests - Code for gathering data from the database.
- /DBHYDRO - Cleaning and compilation of downloaded data from DBHYDRO. Can summarize data for specified stations and dates within an estuary and save data output.<br>
- /DMFM - Previous export of some NFWF data to Excel for a quick graph. Used Bags per acre.<br>
- /FLTIG - .<br>
- /NFWF2 - .<br>
- /Presentations - Data displays for Presentations given to other groups. <br>
- /PBC - Compiling monthly reports from Excel data files into Word doucument outputs.<br>


### REPORT TYPES <br>
***CERP and PBC monthly reports consist of reporting observations of mean salinity, recruitment, and dermo for the specified monthly and most recent month prior. PBC reports also include reporting of mean sedimentation rate and percent organic content for stations north and south of the C-51 output. When applicable, mean live and dead counts from oyster surveys are included. <br>

***CERP and PBC quarterly reports consist of reporting observations of surveys, recruitment, reproduction, disease, growth and mortality, and water quality (Salinity, Temperature, pH, DO mg/L, and Secchi %) for the specified quarter. Figures show data for the most recent 12 months. PBC reports also include reporting of mean sedimentation rate and percent organic content for stations north and south of the C-51 output. <br>

***DMFM Current file from a previous export of some NFWF data to Excel for a quick graph. Used Bags per acre among some other stats. Check before reusing. <br>

***FL-TIG Annual reports consist of reporting obervations of Recruitment rates, Sedimentation rates, and water quality (Temp, Sal, DO, pH, Secchi) for Pensacola (PE) and St Andrew (SA) stations. (ADD in SS, WC, TB, and CR here) <br>

***NFWF Quarterly reports consist of reporting obervations of Recruitment rates, Sedimentation rates, and water quality (Temp, Sal, DO, pH, Secchi) for Apalachicola (AB) and Suwannee Sound (SS) stations. Data included is the past 15 months, though may not include the most recent month(s) depending on sample/data processing. <br>
This report also includes semi-annual Shell Budget Model data for the past 15 months in Apalachicola; and quarterly Shell Budget Model data for the past 15 months in Suwannee Sound. <br>

