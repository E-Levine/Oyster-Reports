# Oyster-Reports
PROJECT DESCRIPTION: <br>
Project for housing code for generating FWRI Oyster reports.  RMarkdown scripts exist for each Report type and funding agency, but their method of use varies based on current data source and compilation method. Please refer to the "File Structure" and "File Use" sections. 
Data managers are currently the only FWRI staff who will be able to execute these scripts. Code is still being written and revised.

GETTING STARTED: <br>
You will need:<br> 
1. Microsoft SQL Server Management Studio (SSMS)
2. R/RStudio
3. Git
4. (Optional) GitHub Desktop or other GUI git tool
<br>
SUGGESTED WORKFLOW:

1. If you need to make changes, "Create an Issue" in GitHub describing what changes, additions, or issues need to be addressed.
2. Pull a current copy of this repo to your local machine.
3. Create a new branch to address the issue. Please use format: issue-#-Example when naming a new branch where # is the issue number and Example briefly describes the issue. Alternatively, on GitHub, on the Issues page: Development > Create a branch.
4. Work on the issue in your local branch.
5. Commit changes to save your work.
6. If more time or contributions from others is needed, publish your branch back to the GitHub repo.
7. When the issue is resolved, issue a pull request to have the changes merged into the main branch and close the outstanding issue.
8. Once a branch is merged, delete that branch.
<br> 
FILE STRUCTURE: <br>
/CERP - RMarkdown scripts for Comprehensive Everglades Restoration Plan project - Current versions: Monthly, Quarterly.<br>
/DBHYDRO - R code for selecting, pulling, and cleaning DBHYDRO data. Cleaned data is maintained in the repo as designated within code. Folders are maintained to help distinguish between data saved locally vs in the repo. Currently tested and released for FLOW data. May required edits for other data types.<br>
/DMFM - RMarkdown scripts for sharing with the Division of Marine Fisheries Management  - Current versions: AS NEEDED.<br>
/FLTIG - RMarkdown scripts for the Florida Trustees Implementation Group Data Gaps project  - Current versions: Annually.<br>
/NFWF2 - RMarkdown scripts for the National Fish and Wildlife Foundation 2.0 project - Current versions: Quarterly..<br>
/Presentations - RMarkdown scripts for Presentations given to other groups. As needed. <br>
/PBC - RMarkdown scripts for Plam Beach County project - Current versions: Monthly, Quarterly.<br>

<br>
FILE USE:<br>
/CERP - Compiling monthly reports from Excel data files into Word doucument outputs. Compile quarterly summary data into html outputs and figures into Word document output.<br>
/DBHYDRO - Cleaning and compilation of downloaded data from DBHYDRO. Can summarize data for specified stations and dates within an estuary and save data output.<br>
/DMFM - Previous export of some NFWF data to Excel for a quick graph. Used Bags per acre.<br>
/FLTIG - .<br>
/NFWF2 - .<br>
/Presentations - Data displays for Presentations given to other groups. <br>
/PBC - Compiling monthly reports from Excel data files into Word doucument outputs.<br>
<br>

REPORT TYPES: <br>
***CERP and PBC monthly reports consist of reporting obervations of mean salinity, recruitment, and dermo for the specified monthly and most recent month prior. PBC reports also include reporting of mean sedimentation rate and percent organic content for stations north and south of the C-51 output. When applicable, mean live and dead counts from oyster surveys are included. <br>
***CERP and PBC quarterly reports consist of reporting obervations of surveys, recruitment, reproduction, disease, growth and mortality, and water quality (Salinity, Temperature, pH, DO mg/L, and Secchi %) for the specified quarter. Figures show data for the most recent 12 months. PBC reports also include reporting of mean sedimentation rate and percent organic content for stations north and south of the C-51 output. <br>
***DMFM Current file from a previous export of some NFWF data to Excel for a quick graph. Used Bags per acre among some other stats. Check before reusing. <br>
***FL-TIG Annual reports consist of reporting obervations of Recruitment rates, Sedimentation rates, and water quality (Temp, Sal, DO, pH, Secchi) for Pensacola (PE) and St Andrew (SA) stations. (ADD in SS, WC, TB, and CR here) <br>
***NFWF Quarterly reports consist of reporting obervations of Recruitment rates, Sedimentation rates, and water quality (Temp, Sal, DO, pH, Secchi) for Apalachicola (AB) and Suwannee Sound (SS) stations. Data included is the past 15 months, though may not include the most recent month(s) depending on sample/data processing. <br>
This report also includes semi-annual Shell Budget Model data for the past 15 months in Apalachicola; and quarterly Shell Budget Model data for the past 15 months in Suwannee Sound. <br>


