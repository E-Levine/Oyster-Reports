# DMFM-Annual-Report
PROJECT DESCRIPTION: <br>
Project for housing code for generating annual FWRI Oyster report by estuary for DMFM / public consumption.   
Data managers are currently the only FWRI staff who will be able to execute these scripts. Code is still being written and revised.

SUGGESTED USE:

1. Open desired file "Annual_XX_Report". Substitute XX for the estuary code you need. 
2. Set the variables for the desired report. 
3. Click Run > 'Run All'. 
4. View report as HTML.
<br> 

FILE STRUCTURE: <br>
/data - temporary storage for non-program data (e.g. DEP Water quality, USGS water flow, etc.). All program data should be accessed via local database copy (see /scripts/ReportLoadData.R)<br>
/outputs - storage for outputs as needed. This may be temporary or finished reports.<br>
/scripts - modular scripts for generating reports
<br>
