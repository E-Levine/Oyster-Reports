# Data
BACKGROUND: Temporary storage for non-program data (e.g. DEP Water quality, USGS water flow, etc.). 
All program data should be accessed via local database copy (see /scripts/ReportLoadData.R)<br>

CONTENTS: <br>
1. DEP Water Quality (Temperature, Salinity, and Dissolved Oxygen) <br>
This file must be manually downloaded by: <br>
- visiting https://cdmo.baruch.sc.edu/aqs/customWaterMet.cfm <br>
- selecting 'Apalachicola Bay, FL <br>
- deselect all except 'apacpwq-p' and 'apadbwq-p'  <br>
- scroll to bottom and click 'Submit locations and proceed to next step' <br>
- select 'Temp', 'Sal', and 'DO_pct' <br>
- click 'Proceed To Choose Dates' <br>
- select '01/01/2002' to today's date <br>
- enter your information. A confirmation email will be sent to you <br>
- you will receive a seperate email with a link to the zip file download <br>
- extract the contents to Downloads
- save the data file (csv) in this folder as TSDO_20260815.csv where the numbers are the data accessed <br>
 <br>
2. 