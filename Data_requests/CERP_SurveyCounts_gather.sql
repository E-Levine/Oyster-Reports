--Code to output data into SAS data file format for CERP annual report

use [Oysters_25-03-31]
go

DROP TABLE #SurveyCounts
select FORMAT(DATEFROMPARTS(Year(CONVERT(DATE, SUBSTRING(Q.SampleEventID, CHARINDEX('_', Q.SampleEventID)+1, 8), 112)), MONTH(CONVERT(DATE, SUBSTRING(Q.SampleEventID, CHARINDEX('_', Q.SampleEventID)+1, 8), 112)), 1), 'MM/dd/yyyy') as SurveyDate,
	SUBSTRING(Q.SampleEventID, 12, 2) as Month,
	SUBSTRING(Q.SampleEventID, 8, 4) as Year,
	CASE WHEN MONTH(CONVERT(DATE, SUBSTRING(Q.SampleEventID, CHARINDEX('_', Q.SampleEventID) + 1, 8), 112)) = 3 THEN 'Spr'
		WHEN MONTH(CONVERT(DATE, SUBSTRING(Q.SampleEventID, CHARINDEX('_', Q.SampleEventID) + 1, 8), 112)) = 6 THEN 'Sum'
		WHEN MONTH(CONVERT(DATE, SUBSTRING(Q.SampleEventID, CHARINDEX('_', Q.SampleEventID) + 1, 8), 112)) = 9 THEN 'Fal'
		ELSE 'Win' END as Season,
	CASE WHEN MONTH(CONVERT(DATE, SUBSTRING(Q.SampleEventID, CHARINDEX('_', Q.SampleEventID) + 1, 8), 112)) = 3 THEN 'Spring'
		WHEN MONTH(CONVERT(DATE, SUBSTRING(Q.SampleEventID, CHARINDEX('_', Q.SampleEventID) + 1, 8), 112)) = 6 THEN 'Summer'
		WHEN MONTH(CONVERT(DATE, SUBSTRING(Q.SampleEventID, CHARINDEX('_', Q.SampleEventID) + 1, 8), 112)) = 9 THEN 'Fall'
		ELSE 'Winter' END + RIGHT(YEAR(CONVERT(DATE, SUBSTRING(Q.SampleEventID, CHARINDEX('_', Q.SampleEventID) + 1, 8), 112)), 2) AS Survey,
	CONCAT(F.Estuary, '-', F.SectionName) as Site,
	FORMAT(CONVERT(DATE, SUBSTRING(Q.SampleEventID, CHARINDEX('_', Q.SampleEventID)+1, 8), 112), 'MM/dd/yyyy') as Date, 
	F.StationNumber as Station,
	Right(Q.QuadratID,2) as Quadrat,
	Q.NumLive as LiveQrt, 
	Q.NumDead as DeadQtr,
	TotalVolume as Volume,
	TotalWeight as Weight
into #SurveyCounts
from hsdb.SurveyQuadrat as Q
inner join FixedLocations as F
on F.FixedLocationID = SUBSTRING(Q.QuadratID, 19, 4)
where (SampleEventID like 'SLSRVY_2024%' or SampleEventID like 'LXSRVY_2024%' or SampleEventID like 'CRSRVY_2024%') and F.FixedLocationID like '0%'

ALTER TABLE #SurveyCounts 
ADD 
	Project AS 'CERP',
	StationName AS 'Z',
	Section AS 'Z',
	Live VARCHAR(5) NULL,
	Dead VARCHAR(5) NULL;

	select * from #SurveyCounts
order by Date, Site, Station, Quadrat
