--Code to output shell height data into SAS data file format for CERP annual report
use [Oysters_25-07-23]
go

DECLARE @Site VARCHAR(2) = 'CR'

DROP TABLE #SurveySHeights
select 
	FORMAT(DATEFROMPARTS(Year(CONVERT(DATE, SUBSTRING(SH.QuadratID, CHARINDEX('_', SH.QuadratID)+1, 8), 112)), MONTH(CONVERT(DATE, SUBSTRING(SH.QuadratID, CHARINDEX('_', SH.QuadratID)+1, 8), 112)), 1), 'MM/dd/yyyy') as SurveyDate,
	SUBSTRING(SH.QuadratID, 12, 2) as Month,
	SUBSTRING(SH.QuadratID, 8, 4) as Year,
	CASE WHEN MONTH(CONVERT(DATE, SUBSTRING(SH.QuadratID, CHARINDEX('_', SH.QuadratID) + 1, 8), 112)) = 3 THEN 'Spr'
		WHEN MONTH(CONVERT(DATE, SUBSTRING(SH.QuadratID, CHARINDEX('_', SH.QuadratID) + 1, 8), 112)) = 6 THEN 'Sum'
		WHEN MONTH(CONVERT(DATE, SUBSTRING(SH.QuadratID, CHARINDEX('_', SH.QuadratID) + 1, 8), 112)) = 9 THEN 'Fal'
		ELSE 'Win' END as Season,
	CASE WHEN MONTH(CONVERT(DATE, SUBSTRING(SH.QuadratID, CHARINDEX('_', SH.QuadratID) + 1, 8), 112)) = 3 THEN 'Spring'
		WHEN MONTH(CONVERT(DATE, SUBSTRING(SH.QuadratID, CHARINDEX('_', SH.QuadratID) + 1, 8), 112)) = 6 THEN 'Summer'
		WHEN MONTH(CONVERT(DATE, SUBSTRING(SH.QuadratID, CHARINDEX('_', SH.QuadratID) + 1, 8), 112)) = 9 THEN 'Fall'
		ELSE 'Winter' END + RIGHT(YEAR(CONVERT(DATE, SUBSTRING(SH.QuadratID, CHARINDEX('_', SH.QuadratID) + 1, 8), 112)), 2) AS Survey,
	CONCAT(F.Estuary, '-', F.SectionName) as Site,
	FORMAT(CONVERT(DATE, SUBSTRING(SH.QuadratID, CHARINDEX('_', SH.QuadratID)+1, 8), 112), 'MM/dd/yyyy') as Date,
	F.StationNumber as Station,
	Right(SH.QuadratID,2) as Quadrat,
	SH.ShellHeight as SH
into #SurveySHeights
from hsdb.SurveySH as SH
inner join FixedLocations as F
on F.FixedLocationID = SUBSTRING(SH.QuadratID, 19, 4) 
where SH.QuadratID like @Site+'%' and F.FixedLocationID like '0%'
order by SH.ShellHeightID


ALTER TABLE #SurveySHeights 
ADD 
	Project AS 'CERP',
		StationName AS '-999'

select * from #SurveySHeights
order by Year, Month, Site, Station, Quadrat