--Gather random survey data from database for mapping 
--Updates to Survey_station_data file

use[Oysters_24-10-31]
go

DECLARE @StartDate DATE = '2024-07-01'
DECLARE @EndDate DATE = '2024-12-31'

select SampleEventID, SUM(NumLive) as TotalLive, SUM(NumDead) as TotalDead
	from SurveyQuadrat where SampleEventID in (select SampleEventID from SampleEvent where TripID in (select TripID from TripInfo where (TripID like 'CRSRVY%' or TripID like 'TBSRVY%') and (TripDate >= @StartDate and TripDate <= @EndDate)))
	group by SampleEventID	
	order by SampleEventID

select SampleEventID, SUM(NumLive) as TotalLive, SUM(NumDead) as TotalDead
	from hsdb.SurveyQuadrat where SampleEventID in (select SampleEventID from hsdb.SampleEvent where TripID in (select TripID from hsdb.TripInfo where (TripID like 'CRSRVY%' or TripID like 'TBSRVY%') and (TripDate >= @StartDate and TripDate <= @EndDate)))
	group by SampleEventID
	order by SampleEventID

