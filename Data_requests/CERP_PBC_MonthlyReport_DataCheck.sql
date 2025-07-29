--Check data status for monthly data

use [Oysters_25-07-23]
go

DECLARE @YrMn VARCHAR(6) = '202506'

--Check for COLL (dermo/WQ) and RCRT, SRVY when applicable
select * from TripInfo
where TripID like 'SL%_'+@YrMn+'%' or TripID like 'CR%_'+@YrMn+'%' or TripID like 'LW%_'+@YrMn+'%'
order by TripID

select * from SampleEventWQ
where SampleEventWQID like 'SLCOLL_'+@YrMn+'%' or SampleEventWQID like 'CRCOLL_'+@YrMn+'%' or SampleEventWQID like 'LWCOLL_'+@YrMn+'%'
order by SampleEventWQID

select * from Dermo
where SampleEventID like 'SLCOLL_'+@YrMn+'%' or SampleEventID like 'CRCOLL_'+@YrMn+'%' or SampleEventID like 'LWCOLL_'+@YrMn+'%'
order by SampleEventID

select * from Recruitment
where SampleEventID like 'SLRCRT_'+@YrMn+'%' or SampleEventID like 'CRRCRT_'+@YrMn+'%' or SampleEventID like 'LWRCRT_'+@YrMn+'%'
order by SampleEventID, ShellReplicate, ShellPosition