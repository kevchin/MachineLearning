 
  SELECT 
	distinct 
	cs.[OSBuildNumber]
--	,cs.[SystemManufacturer]
    ,cs.[SystemProductName]
--	,cs.[BIOSVersion]
--	,ss.SessionId, 
	,ss.StartTime, ss.DripsPercentage, 
	ss.EnergyChange, ss.EnergyChangeRate, 
	ss.Duration
	
FROM [SQMWindowsBlue].[SleepStudySession].[S_SleepStudySessionSummary] ss with (nolock)
join [SQMWindowsBlue].[SleepStudySession].[Common] cs with (nolock)
on ss.SessionId = cs.SessionId
  where ss.[OnAc] = 0
  and cs.[OSBuildNumber] = 9600
  and cs.SystemProductName = 'Surface with Windows RT'
  /*
  'Venue 8 Pro 5830'
  'T100TA'
  'Surface 2'
  */

  and ss.[bitSleepStudySessionSummaryOnAc_AC_Power] = 0
  --and ss.Duration > 29 
  and ss.EnergyChange > 0
  and cs.[ServerUploadDateTimeUTC] > dateadd(day, -7, getdate())
  order by cs.[OSBuildNumber] asc
  --group by SessionId, StartTime