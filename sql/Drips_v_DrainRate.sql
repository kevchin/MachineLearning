 
  SELECT 
--	count (distinct [SessionId])
--	StartTime,
--	*,
	distinct 
	cs.[OSBuildNumber]
	   ,cs.[SystemManufacturer]
      ,cs.[SystemProductName]
	  ,cs.[BIOSVersion]
    
		,ss.SessionId, 
	ss.StartTime, ss.DripsPercentage, 
	ss.EnergyChange, ss.EnergyChangeRate, 
	ss.Duration, ss.ServerUploadDateTimeUTC
	  
	
FROM [SharedDB].[SleepStudySession].[S_SleepStudySessionSummary] ss with (nolock)
join [SharedDb].[SleepStudySession].[Common] cs with (nolock)
on ss.SessionId = cs.SessionId
  where ss.[OnAc] = 0
  and cs.[OSBuildNumber] = 9431
/*
  and cs.[SystemManufacturer] like '%Intel Corp.%'
  and cs.[SystemProductName] like '%VALLEYVIEW%PLATFORM%'

  and cs.[SystemManufacturer] like '%Intel Corporation%'
  and cs.[SystemProductName] like '%Shark Bay Client platform%'

  and cs.[SystemManufacturer] like '%Qualcomm%'
  and cs.[SystemProductName] like '%LiQUID%'

  --and cs.[SystemProductName] like '%OemkS%'
  and cs.[SystemProductName] like 'Covington%'
  and cs.[SystemManufacturer] like '%NVIDIA%'


*/

/*
  and cs.[SystemManufacturer] like '%Intel Corp.%'
  and cs.[SystemProductName] like '%VALLEYVIEW%PLATFORM%'
*/

--  and cs.BIOSVersion like 'BLAKCRB1.86C.0045.R41.1305301434'
  and ss.[bitSleepStudySessionSummaryOnAc_AC_Power] = 0
  and ss.Duration > 29 
  --and ss.EnergyChange > 0
  and ss.ServerUploadDateTimeUTC > '2013-06-01'
  and ss.ServerUploadDateTimeUTC < '2013-06-30'
  order by cs.[OSBuildNumber] asc
  --group by SessionId, StartTime