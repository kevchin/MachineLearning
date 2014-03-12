SELECT TOP 50000  
	cs.OSBuildNumber, 
	'1' as identityField,
	cs.SystemProductName as ProductName,
	case 
	when (TopOffenderType = 0) then 'Activator'
	when (TopOffenderType = 1) then 'Device'
	when (TopOffenderType = 2) then 'PDC'
	when (TopOffenderType = 3) then 'Net'
	when (TopOffenderType = 4) then 'Other'
	when (TopOffenderType = 5) then 'App Detail'
	else 'Unknown'
	end 'OffenderType',
	case 
	when (TopOffenderName like 'Scan%') then 'WU Scan'
	else TopOffenderName
	end OffenderName,
	(cast(ss.[SessionId] as varchar(36)) + ':' +
		cast(ss.[Duration] as varchar(10)) + ':' +
		cast(ss.[EnergyChange] as varchar(10))) as uSessionId,
		offender.TopOffenderActiveTimePercentage,
		ss.DripsPercentage,
		ss.HWDripsPercentage,
		ss.[EnergyChangeRate],
		ss.Duration
  FROM [SQMWindowsBlue].[SleepStudySession].[S_SleepStudySessionSummary] ss with (nolock)
  join [SQMWindowsBlue].[SleepStudySession].[S_SleepStudyTopOffenders] offender with (nolock)
  on ss.[SessionId] = offender.[SessionId] 
  and ss.[Duration] = offender.[Duration] 
  and ss.[EnergyChange] = offender.[EnergyChange]
  join [SQMWindowsBlue].[SleepStudySession].[Common] cs with (nolock)
  on ss.[SessionId] = cs.SessionId
where 
cs.OSBuildNumber = 9600
and   
	cs.[ServerUploadDateTimeUTC] > dateadd(day, -21, getdate())
  --and ss.[ServerUploadDateTimeUTC] > dateadd(day, -15, getdate())
  and ss.OnAc = 0
  and ss.Duration between 30 and 24*60*4
  and offender.TopOffenderType <> 5
  and offender.TopOffenderLevel < 2
  --and offender.TopOffenderType <> 1
  and (ss.DripsPercentage < 1
  or ss.HWDripsPercentage = 0)
  and offender.TopOffenderActiveTimePercentage > 1
  and (
[SystemProductName] = 'Surface with Windows RT'
--or 
--[SystemProductName] = 'Shark Bay Client platform'
--or 
--[SystemProductName] = 'Covington'
--or 
--[SystemProductName] =  'RX-114'
--or 
--[SystemProductName] =  'XYZXYZXYZ'
--or 
--[SystemProductName] =  'VALLEYVIEW B2 PLATFORM'
)