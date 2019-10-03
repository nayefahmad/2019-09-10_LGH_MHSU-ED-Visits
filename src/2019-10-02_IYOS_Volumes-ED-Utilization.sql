/*
Purpose: to pull statistics for the iYOS clients for C.D.
Author: Hans Aisake
Date Created: Oct 2, 2019
Comments:
- definition for iYOS clients and volumes provided by Courtney
*/

---------------------------
-- Find periods to report on
---------------------------

-- find fiscal periods to report on
IF OBJECT_ID('tempdb.dbo.#dates') is not null DROP TABLE #dates;
GO

SELECT distinct fiscalperiodlong, fiscalperiod, fiscalyear, fiscalperiodstartdate, fiscalperiodenddate
INTO #dates
FROM CommunityMart.dim.[date] as D
WHERE fiscalperiodlong >= '2018-04' AND FiscalPeriodEndDate < '2019-09-19'	--from what looks like the start of the PARIS iYOS data to the end date of the LGH ED extract Flora pulled
;
GO

------------------------
-- How to identify iYOS clients
-----------------------
-- The workers are part of the NS Foundry team
-- It's not clear that allocations, case note headers, and cast note contacts are associated with list of staff members provided
-- In short this process is not validated

	--allocated referrals
	SELECT D.FiscalPeriodLong, D.FiscalPeriod, D.FiscalYear
	, X.StaffName
	, COUNT(distinct SourceReferralID) as 'ActiveReferrals'
	, COUNT(distinct PatientID) as 'ActivePatients'
	FROM (
		SELECT distinct StaffName, allocationstartdate, ISNULL(AllocationEndDate, '2050-01-01') as 'AllocationEndDate', [ReferralActionType], SourceReferralID, PatientID, SourceSystemClientID
		FROM CommunityMart.dbo.vwPARISAllocation
		WHERE StaffName in ('WILKIN, CM LESLIE','HOLLAND, CM KEVIN', 'ROBINSON, LIA EDMUND (TED)', 'FERREIRA, CSLR ASHLEE') --1104 rows without key provider
		AND AllocationType='KEY PROVIDER'	--860 with key provider constraint
	)as X
	INNER JOIN #dates as D
	ON D.FiscalPeriodEndDate >= X.AllocationStartDate AND D.FiscalPeriodStartDate <= X.AllocationStartDate	--client active in period 
	GROUP BY D.FiscalPeriodLong, D.FiscalPeriod, D.FiscalYear
	, X.StaffName

---------------------------
-- Find activity of the iYOS - visits and client volumes using case notes
---------------------------

-- iYOS contacts and patients using Case Note contactType Office Visit
-- this number may be too big including parts of NS FOUNDRY that weren't desired.
--
IF OBJECT_ID('tempdb.dbo.#iYOS_counts') is not null DROP TABLE #iYOS_counts;
GO

SELECT [CommunityLHA], [ContactFiscalYear], [ContactFiscalPeriod]
, CASE	WHEN ReferralReasonServiceGroup='Child & Youth Services' THEN 'Child & Youth Services' 
		ELSE 'AllOtherGroups'
END as 'ReferralTypes*'
, COUNT(distinct [SourceCaseNoteHeaderID]) as 'iYOS_Office_Visits'
, COUNT(distinct [PatientID]) as 'iYOS_Acitive_Clients'
INTO #iYOS_counts
FROM CommunityMart.dbo.vwPARISCaseNoteContact as C
WHERE C.CaseNoteTeam='NS FOUNDRY'
AND CaseNoteContactType='Office Visit'
--AND ReferralReasonServiceGroup='Child & Youth Services' --maybe need this filter
GROUP BY [CommunityLHA], ContactFiscalYear, ContactFiscalPeriod
, CASE	WHEN ReferralReasonServiceGroup='Child & Youth Services' THEN 'Child & Youth Services' 
		ELSE 'AllOtherGroups'
END
;
GO

--iYOS contacts and patients using case note staff member
-- didn't seam to make sense; I'd argue the above is a better approach
--IF OBJECT_ID('tempdb.dbo.#iYOS_counts2') is not null DROP TABLE #iYOS_counts2;
--GO

--SELECT StaffName
--, D.FiscalYear
--, D.FiscalPeriod
--,  COUNT(distinct [SourceCaseNoteHeaderID]) as 'NumCaseNotes'
--, COUNT(distinct SourceSystemClientID) as 'NumClients'
--INTO #iYOS_counts2
--FROM CommunityMart.dbo.vwPARISCaseNoteHeader as CNH
--INNER JOIN #dates as D
--ON CNH.CaseNoteDate BETWEEN D.FiscalPeriodStartDate AND D.FiscalPeriodEndDate
--WHERE CaseNoteTeam='NS foundry'
--AND CaseNoteDate >='2018-09-01'
--AND StaffName in ('CM KEVIN HOLLAND','CSLR ASHLEE FERREIRA','LIA EDMUND (TED) ROBINSON','CM LESLIE WILKIN')
--GROUP BY StaffName
--, D.FiscalYear
--, D.FiscalPeriod
--;
--GO

---------------------------
-- Find utilization of ED by iYOS clients
---------------------------

-- uses the referrals to identify active periods instead of the case notes
-- it's unclear which way is better

--iYOS referrals to get clients and active dates
-- on hold might want to use the clients included in the actiity of office visits instead so the data is closer to the same set
IF OBJECT_ID('tempdb.dbo.#iYOS') is not null DROP TABLE #iYOS;
GO

SELECT ReferralTeam, PatientID, ReferralDate, SourceReferralID
, ISNULL(DischargeDate, '2050-01-01') as 'DischargeDate'	--quality of life thing. Set discahrge date to a far future date if null
, CommunityRegion, CommunityLHA
INTO #iYOS
FROM [CommunityMart].[dbo].[vwPARISReferral]
WHERE ReferralTeam like '%foundry%'	--there are several teams with this; seams to be a Primary Care Nurse with her own team label
;
GO

SELECT ED.start_date_fiscal_year_long
, ED.start_date_fiscal_period
, COUNT(distinct ED.encntr_key) as 'Total_ED_Visits'
, COUNT(distinct Y.PatientID) as 'iYOS_Clients_Visiting_ED'
, COUNT(CASE WHEN Y.PatientID is not null THEN ED.encntr_key ELSE NULL END) as 'iYOS_ED_Visits'
FROM DSSI.dbo.SIVC_Denodo_ED as ED
LEFT JOIN #iYOS as Y
ON ED.start_dt_tm BETWEEN Y.ReferralDate AND Y.DischargeDate	--ED visit durring referral
AND ED.patient_id=Y.PatientID	--same patient
GROUP BY ED.start_date_fiscal_year_long
, ED.start_date_fiscal_period



----------- CASE NOTE ED VISIT OVERLAP
--- the IYOS visit data ends up with 100% ED utilization. That concerns me, because if it was this obvious they wouldn't be asking us to pull data right?
IF OBJECT_ID('tempdb.dbo.#iYOS_Clients') is not null DROP TABLE #iYOS_Clients;
GO

SELECT distinct PatientID, ContactFiscalYear, ContactFiscalPeriod, SourceCaseNoteHeaderID
INTO #iYOS_Clients
FROM [CommunityMart].[dbo].[vwPARISCaseNoteContact] as C
WHERE C.CaseNoteTeam='NS FOUNDRY'
AND CaseNoteContactType='Office Visit'
;
GO

-- pull ED visits in period and for iYOS clients
IF OBJECT_ID('tempdb.dbo.#ed') is not null DROP TABLE #ed;
GO

--find ED visits durring active iYOS clients and Ed visits per period
SELECT start_date_fiscal_year, start_date_fiscal_period
, COUNT(distinct ED.encntr_key) as 'iYOS_ED_Visits'
, COUNT(distinct ED.patient_id) as 'Unique_ED_Patients'
, COUNT(Distinct Y.PatientID) as 'iYOS_Patients'
INTO #ed
FROM #iYOS_Clients as Y
LEFT JOIN [DSSI].[dbo].[SIVC_Denodo_ED] as ED
ON ED.patient_id=Y.PatientID
AND ED.start_date_fiscal_year_long = Y.ContactFiscalYear
AND ED.start_date_fiscal_period = Y.ContactFiscalPeriod
WHERE Facility_name in ('LGH Lions Gate','WHC Whistler','SGH Squamish','SSH Sechelt')	--coastal facilities  :  'PEM Ambulatory', 'PEM Pemberton' not included
GROUP BY start_date_fiscal_year, start_date_fiscal_period 
;
GO

---------------------------
-- Assessments - No data really
----------------------------

--iYOS assessments ; guessed definition
-- almost no assessments worth discussing
--IF OBJECT_ID('tempdb.dbo.#iYOS_assessments') is not null DROP TABLE #iYOS_assessments;
--GO

--SELECT AssessmentTeam, PatientID, SourceReferralID, [AssessmentDate], [AssessmentReason]
--INTO #iYOS_assessments
--FROM [CommunityMart].[dbo].[vwPARISAssessment]
--WHERE AssessmentTeam like '%foundry%'	--there are several teams with this; seams to be a Primary Care Nurse with her own team label
--AND SourceReferralID in (select SourceReferralID FROM #iYOS)	--assessment associated with the iYos referrals
--;
--GO
