
/*--------------------------------------------------------------------
lGH MHSU Patients in ED 
2019-09-25
Nayef 

This query will be used to define what counts as an MHSU patient for 
several other analysis pieces related to the biz case for a Psych Emerg
area in LGH ED. 

*/--------------------------------------------------------------------


select patient_id
    , start_date_id
    , chief_complaint_1_system
    , disch_ed_dx_1_cd
    , disch_ed_dx_1_desc 
from emergency
where facility_short_name = 'LGH'
    AND start_date_id BETWEEN '20180401' AND '20190624'
    and (disch_ed_dx_1_cd  BETWEEN ('F00%') AND ('F99%') 
        OR disch_ed_dx_1_cd  BETWEEN ('R44%') AND ('R46%')
        OR chief_complaint_1_system in ('Mental Health', 'SUBSTANCE MISUSE'))
    
    -- use the following to exclude "cognitive decline" conditions: 
    and disch_ed_dx_1_cd <> 'F03'  -- Dementia 
    
    
order by start_date_id
    , patient_id; 
    
    
-- distinct diagnoses 
select distinct disch_ed_dx_1_cd 
, disch_ed_dx_1_desc
from emergency
where facility_short_name = 'LGH'
    AND start_date_id BETWEEN '20180401' AND '20190624'
    and (disch_ed_dx_1_cd  BETWEEN ('F00%') AND ('F99%') 
        OR disch_ed_dx_1_cd  BETWEEN ('R44%') AND ('R46%')
        OR chief_complaint_1_system in ('Mental Health', 'SUBSTANCE MISUSE'))