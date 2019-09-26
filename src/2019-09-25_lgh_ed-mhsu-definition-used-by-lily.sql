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
        OR chief_complaint_1_system in ('Mental health', 'SUBSTANCE USE'))
order by start_date_id
    , patient_id