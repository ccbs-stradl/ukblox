
mdd_exclusions = {
        doc "Exclusions to apply to MDD cases and controls"
        transform('fields.rds') to('mdd_exclusions.rds') {
        R {"""

        library(dplyr)

        fields <- readRDS('$input.fields.rds')
        
        # pipeline derived variables
        icd <- readRDS('$input.icd.rds')
        medications <- readRDS('$input.medications.rds')
        self_report_illness <- readRDS('$input.self_report_illness.rds')


        # List of prescription codes
        # antidepressants
        antidep_coding <- read.table(file.path(Sys.getenv('BPIPE_LIB'), 'treatment_medication_antidep.tsv'), header=T, sep='\\t')
        # anxiolytics
        anxiolytic_coding <- read.csv(file.path(Sys.getenv('BPIPE_LIB'),'treatment_medication_anxiolytic.tsv'), header=T, sep='\\t')
        # antipsychotics
        antipsychotic_coding <- read.csv(file.path(Sys.getenv('BPIPE_LIB'),'treatment_medication_antipsychotic.tsv'), header=T, sep='\\t')
        


        # ICD mental and behavioral disorder exclusions
        parkinsons_icd <- icd %>%
          filter(hes_icd10_block_category %in% 'G20') %>%
          transmute(f.eid, parkinsons_icd=1) %>%
          distinct()

        bipolar_icd <- icd %>%
          filter(hes_icd10_block_category %in% c('F30', 'F31')) %>%
          transmute(f.eid, bipolar_icd=1) %>%
          distinct()
        
        mpd_icd <- icd %>%
          filter(hes_icd10_code %in% c('F44.8')) %>%
          transmute(f.eid, mpd_icd=1) %>%
          distinct()
        
        scz_icd <- icd %>%
          filter(hes_icd10_block_category %in% paste0('F', 20:29)) %>%
          transmute(f.eid, scz_icd=1) %>%
          distinct()
        
        autism_icd <- icd %>%
          filter(hes_icd10_code %in% c('F84.0', 'F84.3', 'F84.5')) %>%
          transmute(f.eid, autism_icd=1) %>%
          distinct()
        
        
        id_icd <- icd %>%
          filter(hes_icd10_block_category %in% 
              c('F70.0', 'F70.1', 'F70.9', 'F72.9', 'F78.9', 'F79.9')) %>%
          transmute(f.eid, id_icd=1) %>%
          distinct()
        
        
        anxiety_icd <- icd %>%
          filter(hes_icd10_block_category %in% paste0('F', 40:43)) %>%
          transmute(f.eid, anxiety_icd=1) %>%
          distinct()
        
        mood_icd <- icd %>%
          filter(hes_icd10_block_category %in% paste0('F', 30:39)) %>%
          transmute(f.eid, mood_icd=1) %>%
          distinct()
        
        mdd_icd <- icd %>%
          filter(hes_icd10_block_category %in% c('F32', 'F33')) %>%
          transmute(f.eid, mdd_icd=1) %>%
          distinct()

        
        # self reported illnesses
        parkinsons_self <- self_report_illness %>%
          filter(non_cancer_illness_code.f.20002 == 1262) %>%
          transmute(f.eid, parkinsons_self=1) %>%
          distinct()
        
        bipolar_self <- self_report_illness %>%
          filter(non_cancer_illness_code.f.20002 == 1291) %>%
          transmute(f.eid, bipolar_self=1) %>%
          distinct()
        
        scz_self <- self_report_illness %>%
          filter(non_cancer_illness_code.f.20002 == 1289) %>%
          transmute(f.eid, scz_self=1) %>%
          distinct()

        # medications
        # Antidepressant prescriptions
        antidep <- medications %>%
          filter(treatment_medication_code_f20003 %in% antidep_coding[['treatment_medication_code']]) %>%
          transmute(f.eid, antidep=1) %>%
          distinct()
        
        # Anxiolytic prescriptions
        anxiolytic <- medications %>%
          filter(treatment_medication_code_f20003 %in% anxiolytic_coding[['treatment_medication_code']]) %>%
          transmute(f.eid, anxiolytic=1) %>%
          distinct()
        
        # Antipsychotic prescriptions
        antipsychotic <- medications %>%
          filter(treatment_medication_code_f20003 %in% antipsychotic_coding[['treatment_medication_code']]) %>%
          transmute(f.eid, antipsychotic=1) %>%
          distinct()
         
        # tag participants with exclusions 
        mdd_exclusions <- fields %>%
       # get all IDs
        select(f.eid) %>%
      # code if individuals has been identified to be excluded
        mutate(parkinsons_icd=f.eid %in% parkinsons_icd[['f.eid']],
               bipolar_icd=f.eid %in% bipolar_icd [['f.eid']],
               mpd_icd=f.eid %in% mpd_icd[['f.eid']],
               scz_icd=f.eid %in% scz_icd[['f.eid']],
               autism_icd=f.eid %in% autism_icd[['f.eid']],
               id_icd=f.eid %in% id_icd[['f.eid']],
               mood_icd=f.eid %in% mood_icd[['f.eid']],
               mdd_icd=f.eid %in% mdd_icd[['f.eid']],
               anxiety_icd=f.eid %in% anxiety_icd[['f.eid']],
               parkinsons_self=f.eid %in% parkinsons_self[['f.eid']],
               bipolar_self=f.eid %in% bipolar_self[['f.eid']],
               scz_self=f.eid %in% scz_self[['f.eid']],
               antidep=f.eid %in% antidep[['f.eid']],
               anxiolytic=f.eid %in% anxiolytic[['f.eid']],
               antipsychotic=f.eid %in% antipsychotic[['f.eid']])

        saveRDS(mdd_exclusions, '$output')

        """}
        }

}
