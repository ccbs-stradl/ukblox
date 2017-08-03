icd = {
        doc "Hospital spell and episode data - ICD"

        transform('fields.rds') to('icd.rds') {
        R {"""

        library(dplyr)
        library(stringr)
        library(ukbiobanker)

        fields <- readRDS('$input.fields.rds')

        
         # ICD10 diagnoses
         icd10_main <- fields %>%
           select(f.eid, starts_with('f.41202.'))
         
         icd10_secondary <- fields %>%
           select(f.eid, starts_with('f.41204.'))
         
         icd10_main_long <- ukb_fieldarray2long(icd10_main) %>%
           rename(hes_icd10=f41202) %>%
           mutate(hes_icd10_precedence='main')
         
         icd10_secondary_long <- ukb_fieldarray2long(icd10_secondary) %>%
           rename(hes_icd10=f41204) %>%
           mutate(hes_icd10_precedence='secondary')
         
         icd10_long <- icd10_main_long %>%
           bind_rows(icd10_secondary_long)
         
         icd10_codes <- icd10_long %>%
           rename(hes_icd10_array=array) %>%
           transform(hes_icd10_block=str_sub(hes_icd10, 1, 1),   
                     hes_icd10_category=str_sub(hes_icd10, 2, 3), 
                     hes_icd10_subcategory=str_sub(hes_icd10, 4, 4), 
                     hes_icd10_block_category=str_sub(hes_icd10, 1, 3)) %>%
           transform(hes_icd10_code=paste(hes_icd10_block_category, hes_icd10_subcategory, sep='.')) %>%
           select(f.eid, instance, hes_icd10, hes_icd10_code, everything())

        saveRDS(icd10_codes, '$output')

        """}
        }


}

