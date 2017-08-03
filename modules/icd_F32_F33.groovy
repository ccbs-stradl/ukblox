
icd_F32_F33 = {
        doc "Depressive disorder from hospital records"

        filter('F32_F33') {

                R {"""

                library(dplyr)
                library(tidyr)

                icd <- readRDS('$input.icd.rds')
                fields <- readRDS('$input.fields.rds')

                # ICD diagnosed depression
                mdd_icd <- as_data_frame(icd) %>%
                  filter(hes_icd10_block_category %in% c('F32', 'F33'))

                # number of diagnoses
                icd_f32_f33_count <- 
                mdd_icd %>%
                group_by(f.eid, hes_icd10_block_category) %>%
                tally() %>%
                ungroup() %>%
                spread(key=hes_icd10_block_category, value=n, fill=0)
                
                with(icd_f32_f33_count, table(F32, F33))
                
                icd_f32_f33 <- 
                icd_f32_f33_count %>%
                transmute(f.eid, icd_mdd=ifelse(F33 >= 1, yes='F33', no='F32'))
                
                # identify individuals with linked records
                # there is a better field for this (41253: source of
                # inpatient records) which we don't have, but comparing
                # participants counts for field 41253 (N=395,948) to
                # field 41142 (episodes containing main ICD10 diagnoses, 
                # N = 392,304), it appears there are only a few thousand
                # participants who underwent linkage but who did
                # not have any records of diagnoses
                has_icd <- unique(icd[['f.eid']])

                mdd_icd_cc <- fields %>%
                select(f.eid) %>%
                mutate(mdd_icd=ifelse(f.eid %in% icd_f32_f33[['f.eid']],
                                      yes=1,
                                      no=ifelse(f.eid %in% has_icd,
                                                yes=0,
                                                no=NA)))

                saveRDS(mdd_icd_cc, '$output')

                """}

        }
}
