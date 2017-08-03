
mdd = {
        doc "Compile MDD phenotypes together, removing exclusions"
        transform('fields.rds') to ('mdd_phenotypes.rds') {
        R {"""

        library(dplyr)
        library(tidyr)

        fields <- readRDS('$input.fields.rds')
        mdd_exclusions <- readRDS('$input.mdd_exclusions.rds')
        mdd_smith_cc <- readRDS('$input.smith_putative_mdd.casecontrol.rds')
        bpd_smith_cc <- readRDS('$input.smith_putative_bpd.casecontrol.rds')
        nerves_cc <- readRDS('$input.nerves.casecontrol.rds')
        mdd_icd <- readRDS('$input.icd.F32_F33.rds')


       bpd_smith_cc_ids <- filter(bpd_smith_cc, bpd_smith == 1)[['f.eid']]

          # Exclusions for cases and controls and just for controls
          mdd_case_control_exclude <- mdd_exclusions %>%
            filter(bipolar_icd | bipolar_self | 
                   mpd_icd |
                   scz_icd | scz_self |
                   antipsychotic | 
                   f.eid %in% bpd_smith_cc_ids)

          mdd_control_exclude <- mdd_exclusions %>%
            filter(mood_icd | antidep)
           
          # functions to set phenotypes of excluded partipants to missing
          na_casecontrols <- function(f.eid, pheno) ifelse(f.eid %in% mdd_case_control_exclude[['f.eid']], yes=NA, no=pheno)
          na_controls <- function(f.eid, pheno) ifelse(f.eid %in% mdd_control_exclude[['f.eid']] & pheno == 0, yes=NA, no=pheno)

          # join phenotypes
          mdd_smith_nerves_icd <- nerves_cc %>%
            full_join(mdd_smith_cc, by='f.eid') %>%
            full_join(mdd_icd, by='f.eid') %>%
          # apply exclusions
            mutate(mdd_nerves=na_casecontrols(f.eid, mdd_nerves),
                   mdd_smith=na_casecontrols(f.eid, mdd_smith),
                   mdd_icd=na_casecontrols(f.eid, mdd_icd)) %>%
            mutate(mdd_nerves=na_controls(f.eid, mdd_nerves),
                   mdd_smith=na_controls(f.eid, mdd_smith),
                   mdd_icd=na_controls(f.eid, mdd_icd))

          table(mdd_smith_nerves_icd[2:4], useNA='a')

          prev <- function(x) {tab <- table(x); tab / sum(tab)}
         
          table(mdd_smith_nerves_icd[['mdd_nerves']])
          prev(mdd_smith_nerves_icd[['mdd_nerves']])
          table(mdd_smith_nerves_icd[['mdd_smith']])
          prev(mdd_smith_nerves_icd[['mdd_smith']])
          table(mdd_smith_nerves_icd[['mdd_icd']])
          prev(mdd_smith_nerves_icd[['mdd_icd']])


          # reconcile: transfer ICD caseness to other measures of MDD
          # and remove smith cases that have no ICD diagnoses from ICD
          # definition
          mdd_reconcile <- mdd_smith_nerves_icd %>%
          mutate(mdd_nerves=ifelse(mdd_icd %in% 1,
                                   yes=1,
                                   no=mdd_nerves),
                 mdd_smith=ifelse(mdd_icd %in% 1,
                                  yes=1,
                                  no=mdd_smith),
                 mdd_icd=ifelse(mdd_icd %in% 0 & mdd_smith %in% 1,
                                yes=NA,
                                no=mdd_icd))

        table(mdd_reconcile[2:4], useNA='a')

        table(mdd_reconcile[['mdd_nerves']])
        prev(mdd_reconcile[['mdd_nerves']])
        table(mdd_reconcile[['mdd_smith']])
        prev(mdd_reconcile[['mdd_smith']])
        table(mdd_reconcile[['mdd_icd']])
        prev(mdd_reconcile[['mdd_icd']])

        saveRDS(mdd_reconcile, '$output')

        """}
        }
}


Bpipe.run { fields + [ self_reported_illness, icd + icd_F32_F33, smith, medications, nerves ] + mdd_exclusions + mdd }
