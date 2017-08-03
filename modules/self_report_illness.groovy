self_reported_illness = {
       doc "Non-cancer illness codes, self-reported, in long format"
       transform('fields.rds') to('self_report_illness.rds') {
       R {"""

       library(ukbiobanker)
       library(dplyr)

       # read in raw fields
       fields <- readRDS('$input.fields.rds')

        # read coding file from the pipeline modules directory
        coding6 <- read.table(file.path(Sys.getenv('BPIPE_LIB'), 'coding6.tsv'), sep='\\t', header=T, stringsAsFactors=FALSE, quote=NULL)


       # get non-cancer illness code and age-of-diagnoses
       illnesses_array <- fields %>%
        select(f.eid, starts_with('f.20002.'), 
                      starts_with('f.20009.'))

        illnesses_long <- ukb_fieldarray2long(illnesses_array) %>%
          # recode missing ages of onset 
          mutate(f20009=ifelse(f20009 %in% c(-1, -2, -3), yes=NA, no=f20009))
        
        illnesses <- illnesses_long %>%
          rename(non_cancer_illness_code.f.20002=f20002,
                 non_cancer_illness_age.f.20009=f20009) %>%
          left_join(coding6[,c('coding', 'meaning')], 
                    by=c('non_cancer_illness_code.f.20002'='coding')) %>%
          select(f.eid, instance,
                 non_cancer_illness_code.f.20002,
                 non_cancer_illness.f.20002=meaning,
                 non_cancer_illness_age.f.20009)


          saveRDS(illnesses, file='$output')
        
       """}

       }


}
