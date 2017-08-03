
medications = {
        doc "Treatment/medications"
        transform('fields.rds') to('medications.rds') {
        R {"""

        library(ukbiobanker)
        library(dplyr)

        fields <- readRDS('$input.fields.rds')

        # read coding file from the pipeline modules directory
        coding4 <- read.table(file.path(Sys.getenv('BPIPE_LIB'), 'coding4.tsv'), sep='\\t', header=T, stringsAsFactors=FALSE, quote=NULL)

        
        # Medications 
        medication_codes <- fields %>%
          select(f.eid, starts_with('f.20003'))
        
        # reshape to long format
        medications_long <- ukb_fieldarray2long(medication_codes)
        
        # get meanings for each coding
        medications <- medications_long %>%
          select(f.eid, instance,
                 treatment_medication_code_f20003=f20003) %>%
          left_join(coding4,  
                    by=c('treatment_medication_code_f20003'='coding')) %>%
          rename(treatment_medication_f20003=meaning)

        saveRDS(medications, '$output')


        """}
        }
}
