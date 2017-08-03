
gp_psych_nerves = {
        doc "Help seeking behavior for nerves, depression, and anxiety"

        transform('fields.rds') to ('nerves.rds') {

                R {"""
                 
                library(ukbiobanker)
                library(dplyr)

                fields <- readRDS('$input.fields.rds')

                nerves_long <- fields %>%
                  select(f.eid, starts_with('f.2090.'),
                                starts_with('f.2100.')) %>%
                  ukb_fields2long()

                # code nerves. 1 if Yes to
                #   2090 Seen doctor (GP) for nerves, anxiety, tension or depression OR
                #   2100 Seen a psychiatrist for nerves, anxiety, tension or depression
                # 0 if 'No' to one or both
                # NA if missing/declined response 
                nerves <- nerves_long %>%
                  mutate(nerves_gp_psych.f2090_2100=
                  ifelse(f2090_0 %in% 'Yes' | f2100_0 %in% 'Yes',
                    yes=1,
                    no=ifelse((f2090_0 %in% 'No' | f2100_0 %in% 'No') &
                              ! f2090_0 %in% 'Prefer not to anwser' &
                              ! f2100_0 %in% 'Prefer not to answer',
                              yes=0, no=NA)))


                saveRDS(nerves, '$output')


                """}

        }

}

nerves_casecontrol = {

        doc "'nerves': ever endorsed seeing a GP or psychiatrist for nerves, anxiety, depression"

        filter('casecontrol') {

        R {"""

        library(dplyr)

        nerves <- readRDS('$input.nerves.rds')

        mdd_nerves_cc <- nerves %>%
        group_by(f.eid) %>%
        summarize(mdd_nerves=max(nerves_gp_psych.f2090_2100, na.rm=T))

        saveRDS(mdd_nerves_cc, '$output')

        """}
        }
}

nerves = segment { gp_psych_nerves + nerves_casecontrol }
