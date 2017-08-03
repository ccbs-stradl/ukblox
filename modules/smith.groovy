
putative_mdd = {
        doc "Putative MDD Smith"
        transform('fields.rds') to ('smith_putative_mdd.rds') {

        R {"""


        library(dplyr)
        library(ukbiobanker)


        fields <- readRDS('$input.fields.rds')

        # get participant ID and depression questions
        mood_vars <- fields %>%
        ##    4598 Ever depressed/down for a whole week, plus
        ##    4609   At least two weeks duration, plus
        ##    4620   Only one episode, plus
        ##    4631 Ever anhedonic (unenthusiasm/uninterest) for a whole 
        ##         week, plus
        ##    5375    At least two weeks, plus
        ##    5386    Only one episode, plus
        ##    2090 Ever seen a GP or 2100 a psychiatrist for nerves,
        ##         anxiety, depression
        select(f.eid, starts_with('f.2090.'),
                      starts_with('f.2100.'),
                      starts_with('f.4598.'),
                      starts_with('f.4609.'),
                      starts_with('f.4620.'),
                      starts_with('f.4631.'),
                      starts_with('f.5375.'),
                      starts_with('f.5386.'))
        
        mood_vars_long <- ukb_fields2long(mood_vars)
        
        ## Response recoding
        
        # Recode
        # data-coding 100291:
        #   -1 Do not know
        #   -3 Prefer not to answer
        # else OK
        ok100291 <- function(x) recode(x, "-1='Do not know';-3='Prefer not to answer';NA=NA;else='OK'", as.factor=TRUE)
        # in data, set -1 and -3 to missing
        na100291 <- function(x) ifelse(x %in% c(-1, -3), yes=NA, no=x)
        
        ok_missing_levels <- c('Do not know', 'Prefer not to answer', NA)
        
        
        # ## Code MDD criteria
        
        mdd_criteria <- mood_vars_long %>%
        ## Single probable episode of major depression:
        ##  EITHER:
        ##    4598 Ever depressed/down for a whole week, plus
        ##    4609 At least two weeks duration, plus
        ##    4620 Only one episode, plus
        ##    2090 Ever seen a GP or 2100 a psychiatrist for nerves,
        ##         anxiety, depression
        mutate(single_criteria1=
               f4598_0 %in% 'Yes' &
               f4609_0 >= 2 &
               f4620_0 == 1 &
               (f2090_0 %in% 'Yes' | f2100_0 %in% 'Yes')) %>%
        ##  OR:
        ##    4631 Ever anhedonic (unenthusiasm/uninterest) for a whole 
        ##         week, plus
        ##    5375 At least two weeks, plus
        ##    5386 Only one episode, plus
        ##    2090 Ever seen a GP or 2100 a psychiatrist for nerves, 
        ##         anxiety, depression
        mutate(single_criteria2=
               f4631_0 %in% 'Yes' &
               f5375_0 >= 2 &
               f5386_0 == 1 &
               (f2090_0 %in% 'Yes' | f2100_0 %in% 'Yes'))
        
        ## Probable recurrent major depression (moderate):
        mdd_criteria <- mdd_criteria %>%
        ## EITHER:
        ##  4598 Ever depressed/down for a whole week, plus
        ##  4609 At least two weeks duration, plus
        ##  4620 At least two episodes, plus
        ##  2090 Ever seen a GP (but not a psychiatrist) for nerves, 
        ##       anxiety, depression
        mutate(moderate_criteria1=
               f4598_0 %in% 'Yes' &
               f4609_0 >= 2 &
               f4620_0 >= 2 &
               f2090_0 %in% 'Yes' &
               !f2100_0 %in% 'Yes') %>%
        ## OR:
        ##  4631 Ever anhedonic (unenthusiasm/uninterest) for a whole
        ##       week, plus
        ##  5375 At least two weeks, plus
        ##  5386 At least two episodes, plus
        ##  2090 Ever seen a GP (but not a psychiatrist) for nerves,   
        ##       anxiety, depression
        mutate(moderate_criteria2=
               f4631_0 %in% 'Yes' &
               f5375_0 >= 2 &
               f5386_0 >= 2 &
               f2090_0 %in% 'Yes' &
               !f2100_0 %in% 'Yes')
        
        ## Probable recurrent major depression (severe):
        mdd_criteria <- mdd_criteria %>%
        ## EITHER:
        ##  4598 Ever depressed/down for a whole week, plus
        ##  4609 At least two weeks duration, plus
        ##  4620 At least two episodes, plus
        ##  2100 Ever seen a psychiatrist for nerves, anxiety, depression
        mutate(severe_criteria1=
              !f4598_0 %in% 'No' &
               f4609_0 >= 2 &
               f4620_0 >= 2 &
               f2100_0 %in% 'Yes') %>%
        ## OR:
        ##  4631 Ever anhedonic (unenthusiasm/uninterest) for a whole
        ##       week, plus
        ##  5375 At least two weeks, plus
        ##  5386 At least two episodes, plus
        ##  2100 Ever seen a psychiatrist for nerves, anxiety, depression
        mutate(severe_criteria2=
              !f4620_0 %in% 'Yes' &
               f5375_0 >= 2 &
               f5386_0 >= 2 &
               f2100_0 %in% 'Yes')
        
        ## unclassified or help-seeking behavior. 
        mdd_criteria <- mdd_criteria %>%
        # answers one of following as Yes
        ##  2090 Ever seen a GP or 2100 a psychiatrist for nerves, 
        ## but does not qualify of the above criteria
        mutate(help_seeking_criteria=
               f2090_0 %in% 'Yes' |
               f2100_0 %in% 'Yes') %>%
        # answers one of the symptoms as yes but has not seen a psychiatrist
        ##  4598 Ever depressed/down for a whole week
        ##  4631 Ever anhedonic (unenthusiasm/uninterest) for a whole
        ##  2100 a psychiatrist for nerves 'No'
        mutate(subsymptom_criteria=
               f4598_0 %in% 'Yes' |
               f4620_0 %in% 'Yes')
        
        # depression absent
        mdd_criteria <- mdd_criteria %>%
        # depression absent (no symptoms):
        ##  4598 Ever depressed/down for a whole week 'No' AND
        ##  4631 Ever anhedonic (unenthusiasm/uninterest) for a whole
        mutate(absent_criteria_nosymptoms=
               f4598_0 %in% 'No' &
               f4631_0 %in% 'No') %>%      
        # depression absent (no help seeking) (but was assessed with mood items)
        ##  2090 Ever seen a GP 'No' OR 2100 a psychiatrist for nerves 'No'
        mutate(nohelp_seeking_criteria=
               !f2090_0 %in% 'Yes' &
               !f2100_0 %in% 'Yes' &
               !is.na(f4598_0) &
               !is.na(f4631_0)) %>%
        # depression absent (probable):
        ##  4598 Ever depressed/down for a whole week 'No' OR
        ##  4631 Ever anhedonic (unenthusiasm/uninterest) for a whole
        ##     AND
        ##  2090 Ever seen a GP 'No' OR 2100 a psychiatrist for nerves 'No'
        mutate(absent_criteria_probable=
               (f4598_0 %in% 'No' |
                f4631_0 %in% 'No') &
               (f2090_0 %in% 'No' |
                f2100_0 %in% 'No')) %>%
        # depression absent: certain. all gating questions 'No'
        ##  4598 Ever depressed/down for a whole week 'No' AND
        ##  4631 Ever anhedonic (unenthusiasm/uninterest) for a whole AND
        ##  2090 Ever seen a GP 'No' AND 2100 a psychiatrist for nerves 'No'
        mutate(absent_criteria_certain=
               f4598_0 %in% 'No' &
               f4631_0 %in% 'No' &
               f2090_0 %in% 'No' &
                f2100_0 %in% 'No')
        
        # code absent, single, moderate, severe, and subsymptomatic where
        # individuals meet the criteria in the category but not any of the
        # more specific or severe criteria.
        # use X %in% TRUE for coding so that NA is FALSE
        code_criterion <- function(default, x, label) ifelse(x %in% TRUE, yes=label, no=default)
        smith <- mdd_criteria %>%
        mutate(mdd_smith_cat=NA) %>%
        mutate(mdd_smith_cat=code_criterion(mdd_smith_cat, absent_criteria_nosymptoms, 'absent (no symptoms)')) %>%
        mutate(mdd_smith_cat=code_criterion(mdd_smith_cat, nohelp_seeking_criteria,    'absent (no help seeking)')) %>%
        mutate(mdd_smith_cat=code_criterion(mdd_smith_cat, absent_criteria_probable,   'absent (probable)')) %>%
        mutate(mdd_smith_cat=code_criterion(mdd_smith_cat, absent_criteria_certain,    'absent')) %>%
        mutate(mdd_smith_cat=code_criterion(mdd_smith_cat, help_seeking_criteria,      'help seeking')) %>%
        mutate(mdd_smith_cat=code_criterion(mdd_smith_cat, subsymptom_criteria,        'subsymptom')) %>%
        mutate(mdd_smith_cat=code_criterion(mdd_smith_cat, single_criteria1,           'single')) %>%
        mutate(mdd_smith_cat=code_criterion(mdd_smith_cat, single_criteria2,           'single')) %>%
        mutate(mdd_smith_cat=code_criterion(mdd_smith_cat, moderate_criteria1,         'moderate')) %>%
        mutate(mdd_smith_cat=code_criterion(mdd_smith_cat, moderate_criteria2,         'moderate')) %>%
        mutate(mdd_smith_cat=code_criterion(mdd_smith_cat, severe_criteria1,           'severe')) %>%
        mutate(mdd_smith_cat=code_criterion(mdd_smith_cat, severe_criteria2,           'severe')) %>%
        mutate(mdd_smith=recode(mdd_smith_cat, "absent" = 0L,
                                               "absent (no symptoms)" = 0L,
                                               "absent (no help seeking)" = 0L,
                                               "absent (probable)" = 0L,
                                               "subsymptom" = 0L,
                                               "help seeking" = 0L,
                                               "single" = 1L,
                                               "moderate" = 1L,
                                               "severe" = 1L,
                                               .default = NA_integer_))


        table(smith$mdd_smith_cat, useNA='a')

        case_control_n <- table(smith$mdd_smith)

        case_control_n / sum(case_control_n)
        sum(case_control_n)


        saveRDS(smith, '$output')
        

                """}
        }

}

putative_mdd_cc = {

        doc "Smith definition of depression at any assessment (case control status)"

        filter('casecontrol') {

        R {"""

        library(dplyr)

        smith_putative_mdd <- readRDS('$input.smith_putative_mdd.rds')
        
        # Smith definition reach at any assessment
        mdd_smith_cc <- smith_putative_mdd %>%
          select(f.eid, mdd_smith) %>%
          group_by(f.eid) %>%
        # rely on variable's numeric coding
          summarize(mdd_smith=max(mdd_smith, na.rm=T))


        saveRDS(mdd_smith_cc, '$output')
        

        """}

        }

        forward(input.rds, output)
}



putative_bpd = {

        doc "Smith definition of putative BPD"

        transform('fields.rds') to ('smith_putative_bpd.rds') {

        R {"""

        library(ukbiobanker)
        library(dplyr)

        fields <- readRDS('$input.fields.rds')
        
         # get participant ID and mood disorder  questions
         mood_vars <- fields %>%
         ## 4642	Ever manic/hyper for 2 days
         ## 4653	Ever highly irritable/argumentative for 2 days
         ## 5663	Length of longest manic/irritable episode
         ## 5674	Severity of manic/irritable episodes	
         select(f.eid, starts_with('f.4642.'),
                       starts_with('f.4653.'),
                       starts_with('f.5663.'),
                       starts_with('f.5674.'))
         
         mood_vars_long <- ukb_fields2long(mood_vars)
         
         # get participant ID and manic symptom questions
         manic_symptoms_long <- fields %>%
         ## 6156	Manic/hyper symptoms
           select(starts_with('f.6156.'), f.eid) %>%
           ukb_fieldarray2long()
          
         # symptoms can appear anywhere in the array 
         manic_symptom_options  <- sort(unique(manic_symptoms_long[['f6156']]))
         
         all_above <- manic_symptom_options[1]
         less_sleep <- manic_symptom_options[2]
         more_active <- manic_symptom_options[3]
         more_ideas <- manic_symptom_options[4]
         more_talk <- manic_symptom_options[5]
         none_above <- manic_symptom_options[6]
         
         # symptoms = 1 if symptom or all of the above were checked
         manic_symptoms <- manic_symptoms_long %>%
           group_by(f.eid, instance) %>%
           summarize(manic_active_f6156=any(c(more_active, all_above) %in% f6156),
                     manic_ideas_f6156=any(c(more_ideas, all_above) %in% f6156),
                     manic_talk_f6156=any(c(more_talk, all_above) %in% f6156),
                     manic_sleep_f6156=any(c(less_sleep, all_above) %in% f6156)) %>%
           # count number of symptoms
           mutate(manic_symptoms_count_f6156=manic_active_f6156 + manic_ideas_f6156 + manic_talk_f6156 + manic_sleep_f6156)
         
         mood <- mood_vars_long %>% 
          left_join(manic_symptoms, by=c('f.eid', 'instance')) %>%
          mutate(manic_symptoms_count_f6156=ifelse(is.na(manic_symptoms_count_f6156), yes=0, no=manic_symptoms_count_f6156))
         
         
         #' ## Code bipolar disorder criteria
         
         manic_duration_week_f5663 <- levels(mood$f5663_0)[5]
         manic_problems_yes_f5674 <- levels(mood$f5674_0)[4]
         
         putative_bpd <- mood %>%
         ## Probable bipolar disorder (type I):
         ## Either: 4642 Ever manic/hyper 2 days or 4653 Ever irritable/
         ##    argumentative for 2 days, plus
         ## At least 3 from 6156.01 (more active), 6156.02 (more talkative),
         ##     6156.03 (needed less sleep), and 6156.04 (more creative/more ideas), 
         ##     plus
         ## 5663 Duration of a week or more, plus
         ## 5674 needed treatment or caused problems at work
         mutate(bpd_typeI=
           (f4642_0 %in% 'Yes' |
            f4653_0 %in% 'Yes') &
           manic_symptoms_count_f6156 >= 3 &
           f5663_0 %in% manic_duration_week_f5663 &
           f5674_0 %in% manic_problems_yes_f5674) %>%
         ## Probable bipolar disorder (type I):
         ## Either: 4642 Ever manic/hyper 2 days or 4653 Ever irritable/
         ##    argumentative for 2 days, plus
         ## At least 3 from 6156.01 (more active), 6156.02 (more talkative),
         ##     6156.03 (needed less sleep), and 6156.04 (more creative/more ideas), 
         ##     plus
         ## 5663 Duration of a week or more, plus
         mutate(bpd_typeII=
           (f4642_0 %in% 'Yes' |
            f4653_0 %in% 'Yes') &
           manic_symptoms_count_f6156 >= 3 &
           f5663_0 %in% manic_duration_week_f5663 &
           ! f5674_0 %in% manic_problems_yes_f5674)

         saveRDS(putative_bpd, '$output')
                 

                """}
        }

}

putative_bpd_cc = {

        doc "Smith BPD definition reach at any assessment"
        filter('casecontrol') {
        R {"""

        library(dplyr)

        smith_putative_bpd <- readRDS('$input.smith_putative_bpd.rds')
        bpd_smith_cc <- smith_putative_bpd %>%
          mutate(bpd_smith=as.numeric(bpd_typeI | bpd_typeII)) %>%
          group_by(f.eid) %>%
        # rely on variable's numeric coding
          summarize(bpd_smith=max(bpd_smith, na.rm=T))

        saveRDS(bpd_smith_cc, '$output')
        """}
        }
}

smith_align = {

        doc "Align putative MDD definition with  Smith dervied variables"

        transform('.rds') to ('.smith_align.txt') {

        R {"""

        library(gdata)
        library(dplyr)
        library(ukbiobanker)

        fields <- readRDS('$input.fields.rds')
        putative_mdd <- readRDS('$input.smith_putative_mdd.rds')
        putative_bpd <- readRDS('$input.smith_putative_bpd.rds')


        bpd_mdd_status <- fields %>%
          select(f.eid, starts_with('f.20126.')) %>%
          ukb_fields2long()


        smith_align_mdd <- 
        putative_mdd %>%
        full_join(bpd_mdd_status, by=c('f.eid', 'instance')) %>%
        group_by(f20126_0, mdd_smith_cat) %>%
        tally() %>%
        filter(n != 0)

        smith_align_bpd <-
        putative_bpd %>%
        full_join(bpd_mdd_status, by=c('f.eid', 'instance')) %>%
        group_by(f20126_0, bpd_typeI, bpd_typeII) %>%
        tally() 

        write.fwf(as.data.frame(smith_align_mdd), file='$output', na='NA')
        write.fwf(as.data.frame(smith_align_bpd), file='$output', na='NA', append=TRUE)


        """}
        }

}

smith = segment {
        [ putative_mdd + putative_mdd_cc, putative_bpd + putative_bpd_cc ] + smith_align
}
