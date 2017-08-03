// Convert csv file of fields to rds

fields = {
        doc "Read in csv and output rds"
        R {"""

        fields <- read.csv('$input.csv', header=T)

        saveRDS(fields, '$output.rds')

        """}
}
