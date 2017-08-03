# ukblox

Derived phenotype pipelines for UK Biobank

### Prerequisites

1. Bpipe <http://docs.bpipe.org/> 
2. The R package `dplyr`
3. Install the `ukbiobanker` R package:
```
library(devtools)
install_github('ukbiobank-MentalHealth-Genetics/ukbiobanker')
```

## UKB MDD and "nerves" phenotypes

Self contained derivation of MDD phenotypes:

- broad depression
- probable major depressive disorder
- ICD-coded MDD

### Running the pipeline

1. **Inputs:** Make an csv with columns `f.eid` and the other fields listed in `mdd_fields.txt`. These field columns should be those output from the `ukb_conv r` command (that is, named as `f.FIELD.INDEX.ARRAY`). Name this file `INPUT.csv` (_INPUT_ can be anything, and may usefully be named after the data release the fields are from, e.g., `ukb1234.sv`).
2. **Execution**: Run the pipeline with `sh mdd.sh INPUT.csv`
