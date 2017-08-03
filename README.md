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
- probable major depressive disorder (Smith 
- ICD-coded MDD

### Running the pipeline

1. **Inputs:** Make an csv with columns `f.eid` and the other fields listed in `mdd_fields.txt`. These field columns should be those output from the `ukb_conv r` command (that is, named as `f.FIELD.INDEX.ARRAY`). Name this file `INPUT.csv` (_INPUT_ can be anything, and may usefully be named after the data release the fields are from, e.g., `ukb1234.csv`).
2. **Execution**: Run the pipeline with `sh mdd.sh INPUT.csv`

### Citation

Genome-wide association study of depression phenotypes in UK Biobank (n = 322,580) identifies the enrichment of variants in excitatory synaptic pathways
David M. Howard, Mark J. Adams, Masoud Shirali, Toni-Kim Clarke, Riccardo E. Marioni, Gail Davies, Jonathan R. I. Coleman, Clara Alloza, Xueyi Shen, Miruna C. Barbu, Eleanor M. Wigmore, Jude Gibson, Saskia Hagenaars, Cathryn M. Lewis, Daniel J. Smith, Patrick F. Sullivan, Chris S. Haley, Gerome Breen, Ian J. Deary, Andrew M. McIntosh
bioRxiv 168732; doi: https://doi.org/10.1101/168732 

### References

Smith, D.J.  et al.  Prevalence and characteristics of probable major depression and bipolar disorder within UK Biobank: cross-sectional study of 172,751 participants. PLoS ONE 8, e75362 (2013). 
