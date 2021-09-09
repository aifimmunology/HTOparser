# HTOparser
### Hash-tag oligo parsing functions for scRNA-seq data

## Requirements

All requirements for HTOparser are available from CRAN, and should be installed automatically by `install_github()`:
```
Imports:
    assertthat (>= 0.2.1),
    dplyr (>= 0.8.0.1),
    purrr (>= 0.3.2),
    R.utils (>= 2.8.0)
Depends:
    data.table (>= 1.12.2),
    Matrix (>= 1.2-17)
```

## Installation

This package can be installed from Github using the `devtools` package.

You may first need to register your GitHub PAT, as this is a private repository.
```
git clone git@github.com:aifimmunology/HTOparser
R -e 'install.packages("HTOparser", type = "source", repos = NULL)'
```
## Test Data

This package includes test datasets that can be used for writing tests, vignettes, or demos. These files are stored in `/inst/testdata/`.

### CITE-seq-Count

Data generated using CITE-seq-Count can be loaded using:
```
library(HTOparser)

test_hto_table <- fread(system.file("testdata/CITE-seq_count_matrix.csv.gz",
                                    package = "HTOparser"))
test_hto_mat <- as(as.matrix(test_hto_table[1:11,-1]), "dgCMatrix")
```
Row names of this matrix can be converted to barcodes using:
```
test_hto_key <- fread(system.file("testdata/CITE-seq_count_tags.csv",
                                  package = "HTOparser"),
                      header = FALSE)

rownames(test_hto_mat) <- test_hto_key$V1[match(test_hto_table[[1]][1:11], test_hto_key$V2)]
```

### `awk`-based results

A table of results from `awk`-based parsing of HTO FASTQ files can be loaded using:
```
library(data.table)

test_hto_table <- fread(system.file("testdata/awk_pipeline_counts_table.csv.gz",
                                    package = "HTOparser"))
colnames(test_hto_table) <- c("count","cell_barcode","hto_barcode")
```

### BioLegend TotalSeq References

CSV files with reference information for the TotalSeqA Hash Tag Oligo sequences are stored in `/inst/data/`, and have convenient wrapper functions to make them easy to load:
```
human_totalseq_ref <- totalseq_a_human()
mouse_totalseq_ref <- totalseq_a_mouse()
biotin_totalseq_ref <- totalseq_a_biotin()

```

## Tests

Tests for `HTOparser` are implemented using the `testthat` testing suite:  
https://testthat.r-lib.org/

To run tests for this package, download the repository from Github and run `devtools::test()` in R from the base directory of the package.

Extra-stringent, CRAN-level package testing can be performed using `devtools::check()` in R.

## Style and Standards

This package aims to conform to the tidyverse style guide:  
https://style.tidyverse.org/index.html

General information about R package conventions can be found in `R Packages`:  
http://r-pkgs.had.co.nz/

