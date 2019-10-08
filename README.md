# HTOparser
### Hash-tag oligo parsing functions for scRNA-seq data

## Requirements

All requirements for HTOparser are available from CRAN, and should be installed automatically by `install_github()`:
```
Imports:
    assertthat (>= 0.2.1),
    purrr (>= 0.3.2)
Depends:
    data.table (>= 1.12.2),
    Matrix (>= 1.2-17)
```

## Installation

This package can be installed from Github using the `devtools` package.

You may first need to register your GitHub PAT, as this is a private repository.
```
Sys.setenv(GITHUB_PAT = "your-access-token-here")
devtools::install_github("aifimmunology/HTOparser")
```

## Tests

Tests for `HTOparser` are implemented using the `testthat` testing suite:  
https://testthat.r-lib.org/

To run tests for this package, download from Github and run `devtools::test()` in R.

Extra-stringet, CRAN-level package testing can be performed using `devtools::check()` in R.

## Style and Standards

This package aims to conform to the tidyverse style guide:  
https://style.tidyverse.org/index.html

General information about R package conventions can be found in `R Packages`:  
http://r-pkgs.had.co.nz/

