# HTOparser
### Hash-tag oligo parsing functions for scRNA-seq data

## Requirements

All requirements for HTOparser are available from CRAN, and should be installed automatically by install.packages:
```
Imports:
    assertthat (>= 0.2.1),
    Matrix (>= 1.2-17),
    purrr (>= 0.3.2)
Requires:
    data.table (>= 1.12.2)
```

## Installation

This package can be installed from Github using the `devtools` package.

You may first need to register your GitHub PAT, as this is a private repository.
```
Sys.setenv(GITHUB_PAT = "your-access-token-here")
devtools::install_github("aifimmunology/HTOparser")
```

