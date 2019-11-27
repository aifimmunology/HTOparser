context("util")
library(HTOparser)

test_that(
  "stm() generates a message",
  {
    test_message <- "hello there"

    test_capture <- capture.output(stm(test_message), type = "message")

    expect_true(grepl(test_message, test_capture))
  }
)

test_that(
  "read_csc_mtx() reads CITE-seq-Count .mtx files",
  {
    test_dir <- system.file("testdata/CITE-seq-Count_umi_mtx/", package = "HTOparser")
    test_umi_dir <- file.path(test_dir, "umi_count")

    test_rownames <- readLines(file.path(test_umi_dir, "features.tsv.gz"))
    test_colnames <- readLines(file.path(test_umi_dir, "barcodes.tsv.gz"))

    test_mat <- read_csc_mtx(test_umi_dir)

    expect_true(class(test_mat) == "matrix")
    expect_identical(rownames(test_mat), test_rownames)
    expect_identical(colnames(test_mat), test_colnames)

  }
)
