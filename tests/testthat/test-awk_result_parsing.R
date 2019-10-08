context("awk_result_parsing")
library(HTOparser)
library(data.table)

# load test data
test_hto_table <- fread(system.file("testdata/awk_pipeline_counts_table.csv.gz",
                                    package = "HTOparser"))
colnames(test_hto_table) <- c("count","cell_barcode","hto_barcode")

# load TrueSeq HTO values
trueseq_table <- fread(system.file("data/TotalSeqA_human_barcodes.csv",
                                   package = "HTOparser"))

valid_barcodes <- trueseq_table$hto_barcode

test_that(
  "fuzzy_filtering removes non-matching hto barcodes",
  {
    ff_result <- fuzzy_filtering(test_hto_table,
                                 match_column = "hto_barcode",
                                 match_values = valid_barcodes[1],
                                 max_distance = 1)

    expect_s3_class(ff_result, "data.table")
    expect_equal(as.character(unique(ff_result[["hto_barcode"]])), valid_barcodes[1])
  }
)


test_that(
  "fuzzy_filtering retains near-matching barcodes",
  {

  }
)


test_that(
  "barcode_table_to_matrix converts a barcode table to a dgCMatrix",
  {

  }
)
