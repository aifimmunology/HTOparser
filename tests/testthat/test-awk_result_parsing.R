context("awk_result_parsing")
library(HTOparser)
library(data.table)

# load test data
test_hto_table <- fread(system.file("testdata/awk_pipeline_counts_table.csv.gz",
                                    package = "HTOparser"))
colnames(test_hto_table) <- c("count","cell_barcode","hto_barcode")

test_that(
  "fuzzy_filtering removes non-matching hto barcodes",
  {

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
