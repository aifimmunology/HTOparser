context("awk_result_parsing")
library(HTOparser)
library(data.table)
options(stringsAsFactors = FALSE)

# load test data
test_hto_table <- fread(system.file("testdata/awk_pipeline_counts_table.csv.gz",
                                    package = "HTOparser"))
colnames(test_hto_table) <- c("count","cell_barcode","hto_barcode")

# load totalseq HTO values
totalseq_table <- totalseq_a_human()

valid_barcodes <- totalseq_table$hto_barcode

test_that(
  "fuzzy_filtering performs filtering for a single barcode.",
  {
    ff_result <- fuzzy_filtering(test_hto_table,
                                 match_column = "hto_barcode",
                                 match_values = valid_barcodes[1],
                                 max_distance = 1)

    expect_s3_class(ff_result, "data.table")
    expect_true(nrow(ff_result) < nrow(test_hto_table))
    expect_equal(as.character(unique(ff_result[["hto_barcode"]])), valid_barcodes[1])
  }
)


test_that(
  "fuzzy_filtering performs filtering for multiple barcodes",
  {
    ff_result <- fuzzy_filtering(test_hto_table,
                                 match_column = "hto_barcode",
                                 match_values = valid_barcodes,
                                 max_distance = 1)

    ff_barcodes <- as.character(unique(ff_result$hto_barcode))

    expect_s3_class(ff_result, "data.table")
    expect_true(nrow(ff_result) < nrow(test_hto_table))
    expect_true(sum(ff_barcodes %in% valid_barcodes) == length(ff_barcodes))
  }
)

test_that(
  "fuzzy_filtering finds near misses based on max_distance.",
  {
    test_input <- data.table(count = c(1,2,4,8,16),
                             cell_barcode = letters[1:5],
                             hto_barcode = c("GATTACA", #exact
                                             "GATAACA", #one substitution
                                             "GATTTACA",#one insertion
                                             "GATTACA", #exact
                                             "CCTTACA"))#two substitutions

    exact_result <- fuzzy_filtering(test_input,
                                 match_column = "hto_barcode",
                                 match_values = "GATTACA",
                                 max_distance = 0)

    expect_s3_class(exact_result, "data.table")
    expect_equal(nrow(exact_result), 2)
    expect_equal(sum(exact_result$count), 9)

    miss1_result <- fuzzy_filtering(test_input,
                                    match_column = "hto_barcode",
                                    match_values = "GATTACA",
                                    max_distance = 1)

    expect_s3_class(miss1_result, "data.table")
    expect_equal(nrow(miss1_result), 3)
    expect_equal(sum(miss1_result$count), 11)

  }
)

test_that(
  "barcode_table_to_matrix converts a barcode table to a dgCMatrix",
  {
    ff_result <- fuzzy_filtering(test_hto_table,
                                 match_column = "hto_barcode",
                                 match_values = valid_barcodes,
                                 max_distance = 1)

    mat_result <- barcode_table_to_matrix(ff_result,
                                          valid_htos = valid_barcodes)

    expect_true(class(mat_result) == "dgCMatrix")
    expect_equal(nrow(mat_result), length(valid_barcodes))
    expect_equal(ncol(mat_result), length(unique(ff_result$cell_barcode)))
    expect_equal(length(mat_result@x), nrow(ff_result))
  }
)
