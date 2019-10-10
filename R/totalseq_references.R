#' Load BioLegend TotalSeq A reference table for human hash tag oligos (HTO)
#'
#' @return a data.table with reference information
#' @export
#'
totalseq_a_human <- function() {
  fread(
    system.file("data/TotalSeqA_human_barcodes.csv",
                package = "HTOparser")
    )
}

#' Load BioLegend TotalSeq A reference table for mouse hash tag oligos (HTO)
#'
#' @return a data.table with reference information
#' @export
#'
totalseq_a_mouse <- function() {
  fread(
    system.file("data/TotalSeqA_mouse_barcodes.csv",
                package = "HTOparser")
  )
}

#' Load BioLegend TotalSeq A reference table for biotin hash tag oligos (HTO)
#'
#' @return a data.table with reference information
#' @export
#'
totalseq_a_biotin <- function() {
  fread(
    system.file("data/TotalSeqA_biotin_barcodes.csv",
                package = "HTOparser")
  )
}
