#' a function to be used in conjunction with the count and percent functions that sorts the dataframe outputs
#' @param fasta_dataframe is the dataframe that is a result of the amino acid count/percentage functions
#' @param sort_by the column to be sorted in the dataframe and how to sort it
#' @param decreasing if false it will be sorted in increasing order if true it will be decreasing
#' @importFrom utils data
#' @export


sort_dataframe <- function(fasta_dataframe, sort_by = "seq_name", decreasing = FALSE) {
  fasta_dataframe_sorted <- fasta_dataframe[order(fasta_dataframe[[sort_by]], decreasing = decreasing), ]
  return(fasta_dataframe_sorted)
}
