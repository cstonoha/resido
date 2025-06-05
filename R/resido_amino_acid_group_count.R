#' function for counting a user-specified amino acid group
#' counts a user-specified amino acid group in the supplied amino acid sequence
#' output is a dataframe with the amino acid group count for each sequence
#' @import stringr
#' @importFrom utils write.csv
#' @importFrom utils data

#' @param file a fasta file the contains the sequence name and amino acid sequence
#' @param amino_acid_group the amino acid group of interest
#' @param sort_by how the resulting output should be sorted sort_by and decreasing
#' @param decreasing if false will sort in increasing order if true will sort decreasing
#' @param output_format either "dataframe" or "csv"
#' @export


resido_amino_acid_group_count <- function(file, amino_acid_group, sort_by = "seq_name", decreasing = FALSE, output_format = "dataframe"){

  #call the read_faa function and read the fasta file into a dataframe
  fasta_dataframe <- resido::read_faa(file)

      #check file extension
      file_ext <- tools::file_ext(file)

      #allowed file extensions (adjust as needed)
      allowed_extensions <- c("fasta", "fa", "FASTA")

      #check if file extension is valid
      if (!file_ext %in% allowed_extensions) {
        stop("Invalid file type. Please provide a FASTA file (with .fasta or .fa extension).")
      }

  #access the amino acid definitions list
  resido::amino_acid_groups

  #access the list of amino acids
  valid_amino_acid_groups <- amino_acid_groups

      # Validate amino acid group choice
      valid_amino_acid_groups <- names(amino_acid_groups)
      if (!amino_acid_group %in% valid_amino_acid_groups) {
        stop("Invalid amino acid group. Please choose from: ", paste(valid_amino_acid_groups, collapse = ", "))
      }

  #count number of one amino acid group
  group_amino_acids <- amino_acid_groups[[amino_acid_group]]
  group_amino_acid_pattern <- paste0(group_amino_acids, collapse = "|")
  amino_acid_group_count <- str_count(fasta_dataframe$seq_aa, group_amino_acid_pattern)

  #add amino acid group count as a new column
  fasta_dataframe$amino_acid_group_count <- (amino_acid_group_count)

  #call and use the sort_dataframe function in the R package
  fasta_dataframe_sorted <- resido::sort_dataframe(fasta_dataframe, sort_by = sort_by, decreasing = decreasing)

      #call and use the save_dataframe_as_csv function in the resido package
      if (output_format == "dataframe") {
        return(fasta_dataframe_sorted)
      } else if (output_format == "csv") {
        save_dataframe_as_csv(fasta_dataframe_sorted)
      } else {
        stop("Invalid output format. Please specify 'dataframe' or 'csv'.")
      }

}
