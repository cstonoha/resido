#' function for calculating percent of a user specified amino acid
#' counts and sorts by percent of a user-specified amino acid in the sequence
#' output is a dataframe with the percent amino acid for each sequence
#' @import stringr
#' @importFrom utils write.csv
#' @importFrom utils data

#' @param file a fasta file the contains the sequence name and amino acid sequence
#' @param amino_acid the amino acid of interest as one-letter code
#' @param sort_by how the resulting output should be sorted sort_by and decreasing
#' @param decreasing if false will sort in increasing order if true will sort decreasing
#' @param output_format either "dataframe" or "csv"
#' @export

resido_amino_acid_percent <- function(file, amino_acid, sort_by = "seq_name", decreasing = FALSE, output_format = "dataframe"){

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
  resido::amino_acid_definitions

  #access the list of amino acids
  valid_amino_acids <- amino_acid_definitions

      # Validate amino acid choice
      if (!amino_acid %in% valid_amino_acids) {
        stop("Invalid amino acid. Please choose from: ", paste(valid_amino_acids, collapse = ", "))
      }

  #count total number of AA
  total_amino_acid_count <- str_count(fasta_dataframe$seq_aa)

  #count number of one amino acid
  single_amino_acid_count <- str_count(fasta_dataframe$seq_aa, amino_acid)

  #calculate and add percent amino acid as a new column
  fasta_dataframe$percent_amino_acid <- (single_amino_acid_count / total_amino_acid_count) * 100

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
