#' Amino acid density score to identify a span of 100 amino acids with the highest count of an amino acid of interest
#' @param file dataframe of amino acid sequences
#' @param target_aa amino acid of interest
#' @param sort_by the column in the final dataframe used for sorting
#' @param decreasing how to sort the final dataframe (TRUE is the default)
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_count
#' @export


calculate_max_aa_per_100 <- function(file, amino_acid, sort_by = "max_aa_count", decreasing = TRUE){

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

  #check the amino acid input
  if (!is.character(amino_acid) || length(amino_acid) != 1 || nchar(amino_acid) != 1) {
    stop("amino_acid must be a single character string representing one amino acid.")
  }

  amino_acid <- toupper(amino_acid)

  if(!amino_acid %in% LETTERS){
    stop("target_aa must be a valid amino acid.")
  }

  results <- fasta_dataframe %>%
    dplyr::mutate(max_aa_count = sapply(seq_aa, function(seq) {
      seq <- toupper(seq)
      seq <- str_replace_all(seq, "[^ACDEFGHIKLMNPQRSTVWY]", "") #Clean non-amino acids

      seq_len <- nchar(seq)
      if (seq_len == 0){
        return(0) #return 0 if sequence is empty after cleaning
      }
      if (seq_len < 100) {
        count <- str_count(seq, amino_acid)
        return(count / seq_len * 100) # Percentage for short sequences
      } else {
        max_count <- 0
        for (i in 1:(seq_len - 99)) {
          sub_seq <- substr(seq, i, i + 99)
          count <- str_count(sub_seq, amino_acid)
          max_count <- max(max_count, count)
        }
        return(max_count)
      }
    })) %>%
    dplyr::select(seq_name, max_aa_count)

  #call and use the sort_dataframe function in the R package
  fasta_dataframe_sorted <- resido::sort_dataframe(results, sort_by = sort_by, decreasing = decreasing)

  return(fasta_dataframe_sorted)
}
