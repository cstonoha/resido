#' Create a dataframe with counts and frequency of all canonical amino acids
#' @param file fasta file containing amino acid sequences
#' @param sort_by the column in the final dataframe used for sorting
#' @param decreasing how to sort the final dataframe (TRUE is the default)
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom tidyr unnest
#' @importFrom reshape2 dcast
#' @export


all_amino_acids_dataframe <- function(file, sort_by = "seq_name", decreasing = TRUE){

  #call the read_faa function and read the fasta file into a dataframe
  fasta_dataframe <- resido::read_faa(file)

  # Check if fasta_dataframe is a time series object
  if (inherits(fasta_dataframe, "ts")) {
    fasta_dataframe <- as.data.frame(fasta_dataframe)
  }

  #check file extension
  file_ext <- tools::file_ext(file)

  #allowed file extensions (adjust as needed)
  allowed_extensions <- c("fasta", "fa", "FASTA")

  #check if file extension is valid
  if (!file_ext %in% allowed_extensions) {
    stop("Invalid file type. Please provide a FASTA file (with .fasta or .fa extension).")
  }

  fasta_dataframe$total_aa <- str_count(fasta_dataframe$seq_aa)

  aa_data <- fasta_dataframe %>%
    dplyr::mutate(seq_aa = toupper(seq_aa)) %>%
    dplyr::mutate(seq_aa = str_replace_all(seq_aa, "[^ACDEFGHIKLMNPQRSTVWY]", "")) %>%
    dplyr::filter(nchar(seq_aa) > 0) %>%
    dplyr::mutate(amino_acid = strsplit(seq_aa, "")) %>%
    tidyr::unnest(cols = c(amino_acid)) %>%
    dplyr::group_by(seq_name, total_aa, amino_acid) %>%
    dplyr::summarize(count = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(seq_name) %>%
    dplyr::mutate(frequency = count / total_aa) %>%
    dplyr::ungroup()

  # Reshape data for dataframe
  aa_data <- reshape2::dcast(aa_data, seq_name ~ amino_acid, value.var = "frequency")

  # call and use the sort_dataframe function in the R package
  fasta_dataframe_sorted <- resido::sort_dataframe(aa_data, sort_by = sort_by, decreasing = decreasing)

  return(fasta_dataframe_sorted)
}
