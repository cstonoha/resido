#' Create an amino acid density heatmap
#' @param file fasta file containing amino acid sequences
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_tile
#' @importFrom ggplot2 scale_fill_gradient
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 theme_bw
#' @importFrom ggplot2 theme
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom tidyr unnest
#' @importFrom reshape2 dcast
#' @importFrom reshape2 melt
#' @export


create_aa_heatmap <- function(file){

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

  # Reshape data for heatmap
  heatmap_data <- reshape2::dcast(aa_data, seq_name ~ amino_acid, value.var = "frequency")

  # Melt data for ggplot
  melted_data <- reshape2::melt(heatmap_data, id.vars = "seq_name", variable.name = "amino_acid")

 # Create the ggplot2 plot
  p <- ggplot2::ggplot(frequency_data, ggplot2::aes(x = amino_acid, y = frequency)) +
    ggplot2::geom_bar(stat = "identity", fill = "skyblue", color = "black") +
    ggplot2::labs(title = "Amino Acid Frequency",
         x = "Amino Acid",
         y = "Frequency") +
    ggplot2::theme_bw() + # Use a clean theme
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))+ #rotate x axis labels if needed
    ggplot2::scale_y_continuous(limits=c(0, max(frequency_data$frequency)*1.1)) #add buffer to y axis

  return(p)
}


