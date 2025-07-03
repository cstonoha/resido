#' Frequency distribution of a single peptide within a sequence
#' @param sequence amino acid sequence as a string to be analyzed
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 theme_bw
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom dplyr mutate
#' @export


plot_amino_acid_frequency <- function(sequence) {

  # Input validation: Check if the input is a character string
  if (!is.character(sequence) || length(sequence) != 1) {
    stop("Input must be a single character string.")
  }

  # Convert to uppercase and remove non-amino acid characters (handling potential errors gracefully)
  sequence <- toupper(sequence)
  sequence <- str_replace_all(sequence, "[^ACDEFGHIKLMNPQRSTVWY]", "")

  if(nchar(sequence) == 0){
    stop("Input sequence contains no valid amino acids.")
  }

  # Split the sequence into individual amino acids
  amino_acids <- strsplit(sequence, "")[[1]]

  # Calculate frequencies
  frequency_data <- data.frame(amino_acid = amino_acids) %>%
    dplyr::group_by(amino_acid) %>%
    dplyr::summarize(count = dplyr::n()) %>%
    dplyr::mutate(frequency = count / sum(count))

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
