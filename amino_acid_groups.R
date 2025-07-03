#' Definition of amino acid groups
#'
#' A list of amino acid groups.
#'
#' @export

amino_acid_groups <- list(
  hydrophobic = c("A", "C", "F", "G", "I", "L", "M", "P", "V", "W"),
  polar = c("C", "D", "E", "H", "K", "N", "O", "Q", "R", "S", "T", "Y"),
  charged = c("D", "E", "K", "R", "H"),
  aromatic = c("F", "H", "W", "Y"),
  aliphatic = c("A", "I", "L", "M", "V"),
  positive = c("R","H","K"),
  negative = c("D","E"),
  sulfur = c("M", "C")
)
