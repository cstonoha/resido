#' The following is from the read_faa function from the ampir package
#' citation: Legana C H W Fingerhut, David J Miller, Jan M Strugnell, Norelle L Daly, Ira R Cooke, ampir: an R package for fast genome-wide prediction of antimicrobial peptides, Bioinformatics, Volume 36, Issue 21, November 2020, Pages 5262–5263, https://doi.org/10.1093/bioinformatics/btaa653
#' Read FASTA amino acids file into a dataframe
#' This function reads a FASTA amino acids file into a dataframe
#' read_faa
#'
#' This function was adapted from `read.fasta.R` by Jinlong Zhang (jinlongzhang01@gmail.com) for the phylotools package (http://github.com/helixcn/phylotools)
#' file path to the FASTA format file containing the protein sequences
#' Dataframe containing the sequence name (seq_name) and sequence (seq_aa) columns
#'
#'
#' @export read_faa
#' @note This function was adapted from `read.fasta.R` by Jinlong Zhang (jinlongzhang01@@gmail.com) for the phylotools package (http://github.com/helixcn/phylotools)
#' @param file file path to the FASTA format file containing the protein sequences
#' Additional details... returns a dataframe containing the sequence name (seq_name) and sequence (seq_aa) columns

read_faa <- function (file = NULL) {
  faa_lines <- readLines(file)

  ### get sequence names
  seq_name_index <- grep(">", faa_lines)
  seq_name <- gsub(">", "", faa_lines[seq_name_index])

  ### get sequence
  seq_aa_start_index <- seq_name_index + 1
  seq_aa_end_index <- c(seq_name_index, length(faa_lines)+1)[-1]-1

  seq_aa <- rep(NA, length(seq_name_index))

  ### replace NA content with actual sequence content, and concatenate the lines
  for(i in seq_along(seq_name_index)){
    seq_aa_start <- seq_aa_start_index[i]
    seq_aa_end   <- seq_aa_end_index[i]
    seq_aa[i] <- gsub("[[:space:]]", "",
                      paste(faa_lines[seq_aa_start:seq_aa_end],
                            collapse = ""))
  }

  data.frame(seq_name, seq_aa, stringsAsFactors = FALSE)
}

