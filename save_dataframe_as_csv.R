#' Function to save a dataframe as a CSV file
#'
#' @param data The dataframe to be saved
#' @param file The output file path
#' @param ... Additional arguments passed to write.csv
#' @importFrom utils write.csv
#' @importFrom utils data
#'
#' @export

save_dataframe_as_csv <- function(data, file = "output.csv", ...) {
  write.csv(data, file = file, ...)
}
