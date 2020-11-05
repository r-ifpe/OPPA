#' Read XLSX files related to the OPPA project
#'
#' @param path The file's path to the XSLX file.
#'
#' @return A data frame.
#'
#' @details  By now, this function suports any XLSX file.
#'
#' @examples
#' # these datasets aren't real ones. But it's here just for test purpose.
#'
#' oppa <- read_oppa(system.file("extdata/arquivos/oppa.xlsx",
#'                               package = "OPPA"))
#' @export
read_oppa <- function(path){
  dplyr::as_tibble(readxl::read_excel(path))
}
