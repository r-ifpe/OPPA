#' Read XLSX files related to the OPPA project
#'
#' @param path The file's path to the XSLX file.
#'
#' @return A data frame.
#'
#' @details  By now, this function suports any XLSX file.
#'
#' @examples
#' # these datasets are real ones. But it's here just for test purpose.
#'
#' # example selecting the file
#' OPPA("E:/Back-up/Projetos - FNO/R projects/Ic/OPPA/inst/extdata/arquivo.xlsx")
#'
#' class(OPPA)
#' @export

OPPA <- function(path){
  dplyr::as_tibble(readxl::read_excel(path))
}
