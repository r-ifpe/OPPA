#' Read XLSX files related to the OPPA project
#'
#' @param path The file's path to the XSLX file.
#'
#' @return A data frame.
#'
#' @details  By now, this function suports any XLSX file.
#'
#' @examples
#' lerEx <- ReadExcell(system.file("extdata/examples/ExSource.xls",
#'                                                 package = "OPPA"))
#' lerEx
#' @export
ReadExcell <- function(path){
  dplyr::as_tibble(readxl::read_excel(path))
}


#' Read XLSX tables related to the OPPA project
#' @param path The file's path to the XSLX file.
#'
#' @param TabelaBase The source table that will be compared in GerarTable
#'
#' @examples
#' lerTabNormalEx <- ReadExcell(system.file("extdata/examples/ExSource.xls",
#'                                                 package = "OPPA"))
#' lerTabNormalEx
#'
#' lerTabBaseEx <- ReadExcell(system.file("extdata/examples/ExSource.xls",
#'                                                 package = "OPPA"))
#' lerTabBaseEx
#'
#' @importFrom dplyr %>% sym
#'
#' @export
LerTable <- function(path, TabelaBase = 0){
    if(TabelaBase == 1){
      dplyr::transmute(ReadExcell(path), "Funcao" = stringr::str_sub(!!sym("Fun\u00e7\u00e3o"), end = 2),
                       "SubfuncaoCod" = stringr::str_sub(!!sym("Subfun\u00e7\u00e3o"), end = 3),
                       "AcaoCod" = stringr::str_sub(!!sym("A\u00e7\u00e3o"), end = 4),
                       "ProgramaCod" = stringr::str_sub(!!sym("Programa"), end = 4)) %>%
        dplyr::distinct()
    }

    else{
      dplyr::mutate(ReadExcell(path), "Funcao" = stringr::str_sub(!!sym("Fun\u00e7\u00e3o"), end = 2),
                    "SubfuncaoCod" = stringr::str_sub(!!sym("Subfun\u00e7\u00e3o"), end = 3),
                    "AcaoCod" = stringr::str_sub(!!sym("A\u00e7\u00e3o"), end = 4),
                    "ProgramaCod" = stringr::str_sub(!!sym("Programa"), end = 4)) %>%
        dplyr::distinct()
    }
}

#' Generate a TABLE comparing the XLSX files read
#' @param Tabela1 One of the tables read with the LerTable function
#'
#' @param Tabela2 The other table read with the LerTable function
#'
#' #' @examples
#' GerarEx <- GerarTable(T1, T2)
#'
#' GerarEx
#'
#' @importFrom dplyr %>% sym
#'
#' @export
GerarTable <- function(Tabela1, Tabela2){
  dplyr::semi_join(Tabela2, Tabela1, by = c("FuncaoCod" = "FuncaoCod",
                                            "SubfuncaoCod" = "SubfuncaoCod",
                                            "AcaoCod" = "AcaoCod",
                                            "ProgramaCod" = "ProgramaCod")) %>%
    dplyr::distinct()
}
