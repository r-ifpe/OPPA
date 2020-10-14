path_Entrada <- "./inst/extdata/"
nome_do_arquivo_Entrada <- "arquivo.xlsx"

OPPA <- dplyr::as_tibble(readxl::read_excel(paste0(path_Entrada,nome_do_arquivo_Entrada)))

OPPA
