path_Entrada <- "C:/Users/Atum/Desktop/"
nome_do_arquivo_Entrada <- "3bb640431c9942aa95754785dc5713d9.xlsx"

OPPA <- dplyr::as_tibble(readxl::read_excel(paste0(path_Entrada,nome_do_arquivo_Entrada)))
OPPA

