library(openxlsx)
library(dplyr)
library(tidyverse)

path <- "C:/Users/italo/OneDrive/Área de Trabalho/"

nome_do_arquivo <- "a33476f9c9114c539dd22609812b59d8"


OPPA <- dplyr::as_tibble(read.xlsx(paste0(path, nome_do_arquivo, ".xlsx")))

OPPA
