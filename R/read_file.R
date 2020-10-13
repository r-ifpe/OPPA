library(openxlsx)
library(dplyr)
library(tidyverse)

path <- "./arquivos/"

nome_do_arquivo <- "a33476f9c9114c539dd22609812b59d8.xlsx"

OPPA <- dplyr::as_tibble(read.xlsx(paste0(path, nome_do_arquivo)))
