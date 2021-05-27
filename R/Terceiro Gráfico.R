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
#'
#' class(ReadExcell)
#' @export
#'

library(tidyverse)
library(dplyr)
library(stringr)
library(deflateBR)
library(ggplot2)
library(openxlsx)

ReadExcell <- function(path){
  as_tibble(readxl::read_excel(path))

}


tabela1 <- ReadExcell("./inst/extdata/arquivos/BASEUNIFICADA_class_ANALISE.xlsx") %>%
  mutate("Ano" = as.integer(Ano),
         "FunçãoCod" = str_sub(Função, end = 2),
         "SubfunçãoCod" = str_sub(Subfunção, end = 3),
         "AçãoCod" = str_sub(Ação, end = 4),
         "ProgramaCod" = str_sub(Programa, end = 4)) %>%
  distinct()


tabela2 <- ReadExcell("./inst/extdata/arquivos/PAGAM.xlsx") %>%
  mutate("Ano" = as.integer(Ano),
         "FunçãoCod" = str_sub(Função, end = 2),
         "SubfunçãoCod" = str_sub(Subfunção, end = 3),
         "AçãoCod" = str_sub(Ação, end = 4),
         "ProgramaCod" = str_sub(Programa, end = 4)) %>%
  distinct()

tabela3 <- ReadExcell("./inst/extdata/arquivos/SIOP.xlsx") %>%
  transmute("Ano" = as.integer(Ano),
            "Pago" = Pago) %>%
  distinct()

Gastos_Tot_Bio <- semi_join(tabela1, tabela2, by = c("FunçãoCod" = "FunçãoCod", "SubfunçãoCod" = "SubfunçãoCod", "AçãoCod" = "AçãoCod", "ProgramaCod" = "ProgramaCod")) %>%
  distinct() %>%
  group_by(Ano) %>%
  summarise(Dot_Ini = sum(`Dotação Inicial`), Pago = sum(`Pago`)) %>%
  transmute("Ano" = Ano,
            "Pago_Bio_A" = deflate(nominal_values = as.numeric(Pago),
                                   nominal_dates = as.Date(paste0(Ano,"-01-01")), real_date = "12/2020")) %>%
  distinct()

Gastos_Tot_Gov <- tabela3 %>%
  group_by(Ano) %>%
  summarise(Pago = sum(`Pago`)) %>%
  transmute("Ano" = Ano,
            "Pago_Gov_A (/10^2)" = deflate(nominal_values = as.numeric(Pago/100),
                                           nominal_dates = as.Date(paste0(Ano,"-01-01")), real_date = "12/2020")) %>%
  distinct()

Compara_Gastos <- inner_join(Gastos_Tot_Bio, Gastos_Tot_Gov, by = "Ano") %>%
  pivot_longer(!Ano, names_to = "Status", values_to = "Values") %>%
  ggplot(mapping = aes(x = Ano, y = Values/1000000000, colour = Status, group = Status)) +
  geom_line() + scale_y_continuous(labels = function(x){paste0(x, 'Bi')}) + theme_grey()

print(Compara_Gastos)

ggsave("At3.png")


####Ylab / Xlab###


####  labs(title="Gastos ... ", subtitle="Dados oriundos do ... ", y="Gastos totais....", x= "Ano", caption="....") ####
