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



tabela1 <- ReadExcell("./inst/extdata/arquivos/PAGAM_2.xlsx") %>%
  group_by(Ano) %>%
  summarise(Pago = sum(Pago)) %>%
  mutate("Pago Ajustado" = deflate(nominal_values = as.numeric(Pago),
                                   nominal_dates = as.Date(paste0(Ano,"-01-01")), real_date = "12/2020") )%>%
  distinct()%>%
  pivot_longer(!Ano, names_to = "Status", values_to = "Values") %>%
  ggplot(mapping = aes(x = Ano, y = Values/1000000000, colour = Status, group = Status)) +
  geom_line() + scale_y_log10(labels = function(x){paste0(x, 'Bi')}) + theme_bw()

ggsave("At1.png")



####Ylab / Xlab###


####  labs(title="Gastos ... ", subtitle="Dados oriundos do ... ", y="Gastos totais....", x= "Ano", caption="....") ####
