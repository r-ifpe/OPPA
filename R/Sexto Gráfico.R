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
library(ipeadatar)

library(knitr)
library(tidyr)
library(DT)
library(magrittr)

library(hrbrthemes)
library(viridis)



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


Orgao_Tit <- transmute(tabela1, "OrgãoOrçCod" = `Órgão Orçamentário`) %>%
  distinct()

Orgao_Cod <- transmute(tabela1, "c" = as.numeric(str_sub(`Órgão Orçamentário`, end = 5))) %>%
  distinct()






Orgao_Orç <- transmute(tabela2, "Ano" = as.integer(Ano),
                       "Pago" = Pago,
                       "OrgãoOrçCod" = `Órgão Orçamentário`) %>%
  group_by(OrgãoOrçCod, Ano) %>%
  inner_join(Orgao_Tit, by = "OrgãoOrçCod") %>%
  summarise(Gastos_Orgao_Orç = sum(`Pago`)) %>%
  transmute("Ano" = as.integer(Ano),
            "OrgãoOrçCod" = OrgãoOrçCod,
            "Gastos_Orgao_Orç_A" = deflate(nominal_values = as.numeric(Gastos_Orgao_Orç),
                                           nominal_dates = as.Date(paste0(Ano,"-01-01")), real_date = "12/2020")) %>%
  pivot_wider(names_from = OrgãoOrçCod, values_from = Gastos_Orgao_Orç_A)


Gastos_Tot_Bio <- semi_join(tabela1, tabela2, by = c("FunçãoCod" = "FunçãoCod", "SubfunçãoCod" = "SubfunçãoCod", "AçãoCod" = "AçãoCod", "ProgramaCod" = "ProgramaCod")) %>%
  distinct() %>%
  group_by(Ano) %>%
  summarise(Dot_Ini = sum(`Dotação Inicial`), Pago = sum(`Pago`)) %>%
  transmute("Ano" = Ano,
            "Pago_Bio_A" = deflate(nominal_values = as.numeric(Pago),
                                   nominal_dates = as.Date(paste0(Ano,"-01-01")), real_date = "12/2020")) %>%
  distinct()


Gastos_Bio_Orç <- inner_join(Gastos_Tot_Bio, Orgao_Orç, by = "Ano") %>%

  pivot_longer(!Ano, names_to = "Status", values_to = "Values") %>%

  ggplot( aes(x = Ano, y = Values/10000000000, group=Status, fill=Status)) +
  geom_area() +
  scale_fill_viridis(discrete = TRUE) +
  theme(legend.position="none") +
  ggtitle("Grafico 6") +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0, "lines"),
    strip.text.x = element_text(size = 8),
    plot.title = element_text(size=13)
  ) +

  facet_wrap(~Status)


print(Gastos_Bio_Orç)

ggsave("At6.png")


####Ylab / Xlab###


####  labs(title="Gastos ... ", subtitle="Dados oriundos do ... ", y="Gastos totais....", x= "Ano", caption="....") ####



