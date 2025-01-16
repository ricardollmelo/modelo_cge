

##### Carregando os pacotes necessários 

library(tidyverse)
library(PNADcIBGE)
library(deflateBR)
library(cowplot)
library(purrr)
library(readr)
library(writexl)
library(openxlsx)
theme_set(theme_minimal())
tema <- theme_minimal()
caminho_arquivo <- "C:/Users/ricar/OneDrive/Documentos/PUCRS/Graduação_Economia/TCC1/pessoas_formal_informal_2023.xlsx"


##### Carregando os dados e separando as variáveis

dados_2023 <- get_pnadc(year=2023,interview  = 1)

X2023 <- dados_2023$variables

dados_separados_2023 = dplyr::select(X2023, Ano, Trimestre, UF,
                                     VD4020, V4013,V1032,V4029)

dados_limpos_2023 = na.omit(dados_separados_2023)

##### Ponderando pelos pesos amostrais a renda

dados_limpos_2023$RendaPonderada <- dados_limpos_2023$VD4020 * dados_limpos_2023$V1032


##### Reclassificando de acordo com a SCN 

capture_first_four_digits <- function(cnae) {
  substr(cnae, 1, 4)
}

dados_limpos_2017 <- dados_limpos_2017 %>%
  mutate(CNAE_code_short = sapply(V4013, capture_first_four_digits))


planilha$codigo_atv <- as.character(planilha$codigo_atv)
planilha$resumo_cnae <- as.character(planilha$resumo_cnae)

# Preparar dados2014
dados2014$cnae_reduzida <- as.character(dados2014$cnae_reduzida)

# Preparar a planilha com chaves de 2, 3 e 4 dígitos
planilha <- planilha %>%
  mutate(codigo_2d = substr(codigo_atv, 1, 2),
         codigo_3d = substr(codigo_atv, 1, 3),
         codigo_4d = substr(codigo_atv, 1, 4))

# Fazer a correspondência a partir da chave mais específica para a menos específica
dados2014 <- dados2014 %>%
  left_join(planilha, by = c("cnae_reduzida" = "codigo_4d")) %>%
  mutate(codigo_atv = coalesce(codigo_atv, planilha$codigo_atv[match(substr(cnae_reduzida, 1, 3), planilha$codigo_3d)])) %>%
  mutate(codigo_atv = coalesce(codigo_atv, planilha$codigo_atv[match(substr(cnae_reduzida, 1, 2), planilha$codigo_2d)]))

dados2014$RendaPonderada <- as.numeric(dados2014$RendaPonderada)

sumario <- dados_limpos_2023 %>%
  group_by(V4013, V4029) %>%
  summarise(SomaRendaPonderada = sum(V1032, na.rm = TRUE))

print(sumario)

sumario <- dados_reclassificados_2014 %>%
  group_by(`Classificação SCN`, V4029) %>%
  summarise(SomaRendaPonderada = sum(RendaPonderada, na.rm = TRUE), .groups = 'drop')

print(sumario)


write.xlsx(sumario, file = caminho_arquivo)



