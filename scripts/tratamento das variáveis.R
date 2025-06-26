library(lubridate)
library(tidyverse)
library(ggplot2)
library(sf)
library(readxl)
library(gridExtra)
library(tmap)
library(BlandAltmanLeh)
library(ggthemes)
library(stringr)
library(irr)
library(rmapshaper)
library(shinydashboard)
library(shiny)

## Dados

#Carregando o Mapa do municipio de Santa Catarina
map = read_sf("SC_Municipios_2024.shp")

#Deixando o mapa mais simples/leve
mapa_simples = ms_simplify(input = map,
                           keep = 0.1,
                           keep_shapes = TRUE)

#Tratamento de base de dados do Copernicus

dados = read_csv(file = "PM2.5_diario_2023.csv")

dados$Data = ymd(dados$Date)
dados$mes = month(dados$Data)
dados$ano = year(dados$Data)
dados$wday = weekdays(dados$Data)
dados$UF = substr(dados$Cod, 1, 2)

#Selecionando apenas o Município de Santa Catarina
santa_catarina = subset(dados, dados$UF == 42)

scmes = santa_catarina |>
  group_by(Cod, mes) |>
  summarise(PM2.5 = mean(PM2.5, na.rm = TRUE))

scano = santa_catarina |>
  group_by(Cod) |>
  summarise(PM2.5 = mean(PM2.5, na.rm = TRUE))

scUF = santa_catarina |>
  group_by(mes) |>
  summarise(PM2.5 = mean(PM2.5, na.rm = TRUE))

## Tratamento de base de dados do Donkelar

#Carregando a base de dados
dados2 = read_excel(path = "dados_completos_consolidado_donkelar.xlsx")

#Selecionando apenas o Município de Santa Catarina
santa_catarinad2 = subset(dados2, dados2$SIGLA_UF == 42)

scmes2 = santa_catarinad2 |>
  select(CD_MUN, Mes, Media_PM25) |>
  group_by(CD_MUN)

scano2 = santa_catarinad2 |>
  group_by(CD_MUN) |>
  summarise(Media_PM25 = mean(Media_PM25, na.rm = TRUE))

scUF2 = santa_catarinad2 |>
  group_by(mes = Mes) |>
  summarise(Media_PM25 = mean(Media_PM25, na.rm = TRUE))


#Juntado ás variáveis necessárias

base_ano_unida = left_join(x = scano, y = scano2, by = c("Cod" = "CD_MUN"))


base_u2 = left_join(x = scUF, y = scUF2, by = c("mes" = "mes"))


base_long = base_u2 |>
  select(mes, PM2.5, Media_PM25) |>
  pivot_longer(cols = c(PM2.5, Media_PM25),
               names_to = "tipo",
               values_to = "valor")


base_long$mes <- factor(base_long$mes, levels = 1:12,
                        labels = c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", 
                                   "Jul", "Ago", "Set", "Out", "Nov", "Dez"))




base_uf_u2 = left_join(x = scUF, y = scUF2, by = "mes")


scano$Cod = as.character(scano$Cod)
scano2$CD_MUN = as.character(scano2$CD_MUN)


base_map_uf <- left_join(mapa_simples, scano, by = c("CD_MUN" = "Cod"))

base_map_uf2 <- left_join(mapa_simples, scano2, by = c("CD_MUN" = "CD_MUN"))


base_map_uf = base_map_uf |>
  mutate(nome_casos = str_c(NM_MUN, " - ", round(PM2.5, 4)))


base_map_uf2 = base_map_uf2 |>
  mutate(nome_casos = str_c(NM_MUN, " - ", round(Media_PM25, 4)))

scmes1 <- scmes %>%
  mutate(Variavel = "PM2.5") %>%
  rename(Valor = `PM2.5`)

scmes2_5 <- scmes2 %>%
  mutate(Variavel = "Media_PM25") %>%
  rename(Valor = Media_PM25, mes = Mes, Cod = CD_MUN)

box = rbind(scmes1, scmes2_5)

box <- box |>
  mutate(Variavel = recode(Variavel,
                           "Media_PM25" = "Donkelar",
                           "PM2.5" = "Copernicus"))

# ICC
BaseUF = data.frame(pm2.5 = scUF$PM2.5, Media_PM25 = scUF2$Media_PM25)
avaliacoes = BaseUF
iccUF = icc(avaliacoes, model = "twoway", type = "consistency", unit = "single")
ValorICC_UF = round(iccUF$value, 2)
IC95_ICC_UF = c(round(iccUF$lbound, 3), round(iccUF$ubound, 3))



classificacao_icc <- function(valor_icc) {
  case_when(
    valor_icc <= 0.20 ~ "Concordância pobre",
    valor_icc <= 0.40 ~ "Concordância razoável",
    valor_icc <= 0.60 ~ "Concordância boa",
    valor_icc <= 0.80 ~ "Concordância muito boa",
    valor_icc <= 1.00 ~ "Concordância excelente",
    TRUE ~ "Valor inválido"
  )
}