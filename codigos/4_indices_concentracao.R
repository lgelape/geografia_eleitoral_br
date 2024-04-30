################################################################################
################################################################################
####
#### Indicadores de concentracao de votos
####

# Lucas Gelape
# Glauco Peres da Silva

library(dplyr)
library(tidyr)
library(readr)
library(sf)
library(geobr)
library(ggplot2)
library(ggspatial)
library(classInt)

################################################################################

# Indice G (Avelino, Biderman e Silva, 2011)
# Adaptado de: https://github.com/lgelape/dissertacao_replicacao/blob/master/2_Replicacao_SP2012.R

G <- function(base, candidato, agregacao, votos){

  # Cria tabela auxiliar com o n. de votos do candidato por agregacao
  x <- base |>
    select({{candidato}}, {{agregacao}}, {{votos}}) |>
    group_by({{candidato}}, {{agregacao}}) |>
    summarise(votos = sum({{votos}}, na.rm = T)) |>
    ungroup()

  # Cria uma tabela com todas as agregacoes por candidato
  expand(x, {{candidato}}, {{agregacao}}) |>
    left_join(x) |>
    mutate(votos = ifelse(is.na(votos), 0, votos),
           total = sum(votos, na.rm = T)) |>
    group_by({{candidato}}) |>
    mutate(tot_cand = sum(votos, na.rm = T)) |>
    ungroup() |>
    group_by({{agregacao}}) |>
    mutate(tot_agregacao = sum(votos, na.rm = T)) |>
    ungroup() |>
    group_by({{candidato}}, {{agregacao}}) |>
    summarise(indiceG = ((votos / tot_cand) - (tot_agregacao/total))^2) |>
    ungroup() |>
    group_by({{candidato}}) |>
    summarise(indiceG = sum(indiceG, na.rm = T)) |>
    ungroup()
}

################################################################################

# QL (Silva e Davidian, 2013)

QL <- function(base, candidato, agregacao, votos){

  # Cria tabela auxiliar com o n. de votos do candidato por agregacao
  x <- base |>
    select({{candidato}}, {{agregacao}}, {{votos}}) |>
    group_by({{candidato}}, {{agregacao}}) |>
    summarise(votos = sum({{votos}}, na.rm = T)) |>
    ungroup()

  # Cria uma tabela com todas as agregacoes por candidato
  expand(x, {{candidato}}, {{agregacao}}) |>
    left_join(x) |>
    mutate(votos = ifelse(is.na(votos), 0, votos),
           total = sum(votos, na.rm = T)) |>
    group_by({{candidato}}) |>
    mutate(tot_cand = sum(votos, na.rm = T)) |>
    ungroup() |>
    group_by({{agregacao}}) |>
    mutate(tot_agregacao = sum(votos, na.rm = T)) |>
    ungroup() |>
    group_by({{candidato}}, {{agregacao}}) |>
    summarise(LQ = (votos/tot_cand) / (tot_agregacao/total)) |>
    ungroup()

}

################################################################################

# HC (Silva e Davidian, 2013)

HC_QL <- function(base, candidato, agregacao, votos){

  # Cria tabela auxiliar com o n. de votos do candidato por agregacao
  x <- base |>
    select({{candidato}}, {{agregacao}}, {{votos}}) |>
    group_by({{candidato}}, {{agregacao}}) |>
    summarise(votos = sum({{votos}}, na.rm = T)) |>
    ungroup()

  # Cria uma tabela com todas as agregacoes por candidato
  expand(x, {{candidato}}, {{agregacao}}) |>
    left_join(x) |>
    mutate(votos = ifelse(is.na(votos), 0, votos),
           total = sum(votos, na.rm = T)) |>
    group_by({{candidato}}) |>
    mutate(tot_cand = sum(votos, na.rm = T)) |>
    ungroup() |>
    group_by({{agregacao}}) |>
    mutate(tot_agregacao = sum(votos, na.rm = T)) |>
    ungroup() |>
    group_by({{candidato}}, {{agregacao}}) |>
    summarise(
      indice_ql = (votos/tot_cand) / (tot_agregacao/total),
      indice_hc = ((tot_cand*tot_agregacao)/total) * (indice_ql-1)
    ) |>
    ungroup()

}

################################################################################

##### Aplicacao: candidatos a deputado federal, Mato Grosso do Sul

# Importa os dados
ms_depfed_2022 <- readRDS("dados/ms_depfed_2022.rds")

# Calculo do G
g_deputados_ms22 <- G(ms_depfed_2022,
                      NM_CANDIDATO, CD_MUNICIPIO, QT_VOTOS_NOMINAIS)

# Calculo do HC e do QL
hcql_deputados_ms22 <- HC_QL(ms_depfed_2022,
                             NM_CANDIDATO, NM_MUNICIPIO, QT_VOTOS_NOMINAIS)

################################################################################

##### Visualizacao: Camila Jara (PT-MS)

# Tabela de IDs TSE-IBGE
ids_ibge <- read_csv2("http://cepespdata.io/static/docs/cod_municipios.csv",
                      col_types = c("cccccc")) |>
  # Mantemos somente essas 3 colunas
  select(UF, COD_MUN_TSE, COD_MUN_IBGE) |>
  distinct()

# Resultado para Camila Jara
hcql_camilajara <- HC_QL(ms_depfed_2022,
                         NM_CANDIDATO, CD_MUNICIPIO, QT_VOTOS_NOMINAIS) |>
  filter(NM_CANDIDATO == "CAMILA BAZACHI JARA MARZOCHI") |>
  left_join(ids_ibge, by = c("CD_MUNICIPIO" = "COD_MUN_TSE"))

# Baixa sf de municípios do MS
ms_municipios <- read_municipality(code_muni = "MS")

# Cria sf com indicadores de Camila Jara por municipio
camilajara_espacial <- ms_municipios |>
  mutate(code_muni = as.character(code_muni)) |>
  left_join(hcql_camilajara, by = c("code_muni" = "COD_MUN_IBGE")) |>
  mutate(
    ql_cortes = case_when(
      indice_ql < 0.5 ~ "< 0,5",
      indice_ql > 0.5 & indice_ql <= 0.9999999 ~ "0,5-1",
      indice_ql == 1 ~ "1",
      indice_ql > 1 & indice_ql <= 1.5 ~ "1-1,5",
      indice_ql > 1.5 ~ "> 1,5"),

    ql_cortes = factor(
      ql_cortes,
      levels = c("< 0,5", "0,5-1", "1", "1-1,5", "> 1,5")),

    hc_cortes = case_when(
      indice_hc > -2500 & indice_hc <= -1500 ~ "-2500 a -1500",
      indice_hc > -1500 & indice_hc <= -500 ~ "-1500 a -500",
      indice_hc > -500 & indice_hc < 0 ~ "-500 a 0",
      indice_hc == 0 ~ "0",
      indice_hc > 0 & indice_hc <= 500 ~ "0 a 500",
      indice_hc > 500 & indice_hc <= 1500 ~ "500 a 1500",
      indice_hc > 1500 ~ "> 1500"),

    hc_cortes = factor(
      hc_cortes,
      levels = c("-2500 a -1500", "-1500 a -500", "-500 a 0",
                 "0", "0 a 500", "500 a 1500", "> 1500")))

### Visualizacao QL

camilajara_espacial |>
  ggplot() +
  geom_sf(aes(fill = ql_cortes), linewidth = 0.03) +
  labs(
    title = "Índice QL - Camila Jara",
    subtitle = "(Deputada Federal, PT-MS)",
    caption = "Fonte: produzido por Gelape e Silva,\na partir de dados do TSE e do IBGE.") +
  scale_fill_brewer(
    palette = "RdBu", direction = -1, drop = FALSE, name = NULL) +
  annotation_scale(location = "bl", width_hint = 0.2) +
  coord_sf(datum = NA) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    plot.caption = element_text(hjust = 0.5, size = 10),
    legend.position = "bottom",
    legend.direction = "horizontal")

ggsave("imagens/camila_jara_ql.jpg", width = 5.5, height = 5.5)

### Visualizacao HC

camilajara_espacial |>
  ggplot() +
  geom_sf(aes(fill = hc_cortes), linewidth = 0.03) +
  labs(
    title = "Índice HC - Camila Jara",
    subtitle = "(Deputada Federal, PT-MS)",
    caption = "Fonte: produzido por Gelape e Silva,\na partir de dados do TSE e do IBGE.") +
  scale_fill_brewer(
    palette = "RdBu", direction = -1,
    drop = FALSE,
    name = NULL) +
  annotation_scale(location = "bl", width_hint = 0.2) +
  coord_sf(datum = NA) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    plot.caption = element_text(hjust = 0.5, size = 10))

ggsave("imagens/camila_jara_hc.jpg", width = 6.5, height = 5.5)

### Simbolos proporcionais

# Criamos um sf com centroids
centroides_ms <- st_centroid(camilajara_espacial)

ggplot() +
  geom_sf(
    data = ms_municipios,
    fill = "white",
    linewidth = 0.1) +
  geom_sf(
    data = centroides_ms,
    aes(
      size = indice_hc, color = hc_cortes)) +
  labs(
    title = "Índice HC - Camila Jara",
    subtitle = "(Deputada Federal, PT-MS)",
    caption = "Fonte: produzido por Gelape e Silva,\na partir de dados do TSE e do IBGE.") +
  scale_color_brewer(
    palette = "RdBu", direction = -1, drop = FALSE, name = NULL) +
  scale_radius(range = c(1, 25),
               breaks = c(0, 500, 1500, 2500, 19000),
               name = NULL) +
  annotation_scale(location = "bl", width_hint = 0.2) +
  coord_sf(datum = NA) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    plot.caption = element_text(hjust = 0.5, size = 10))

ggsave("imagens/camila_jara_hc_prop.jpg", width = 6.5, height = 5.5)

################################################################################

##### Informacoes da sessao

sessionInfo()

# R version 4.2.2 (2022-10-31)
# Platform: aarch64-apple-darwin20 (64-bit)
# Running under: macOS 14.4.1
#
# Matrix products: default
# LAPACK: /Library/Frameworks/R.framework/Versions/4.2-arm64/Resources/lib/libRlapack.dylib
#
# locale:
#  [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
#
# attached base packages:
#  [1] stats     graphics  grDevices utils     datasets  methods   base
#
# other attached packages:
#  [1] classInt_0.4-9  ggspatial_1.1.8 ggplot2_3.4.2   geobr_1.7.0     sf_1.0-15       readr_2.1.4     tidyr_1.3.0
#  [8] dplyr_1.1.2
#
# loaded via a namespace (and not attached):
#  [1] Rcpp_1.0.10        RColorBrewer_1.1-3 pillar_1.9.0       compiler_4.2.2     class_7.3-20       tools_4.2.2
#  [7] bit_4.0.5          lifecycle_1.0.3    tibble_3.2.1       gtable_0.3.3       pkgconfig_2.0.3    rlang_1.1.1
# [13] DBI_1.2.2          cli_3.6.1          rstudioapi_0.14    parallel_4.2.2     curl_5.0.0         e1071_1.7-13
# [19] httr_1.4.5         withr_2.5.0        systemfonts_1.0.4  generics_0.1.3     vctrs_0.6.2        hms_1.1.3
# [25] bit64_4.0.5        grid_4.2.2         tidyselect_1.2.0   glue_1.6.2         data.table_1.14.8  R6_2.5.1
# [31] textshaping_0.3.6  fansi_1.0.4        vroom_1.6.3        farver_2.1.1       purrr_1.0.1        tzdb_0.3.0
# [37] magrittr_2.0.3     scales_1.2.1       units_0.8-2        colorspace_2.1-0   ragg_1.2.5         KernSmooth_2.23-20
# [43] utf8_1.2.3         proxy_0.4-27       munsell_0.5.0      crayon_1.5.2

