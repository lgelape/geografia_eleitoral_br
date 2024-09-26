################################################################################
################################################################################
####
#### Visualizacao de resultados eleitorais
####

# Lucas Gelape
# Glauco Peres da Silva

library(dplyr)
library(readr)
library(ggplot2)
library(sf)
library(geobr)
library(ggspatial)
library(classInt)

# Tabela de IDs TSE-IBGE
ids_ibge <- read_csv(
  "https://raw.githubusercontent.com/GV-CEPESP/cepespdata/refs/heads/main/tabelas_auxiliares/dados/codigo_municipio_ibge_tse.csv",
  col_types = c("cccccccccccc")) |>
  select(UF, COD_MUN_TSE, COD_MUN_IBGE) |>
  distinct()

################################################################################

##### Lula 2022

# Importa resultados
lula2022 <- readRDS("dados/lula2022.rds") |>
  left_join(ids_ibge,
            by = c("CD_MUNICIPIO" = "COD_MUN_TSE", "SG_UF" = "UF"))

# Faz download de objeto espacial com os municipios brasileiros
br_municipios <- read_municipality() |>
  st_transform(5880)

# Cria objeto sf com votacao de Lula
lula2022_espacial <- br_municipios |>
  mutate(code_muni = as.character(code_muni)) |>
  left_join(lula2022, by = c("code_muni" = "COD_MUN_IBGE"))

# Produz mapa com ggplot2
lula2022_espacial |>
  ggplot() +
  geom_sf(aes(fill = PERCENTUAL), linewidth = 0.01) +
  labs(
    title = "Votação de Lula (1º turno, 2022)",
    caption = "Fonte: produzido por Gelape e Silva,\na partir de dados do TSE e do IBGE.",
    fill = "Votos válidos\n(em %)") +
  scale_fill_distiller(
    palette = "Reds", direction = 1) +
  annotation_scale(
    location = "br", width_hint = 0.2) +
  annotation_north_arrow(
    location = "br", pad_y = unit(0.5, "in")) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    plot.caption = element_text(hjust = 0.5, size = 10),
    legend.position = "bottom",
    legend.direction = "horizontal")

ggsave("imagens/lula_2022.jpg", width = 6, height = 7)

################################################################################

##### Arthur Lira/2022

# Importa os resultados eleitorais de Arthur Lira
arthur_lira_2022 <- readRDS("dados/arthur_lira_2022.rds") |>
  rename(PERCENTUAL_MUN = PERCENTUAL) |>
  mutate(PERCENTUAL_CAND = 100*(VOTOS/sum(VOTOS))) |>
  mutate(
    PERC_CAND_C = cut(PERCENTUAL_CAND, breaks = seq(0, 8, 2),
                      labels = c("0-2%", "2-4%", "4-6%", "6-8%")),
    PERC_MUN_C  = cut(PERCENTUAL_MUN, breaks = seq(0, 60, 10),
                      labels = c("0-10%", "10-20%", "20-30%",
                                 "30-40%", "40-50%", "50-60%"))) |>
  left_join(ids_ibge,
            by = c("CD_MUNICIPIO" = "COD_MUN_TSE", "SG_UF" = "UF"))

# Importa um objeto sf com municipios de Alagoas
al_municipios <- read_municipality(code_muni = "AL")

# Cria objeto sf com votacao de Arthur Lira
arthur_lira_2022_espacial <- al_municipios |>
  mutate(code_muni = as.character(code_muni)) |>
  left_join(arthur_lira_2022, by = c("code_muni" = "COD_MUN_IBGE"))

## Adiciona uma categoria com cortes de jenks
cortes_jenks <- classIntervals(
  arthur_lira_2022_espacial$PERCENTUAL_CAND, 4, "jenks")

arthur_lira_2022_espacial$PERC_CAND_CORTES <- cut(
  arthur_lira_2022_espacial$PERCENTUAL_CAND, cortes_jenks$brks,
  include.lowest = T)

## Mapa de percentual de votos do municipio
arthur_lira_2022_espacial |>
  ggplot() +
  geom_sf(
    aes(fill = PERC_MUN_C), linewidth = 0.1) +
  labs(
    title = "Votação de Arthur Lira (Deputado Federal, 2022)",
    subtitle = "Percentual do total de votos no município",
    caption = "\nFonte: produzido por Gelape e Silva, a partir de dados do TSE e do IBGE.\n",
    fill = "Votos válidos\n(em %)") +
  annotation_north_arrow(
    location = "br", pad_y = unit(0.5, "in"), style = north_arrow_nautical) +
  scale_fill_brewer(palette = "BuPu") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    plot.caption = element_text(hjust = 0.5, size = 10),
    legend.position = "right",
    legend.direction = "vertical")

ggsave("imagens/arthur_lira_2022_percmun.jpg", width = 7.5, height = 4.5)

## Mapa de percentual de votos do candidato
arthur_lira_2022_espacial |>
  ggplot() +
  geom_sf(
    aes(fill = PERC_CAND_C), linewidth = 0.1) +
  labs(
    title = "Votação de Arthur Lira (Deputado Federal, 2022)",
    subtitle = "Percentual do total de votos do candidato",
    caption = "\nFonte: produzido por Gelape e Silva, a partir de dados do TSE e do IBGE.\n",
    fill = "Votos do candidato\n(em %)") +
  annotation_north_arrow(
    location = "br", pad_y = unit(0.5, "in"), style = north_arrow_nautical) +
  scale_fill_brewer(palette = "BuPu") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    plot.caption = element_text(hjust = 0.5, size = 10),
    legend.position = "right",
    legend.direction = "vertical")

ggsave("imagens/arthur_lira_2022_perccand.jpg", width = 7.5, height = 4.5)

## Mapa de percentual de votos do candidato (cortes de jenks)
arthur_lira_2022_espacial |>
  ggplot() +
  geom_sf(
    aes(fill = PERC_CAND_CORTES), linewidth = 0.1) +
  labs(
    title = "Votação de Arthur Lira (Deputado Federal, 2022)",
    subtitle = "Percentual do total de votos do candidato (Cortes de Jenks)",
    caption = "\nFonte: produzido por Gelape e Silva, a partir de dados do TSE e do IBGE.\n",
    fill = "Votos do candidato\n(em %)") +
  annotation_north_arrow(
    location = "br", pad_y = unit(0.5, "in"), style = north_arrow_nautical) +
  scale_fill_brewer(palette = "BuPu") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    plot.caption = element_text(hjust = 0.5, size = 10),
    legend.position = "right",
    legend.direction = "vertical")

ggsave("imagens/arthur_lira_2022_jenks.jpg", width = 7.5, height = 4.5)

################################################################################

##### Nikolas Ferreira, BH/2020

# Importa os resultados de Nikolas Ferreira
nikolas_2020_bh <- readRDS("dados/nikolas_2020_bh_lv.rds")

# Importa os lv BH/2020
lvbh2020 <- readRDS("dados/lvbh2020.rds")

# Une dados Nikolas + LVs
nikolas_2020_bh_espacial <- left_join(
  nikolas_2020_bh, lvbh2020,
  by = c("NR_ZONA" = "nr_zona", "NR_LOCAL_VOTACAO" = "nr_locvot")) |>
  st_as_sf(coords = c("long", "lat"), crs = 4326)

# Importa bairros de BH
bh_bairros <- read_sf("dados/BAIRRO_OFICIAL/BAIRRO_OFICIAL.shp",
                      options = "ENCODING=WINDOWS-1252") |>
  st_transform(4326)

# Passa info de bairros a votacao
nikolas_2020_bairro <- st_join(nikolas_2020_bh_espacial, bh_bairros)

# Calcula votacao-bairro
nikolas_2020_bairro <- nikolas_2020_bairro |>
  group_by(CODIGO, NOME) |>
  summarise(VOTOS_BAIRRO = sum(VOTOS)) |>
  ungroup() |>
  st_drop_geometry()

# Passa voto-bairro ao objeto que tem todos os bairros
nikolas_2020_final <- left_join(bh_bairros, nikolas_2020_bairro)

# Cria cortes por quartis
cortes <- classIntervals(nikolas_2020_final$VOTOS_BAIRRO, 4, "quantile")

# Insere os cortes na base de dados final
nikolas_2020_final$quartis <- cut(nikolas_2020_final$VOTOS_BAIRRO,
                                  cortes$brks, include.lowest = T)

# Cria categoria especial para bairros sem LV
nikolas_2020_final$quartis <- factor(
  ifelse(
    is.na(nikolas_2020_final$quartis),
    "Sem LV", nikolas_2020_final$quartis),
  labels = c("Sem LV", "[7,56]", "(56,104]", "(104,175]", "(175,610]"))

# Mapa de votos (absolutos) por bairro
nikolas_2020_final |>
  ggplot() +
  geom_sf(
    aes(fill = quartis), linewidth = 0.1) +
  labs(
    title = "Votação de Nikolas Ferreira (Vereador, 2020)",
    subtitle = "Bairros do município de Belo Horizonte",
    caption = "\nFonte: produzido por Gelape e Silva, a partir de dados do TSE e da PBH.\n",
    fill = "Votos") +
  annotation_scale(
    location = "br", width_hint = 0.3) +
  scale_fill_brewer(palette = "Greens") +
  theme(
    panel.background = element_rect(fill = "white"),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    plot.caption = element_text(hjust = 0.5, size = 10),
    legend.position = "bottom",
    legend.direction = "horizontal")

ggsave("imagens/nikolas_2020.jpg", width = 5, height = 6)

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
#  [1] classInt_0.4-9  ggspatial_1.1.8 geobr_1.7.0     sf_1.0-15       ggplot2_3.4.2   readr_2.1.4     dplyr_1.1.2
#
# loaded via a namespace (and not attached):
#  [1] Rcpp_1.0.10        RColorBrewer_1.1-3 pillar_1.9.0       compiler_4.2.2     class_7.3-20       tools_4.2.2
#  [7] bit_4.0.5          lifecycle_1.0.3    tibble_3.2.1       gtable_0.3.3       pkgconfig_2.0.3    rlang_1.1.1
# [13] cli_3.6.1          DBI_1.2.2          rstudioapi_0.14    parallel_4.2.2     curl_5.0.0         e1071_1.7-13
# [19] s2_1.1.4           httr_1.4.5         withr_2.5.0        systemfonts_1.0.4  generics_0.1.3     vctrs_0.6.2
# [25] hms_1.1.3          bit64_4.0.5        grid_4.2.2         tidyselect_1.2.0   glue_1.6.2         data.table_1.14.8
# [31] R6_2.5.1           textshaping_0.3.6  fansi_1.0.4        vroom_1.6.3        farver_2.1.1       tzdb_0.3.0
# [37] magrittr_2.0.3     scales_1.2.1       units_0.8-2        colorspace_2.1-0   ragg_1.2.5         labeling_0.4.2
# [43] utf8_1.2.3         KernSmooth_2.23-20 proxy_0.4-27       wk_0.7.3           munsell_0.5.0      crayon_1.5.2

