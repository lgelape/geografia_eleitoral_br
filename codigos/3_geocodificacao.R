################################################################################
################################################################################
####
#### Geocodificacao
####

# Lucas Gelape
# Glauco Peres da Silva

library(dplyr)
library(readr)
library(stringr)
library(tidygeocoder)
library(sf)
library(geobr)
library(ggplot2)

################################################################################

##### Imporacao dos dados de locais de votacao/2022

# Baixa dados de eleitorado-local/2022 do site do TSE
download.file(
  "https://cdn.tse.jus.br/estatistica/sead/odsele/eleitorado_locais_votacao/eleitorado_local_votacao_2022.zip",
  "dados/eleitorado_local_votacao_2022.zip")

unzip("dados/eleitorado_local_votacao_2022.zip",
      exdir = "dados/eleitorado_local_votacao_2022")

# Importacao e limpa dados de eleitorado-local
eleitorado_secao_22 <- read_csv2(
  "dados/eleitorado_local_votacao_2022/eleitorado_local_votacao_2022.csv",
  locale = locale(encoding = "latin1"),
  col_types = str_c(rep("c", 41), collapse = "")) |>
  mutate(
    NR_LONGITUDE = ifelse(NR_LONGITUDE == "-1", NA, NR_LONGITUDE),
    NR_LATITUDE  = ifelse(NR_LATITUDE == "-1", NA, NR_LATITUDE),
    DS_ENDERECO  = str_squish(DS_ENDERECO)) |>
  filter(!SG_UF == "ZZ",
         NR_TURNO == 1)

# Mantem somente variaveis de interesse
# DT_GERACAO: 26/03/2024
lv22 <- eleitorado_secao_22 |>
  select(DT_GERACAO, AA_ELEICAO,
         SG_UF, CD_MUNICIPIO, NM_MUNICIPIO, NR_ZONA, NR_SECAO,
         NR_LOCAL_VOTACAO, NM_LOCAL_VOTACAO, DS_ENDERECO, NM_BAIRRO, NR_CEP,
         NR_LATITUDE, NR_LONGITUDE)

saveRDS(lv22, "dados/lvs2022.rds")

file.remove("dados/eleitorado_local_votacao_2022.zip")
unlink("dados/eleitorado_local_votacao_2022/", recursive = T)

################################################################################

##### Geocodificacao de LV-SP/2022 cujo lat-long nao foi incluido

# Selecionamos somente o municipio de SP
lvsp22 <- filter(lv22, CD_MUNICIPIO == 71072)

# Filtra somente por aqueles que nao tem lat-long
lvsp22_na <- filter(lvsp22, is.na(NR_LATITUDE))

# Geocodificar os enderecos
lvsp22_geocode <- lvsp22_na |>
  mutate(PAIS = "BRASIL") |>
  geocode(
    street = DS_ENDERECO, city = NM_MUNICIPIO, state = SG_UF, country = PAIS,
    # Usaremos o openstreetmap
    method = 'osm',
    lat = lat_osm, long = long_osm)

# Existe NA?
table(is.na(lvsp22_geocode$lat_osm))

### Consistencia: pontos dentro do municipio

# Transformar objeto com os locais de votacao em sf
lvsp22_geocode_sf <- lvsp22_geocode |>
  filter(!is.na(lat_osm)) |>
  st_as_sf(coords = c("long_osm", "lat_osm"), crs = 4326) |>
  st_transform(4674)

# Baixa os limites de SP (pacote geobr)
sp_geobr <- geobr::read_municipality(code_muni = 3550308)

# Une espacialmente esses dois objetos
lvsp22_geocode_sp_join <- st_join(lvsp22_geocode_sf, sp_geobr)

# Inspecionar o numero de NA em variaveis anexadas do objeto sp_geobr
table(is.na(lvsp22_geocode_sp_join$code_muni))

# Inspecao visual desse resultado
ggplot() +
  geom_sf(data = sp_geobr) +
  geom_sf(data = lvsp22_geocode_sf) +
  labs(
    title = "Locais de votação geocodificados (São Paulo, 2022)",
    caption = "\nFonte: produzido por Gelape e Silva, a partir de dados do TSE e OpenStreetMap.\n")

ggsave("imagens/geocode2022.jpg", width = 8, height = 5)

################################################################################

##### Calculo de distancias: quao distantes elas estao dos limites municipais?

# Selecionamos LVs unicos fora dos limites municipais
fora_sp <- filter(lvsp22_geocode_sp_join, is.na(code_muni)) |>
  select(AA_ELEICAO, SG_UF, CD_MUNICIPIO, NM_MUNICIPIO,
         NM_LOCAL_VOTACAO, DS_ENDERECO, NM_BAIRRO, NR_CEP) |>
  distinct()

# Transforma o src para coordenadas projetadas para facilitar interpretacao
fora_sp <- st_transform(fora_sp, 31983)
sp_geobr <- st_transform(sp_geobr, 31983)

# Calculo da distancia de cada ponto ate a extremidade mais proxima do poligono
distancias_pontos <- st_distance(fora_sp, sp_geobr)

# Transforma o resultado em df e inclui no objeto original
fora_sp <- bind_cols(fora_sp, as.data.frame(distancias_pontos))

# Estatisticas descritivas dos resultados (em metros)
summary(fora_sp$distancias_pontos)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max.
# 15.6   3319.6  83553.5 148309.7 274284.1 491694.5

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
#   [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
#
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base
#
# other attached packages:
#   [1] ggplot2_3.4.2      geobr_1.7.0        sf_1.0-15          tidygeocoder_1.0.5 stringr_1.5.0      readr_2.1.4
# [7] dplyr_1.1.2
#
# loaded via a namespace (and not attached):
#  [1] Rcpp_1.0.10        pillar_1.9.0       compiler_4.2.2     prettyunits_1.1.1  progress_1.2.2     class_7.3-20
#  [7] tools_4.2.2        bit_4.0.5          jsonlite_1.8.4     lifecycle_1.0.3    tibble_3.2.1       gtable_0.3.3
# [13] pkgconfig_2.0.3    rlang_1.1.1        DBI_1.2.2          cli_3.6.1          rstudioapi_0.14    parallel_4.2.2
# [19] curl_5.0.0         e1071_1.7-13       s2_1.1.4           httr_1.4.5         withr_2.5.0        systemfonts_1.0.4
# [25] generics_0.1.3     vctrs_0.6.2        hms_1.1.3          bit64_4.0.5        classInt_0.4-9     grid_4.2.2
# [31] tidyselect_1.2.0   glue_1.6.2         data.table_1.14.8  R6_2.5.1           textshaping_0.3.6  fansi_1.0.4
# [37] vroom_1.6.3        farver_2.1.1       tzdb_0.3.0         magrittr_2.0.3     scales_1.2.1       units_0.8-2
# [43] colorspace_2.1-0   ragg_1.2.5         KernSmooth_2.23-20 utf8_1.2.3         proxy_0.4-27       stringi_1.7.12
# [49] wk_0.7.3           munsell_0.5.0      crayon_1.5.2
