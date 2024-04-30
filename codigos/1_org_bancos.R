################################################################################
################################################################################
####
#### * Organizacao de dados eleitorais
#### * Uniao com dados de outras fontes
####

# Lucas Gelape
# Glauco Peres da Silva

library(dplyr)
library(readr)
library(electionsBR)
library(sf)

################################################################################

##### Download via electionsBR (v. 0.4.0)

voto_mun_zona_2022 <- elections_tse(
  2022, type = "vote_mun_zone", br_archive = T)

partido_zona_mun_2022 <- elections_tse(2022, type = "party_mun_zone")

voto_secao_2020 <- elections_tse(
  2020, type = "vote_section", uf = "MG")

##### Download Locais de Votacao

# Baixa o arquivo de Daniel Hidalgo com todos os LV entre 2006-2020
download.file(
  "https://github.com/fdhidalgo/geocode_br_polling_stations/releases/download/v0.13-beta/geocoded_polling_stations.csv.gz",
  "dados/geocoded_polling_stations.csv.gz")

# Importa os dados, filtra BH/2020
lvbh2020 <- read_csv("dados/geocoded_polling_stations.csv.gz") |>
  filter(ano == 2020,
         cd_localidade_tse == 41238) |>
  select(nr_zona, nr_locvot, long, lat)

saveRDS(lvbh2020, "dados/lvbh2020.rds")

file.remove("dados/geocoded_polling_stations.csv.gz")

################################################################################

##### Lula 1T/2022 (votacao percentual e absoluta)

lula2022 <- voto_mun_zona_2022 |>
  filter(NR_TURNO == 1 & DS_CARGO == "Presidente") |>
  group_by(SG_UF, CD_MUNICIPIO, NM_MUNICIPIO) |>
  mutate(VOTOS_MUN = sum(QT_VOTOS_NOMINAIS)) |>
  ungroup() |>
  filter(NR_PARTIDO == 13) |>
  group_by(SG_UF, CD_MUNICIPIO, NM_MUNICIPIO, VOTOS_MUN) |>
  summarise(VOTOS = sum(QT_VOTOS_NOMINAIS)) |>
  ungroup() |>
  mutate(PERCENTUAL = 100*(VOTOS/VOTOS_MUN))

saveRDS(lula2022, "dados/lula2022.rds")

################################################################################

##### Arthur Lira 1T/2022 (votacao percentual e absoluta)

# Prepara partido_zona_mun_2022 (votos de legenda)
votos_legenda_al <- partido_zona_mun_2022 |>
  filter(DS_CARGO == "Deputado Federal" & SG_UF == "AL") |>
  group_by(ANO_ELEICAO, SG_UF, CD_MUNICIPIO, NM_MUNICIPIO, SG_PARTIDO) |>
  summarise(VOTOS = sum(QT_TOTAL_VOTOS_LEG_VALIDOS)) |>
  ungroup() |>
  rename(NM_URNA_CANDIDATO = SG_PARTIDO)

# Cria objeto com percentual de votos validos do Arthur Lira/municipio
arthur_lira_2022 <- voto_mun_zona_2022 |>
  filter(DS_CARGO == "Deputado Federal" & SG_UF == "AL") |>
  group_by(ANO_ELEICAO, SG_UF, CD_MUNICIPIO, NM_MUNICIPIO, NM_URNA_CANDIDATO) |>
  summarise(VOTOS = sum(QT_VOTOS_NOMINAIS)) |>
  ungroup() |>
  bind_rows(votos_legenda_al) |>
  group_by(SG_UF, CD_MUNICIPIO, NM_MUNICIPIO) |>
  mutate(VOTOS_MUN = sum(VOTOS)) |>
  ungroup() |>
  filter(NM_URNA_CANDIDATO == "ARTHUR LIRA") |>
  mutate(PERCENTUAL = 100*(VOTOS/VOTOS_MUN))

saveRDS(arthur_lira_2022, "dados/arthur_lira_2022.rds")

# Cria objeto com votos nominais para deputado federal MS/2022
ms_depfed_2022 <- voto_mun_zona_2022 |>
  filter(DS_CARGO == "Deputado Federal" & SG_UF == "MS")

saveRDS(ms_depfed_2022, "dados/ms_depfed_2022.rds")

################################################################################

##### Nikolas Ferreira (votacao absoluta)

# Filtra pelo municipio de Belo Horizonte
voto_secao_2020_bh <- voto_secao_2020 |>
  filter(CD_MUNICIPIO == 41238)

# Cria uma lista com todos as zonas-secoes da cidade
zona_secao_2020_bh <- voto_secao_2020_bh |>
  select(ANO_ELEICAO, NR_ZONA, NR_SECAO, NR_LOCAL_VOTACAO) |>
  distinct()

# Calcula os votos de Nikolas Ferreira por local de votação
nikolas_2020_bh <- voto_secao_2020_bh |>
  filter(NM_VOTAVEL == "NIKOLAS FERREIRA DE OLIVEIRA") |>
  right_join(zona_secao_2020_bh) |>
  mutate(QT_VOTOS = ifelse(is.na(QT_VOTOS), 0, QT_VOTOS),
         NR_VOTAVEL = ifelse(is.na(NR_VOTAVEL), 28000, NR_VOTAVEL)) |>
  group_by(ANO_ELEICAO, NR_VOTAVEL, NR_ZONA, NR_LOCAL_VOTACAO) |>
  summarise(VOTOS = sum(QT_VOTOS)) |>
  ungroup()

saveRDS(nikolas_2020_bh, "dados/nikolas_2020_bh_lv.rds")

################################################################################

##### Uniao de dados nao-espaciais: indice de Gini

### Tabela de IDs TSE-IBGE
ids_ibge <- read_csv2("http://cepespdata.io/static/docs/cod_municipios.csv",
                      col_types = c("cccccc")) |>
  select(UF, COD_MUN_TSE, COD_MUN_IBGE) |>
  distinct()

### Indice de Gini por municipio (IPEA Data)
gini2010 <- read_csv2("dados/ipeadata_gini.csv", skip = 1) |>
  rename_with(~ c("uf", "cod_mun", "municipio", "gini2010", "vazio")) |>
  mutate(cod_mun = as.character(cod_mun)) |>
  select(uf, cod_mun, gini2010)

### Lula 2022 + Gini/2010
lula2022 <- lula2022 |>
  left_join(ids_ibge,
            by = c("CD_MUNICIPIO" = "COD_MUN_TSE", "SG_UF" = "UF")) |>
  left_join(gini2010, by = c("COD_MUN_IBGE" = "cod_mun"))

################################################################################

##### Uniao por atributos espaciais: bairros de BH

# Importa shapefile de resultados por local de votacao de BH (CEM-USP)
lv_bh <- read_sf("dados/EL2022_LV_RMBRA_CEM_V1/EL2022_LV_RMBRA_CEM_V1.shp",
                  options = "ENCODING=WINDOWS-1252", crs = 4674) |>
  filter(MUN_NOME == "BELO HORIZONTE") |>
  select(ID, ANO_ELE, COD_LV, NOME_LV, LV_TIT, LV_TIPO, END_LV, RME_NOME,
         MUN_SIG, MUN_NOME, MUN_NM_A, CD_MUN_T, CD_MUN_I, SG_UF, CD_UF,
         ZE_COD, ZE_NUM, ZE_NOME, FR_LIM, ORIG_LL)

# Importa shapefile de bairros de Belo Horizonte (PBH)
bh_bairros <- read_sf("dados/BAIRRO_OFICIAL/BAIRRO_OFICIAL.shp",
                      options = "ENCODING=WINDOWS-1252") |>
  st_transform(4674)

# Uniao espacial dos dois objetos
lv_bh_bairros <- st_join(lv_bh, bh_bairros)

# N. de bairros unicos com locais de votacao (195):
# Numero menor que o total de bairros na cidade
length(unique(lv_bh_bairros$NOME))

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
#  [1] sf_1.0-15         electionsBR_0.4.0 readr_2.1.4       dplyr_1.1.2
#
# loaded via a namespace (and not attached):
#  [1] Rcpp_1.0.10        pillar_1.9.0       compiler_4.2.2     class_7.3-20       tools_4.2.2        bit_4.0.5
#  [7] lifecycle_1.0.3    tibble_3.2.1       pkgconfig_2.0.3    rlang_1.1.1        DBI_1.2.2          cli_3.6.1
# [13] rstudioapi_0.14    curl_5.0.0         parallel_4.2.2     e1071_1.7-13       withr_2.5.0        httr_1.4.5
# [19] generics_0.1.3     vctrs_0.6.2        hms_1.1.3          classInt_0.4-9     bit64_4.0.5        grid_4.2.2
# [25] tidyselect_1.2.0   glue_1.6.2         data.table_1.14.8  R6_2.5.1           fansi_1.0.4        vroom_1.6.3
# [31] tzdb_0.3.0         magrittr_2.0.3     units_0.8-2        utf8_1.2.3         KernSmooth_2.23-20 proxy_0.4-27
# [37] crayon_1.5.2
