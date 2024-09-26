################################################################################
################################################################################
####
#### Autocorrelacao espacial
####

# Lucas Gelape
# Glauco Peres da Silva

library(dplyr)
library(readr)
library(sf)
library(spdep)
library(ggplot2)

################################################################################

# Tabela auxiliar de codigos
ids_ibge <- read_csv(
  "https://raw.githubusercontent.com/GV-CEPESP/cepespdata/refs/heads/main/tabelas_auxiliares/dados/codigo_municipio_ibge_tse.csv",
  col_types = c("cccccccccccc")) |>
  select(UF, COD_MUN_TSE, COD_MUN_IBGE) |>
  distinct()

# Importa a votacao de Lula
lula2022 <- readRDS("dados/lula2022.rds") |>
  left_join(ids_ibge,
            by = c("CD_MUNICIPIO" = "COD_MUN_TSE", "SG_UF" = "UF")) |>
  filter(!SG_UF == "ZZ")

# Importa sf com municipios brasileiros
municipios_br <- geobr::read_municipality(simplified = T) |>
  mutate(code_muni = as.character(code_muni)) |>
  st_make_valid()

# Acrescenta votos de Lula a sf
municipios_lula <- municipios_br |>
  mutate(code_muni = as.character(code_muni)) |>
  left_join(lula2022, by = c("code_muni" = "COD_MUN_IBGE")) |>
  filter(!is.na(PERCENTUAL)) |>
  # Remove os municipios-ilhas
  filter(!code_muni %in% c("2605459", "3520400"))

# Cria a lista/matriz de vizinhos
w_pais <- poly2nb(
  municipios_lula,
  row.names = municipios_lula$code_muni)

# Preenche a lista/matriz
ww_pais <- nb2listw(
  w_pais,
  style = "W")

################################################################################

### Moran Global

# Fazemos o teste com a funcao moran.test, do pacote spdep
moran_lula22 <- moran.test(municipios_lula$PERCENTUAL, ww_pais)

# Tambem podemos observar esse teste graficamente
moran_plot_lula <- moran.plot(
  municipios_lula$PERCENTUAL, ww_pais,
  labels = F,
  xlab = "Votos em Lula/2022 (%)",
  ylab = "Votos em Lula/2022 (%) - defasado espacialmente")

################################################################################

### Moran Local

# Teste
lisa <- localmoran(municipios_lula$PERCENTUAL, ww_pais)

### Visualizacao: preparacao de dados

# Padronizacao
municipios_lula$s_n_percentual <- as.vector(
  scale(municipios_lula$PERCENTUAL))

# Variavel espacialmente defasada
municipios_lula$lag_n_percentual <- lag.listw(
  ww_pais, municipios_lula$s_n_percentual)

# Anexa os resultados ao objeto sf original
municipios_lula <- cbind(municipios_lula, lisa)

# Renomeia penultima coluna
names(municipios_lula)[length(names(municipios_lula))-1] <- "p_value"

# Cria variavel com as categorias de LISA
municipios_lula <- municipios_lula |>
  mutate(
    lisa_95 = ifelse(
      s_n_percentual > 0 & lag_n_percentual > 0 & p_value <= 0.05,
      "Alto-Alto", NA),
    lisa_95 = ifelse(
      s_n_percentual <= 0 & lag_n_percentual <= 0 & p_value <= 0.05,
      "Baixo-Baixo", lisa_95),
    lisa_95 = ifelse(
      s_n_percentual > 0 & lag_n_percentual <= 0 & p_value <= 0.05,
      "Alto-Baixo", lisa_95),
    lisa_95 = ifelse(
      s_n_percentual <= 0 & lag_n_percentual > 0 & p_value <= 0.05,
      "Baixo-Alto", lisa_95),
    lisa_95 = ifelse(is.na(lisa_95), "Não significante", lisa_95))  |>
  mutate(lisa_95 = factor(
    lisa_95,
    levels = c("Alto-Alto", "Alto-Baixo", "Baixo-Baixo", "Baixo-Alto",
               "Não significante")))

### Visualizacao: ggplot2

municipios_lula |>
  ggplot() +
  geom_sf(aes(fill = lisa_95), color = "black", linewidth = 0.01) +
  scale_fill_manual(
    values = c("#FF0000", "#F08080", "#0000FF", "#87CEEB", "#D3D3D3"),
    drop = FALSE) +
  theme_void() +
  labs(
    title = "LISA da votação de Lula (1º turno, 2022)",
    subtitle = "(em % de válidos)",
    caption = "Fonte: produzido por Gelape e Silva,\na partir de dados do TSE e do IBGE."
  ) +
  guides(fill = guide_legend(title = "LISA", nrow = 2)) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 14),
    plot.caption = element_text(hjust = 0.5, size = 10),
    legend.position = "bottom",
    legend.direction = "horizontal")

ggsave("imagens/lula_2022_moranlocal.jpg", width = 6, height = 7)

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
#  [1] ggplot2_3.4.2 spdep_1.2-8   spData_2.3.0  sf_1.0-15     readr_2.1.4   dplyr_1.1.2
#
# loaded via a namespace (and not attached):
#  [1] Rcpp_1.0.10        pillar_1.9.0       compiler_4.2.2     geobr_1.7.0        class_7.3-20       tools_4.2.2
#  [7] bit_4.0.5          boot_1.3-28        gtable_0.3.3       lifecycle_1.0.3    tibble_3.2.1       lattice_0.20-45
# [13] pkgconfig_2.0.3    rlang_1.1.1        DBI_1.2.2          cli_3.6.1          rstudioapi_0.14    parallel_4.2.2
# [19] curl_5.0.0         e1071_1.7-13       httr_1.4.5         withr_2.5.0        s2_1.1.4           systemfonts_1.0.4
# [25] generics_0.1.3     vctrs_0.6.2        hms_1.1.3          bit64_4.0.5        classInt_0.4-9     grid_4.2.2
# [31] tidyselect_1.2.0   glue_1.6.2         data.table_1.14.8  R6_2.5.1           textshaping_0.3.6  fansi_1.0.4
# [37] vroom_1.6.3        sp_1.6-1           farver_2.1.1       tzdb_0.3.0         deldir_1.0-9       magrittr_2.0.3
# [43] scales_1.2.1       units_0.8-2        colorspace_2.1-0   ragg_1.2.5         KernSmooth_2.23-20 utf8_1.2.3
# [49] proxy_0.4-27       wk_0.7.3           munsell_0.5.0      crayon_1.5.2
