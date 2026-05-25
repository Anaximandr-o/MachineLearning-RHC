# ============================================
# 1. Configuração e Bibliotecas
# ============================================
library(tidyverse)
library(lubridate)
library(tidymodels)
library(themis)
library(summarytools)
library(geobr)
library(sf)
library(arrow)

# Definição de semente para reprodutibilidade
set.seed(123) 

# ============================================
# 2. Leitura dos Dados
# ============================================
dadosCancer <- readRDS("dados_RHC_Geral.RDS")

# ============================================
# 3. Tratamento, Limpeza e Engenharia de Atributos
# ============================================
muni <- read_municipality(code_muni = "all", year = 2022) # Cria um data frame com todos os municípios do Brasil

muniPontos <- st_centroid(muni) |>
  dplyr::mutate(code_muni = as.character(code_muni)) |> # Fiz isso porque code_muni é double, enquanto PROCEDEN/MUUH é character, então dá erro no left join. Isso resolve :)
  dplyr::select(code_muni) # Cria um data frame com os centróides de todos os municípios, selecionando apenas seus códigos e geometrias

dadosFinal <- dadosCancer %>%
  # --- Transformação de idades impossíveis (x<0 & x>120) em NA ---
  dplyr::mutate(
    IDADE = if_else(
      IDADE < 0 | IDADE > 116,
      NA_real_,
      IDADE
    )
  ) %>%
  
  # --- Renomeação das siglas 99, SI, EX e OP ---
  dplyr::mutate(
    LOCALNAS = dplyr::case_when(
      LOCALNAS %in% c("99", "SI") ~ NA_character_,
      LOCALNAS %in% c("EX", "OP") ~ "Estrangeiro(a)",
      TRUE ~ LOCALNAS
    )
  ) %>% 

  # --- Criação da variável "Região" ---
  dplyr::mutate(
    Regiao = dplyr::case_when(
      LOCALNAS %in% c("AM", "PA", "AC", "RO", "RR", "AP", "TO") ~ "Norte",
      LOCALNAS %in% c("MA", "PI", "CE", "RN", "PB", "PE", "AL", "SE", "BA") ~ "Nordeste",
      LOCALNAS %in% c("MT", "MS", "GO", "DF") ~ "Centro-Oeste",
      LOCALNAS %in% c("MG", "SP", "RJ", "ES") ~ "Sudeste",
      LOCALNAS %in% c("PR", "SC", "RS") ~ "Sul",
      LOCALNAS %in% c("Estrangeiro(a)") ~ "Exterior",
      is.na(LOCALNAS) ~ NA_character_,
      TRUE ~ NA_character_
    )
  ) %>%
  
  # --- Transformação da RACACOR "Sem informação" em NA ---
  dplyr::mutate(
    RACACOR = factor(
      dplyr::na_if(as.character(RACACOR), "Sem informação")
    )
  ) %>% 
  
  # --- Transformação da INSTRUC "Sem informação" em NA ---
  dplyr::mutate(
    INSTRUC = factor(
      dplyr::na_if(as.character(INSTRUC), "Sem informação")
    )
  ) %>%
  
  # --- Transformação da CLIATEN "Sem informação" em NA ---
  dplyr::mutate(
    CLIATEN = factor(
      dplyr::na_if(as.character(CLIATEN), "Sem informação")
    )
  ) %>%
  
  # --- Transformação da CLITRAT "Sem informação" em NA ---
  dplyr::mutate(
    CLITRAT = factor(
      dplyr::na_if(as.character(CLITRAT), "Sem informação")
    )
  ) %>%
  
  # --- Transformação da HISTFAMC "Sem informação" em NA ---
  dplyr::mutate(
    HISTFAMC = factor(
      dplyr::na_if(as.character(HISTFAMC), "Sem informação")
    )
  ) %>%
  
  # --- Transformação da ALCOOLIS "Sem informação" em NA ---
  dplyr::mutate(
    ALCOOLIS = factor(
      dplyr::na_if(as.character(ALCOOLIS), "Sem informação")
    )
  ) %>%
  
  # --- Transformação da TABAGISM "Sem informação" em NA ---
  dplyr::mutate(
    TABAGISM = factor(
      dplyr::na_if(as.character(TABAGISM), "Sem informação")
    )
  ) %>%
  
  # --- Transformação das anomalias de ESTRADES em NA ---
  dplyr::mutate(
    ESTADRES = dplyr::case_when(
      ESTADRES %in% c("77", "99") ~ NA_character_,
      TRUE ~ ESTADRES
    )
  ) %>% 

  # --- Criação da variável "RegiãoResidência" ---
  dplyr::mutate(
    RegiaoResidencia = dplyr::case_when(
      ESTADRES %in% c("AM", "PA", "AC", "RO", "RR", "AP", "TO") ~ "Norte",
      ESTADRES %in% c("MA", "PI", "CE", "RN", "PB", "PE", "AL", "SE", "BA") ~ "Nordeste",
      ESTADRES %in% c("MT", "MS", "GO", "DF") ~ "Centro-Oeste",
      ESTADRES %in% c("MG", "SP", "RJ", "ES") ~ "Sudeste",
      ESTADRES %in% c("PR", "SC", "RS") ~ "Sul",
      is.na(ESTADRES) ~ NA_character_,
      TRUE ~ NA_character_
    )
  ) %>% 
  
  # --- Criação da variável "RegiãoHospital" ---
  dplyr::mutate(
    RegiaoHospital = dplyr::case_when(
      UFUH %in% c("AM", "PA", "AC", "RO", "RR", "AP", "TO") ~ "Norte",
      UFUH %in% c("MA", "PI", "CE", "RN", "PB", "PE", "AL", "SE", "BA") ~ "Nordeste",
      UFUH %in% c("MT", "MS", "GO", "DF") ~ "Centro-Oeste",
      UFUH %in% c("MG", "SP", "RJ", "ES") ~ "Sudeste",
      UFUH %in% c("PR", "SC", "RS") ~ "Sul",
      TRUE ~ NA_character_
    )
  ) %>% 
  
  # --- Transformando 1899, a data "sentinela", em NA na coluna ANOPRIDI ---
  dplyr::mutate(
    ANOPRIDI = dplyr::case_when(
      ANOPRIDI == 1899 ~ NA_integer_,
      TRUE ~ ANOPRIDI
    )
  ) %>% 
  
  # --- Transformação da ORIENC "Sem informação" em NA ---
  dplyr::mutate(
    ORIENC = factor(
      dplyr::na_if(as.character(ORIENC), "Sem informação")
    )
  ) %>%
  
  # --- Transformação da EXDIAG "Sem informação" em NA ---
  dplyr::mutate(
    EXDIAG = factor(
      dplyr::na_if(as.character(EXDIAG), "Sem informação")
    )
  ) %>%
  
  # --- Transformação da ESTCONJ "Sem informação" em NA ---
  dplyr::mutate(
    ESTCONJ = factor(
      dplyr::na_if(as.character(ESTCONJ), "Sem informação")
    )
  ) %>%
  
  # --- Limpando ESTADIAM para corrigir valores sentinelas ---
  dplyr::mutate(
    ESTADIAM = dplyr::case_when(
      ESTADIAM %in% c(0, 1, 2, 3, 4) ~ ESTADIAM,
      TRUE ~ NA_real_
    ),
    ESTADIAM = as.factor(ESTADIAM)
  ) %>% 
  
  # --- Transformando 8888 e 9999 em NA na coluna ANTRI ---
  dplyr::mutate(
    ANTRI = dplyr::case_when(
      ANTRI %in% c(8888, 9999) ~ NA_integer_,
      TRUE ~ ANTRI
    )
  ) %>%
  
  # --- Transformação da BASMAIMP "Sem informação" em NA ---
  dplyr::mutate(
    BASMAIMP = factor(
      dplyr::na_if(as.character(BASMAIMP), "Sem informação")
    )
  ) %>%
  
  # --- Transformação da  LATERALI "Sem informação" em NA ---
  dplyr::mutate(
    LATERALI = factor(
      dplyr::na_if(as.character(LATERALI), "Sem informação")
    )
  ) %>%
  
  # --- Transformação da  RZNTR "Sem informação" em NA ---
  dplyr::mutate(
    RZNTR = factor(
      dplyr::na_if(as.character(RZNTR), "Sem informação")
    )
  ) %>%
  
  # --- Transformando 1899, a data "sentinela", em NA na coluna DTINITRT ---
  dplyr::mutate(
    DTINITRT = dplyr::case_when(
      DTINITRT == 1899 ~ NA_integer_,
      TRUE ~ DTINITRT
    )
  ) %>% 
  
  # --- Transformação da  PRITRATH "Sem informação" em NA ---
  dplyr::mutate(
    PRITRATH = factor(
      dplyr::na_if(as.character(PRITRATH), "Sem informação")
    )
  ) %>%
  
  # --- Transformação da  ESTDFIMT "Sem informação" em NA ---
  dplyr::mutate(
    ESTDFIMT = factor(
      dplyr::na_if(as.character(ESTDFIMT), "Sem informação")
    )
  ) %>%
  
  # --- Formatação de Datas ---
  dplyr::mutate(
    DTDIAGNO   = as_date(DTDIAGNO),
    DATAINITRT = as_date(DATAINITRT)
  ) %>%
  
  # --- Transformando datas "1899-12-30" em NA na coluna DTDIAGNO---
  dplyr::mutate(
    DTDIAGNO = dplyr::case_when(
      format(DTDIAGNO, "%Y") == "1899" ~ as.Date(NA),
      TRUE ~ DTDIAGNO
    )
  ) %>% 
  
  # --- Transformando datas menores que 1900 em "20**" ou "19**" na coluna DTDIAGNO ---
  dplyr::mutate(
    DTDIAGNO = dplyr::if_else(
      year(DTDIAGNO) < 1900, 
      make_date(
        year = if_else(
          year(DTDIAGNO) %% 100 < 25,
          2000 + year(DTDIAGNO) %% 100,
          1900 + year(DTDIAGNO) %% 100
        ),
        month = month(DTDIAGNO),
        day = day(DTDIAGNO)
      ),
      DTDIAGNO
      )
    ) %>% 
  
  # --- Transformando datas menores que 1900 em "20**" ou "19**" na coluna DTTRIAGE (não existe datas 1899) ---
  dplyr::mutate(
    DTTRIAGE = dplyr::if_else(
      year(DTTRIAGE) < 1900, 
      make_date(
        year = if_else(
          year(DTTRIAGE) %% 100 < 25,
          2000 + year(DTTRIAGE) %% 100,
          1900 + year(DTTRIAGE) %% 100
        ),
        month = month(DTTRIAGE),
        day = day(DTTRIAGE)
      ),
      DTTRIAGE
    )
  ) %>% 
  
  # --- Criação da variável distância entre municípios (residência e hospital) ---
  dplyr::left_join( # Left join com os códigos dos pacientes
    muniPontos,
    by = c("PROCEDEN" = "code_muni") 
  ) %>% 
  dplyr::rename(geomPacientes = geometry) %>% # Renomeando a coluna para que ela seja única
  
  dplyr::left_join( # Left join com os códigos dos hospitais
    muniPontos,
    by = c("MUUH" = "code_muni")
  ) %>% 
  dplyr::rename(geomHospitais = geometry) %>% # Renomeando a coluna para que ela seja única
  
  dplyr::mutate(distanciaResidenciaHospital = as.numeric(st_distance(geomPacientes, geomHospitais, by_element = TRUE))/1000) # Dividimos por 1000 para transformar em kms
  # O by_element = TRUE é necessário porque, sem ele, o código roda cada ponto do geomPacientes com cada ponto do geomHospitais, resultando nisso:
  # cannot allocate vector of size 224620.4 Gb (pesquisei e isso dá mais ou menos 220 terabytes. Sinistro.)
  
  # --- Criação da coluna classificacaoMunicipioResidencia e Hospital
  centroLocal <- readRDS("centroLocal.rds")
  dadosFinal <- dadosFinal %>% 
  dplyr::mutate(
    classificacaoMunicipioResidencia = ifelse( # If else para verificar se o código do município está no centroLocal. Se tiver, é considerado interior. Caso não, capital.
     PROCEDEN %in% centroLocal$codmun,
     "Interior",
     "Capital"
    )
  ) %>% 
  
  dplyr::mutate(
    classificacaoMunicipioHospital = ifelse( # If else para verificar se o código do município está no centroLocal. Se tiver, é considerado interior. Caso não, capital.
      MUUH %in% centroLocal$codmun,
      "Interior",
      "Capital"
    )
  ) %>% 
  
  # --- Criação da variável deslocamentoTratamento ---
  dplyr::mutate(
    deslocamentoTratamento = case_when(
      classificacaoMunicipioResidencia == "Interior" & 
        classificacaoMunicipioHospital == "Interior" ~ "Permanência no Interior",
      classificacaoMunicipioResidencia == "Capital" &
        classificacaoMunicipioHospital == "Capital" ~ "Permanência na Capital",
      classificacaoMunicipioResidencia == "Interior" &
        classificacaoMunicipioHospital == "Capital" ~ "Êxodo rural",
      classificacaoMunicipioResidencia == "Capital" &
        classificacaoMunicipioHospital == "Interior" ~ "Êxodo urbano"
    )
  ) %>% 
  
  # --- Cálculo da Lei dos 60 Dias (Lei nº 12.732/2012) ---
  # O tempo conta do diagnóstico definitivo até o primeiro tratamento
  dplyr::mutate(
    dias_ate_trat = as.numeric(difftime(DATAINITRT, DTDIAGNO, units = "days"))
  ) %>%
  
  # --- Limpeza de Consistência (Data Cleaning) ---
  # Removemos NA (sem data de tratamento) e datas negativas (erro de registro: tratamento antes do diagnóstico)
  dplyr::filter(
    !is.na(dias_ate_trat),
    dias_ate_trat >= 0
  ) %>%
  
  # --- Definição do Desfecho (Target) ---
  dplyr::mutate(
    atraso60 = if_else(dias_ate_trat > 60, "Atraso", "Pontual"),
    atraso60 = factor(atraso60, levels = c("Pontual", "Atraso"))
  )
  

# ============================================
# 4. Verificação (Sanity Check)
# ============================================
#glimpse(dadosFinal)
#count(dadosFinal, atraso60) %>% 
#  mutate(prop = n / sum(n)) # relativamente balanceados

# Removendo variáveis desnecessárias
dadosFinal$geomPacientes <- NULL
dadosFinal$geomHospitais <- NULL
dadosFinal$origem <- NULL
dadosFinal$PROCEDEN <- NULL
dadosFinal$MUUH <- NULL
dadosFinal$CNES <- NULL
dadosFinal$TPCASO <- NULL
dadosFinal$VALOR_TOT <- NULL
dadosFinal$ESTADIAG <- NULL

# Removendo datas que permitem ao modelo "prever" o futuro
dadosFinal$DTDIAGNO <- NULL
dadosFinal$DATAINITRT <- NULL
dadosFinal$DTTRIAGE <- NULL
dadosFinal$DTINITRT <- NULL
dadosFinal$dias_ate_trat <- NULL
dadosFinal$PRITRATH <- NULL
dadosFinal$ESTDFIMT <- NULL
dadosFinal$DTPRICON <- NULL
dadosFinal$DATAPRICON <- NULL
dadosFinal$RZNTR <- NULL
dadosFinal$ANTRI <- NULL
dadosFinal$CLITRAT <- NULL
dadosFinal$ANOPRIDI <- NULL
dadosFinal$DIAGANT <- NULL

# Removendo variáveis com % de NA acima de 60
dadosFinal$OUTROESTA <- NULL
dadosFinal$LOCTUPRO <- NULL
dadosFinal$DATAOBITO <- NULL
dadosFinal$HISTFAMC <- NULL

arrow::write_parquet(dadosFinal, "dadosFinal.parquet")

