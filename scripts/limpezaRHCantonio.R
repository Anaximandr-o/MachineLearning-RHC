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
dadosCancer <- readRDS("data/dados_RHC_Geral.RDS")

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
  
  # --- Transformação das anomalias de ESTADRES em NA ---
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
  
  # --- Transformando e agrupando os valores do Código Brasileiro de Ocupações ---
  dplyr::mutate(
    OcupacaoAgregado = dplyr::case_when(
      OCUPACAO %in% c("888", "88888") ~ "Estudantes e crianças",
      OCUPACAO %in% c("540", "520", "552") ~ "Serviços domésticos e limpeza",
      OCUPACAO %in% c("639", "621", "664", "669", "663", "612", "020", "671", "611", "649", "600") ~ "Agricultura, agropecuária, florestal e aquicultura",
      OCUPACAO %in% c("985", "989", "360") ~ "Transportes",
      OCUPACAO %in% c("410", "451", "432", "490", "441", "421", "243", "452") ~ "Comércio e vendas",
      OCUPACAO %in% c("149", "142", "141", "139", "131", "143", "144") ~ "Professores",
      OCUPACAO %in% c("795", "791", "811", "560", "799", "833", "754", "801") ~ "Trabalhadores manuais e artesanais (alfaiates, marceneiros)",
      OCUPACAO %in% c("531", "776", "532", "779", "774") ~ "Alimentação e serviços",
      OCUPACAO %in% c("214") ~ "Funcionários públicos",
      OCUPACAO %in% c("570") ~ "Estética e beleza",
      OCUPACAO %in% c("991", "971") ~ "Trabalhadores braçais",
      OCUPACAO %in% c("072", "071", "572", "061", "073", "074", "067", "063", "079") ~ "Saúde",
      OCUPACAO %in% c("959", "951", "954", "855", "969", "859", "872", "021", "034", "701", "974", "029", "871") ~ "Construção civil",
      OCUPACAO %in% c("843", "845") ~ "Mecânica",
      OCUPACAO %in% c("311", "393", "092", "093", "331", "321", "319", "339", "399", "241", "030", "091") ~ "Administrativo, contabilidade, economia e escritórios",
      OCUPACAO %in% c("583", "589", "582", "581") ~ "Policiais, guardas, bombeiros e serviços de proteção",
      OCUPACAO %in% c("121") ~ "Direito",
      OCUPACAO %in% c("332", "394", "380") ~ "Atendimento ao público",
      OCUPACAO %in% c("174", "329") ~ "Empresários e diretores de empresa",
      OCUPACAO %in% c("729", "832", "835", "939", "931", "179") ~ "Indústria e metalurgia",
      OCUPACAO %in% c("039") ~ "Técnicos",
      OCUPACAO %in% c("X70", "X50", "X60", "X40", "X", "X80") ~ "Não classificados",
      OCUPACAO %in% c("599") ~ "Turismo",
      OCUPACAO %in% c("152", "163") ~ "Jornalistas, fotógrafos e operadores de câmeras",
      TRUE ~ "Outros" # Ocupações com <1000 pacientes
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
  centroLocal <- readRDS("data/centroLocal.rds")
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
  # --- Agrupando categorias de escolaridade ---
  dplyr::mutate(
    INSTRUC = case_when(
      INSTRUC %in% c("Fundamental incompleto", "Fundamental completo") ~ "Fundamental",
      INSTRUC %in% c("Superior incompleto", "Superior completo") ~ "Superior",
      TRUE ~ INSTRUC
    )
  ) %>%
  
  # --- Transformando behavior_num em uma variável legível ---
  # behavior_num foi uma variável gerada a partir da variável TIPOHIST, extraindo o número após a barra. 
  # Esse código transforma a variável em factor e dá nomes aos números, que representam o comportamento do tumor.
  dplyr::mutate( # Aproveitei para mudar o nome da variável e deixar mais intuitivo
    comportamentoTumor = factor(behavior_num, levels = c(0, 1, 2, 3, 6, 9),
      labels = c(
        "Benigno",
        "Borderline",
        "In situ",
        "Maligno primário",
        "Metastático",
        "Origem incerta")
      )
    )%>%
    select(-behavior_num)%>% # Removendo a variável com o nome antigo
    
    # Remove da base de dados os casos que não se encaixam como câncer
    dplyr::filter(!comportamentoTumor %in% c("Benigno", "Borderline", "In situ")) %>%
    droplevels() |> 
  
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
  
# Transformando numéricas em categóricas
  dadosFinal <- dadosFinal %>%
    mutate(
      TNM = as.character(TNM),
      PTNM = as.character(PTNM)
    )

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
dadosFinal$TIPOHIST_behav <- NULL
dadosFinal$TIPOHIST_chr <- NULL
dadosFinal$TIPOHIST_morph4 <- NULL
dadosFinal$icdo3_morph4 <- NULL
dadosFinal$behavior_label <- NULL
dadosFinal$icdo3_description <- NULL
dadosFinal$icdo3_site_field <- NULL
dadosFinal$icd9cm <- NULL
dadosFinal$icd10 <- NULL
dadosFinal$icd10cm <- NULL

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

arrow::write_parquet(dadosFinal, "outputs/dadosFinal.parquet")

# Dividindo o banco entre as 5 regiões do país
# Importante notar que esse processo retira NAs e também não contabiliza pacientes do exterior

dadosNorte <- dadosFinal |> 
  dplyr::filter(RegiaoResidencia == "Norte")
arrow::write_parquet(dadosNorte, "outputs/dadosNorte.parquet")

dadosNordeste <- dadosFinal |> 
  dplyr::filter(RegiaoResidencia == "Nordeste")
arrow::write_parquet(dadosNordeste, "outputs/dadosNordeste.parquet")

dadosCentroOeste <- dadosFinal |> 
  dplyr::filter(RegiaoResidencia == "Centro-Oeste")
arrow::write_parquet(dadosCentroOeste, "outputs/dadosCentroOeste.parquet")

dadosSudeste <- dadosFinal |> 
  dplyr::filter(RegiaoResidencia == "Sudeste")
arrow::write_parquet(dadosSudeste, "outputs/dadosSudeste.parquet")

dadosSul <- dadosFinal |> 
  dplyr::filter(RegiaoResidencia == "Sul")
arrow::write_parquet(dadosSul, "outputs/dadosSul.parquet")

