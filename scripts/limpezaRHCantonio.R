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

# Testar o banco final
teste <- arrow::read_parquet("../outputs/dadosFinal.parquet")

# ============================================
# 2. Leitura dos Dados
# ============================================
dadosCancer <- readRDS("../data/dados_RHC_Geral.RDS")

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
  centroLocal <- readRDS("../data/centroLocal.rds")
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
    
  # --- Tratando LOCTUDET e melhorando a legibilidade
    dplyr::mutate(
      LOCTUDET = case_when(
        LOCTUDET %in% c("C","C99") ~ NA_character_,
        LOCTUDET %in% ("C39") ~ "Outras localizações do aparelho respiratório",
        LOCTUDET %in% ("C43") ~ "Pele",
        LOCTUDET %in% ("C45") ~ "Mesotelioma",
        LOCTUDET %in% ("C46") ~ "Sarcoma de Kaposi",
        LOCTUDET %in% ("C78") ~ "Secundária dos órgãos respiratórios e digestivos",
        LOCTUDET %in% ("C79") ~ "Secundária de outras localizações",
        LOCTUDET %in% ("C8") ~ "Outras glândulas salivares",
        LOCTUDET %in% ("C81") ~ "Doença de Hodgkin",
        LOCTUDET %in% ("C82") ~ "Linfoma não-Hodgkin, folicular (nodular)",
        LOCTUDET %in% ("C83") ~ "Linfoma não-Hodgkin difuso",
        LOCTUDET %in% ("C84") ~ "Linfomas de células T cutâneas e periféricas",
        LOCTUDET %in% ("C85") ~ "Linfoma não-Hodgkin de outros tipos",
        LOCTUDET %in% ("C88") ~ "Doenças imunoproliferativas",
        LOCTUDET %in% ("C90") ~ "Mieloma múltiplo",
        LOCTUDET %in% ("C91") ~ "Leucemia linfóide",
        LOCTUDET %in% ("C92") ~ "Leucemia mielóide",
        LOCTUDET %in% ("C93") ~ "Leucemia monocítica",
        LOCTUDET %in% ("C96") ~ "Outras dos tecidos linfáticos, hematopoéticos e correlatos",
        LOCTUDET %in% ("C97") ~ "Localizações múltiplas independentes",
        LOCTUDET %in% ("D46") ~ "Síndromes mielodisplásicas (NÃO ONCOLÓGICO)",
        LOCTUDET %in% ("E05") ~ "Hipertireoidismo (NÃO ONCOLÓGICO)",
        LOCTUDET %in% ("N62") ~ "Hipertrofia da mama (NÃO ONCOLÓGICO)",
        TRUE ~ LOCTUDET
      )
    ) %>%
    
    # --- Tratando LOCTUPRI e melhorando a legibilidade
    dplyr::mutate(
      LOCTUPRI = case_when(
        LOCTUPRI %in% c("C  .","C  .0","C00","C00.","C99.9") ~ NA_character_,
        grepl("^C00", LOCTUPRI) ~ "Lábio",
        grepl("^C01", LOCTUPRI) ~ "Língua",
        grepl("^C02", LOCTUPRI) ~ "Língua",
        grepl("^C03", LOCTUPRI) ~ "Gengiva",
        grepl("^C04", LOCTUPRI) ~ "Boca",
        grepl("^C05", LOCTUPRI) ~ "Palato",
        grepl("^C06", LOCTUPRI) ~ "Outras partes da boca",
        grepl("^C07", LOCTUPRI) ~ "Glândula Parótida",
        grepl("^C08", LOCTUPRI) ~ "Glândula submandibular, sublingual e salivar",
        grepl("^C09", LOCTUPRI) ~ "Amígdala",
        grepl("^C10", LOCTUPRI) ~ "Orofaringe, valécula e epiglote",
        grepl("^C11", LOCTUPRI) ~ "Nasofaringe",
        grepl("^C12", LOCTUPRI) ~ "Seio piriforme",
        grepl("^C13", LOCTUPRI) ~ "Hipofaringe",
        grepl("^C14", LOCTUPRI) ~ "Faringe",
        grepl("^C15", LOCTUPRI) ~ "Esôfago",      
        grepl("^C16", LOCTUPRI) ~ "Estômago",      
        grepl("^C17", LOCTUPRI) ~ "Intestino delgado",      
        grepl("^C18", LOCTUPRI) ~ "Cólon",      
        grepl("^C19", LOCTUPRI) ~ "Junção retossigmóide",      
        grepl("^C20", LOCTUPRI) ~ "Reto",      
        grepl("^C21", LOCTUPRI) ~ "Ânus e canal anal",      
        grepl("^C22", LOCTUPRI) ~ "Fígado e vias biliares intra-hepáticas"  ,   
        grepl("^C23", LOCTUPRI) ~ "Vesícula biliar"  ,   
        grepl("^C24", LOCTUPRI) ~ "Outras partes das vias biliares"  ,   
        grepl("^C25", LOCTUPRI) ~ "Pâncreas"  ,   
        grepl("^C26", LOCTUPRI) ~ "Outros órgãos digestivos"  ,   
        grepl("^C30", LOCTUPRI) ~ "Cavidade nasal"  ,   
        grepl("^C31", LOCTUPRI) ~ "Seios da face"  ,   
        grepl("^C32", LOCTUPRI) ~ "Laringe"  ,   
        grepl("^C33", LOCTUPRI) ~ "Traquéia"  ,   
        grepl("^C34", LOCTUPRI) ~ "Brônquios e pulmões"  ,
        grepl("^C37", LOCTUPRI) ~ "Timo"  ,   
        grepl("^C38", LOCTUPRI) ~ "Coração, mediastino e pleura"  ,   
        grepl("^C39", LOCTUPRI) ~ "Outras localizações do aparelho respiratório e dos órgãos intratorácicos"  ,   
        grepl("^C40", LOCTUPRI) ~ "Ossos e cartilagens articulares dos membros"  ,   
        grepl("^C41", LOCTUPRI) ~ "Ossos e cartilagens articulares de outras localizações"  ,   
        grepl("^C42", LOCTUPRI) ~ NA_character_  ,   
        grepl("^C43", LOCTUPRI) ~ "Pele"  ,   
        grepl("^C44", LOCTUPRI) ~ "Outras da pele"  ,   
        grepl("^C45", LOCTUPRI) ~ "Mesotelioma"  ,   
        grepl("^C46", LOCTUPRI) ~ "Sarcoma de Kaposi"  ,   
        grepl("^C47", LOCTUPRI) ~ "Nervos periféricos e sistema nervoso autônomo"  ,   
        grepl("^C48", LOCTUPRI) ~ "Tecidos moles do retroperitônio e do peritônio"  ,   
        grepl("^C49", LOCTUPRI) ~ "Tecido conjuntivo e outros tecidos moles"  ,   
        grepl("^C50", LOCTUPRI) ~ "Mama"  ,   
        grepl("^C51", LOCTUPRI) ~ "Vulva"  ,   
        grepl("^C52", LOCTUPRI) ~ "Vagina"  ,   
        grepl("^C53", LOCTUPRI) ~ "Colo do útero"  ,   
        grepl("^C54", LOCTUPRI) ~ "Corpo do útero"  ,   
        grepl("^C55", LOCTUPRI) ~ "Útero, porção não especificada"  ,   
        grepl("^C56", LOCTUPRI) ~ "Ovário"  ,   
        grepl("^C57", LOCTUPRI) ~ "Outros órgãos genitais femininos"  ,   
        grepl("^C58", LOCTUPRI) ~ "Placenta"  ,   
        grepl("^C60", LOCTUPRI) ~ "Pênis"  ,   
        grepl("^C61", LOCTUPRI) ~ "Próstata"  ,   
        grepl("^C62", LOCTUPRI) ~ "Testículos"  ,   
        grepl("^C63", LOCTUPRI) ~ "Outros órgãos genitais masculinos"  ,   
        grepl("^C64", LOCTUPRI) ~ "Rim, exceto pelve renal"  ,   
        grepl("^C65", LOCTUPRI) ~ "Pelve renal"  ,   
        grepl("^C66", LOCTUPRI) ~ "Ureteres"  ,   
        grepl("^C67", LOCTUPRI) ~ "Bexiga"  ,   
        grepl("^C68", LOCTUPRI) ~ "Outros órgãos urinários"  ,   
        grepl("^C69", LOCTUPRI) ~ "Olhos e anexos"  ,   
        grepl("^C70", LOCTUPRI) ~ "Meninges"  ,   
        grepl("^C71", LOCTUPRI) ~ "Encéfalo"  ,   
        grepl("^C72", LOCTUPRI) ~ "Medula espinhal, nervos cranianos e outras partes do sistema nervoso central"  ,   
        grepl("^C73", LOCTUPRI) ~ "Glândula tireóide"  ,   
        grepl("^C74", LOCTUPRI) ~ "Glândula supra-renal"  ,   
        grepl("^C75", LOCTUPRI) ~ "Outras glândulas endócrinas"  ,   
        grepl("^C76", LOCTUPRI) ~ "Outras localizações e localizações mal definidas"  ,   
        grepl("^C77", LOCTUPRI) ~ "Secundária e não especificada dos gânglios linfáticos"  ,   
        grepl("^C78", LOCTUPRI) ~ "Secundária dos órgãos respiratórios e digestivos"  ,   
        grepl("^C79", LOCTUPRI) ~ "Secundária de outras localizações"  ,   
        grepl("^C80", LOCTUPRI) ~ "Sem especificação de localização"  ,   
        grepl("^C81", LOCTUPRI) ~ "Doença de Hodgkin"  ,   
        grepl("^C82", LOCTUPRI) ~ "Linfoma não-Hodgkin, folicular (nodular)"  ,   
        grepl("^C83", LOCTUPRI) ~ "Linfoma não-Hodgkin difuso"  ,   
        grepl("^C84", LOCTUPRI) ~ "Linfomas de células T cutâneas e periféricas"  ,   
        grepl("^C85", LOCTUPRI) ~ "Linfoma não-Hodgkin de outros tipos e de tipo não especificado",  
        grepl("^C88", LOCTUPRI) ~ "Doenças imunoproliferativas malignas"  ,   
        grepl("^C90", LOCTUPRI) ~ "Mieloma múltiplo e neoplasias malignas de plasmócitos"  ,   
        grepl("^C91", LOCTUPRI) ~ "Leucemia linfóide"  ,   
        grepl("^C92", LOCTUPRI) ~ "Leucemia mielóide"  ,   
        grepl("^C93", LOCTUPRI) ~ "Leucemia monocítica"  ,   
        grepl("^C96", LOCTUPRI) ~ "Outras e não especificadas dos tecidos linfático, hematopoético e tecidos correlatos"  ,   
        grepl("^C97", LOCTUPRI) ~ "Localizações múltiplas independentes (primárias)"  ,   
        TRUE ~ LOCTUPRI
      )
    ) %>%
    
    # --- Tratando TIPOHIST e melhorando a legibilidade
    dplyr::mutate(
      TIPOHIST = case_when(
        TIPOHIST %in% c("/","/3)","0000/0","1   /","1401/3","5220/3","8000//","8006/3","8010/","8014/0","8040/3","8070/","8070/9","8071/7","8077/3","8100/3","8140/9","8146/3",
                        "8163/3","8170/5","8211/1","8500//","8503/","8720/","9999/9","99999","C090/3","8260/6","8261/1","8390/1","8440/5","8442/3",
                        "8451/3","8462/3","8472/3","8473/3","8491/3","8500/0","8500/5","8500/6","8500/9","8511/3","8551/0",
                        "8590/3","8636/3","8640/0","8687/3","8712/3","8732/3","8832/","8825/3","8870/3","8888/0","8888/8",
                        "89363","9031/3","9161/3","9190/3","9291/3","9350/3","9395/3","9400/8","9421/3","9422/3","9425/3",
                        "9536/0","9551/3","9592/3","9593/3","9594/3","9595/3","9600/3","9620/3","9657/3","9658/3","9660/3",
                        "9666/3","9669/3","9672/3","9674/3","9676/3","9681/3","9682/3","9683/3","9685/3","9686/3",
                        "9692/3","9693/3","9694/3","9696/1","9696/3","9697/3","9703/3","9704/3","9706/3","9707/3",
                        "9710/3","9711/3","9713/3","9715/3","9717//","9720/3","9722/3","9723/3","9731/2","9732/1",
                        "9733/2","9735/3","9751/3","9803/3","9804/3","9821/3","9822/3","9824/3","9825/3","9830/3",
                        "9831/3","9850/3","9862/3","9864/3","9868/3","9880/3","9890/3","9950/","9893/3","9899/3",
                        "9900/3","9932/3","9941/3","9950/1","9960/1","9961/1","9962/1","9980/1","9981/1","9982/1",
                        "9983/1","9984/1","9989/1","9990/1","9990/3","9990/9","9991/6","9991/7","9992/0","9992/2","9992/6") ~ NA_character_,
        TIPOHIST %in% ("7400/8") ~ "Carcinoma in situ (NÃO INVASIVO)",
        # 800 NEOPLASIAS, SOE
        TIPOHIST %in% ("8000/3") ~ "Neoplasia maligna",
        TIPOHIST %in% ("8000/6") ~ "Neoplasia metástica",
        TIPOHIST %in% ("8000/9") ~ "Neoplasia maligna, incerta se primária ou metastática",
        TIPOHIST %in% ("8001/3") ~ "Células tumorais malignas",
        TIPOHIST %in% ("8002/3") ~ "Tumor maligno de células pequenas",
        TIPOHIST %in% ("8003/3") ~ "Tumor maligno de células gigantes",
        TIPOHIST %in% ("8004/3") ~ "Tumor maligno de células fusiformes",
        TIPOHIST %in% ("8005/3") ~ "Tumor maligno de células claras",
        # 801-804 NEOPLASIAS EPITELIAIS, SOE
        TIPOHIST %in% ("8010/3") ~ "Carcinoma, SOE",
        TIPOHIST %in% ("8010/6") ~ "Carcinoma metastático, SOE",
        TIPOHIST %in% ("8010/9") ~ "Carcinomatose",
        TIPOHIST %in% ("8011/3") ~ "Epitelioma maligno",
        TIPOHIST %in% ("8012/3") ~ "Carcinomas de células grandes, SOE",
        TIPOHIST %in% ("8013/3") ~ "Carcinoma neuroendócrino de grandes células",
        TIPOHIST %in% ("8014/3") ~ "Carcinoma de grandes células, rabdóide",
        TIPOHIST %in% ("8015/3") ~ "Carcinoma de células “vitreas”",
        TIPOHIST %in% ("8020/3") ~ "Carcinoma indiferenciado, SOE",
        TIPOHIST %in% ("8021/3") ~ "Carcinoma anaplásico, SOE",
        TIPOHIST %in% ("8022/3") ~ "Carcinoma pleomórfico",
        TIPOHIST %in% ("8030/3") ~ "Carcinoma de células gigantes e fusocelular",
        TIPOHIST %in% ("8031/3") ~ "Carcinoma de células gigantes",
        TIPOHIST %in% ("8032/3") ~ "Carcinoma fusocelular, SOE",
        TIPOHIST %in% ("8033/3") ~ "Carcinona pseudossarcomatoso",
        TIPOHIST %in% ("8034/3") ~ "Carcinoma de células poligonais",
        TIPOHIST %in% ("8035/3") ~ "Carcinoma de células gigantes semelhantes a osteoclasto",
        TIPOHIST %in% ("8041/3") ~ "Carcinoma de células pequenas, SOE",
        TIPOHIST %in% ("8042/3") ~ "Carcinoma “oat cell” (C34.-)",
        TIPOHIST %in% ("8043/3") ~ "Carcinoma de células pequenas, fusiformes (C34.-)",
        TIPOHIST %in% ("8044/3") ~ "Carcinoma de células pequenas, intermediárias (C34.-)",
        TIPOHIST %in% ("8045/3") ~ "Carcinoma de células pequenas e de células grandes (C34.-)",
        TIPOHIST %in% ("8046/3") ~ "Carcinoma de células não pequenas (C34.-)",
        # 805-808 NEOPLASIAS DE CÉLULAS ESCAMOSAS
        TIPOHIST %in% ("8050/3") ~ "Carcinoma papilar, SOE",
        TIPOHIST %in% ("8051/3") ~ "Carcinoma verrucoso, SOE",
        TIPOHIST %in% ("8052/3") ~ "Carcinoma escamocelular papilar",
        TIPOHIST %in% ("8070/3") ~ "Carcinoma escamocelular, SOE",
        TIPOHIST %in% ("8070/6") ~ "Carcinoma escamoso metastático, SOE",
        TIPOHIST %in% ("8071/3") ~ "Carcinoma escamoso ceratinizado, SOE",
        TIPOHIST %in% ("8072/3") ~ "Carcinoma escamoso de células grandes, não ceratinizado, SOE",
        TIPOHIST %in% ("8073/3") ~ "Carcinoma escamoso de células pequenas, não ceratinizado",
        TIPOHIST %in% ("8074/3") ~ "Carcinoma escamoso de células fusiformes",
        TIPOHIST %in% ("8075/3") ~ "Carcinoma escamoso adenóide",
        TIPOHIST %in% ("8076/3") ~ "Carcinoma escamoso, micro-invasor",
        TIPOHIST %in% ("8078/3") ~ "Carcinoma escamoso com formação córnea",
        TIPOHIST %in% ("8082/3") ~ "Carcinoma linfoepitelial",
        TIPOHIST %in% ("8083/3") ~ "Carcinoma escamoso, basalóide",
        TIPOHIST %in% ("8084/3") ~ "Carcinoma escamoso tipo células claras",
        # 809-811 NEOPLASIAS BASOCELULARES
        TIPOHIST %in% ("8090/3") ~ "Carcinoma basocelular, SOE (C44.-)",
        TIPOHIST %in% ("8091/3") ~ "Carcinoma basocelular, superficial multifocal (C44.-)",
        TIPOHIST %in% ("8092/3") ~ "Carcinoma basocelular infiltrativo, SOE (C44.-)",
        TIPOHIST %in% ("8093/3") ~ "Carcinoma basocelular, fibroepitelial (C44.-)",
        TIPOHIST %in% ("8094/3") ~ "Carcinoma basoescamoso (C44.-)",
        TIPOHIST %in% ("8095/3") ~ "Carcinoma metatípico (C44.-)",
        TIPOHIST %in% ("8097/3") ~ "Carcinoma basocelular nodular (C44.-)",
        TIPOHIST %in% ("8098/3") ~ "Carcinoma basal adenóide (C53.-)",
        TIPOHIST %in% ("8102/3") ~ "Tricolemocarcinoma (C44.-)",
        TIPOHIST %in% ("8110/3") ~ "Carcinoma pilomatricial maligno (C44.-)",
        # 812-813 PAPILOMAS E CARCINOMAS DE CÉLULAS TRANSICIONAIS
        TIPOHIST %in% ("8120/3") ~ "Carcinoma de células transicionais, SOE",
        TIPOHIST %in% ("8121/3") ~ "Carcinoma Schneideriano (C30.0, C31.-)",
        TIPOHIST %in% ("8122/3") ~ "Carcinoma de células transicionais fusiformes",
        TIPOHIST %in% ("8123/3") ~ "Carcinoma basalóide",
        TIPOHIST %in% ("8124/3") ~ "Carcinoma cloacogênico (C21.2)",
        TIPOHIST %in% ("8130/3") ~ "Carcinoma papilar de células transicionais (C67.-)",
        TIPOHIST %in% ("8131/3") ~ "Carcinoma de células de transição, micropapilar (C67.-)",
        # 814-838 ADENOMAS E ADENOCARCINOMAS
        TIPOHIST %in% ("8140/3") ~ "Adenocarcinoma, SOE",
        TIPOHIST %in% ("8140/6") ~ "Adenocarcinoma metastático, SOE",
        TIPOHIST %in% ("8141/3") ~ "Adenocarcinoma esquirroso",
        TIPOHIST %in% ("8142/3") ~ "Linite plástica (C16.-)",
        TIPOHIST %in% ("8143/3") ~ "Adenocarcinoma de propagação superficial",
        TIPOHIST %in% ("8144/3") ~ "Adenocarcinoma, tipo intestinal (C16.-)",
        TIPOHIST %in% ("8145/3") ~ "Carcinoma, tipo difuso (C16.-)",
        TIPOHIST %in% ("8147/3") ~ "Adenocarcinoma basocelular",
        TIPOHIST %in% ("8150/3") ~ "Carcinoma de células das ilhotas (C25.-)",
        TIPOHIST %in% ("8151/3") ~ "Insulinoma maligno (C25.-)",
        TIPOHIST %in% ("8152/3") ~ "Glucagonoma maligno (C25.-)",
        TIPOHIST %in% ("8153/3") ~ "Gastrinoma maligno",
        TIPOHIST %in% ("8154/3") ~ "Adenocarcinoma exócrino misto, de células das ilhotas (C25.-)",
        TIPOHIST %in% ("8155/3") ~ "Vipoma maligno",
        TIPOHIST %in% ("8156/3") ~ "Somatostatinoma maligno",
        TIPOHIST %in% ("8160/3") ~ "Colangiocarcinoma (C22.1 C24.0)",
        TIPOHIST %in% ("8161/3") ~ "Cistadenocarcinoma de ductos biliares (C22.1, C24.0)",
        TIPOHIST %in% ("8162/3") ~ "Tumor de Klatskin (C22.1, C24.0)",
        TIPOHIST %in% ("8170/3") ~ "Carcinoma hepatocelular, SOE (C22.0)",
        TIPOHIST %in% ("8171/3") ~ "Carcinoma hepatocelular fibrolamelar (C22.0)",
        TIPOHIST %in% ("8172/3") ~ "Carcinoma hepatocelular esquirroso (C22.0)",
        TIPOHIST %in% ("8173/3") ~ "Carcinoma hepatocelular tipo fusocelular (C22.0)",
        TIPOHIST %in% ("8174/3") ~ "Carcinoma hepatocelular de células claras (C22.0)",
        TIPOHIST %in% ("8175/3") ~ "Carcinoma hepatocelular pleomórfico (C22.0)",
        TIPOHIST %in% ("8180/3") ~ "Carcinoma hepatocelular e colangiocarcinoma combinados (C22.0)",
        TIPOHIST %in% ("8190/3") ~ "Adenocarcinoma trabecular",
        TIPOHIST %in% ("8200/3") ~ "Carcinoma adenóide cístico",
        TIPOHIST %in% ("8201/3") ~ "Carcinoma cribriforme, SOE",
        TIPOHIST %in% ("8210/3") ~ "Adenocarcinoma em pólipo adenomatoso",
        TIPOHIST %in% ("8211/3") ~ "Adenocarcinoma tubular",
        TIPOHIST %in% ("8214/3") ~ "Carcinoma de células parietais (C16.-)",
        TIPOHIST %in% ("8215/3") ~ "Adenocarcinoma de glândulas da região anal (C21.1)",
        TIPOHIST %in% ("8220/3") ~ "Adenocarcinoma em polipose adenomatosa do cólon (C18.-)",
        TIPOHIST %in% ("8221/3") ~ "Adenocarcinoma em pólipos adenomatosos múltiplos",
        TIPOHIST %in% ("8230/3") ~ "Carcinoma sólido, SOE",
        TIPOHIST %in% ("8231/3") ~ "Carcinoma simples",
        TIPOHIST %in% ("8240/3") ~ "Tumor carcinóide SOE",
        TIPOHIST %in% ("8241/3") ~ "Carcinóide de células enterocromafínicas",
        TIPOHIST %in% ("8242/3") ~ "Tumor maligno de células semelhantes a enterocromafínicas",
        TIPOHIST %in% ("8243/3") ~ "Carcinóide de células caliciformes",
        TIPOHIST %in% ("8244/3") ~ "Carcinóide composto",
        TIPOHIST %in% ("8245/3") ~ "Tumor adenocarcinóide",
        TIPOHIST %in% ("8246/3") ~ "Carcinoma neuroendócrino, SOE",
        TIPOHIST %in% ("8247/3") ~ "Carcinoma de células de Merkel (C44.-)",
        TIPOHIST %in% ("8249/3") ~ "Tumor carcinóide atípico",
        TIPOHIST %in% ("8250/3") ~ "Adenocarcinoma bronquíolo-alveolar, SOE (C34.-)",
        TIPOHIST %in% ("8251/3") ~ "Adenocarcinoma alveolar (C34.-)",
        TIPOHIST %in% ("8252/3") ~ "Carcinoma bronquíolo alveolar não mucinoso (C34.-)",
        TIPOHIST %in% ("8253/3") ~ "Carcinoma bronquíolo alveolar mucinoso (C34.-)",
        TIPOHIST %in% ("8254/3") ~ "Carcinoma bronquíolo alveolar misto, mucinoso e não mucinoso (C34.-)",      
        TIPOHIST %in% ("8255/3") ~ "Adenocarcinoma com subtipos mistos",
        TIPOHIST %in% ("8260/3") ~ "Adenocarcinoma papilar, SOE",
        TIPOHIST %in% ("8261/3") ~ "Adenocarcinoma em adenoma viloso",
        TIPOHIST %in% ("8262/3") ~ "Adenocarcinoma viloso",
        TIPOHIST %in% ("8263/3") ~ "Adenocarcinoma em adenoma tubuloviloso",
        TIPOHIST %in% ("8270/3") ~ "Carcinoma cromófobo (C75.1)",
        TIPOHIST %in% ("8272/3") ~ "Carcinoma da glândula pituitária, SOE (C75.1)",
        TIPOHIST %in% ("8280/3") ~ "Carcinoma acidófilo (C75.1)",
        TIPOHIST %in% ("8281/3") ~ "Carcinoma misto acidófilo-basófilo (C75.1)",
        TIPOHIST %in% ("8290/3") ~ "Adenocarcinoma oxifílico",
        TIPOHIST %in% ("8300/3") ~ "Carcinoma basófilo (C75.1)",
        TIPOHIST %in% ("8310/3") ~ "Adenocarcinoma de células claras, SOE",
        TIPOHIST %in% ("8312/3") ~ "Carcinoma de células renais, SOE (C64.9)",
        TIPOHIST %in% ("8313/3") ~ "Adenocarcinofibroma de células claras (C56.9)",
        TIPOHIST %in% ("8314/3") ~ "Carcinoma rico em lípides (C50.-)",
        TIPOHIST %in% ("8315/3") ~ "Carcinoma rico em glicogênio (C50.-)",
        TIPOHIST %in% ("8316/3") ~ "Carcinoma de células renais, associado a cisto (C64.9)",
        TIPOHIST %in% ("8317/3") ~ "Carcinoma de células renais, tipo cromófobo (C64.9)",
        TIPOHIST %in% ("8318/3") ~ "Carcinoma de células renais sarcomatóide (C64.9)",
        TIPOHIST %in% ("8319/3") ~ "Carcinoma de ductos coletores (C64.9)",
        TIPOHIST %in% ("8320/3") ~ "Carcinoma de células granulares",
        TIPOHIST %in% ("8322/3") ~ "Adenocarcinoma de células claras e “água de rocha” (C75.0)",
        TIPOHIST %in% ("8323/3") ~ "Adenocarcinoma de células mistas",
        TIPOHIST %in% ("8330/3") ~ "Adenocarcinoma folicular, SOE (C73.9)",
        TIPOHIST %in% ("8331/3") ~ "Adenocarcinoma folicular bem diferenciado (C73.9)",
        TIPOHIST %in% ("8332/3") ~ "Adenocarcinoma folicular, trabecular (C73.9)",
        TIPOHIST %in% ("8333/3") ~ "Adenocarcinoma fetal",
        TIPOHIST %in% ("8335/3") ~ "Carcinoma folicular pouco invasivo (C73.9)",
        TIPOHIST %in% ("8337/3") ~ "Carcinoma insular (C73.9)",
        TIPOHIST %in% ("8340/3") ~ "Carcinoma papilar, variante folicular (C73.9)",
        TIPOHIST %in% ("8341/3") ~ "Microcarcinoma papilar (C73.9)",
        TIPOHIST %in% ("8342/3") ~ "Carcinoma papilar, células oxifilicas (C73.9)",
        TIPOHIST %in% ("8343/3") ~ "Carcinoma papilar, encapsulado (C73.9)",
        TIPOHIST %in% ("8344/3") ~ "Carcinoma papilar, células colunares (C73.9)",
        TIPOHIST %in% ("8345/3") ~ "Carcinoma medular com estroma amilóide (C73.9)",
        TIPOHIST %in% ("8346/3") ~ "Carcinoma misto medular folicular (C73.9)",
        TIPOHIST %in% ("8347/3") ~ "Carcinoma misto medular-papilar (C73.9)",
        TIPOHIST %in% ("8350/3") ~ "Carcinoma esclerosante não-encapsulado (C73.9)",
        TIPOHIST %in% ("8370/3") ~ "Carcinoma do córtex supra-renal (C74.0)",
        TIPOHIST %in% ("8380/3") ~ "Adenocarcinoma endometrióide, SOE",
        TIPOHIST %in% ("8381/3") ~ "Adenofibroma endometrióide maligno",
        TIPOHIST %in% ("8382/3") ~ "Adenocarcinoma endometrióide tipo secretor",
        TIPOHIST %in% ("8383/3") ~ "Adenocarcinoma endometrióide de células ciliadas",
        TIPOHIST %in% ("8384/3") ~ "Adenocarcinoma tipo endocervical",
        # 839-842 NEOPLASIAS DOS ANEXOS E APÊNDICES CUTÂNEOS
        TIPOHIST %in% ("8390/3") ~ "Carcinoma de apêndice cutâneo (C44.-)",
        TIPOHIST %in% ("8400/3") ~ "Adenocarcinoma de glândula sudorípara (C44.-)",
        TIPOHIST %in% ("8401/3") ~ "Adenocarcinoma apócrino",
        TIPOHIST %in% ("8402/3") ~ "Hidradenoma nodular maligno (C44.-)",
        TIPOHIST %in% ("8403/3") ~ "Espiradenoma écrino maligno (C44.-)",
        TIPOHIST %in% ("8407/3") ~ "Carcinoma ductal de glândula sudorípara esclerosante (C44.-)",
        TIPOHIST %in% ("8408/3") ~ "Adenocarcinoma papilar écrino (C44.-)",
        TIPOHIST %in% ("8409/3") ~ "Poroma écrino maligno",
        TIPOHIST %in% ("8410/3") ~ "Adenocarcinoma sebáceo (C44.-)",
        TIPOHIST %in% ("8413/3") ~ "Adenocarcinoma écrino (C44.-)",
        TIPOHIST %in% ("8420/3") ~ "Adenocarcinoma ceruminoso (C44.2)",
        # 843 NEOPLASIAS MUCOEPIDERMÓIDES
        TIPOHIST %in% ("8430/3") ~ "Carcinoma mucoepidermóide",
        # 844-849 NEOPLASIAS CÍSTICAS, MUCINOSAS E SEROSAS
        TIPOHIST %in% ("8440/3") ~ "Cistadenocarcinoma, SOE",
        TIPOHIST %in% ("8441/3") ~ "Cistadenocarcinoma seroso, SOE (C56.9)",
        TIPOHIST %in% ("8450/3") ~ "Cistadenocarcinoma papilar, SOE (C56.9)",
        TIPOHIST %in% ("8452/3") ~ "Carcinoma pseudopapilar sólido (C25.-)",
        TIPOHIST %in% ("8453/1") ~ "Tumor papilar intra-ductal mucinoso com displasia moderada (C25.-)",
        TIPOHIST %in% ("8453/3") ~ "Carcinoma papilar intra-ductal mucinoso, invasivo (C25.-)",
        TIPOHIST %in% ("8460/0") ~ "Cistadenoma papilar seroso, SOE (C56.9)",
        TIPOHIST %in% ("8460/3") ~ "Cistadenocarcinoma papilar seroso (C56.9)",
        TIPOHIST %in% ("8461/3") ~ "Carcinoma papilar de superfície serosa (C56.9)",
        TIPOHIST %in% ("8462/1") ~ "Tumor cístico papilar seroso de malignidade limítrofe (C56.9)",
        TIPOHIST %in% ("8470/3") ~ "Cistadenocarcinoma mucinoso, SOE (C56.9)",
        TIPOHIST %in% ("8471/3") ~ "Cistadenocarcinoma mucinoso papilar (C56.9)",
        TIPOHIST %in% ("8480/3") ~ "Adenocarcinoma mucinoso",
        TIPOHIST %in% ("8480/6") ~ "Pseudomixoma do peritonio (C48.2)",
        TIPOHIST %in% ("8481/3") ~ "Adenocarcinoma produtor de mucina",
        TIPOHIST %in% ("8482/3") ~ "Adenocarcinoma mucinoso, tipo endocervical",
        TIPOHIST %in% ("8490/3") ~ "Carcinoma de células em anel de sinete",
        TIPOHIST %in% ("8490/6") ~ "Carcinoma metastático de células em anel de sinete",
        # 850-854 NEOPLASIAS DUCTAIS E LOBULARES
        TIPOHIST %in% ("8500/3") ~ "Carcinoma ductal infiltrante, SOE (C50.-)",
        TIPOHIST %in% ("8501/3") ~ "Comedocarcinoma, SOE (C50.-)",
        TIPOHIST %in% ("8502/3") ~ "Carcinoma secretor da mama (C50.-)",
        TIPOHIST %in% ("8503/3") ~ "Adenocarcinoma papilar intraductal com invasão (C50.-)",
        TIPOHIST %in% ("8504/3") ~ "Carcinoma intracístico, SOE",
        TIPOHIST %in% ("8508/3") ~ "Carcinoma cístico hiperssecretor (C50.-)",
        TIPOHIST %in% ("8510/3") ~ "Carcinoma medular, SOE",
        TIPOHIST %in% ("8512/3") ~ "Carcinoma medular com estroma linfóide",
        TIPOHIST %in% ("8513/3") ~ "Carcinoma medular atípico (C50.-)",
        TIPOHIST %in% ("8514/3") ~ "Carcinoma ductal desmoplásico",
        TIPOHIST %in% ("8520/3") ~ "Carcinoma lobular, SOE (C50.-)",
        TIPOHIST %in% ("8521/3") ~ "Carcinoma ductular infiltrante (C50.-)",
        TIPOHIST %in% ("8522/3") ~ "Carcinoma ductal infiltrante e lobular (C50.-)",
        TIPOHIST %in% ("8523/3") ~ "Carcinoma ductal infiltrativo misto, com outros tipos de carcinoma (C50.-)",
        TIPOHIST %in% ("8524/3") ~ "Carcinoma lobular infiltrante misto com outros tipos de carcinoma (C50.-)",
        TIPOHIST %in% ("8525/3") ~ "Adenocarcinoma polimorfo de baixo grau",
        TIPOHIST %in% ("8530/3") ~ "Carcinoma inflamatório (C50.-)",
        TIPOHIST %in% ("8540/3") ~ "Doença de Paget, mamária (C50.-)",
        TIPOHIST %in% ("8541/3") ~ "Doença de Paget e carcinoma ductal infiltrante da mama (C50.-)",
        TIPOHIST %in% ("8542/3") ~ "Doença de Paget extramamária",
        TIPOHIST %in% ("8543/3") ~ "Doença de Paget e carcinoma intraductal da mama (C50.-)",
        # 855 NEOPLASIAS DE CÉLULAS ACINOSAS
        TIPOHIST %in% ("8550/3") ~ "Carcinoma de células acinosas",
        TIPOHIST %in% ("8551/3") ~ "Cistadenocarcinoma de células acinosas",
        # 856-857 NEOPLASIAS EPITELIAIS COMPLEXAS
        TIPOHIST %in% ("8560/3") ~ "Carcinoma adenoescamoso",
        TIPOHIST %in% ("8562/3") ~ "Carcinoma epitelial-mioepitelial",
        TIPOHIST %in% ("8570/3") ~ "Adenocarcinoma com metaplasia escamosa",
        TIPOHIST %in% ("8571/3") ~ "Adenocarcinoma com metaplasia cartilaginosa e óssea",
        TIPOHIST %in% ("8572/3") ~ "Adenocarcinoma com metaplasia fusocelular",
        TIPOHIST %in% ("8573/3") ~ "Adenocarcinoma com metaplasia apócrina",
        TIPOHIST %in% ("8574/3") ~ "Adenocarcinoma com diferenciação neuroendócrina",
        TIPOHIST %in% ("8575/3") ~ "Carcinoma metaplásico, SOE",
        TIPOHIST %in% ("8576/3") ~ "Adenocarcinoma hepatoide",
        # 858 NEOPLASIAS EPITELIAS TÍMICAS
        TIPOHIST %in% ("8580/3") ~ "Timoma maligno, SOE (C37.9)",
        TIPOHIST %in% ("8581/3") ~ "Timoma, tipo A, maligno (C37.9)",
        TIPOHIST %in% ("8582/3") ~ "Timoma, tipo AB maligno (C37.9)",
        TIPOHIST %in% ("8583/3") ~ "Timoma tipo B1 maligno (C37.9)", 
        TIPOHIST %in% ("8584/3") ~ "Timoma tipo B2 maligno (C37.9)", # Tá escrito 8484 no documento, mas eu imagino que esteja errado
        TIPOHIST %in% ("8585/3") ~ "Timoma tipo B3, maligno (C37.9)",
        TIPOHIST %in% ("8586/3") ~ "Carcinoma tímico, SOE (C37.9)",
        TIPOHIST %in% ("8588/3") ~ "Tumor epitelial fusocelular semelhante a células tímicas",
        TIPOHIST %in% ("8589/3") ~ "Carcinoma com elementos semelhantes ao timus",
        # 859-867 NEOPLASIAS ESPECIALIZADAS DAS GÔNADAS
        TIPOHIST %in% ("8600/3") ~ "Tecoma maligno (C56.9)",
        TIPOHIST %in% ("8620/3") ~ "Tumor maligno de células da granulosa (C56.9)",
        TIPOHIST %in% ("8630/3") ~ "Androblastoma maligno",
        TIPOHIST %in% ("8631/3") ~ "Tumor de células de Sertoli-Leydig, pouco diferenciada",
        TIPOHIST %in% ("8634/3") ~ "Tumor de células Sertoli-Leydig pouco diferenciado com elementos heterólogos",
        TIPOHIST %in% ("8640/3") ~ "Carcinoma de células de Sertoli (C62.-)",
        TIPOHIST %in% ("8650/3") ~ "Tumor maligno de células de Leydig (C62.-)",
        TIPOHIST %in% ("8670/3") ~ "Tumor de células esteroide, maligno",
        # 868-871 PARAGANGLIOMAS E TUMORES GLÔMICOS
        TIPOHIST %in% ("8680/3") ~ "Paraganglioma maligno",
        TIPOHIST %in% ("8693/3") ~ "Paraganglioma extra-supra-renal maligno",
        TIPOHIST %in% ("8700/3") ~ "Feocromocitoma maligno (C74.1)",
        TIPOHIST %in% ("8710/3") ~ "Glomangiossarcoma",
        TIPOHIST %in% ("8711/3") ~ "Tumor glômico maligno",
        # 872-879 NEVOS E MELANOMAS
        TIPOHIST %in% ("8720/3") ~ "Melanoma maligno, SOE",
        TIPOHIST %in% ("8721/3") ~ "Melanoma nodular (C44.-)",
        TIPOHIST %in% ("8722/3") ~ "Melanoma de células baloniformes (C44.-)",
        TIPOHIST %in% ("8723/3") ~ "Melanoma maligno em regressão (C44.-)",
        TIPOHIST %in% ("8728/3") ~ "Melanomatose meningeana (C70.9)",
        TIPOHIST %in% ("8730/3") ~ "Melanoma amelanótico (C44.-)",
        TIPOHIST %in% ("8740/3") ~ "Melanoma maligno em nevo juncional (C44.-)",
        TIPOHIST %in% ("8741/3") ~ "Melanoma maligno em melanose pré-cancerosa (C44.-)",
        TIPOHIST %in% ("8742/3") ~ "Melanoma em lentigo maligno (C44.-)",
        TIPOHIST %in% ("8743/3") ~ "Melanoma de propagação superficial (C44.-)",
        TIPOHIST %in% ("8744/3") ~ "Melanoma lentiginoso maligno das extremidades (C44.-)",
        TIPOHIST %in% ("8745/3") ~ "Melanoma desmoplásico maligno (C44.-)",
        TIPOHIST %in% ("8746/3") ~ "Melanoma lentiginoso de mucosa",
        TIPOHIST %in% ("8761/3") ~ "Melanoma maligno em nevo pigmentado gigante (C44.-)",
        TIPOHIST %in% ("8770/3") ~ "Melanoma misto epitelióide e fusocelular",
        TIPOHIST %in% ("8771/3") ~ "Melanoma de células epitelióides",
        TIPOHIST %in% ("8772/3") ~ "Melanoma fusocelular, SOE",
        TIPOHIST %in% ("8773/3") ~ "Melanoma fusocelular, tipo A (C69.-)",
        TIPOHIST %in% ("8774/3") ~ "Melanoma fusocelular, tipo B (C69.-)",
        TIPOHIST %in% ("8780/3") ~ "Nevo azul maligno (C44.-)",
        # 880 TUMORES E SARCOMAS DE PARTES MOLES, SOE
        TIPOHIST %in% ("8800/3") ~ "Sarcoma, SOE",
        TIPOHIST %in% ("8800/9") ~ "Sarcomatose, SOE",
        TIPOHIST %in% ("8801/3") ~ "Sarcoma fusocelular",
        TIPOHIST %in% ("8802/3") ~ "Sarcoma de células gigantes",
        TIPOHIST %in% ("8803/3") ~ "Sarcoma de células pequenas",
        TIPOHIST %in% ("8804/3") ~ "Sarcoma epitelióide",
        TIPOHIST %in% ("8805/3") ~ "Sarcoma indiferenciado",
        TIPOHIST %in% ("8806/3") ~ "Tumor desmoplásico de células pequenas e redondas",
        # 881-883 NEOPLASIAS FIBROMATOSAS
        TIPOHIST %in% ("8810/3") ~ "Fibrossarcoma, SOE",
        TIPOHIST %in% ("8811/3") ~ "Fibromixossarcoma",
        TIPOHIST %in% ("8812/3") ~ "Fibrossarcoma periostal (C40.-, C41.-)",
        TIPOHIST %in% ("8813/3") ~ "Fibrossarcoma de fáscia",
        TIPOHIST %in% ("8814/3") ~ "Fibrossarcoma infantil",
        TIPOHIST %in% ("8815/3") ~ "Tumor maligno fibroso, solitário",
        TIPOHIST %in% ("8830/3") ~ "Histiocitoma fibroso maligno",
        TIPOHIST %in% ("8832/3") ~ "Dermatofibrossarcoma, SOE (C44.-)",
        TIPOHIST %in% ("8833/3") ~ "Dermatofibrossarcoma protuberans pigmentado (C44.-)",
        # 884 NEOPLASIAS MIXOMATOSAS
        TIPOHIST %in% ("8840/3") ~ "Mixossarcoma",
        # 885-888 NEOPLASIAS LIPOMATOSAS
        TIPOHIST %in% ("8850/3") ~ "Lipossarcoma, SOE",
        TIPOHIST %in% ("8851/3") ~ "Lipossarcoma bem diferenciado",
        TIPOHIST %in% ("8852/3") ~ "Lipossarcoma mixóide",
        TIPOHIST %in% ("8853/3") ~ "Lipossarcoma de células redondas",
        TIPOHIST %in% ("8854/3") ~ "Lipossarcoma pleomórfico",
        TIPOHIST %in% ("8855/3") ~ "Lipossarcoma misto",
        TIPOHIST %in% ("8857/3") ~ "Lipossarcoma fibroblástico",
        TIPOHIST %in% ("8858/3") ~ "Lipossarcoma dediferenciado",
        # 889-892 NEOPLASIAS MIOMATOSAS
        TIPOHIST %in% ("8890/3") ~ "Leiomiossarcoma, SOE",
        TIPOHIST %in% ("8891/3") ~ "Leiomiossarcoma epitelioide",
        TIPOHIST %in% ("8894/3") ~ "Angiomiossarcoma",
        TIPOHIST %in% ("8895/3") ~ "Miossarcoma",
        TIPOHIST %in% ("8896/3") ~ "Leiomiossarcoma mixóide",
        TIPOHIST %in% ("8900/3") ~ "Rabdomiossarcoma, SOE",
        TIPOHIST %in% ("8901/3") ~ "Rabdomiossarcoma pleomórfico tipo adulto",
        TIPOHIST %in% ("8902/3") ~ "Rabdomiossarcoma tipo misto",
        TIPOHIST %in% ("8910/3") ~ "Rabdomiossarcoma embrionário, SOE",
        TIPOHIST %in% ("8912/3") ~ "Rabdomiossarcoma fusocelular",
        TIPOHIST %in% ("8920/3") ~ "Rabdomiossarcoma alveolar",
        TIPOHIST %in% ("8921/3") ~ "Rabdomiosarcoma com diferenciação gangliônica",
        # 893-899 NEOPLASIAS COMPLEXAS MISTAS E DO ESTROMA
        TIPOHIST %in% ("8930/3") ~ "Sarcoma do estroma endometrial, SOE (C54.1)",
        TIPOHIST %in% ("8931/3") ~ "Sarcoma do estroma endometrial, baixo grau (C54.1)",
        TIPOHIST %in% ("8933/3") ~ "Adenossarcoma",
        TIPOHIST %in% ("8934/3") ~ "Carcinofibroma",
        TIPOHIST %in% ("8935/3") ~ "Sarcoma estromal, SOE",
        TIPOHIST %in% ("8936/3") ~ "Sarcoma estromal gastrointestinal",
        TIPOHIST %in% ("8940/3") ~ "Tumor misto maligno, SOE",
        TIPOHIST %in% ("8941/3") ~ "Carcinoma em adenoma pleomórfico (C07.-, C08.-)",
        TIPOHIST %in% ("8950/3") ~ "Tumor misto mulleriano (C54.-)",
        TIPOHIST %in% ("8951/3") ~ "Tumor misto mesodérmico",
        TIPOHIST %in% ("8959/3") ~ "Nefroma maligno cístico (C64.9)",
        TIPOHIST %in% ("8960/3") ~ "Nefroblastoma, SOE (C64.9)",
        TIPOHIST %in% ("8963/3") ~ "Tumor rabdóide maligno",
        TIPOHIST %in% ("8964/3") ~ "Sarcoma de células claras do rim (C64.9)",
        TIPOHIST %in% ("8970/3") ~ "Hepatoblastoma (C22.0)",
        TIPOHIST %in% ("8971/3") ~ "Pancreatoblastoma (C25.-)",
        TIPOHIST %in% ("8972/3") ~ "Blastoma pulmonar (C34.-)",
        TIPOHIST %in% ("8973/3") ~ "Blastoma pleuro pulmonar",
        TIPOHIST %in% ("8980/3") ~ "Carcinossarcoma, SOE",
        TIPOHIST %in% ("8981/3") ~ "Carcinossarcoma embrionário",
        TIPOHIST %in% ("8982/3") ~ "Mioepitelioma maligno",
        TIPOHIST %in% ("8990/3") ~ "Mesenquimoma maligno",
        TIPOHIST %in% ("8991/3") ~ "Sarcoma embrionário",
        # 900-903 NEOPLASIAS FIBROEPITELIAS
        TIPOHIST %in% ("9000/3") ~ "Tumor de Brenner maligno (C56.9)",
        TIPOHIST %in% ("9014/3") ~ "Adenocarcinofibroma seroso",
        TIPOHIST %in% ("9015/3") ~ "Adenocarcinofibroma mucinoso",
        TIPOHIST %in% ("9020/3") ~ "Tumor filodes maligno (C50.-)",
        # 904 NEOPLASIAS SINOVIAIS
        TIPOHIST %in% ("9040/3") ~ "Sarcoma sinovial, SOE",
        TIPOHIST %in% ("9041/3") ~ "Sarcoma sinovial fusocelular",
        TIPOHIST %in% ("9042/3") ~ "Sarcoma sinovial de células epitelióides",
        TIPOHIST %in% ("9043/3") ~ "Sarcoma sinovial bifásico",
        TIPOHIST %in% ("9044/3") ~ "Sarcoma de células claras, SOE",
        # 905 NEOPLASIAS MESOTELIAIS
        TIPOHIST %in% ("9050/3") ~ "Mesotelioma maligno",
        TIPOHIST %in% ("9051/3") ~ "Mesotelioma fibroso maligno",
        TIPOHIST %in% ("9052/3") ~ "Mesotelioma epitelióide maligno",
        TIPOHIST %in% ("9053/3") ~ "Mesotelioma bifásico maligno",
        # 906-909 NEOPLASIAS DE CÉLULAS GERMINATIVAS
        TIPOHIST %in% ("9060/3") ~ "Disgerminoma",
        TIPOHIST %in% ("9061/3") ~ "Seminoma, SOE (C62.-)",
        TIPOHIST %in% ("9062/3") ~ "Seminoma anaplásico (C62.-)",
        TIPOHIST %in% ("9063/3") ~ "Seminoma espermatocítico (C62.-)",
        TIPOHIST %in% ("9064/3") ~ "Germinoma",
        TIPOHIST %in% ("9065/3") ~ "Tumor de células germinativas não seminomatoso (C62.-)",
        TIPOHIST %in% ("9070/3") ~ "Carcinoma embrionário, SOE",
        TIPOHIST %in% ("9071/3") ~ "Tumor de saco vitelino",
        TIPOHIST %in% ("9072/3") ~ "Poliembrioma",
        TIPOHIST %in% ("9080/3") ~ "Teratoma maligno, SOE",
        TIPOHIST %in% ("9081/3") ~ "Teratocarcinoma",
        TIPOHIST %in% ("9082/3") ~ "Teratoma maligno indiferenciado",
        TIPOHIST %in% ("9083/3") ~ "Teratoma maligno intermediário",
        TIPOHIST %in% ("9084/3") ~ "Teratoma com transformação maligna",
        TIPOHIST %in% ("9085/3") ~ "Tumor misto de células germinativas",
        TIPOHIST %in% ("9090/3") ~ "Estroma ovariano maligno (C56.9)",
        # 910 NEOPLASIAS TROFOBLÁSTICAS
        TIPOHIST %in% ("9100/3") ~ "Coriocarcinoma, SOE",
        TIPOHIST %in% ("9101/3") ~ "Coriocarcinoma combinado com outros elementos de células germinativas",
        TIPOHIST %in% ("9102/3") ~ "Teratoma maligno trofoblástico",
        TIPOHIST %in% ("9105/3") ~ "Tumor trofoblástico epitelióide",
        # 911 MESONEFROMAS
        TIPOHIST %in% ("9110/3") ~ "Mesonefroma maligno",
        # 912-916 TUMORES DE VASOS SANGUÍNEOS
        TIPOHIST %in% ("9120/3") ~ "Hemangiossarcoma",
        TIPOHIST %in% ("9124/3") ~ "Sarcoma das células de Kupffer (C22.0)",
        TIPOHIST %in% ("9130/3") ~ "Hemangioendotelioma maligno",
        TIPOHIST %in% ("9133/3") ~ "Hemangioendotelioma epitelióide maligno",
        TIPOHIST %in% ("9140/3") ~ "Sarcoma de Kaposi",
        TIPOHIST %in% ("9150/3") ~ "Hemangiopericitoma maligno",
        # 917 TUMORES DE VASOS LINFÁTICOS
        TIPOHIST %in% ("9170/3") ~ "Linfangiossarcoma",
        # 918-924 NEOPLASIAS ÓSSEAS E CONDROMATOSAS
        TIPOHIST %in% ("9180/3") ~ "Osteossarcoma, SOE (C40.-, C41.-)",
        TIPOHIST %in% ("9181/3") ~ "Osteossarcoma condroblástico (C40.-, C41.-)",
        TIPOHIST %in% ("9182/3") ~ "Osteossarcoma fibroblástico (C40.-, C41.-)",
        TIPOHIST %in% ("9183/3") ~ "Osteossarcoma telangiectásico (C40.-, C41.-)",
        TIPOHIST %in% ("9184/3") ~ "Osteossarcoma em doença de Paget do osso (C40.-, C41.-)",
        TIPOHIST %in% ("9185/3") ~ "Osteossarcoma de células pequenas (C40.-, C41.-)",
        TIPOHIST %in% ("9186/3") ~ "Osteossarcoma central (C40.-, C41.-)",
        TIPOHIST %in% ("9187/3") ~ "Osteossarcoma intra-ósseo, bem diferenciado (C40.-, C41.-)",
        TIPOHIST %in% ("9192/3") ~ "Osteossarcoma paraostal (C40.-, C41.-)",
        TIPOHIST %in% ("9193/3") ~ "Osteossarcoma periostal (C40.-, C41.-)",
        TIPOHIST %in% ("9194/3") ~ "Osteossarcoma de superfície de alto grau (C40.-, C41.-)",
        TIPOHIST %in% ("9195/3") ~ "Osteossarcoma intra-cortical (C40.-, C41.-)",
        TIPOHIST %in% ("9220/3") ~ "Condrossarcoma, SOE (C40.-, C41.-)",
        TIPOHIST %in% ("9221/3") ~ "Condrossarcoma justacortical (C40.-, C41.-)",
        TIPOHIST %in% ("9230/3") ~ "Condroblastoma maligno (C40.-, C41.-)",
        TIPOHIST %in% ("9231/3") ~ "Condrossarcoma mixóide",
        TIPOHIST %in% ("9240/3") ~ "Condrossarcoma mesenquimal",
        TIPOHIST %in% ("9242/3") ~ "Condrossarcoma de células claras (C40.-, C41.-)",
        TIPOHIST %in% ("9243/3") ~ "Condrossarcoma dediferenciado (C40.-, C41.-)",
        # 925 TUMORES DE CÉLULAS GIGANTES
        TIPOHIST %in% ("9250/3") ~ "Tumor maligno de células gigantes do osso (C40.-,C41.-)",
        TIPOHIST %in% ("9251/3") ~ "Tumor maligno de células gigantes de partes moles",
        TIPOHIST %in% ("9252/3") ~ "Tumor maligno de células gigantes tenossinovial (C49.-)",
        # 926 TUMORES ÓSSEOS DIVERSOS (C40.-,C41.-)
        TIPOHIST %in% ("9260/3") ~ "Sarcoma de Ewing",
        TIPOHIST %in% ("9261/3") ~ "Adamantinoma de ossos longos (C40.-)",
        # 927-934 TUMORES ODONTOGÊNICOS (C41.-)
        TIPOHIST %in% ("9270/3") ~ "Tumor odontogênico maligno",
        TIPOHIST %in% ("9290/3") ~ "Odontossarcoma ameloblástico",
        TIPOHIST %in% ("9310/3") ~ "Ameloblastoma maligno",
        TIPOHIST %in% ("9330/3") ~ "Fibrossarcoma ameloblástico",
        TIPOHIST %in% ("9342/3") ~ "Carcinossarcoma odontogênico",
        # 935-937 TUMORES DIVERSOS
        TIPOHIST %in% ("9362/3") ~ "Pineoblastoma (C75.3)",
        TIPOHIST %in% ("9364/3") ~ "Tumor neuroectodérmico periférico",
        TIPOHIST %in% ("9365/3") ~ "Tumor de Askin",
        TIPOHIST %in% ("9370/3") ~ "Cordoma,SOE",
        TIPOHIST %in% ("9371/3") ~ "Cordoma condroide",
        TIPOHIST %in% ("9372/3") ~ "Cordoma dediferenciado",
        # 938-948 GLIOMAS
        TIPOHIST %in% ("9380/3") ~ "Glioma maligno (C71.-)",
        TIPOHIST %in% ("9381/3") ~ "Gliomatose cerebral (C71.-)",
        TIPOHIST %in% ("9382/3") ~ "Glioma misto (C71.-)",
        TIPOHIST %in% ("9390/3") ~ "Carcinoma de plexo coróide (C71.5)",
        TIPOHIST %in% ("9391/3") ~ "Ependimoma, SOE (C71.-)",
        TIPOHIST %in% ("9392/3") ~ "Ependimoma anaplásico (C71.-)",
        TIPOHIST %in% ("9393/3") ~ "Ependimoma papilar (C71.-)",
        TIPOHIST %in% ("9400/3") ~ "Astrocitoma, SOE (C71.-)",
        TIPOHIST %in% ("9401/3") ~ "Astrocitoma anaplásico (C71.-)",
        TIPOHIST %in% ("9410/3") ~ "Astrocitoma protoplasmático (C71.-)",
        TIPOHIST %in% ("9411/3") ~ "Astrocitoma gemistocítico (C71.-)",
        TIPOHIST %in% ("9420/3") ~ "Astrocitoma fibrilar (C71.-)",
        TIPOHIST %in% ("9423/3") ~ "Espongioblastoma polar (C71.-)",
        TIPOHIST %in% ("9424/3") ~ "Xantoastrocitoma pleomórfico (C71.-)",
        TIPOHIST %in% ("9430/3") ~ "Astroblastoma (C71.-)",
        TIPOHIST %in% ("9440/3") ~ "Glioblastoma, SOE (C71.-)",
        TIPOHIST %in% ("9441/3") ~ "Glioblastoma de células gigantes (C71.-)",
        TIPOHIST %in% ("9442/3") ~ "Gliossarcoma (C71.-)",
        TIPOHIST %in% ("9450/3") ~ "Oligodendroglioma, SOE (C71.-)",
        TIPOHIST %in% ("9451/3") ~ "Oligodendroglioma anaplásico (C71.-)",
        TIPOHIST %in% ("9460/3") ~ "Oligodendroblastoma (C71.-)",
        TIPOHIST %in% ("9470/3") ~ "Meduloblastoma, SOE (C71.6)",
        TIPOHIST %in% ("9471/3") ~ "Meduloblastoma desmoplásico nodular (C71.6)",
        TIPOHIST %in% ("9472/3") ~ "Medulomioblastoma (C71.6)",
        TIPOHIST %in% ("9473/3") ~ "Tumor neuroectodérmico primitivo, SOE(C71.-)",
        TIPOHIST %in% ("9474/3") ~ "Meduloblastoma de células grandes (C71.6)",
        TIPOHIST %in% ("9480/3") ~ "Sarcoma cerebelar, SOE (C71.6)",
        # 949-952 NEOPLASIAS NEUROEPITELIAIS
        TIPOHIST %in% ("9490/3") ~ "Ganglioneuroblastoma",
        TIPOHIST %in% ("9500/3") ~ "Neuroblastoma, SOE",
        TIPOHIST %in% ("9501/3") ~ "Meduloepitelioma, SOE",
        TIPOHIST %in% ("9502/3") ~ "Meduloepitelioma teratóide",
        TIPOHIST %in% ("9503/3") ~ "Neuroepitelioma, SOE",
        TIPOHIST %in% ("9504/3") ~ "Espongioneuroblastoma",
        TIPOHIST %in% ("9505/3") ~ "Ganglioglioma anaplásico",
        TIPOHIST %in% ("9508/3") ~ "Tumor teratóide/rabdóide atípico (C71.-)",
        TIPOHIST %in% ("9510/3") ~ "Retinoblastoma, SOE (C69.2)",
        TIPOHIST %in% ("9511/3") ~ "Retinoblastoma diferenciado (C69.2)",
        TIPOHIST %in% ("9512/3") ~ "Retinoblastoma indiferenciado (C69.2)",
        TIPOHIST %in% ("9513/3") ~ "Retinoblastoma difuso (C69.2)",
        TIPOHIST %in% ("9520/3") ~ "Tumor neurogênico olfativo",
        TIPOHIST %in% ("9521/3") ~ "Neurocitoma olfatório (C30.0)",
        TIPOHIST %in% ("9522/3") ~ "Neuroblastoma olfativo (C30.0)",
        TIPOHIST %in% ("9523/3") ~ "Neuroepitelioma olfativo (C30.0)",
        # 953 MENINGIOMAS (C70.-)
        TIPOHIST %in% ("9530/3") ~ "Meningeoma maligno",
        TIPOHIST %in% ("9538/3") ~ "Meningioma papilar",
        TIPOHIST %in% ("9539/3") ~ "Sarcomatose meníngea",
        # 954-957 TUMORES DA BAINHA NERVOSA
        TIPOHIST %in% ("9540/3") ~ "Tumor maligno da bainha de nervo periférico",
        TIPOHIST %in% ("9560/3") ~ "Neurilemoma maligno",
        TIPOHIST %in% ("9561/3") ~ "Tumor maligno da bainha de nervo periférico com diferenciação rabdomioblástica",
        TIPOHIST %in% ("9571/3") ~ "Perineuroma maligno",
        # 958 TUMORES DE CÉLULAS GRANULARES E SARCOMA ALVEOLAR DE PARTES MOLES
        TIPOHIST %in% ("9580/3") ~ "Tumor de células granulares, maligno",
        TIPOHIST %in% ("9581/3") ~ "Sarcoma alveolar de partes moles",
        # 959-972 LINFOMAS DE HODGKIN E NÃO-HODGKIN
        TIPOHIST %in% ("9590/3") ~ "Linfoma maligno, SOE",
        TIPOHIST %in% ("9591/3") ~ "Linfoma maligno não-Hodgkin, SOE", # Até o 9596/3 existe um gap sem classificações
        TIPOHIST %in% ("9596/3") ~ "Linfoma composto: Hodgkin e não Hodgkin",
        TIPOHIST %in% ("9650/3") ~ "Linfoma de Hodgkin, SOE",
        TIPOHIST %in% ("9651/3") ~ "Linfoma de Hodgkin tipo linfocítico",
        TIPOHIST %in% ("9652/3") ~ "Linfoma de Hodgkin, celularidade mista, SOE",
        TIPOHIST %in% ("9653/3") ~ "Linfoma de Hodgkin, depleção linfocítica, SOE",
        TIPOHIST %in% ("9654/3") ~ "Linfoma de Hodgkin, depleção linfocítica, com fibrose difusa",
        TIPOHIST %in% ("9655/3") ~ "Linfoma de Hodgkin, depleção linfocítica, reticular",
        TIPOHIST %in% ("9659/3") ~ "Linfoma de Hodgkin, predominância linfocítica, nodular",
        TIPOHIST %in% ("9661/3") ~ "Granuloma de Hodgkin",
        TIPOHIST %in% ("9662/3") ~ "Sarcoma de Hodgkin",
        TIPOHIST %in% ("9663/3") ~ "Linfoma de Hodgkin, esclerose nodular, SOE",
        TIPOHIST %in% ("9664/3") ~ "Linfoma de Hodgkin, esclerose nodular, fase celular",
        TIPOHIST %in% ("9665/3") ~ "Linfoma de Hodgkin, esclerose nodular, grau 1",
        TIPOHIST %in% ("9667/3") ~ "Linfoma de Hodgkin, esclerose nodular, grau 2",
        TIPOHIST %in% ("9670/3") ~ "Linfoma maligno de linfócitos células B pequenas, SOE",
        TIPOHIST %in% ("9671/3") ~ "Linfoma maligno linfoplasmocitário",
        TIPOHIST %in% ("9673/3") ~ "Linfoma de células da zona do manto",
        TIPOHIST %in% ("9675/3") ~ "Linfoma maligno, misto de células pequenas e grandes, difuso",
        TIPOHIST %in% ("9678/3") ~ "Efusão por linfoma primário",
        TIPOHIST %in% ("9679/3") ~ "Linfoma do mediastino de células grandes B (C38.3)",
        TIPOHIST %in% ("9680/3") ~ "Linfoma maligno de células grandes B, difuso, SOE",
        TIPOHIST %in% ("9684/3") ~ "Linfoma maligno de células grandes B, difuso, imunoblástico, SOE",
        TIPOHIST %in% ("9687/3") ~ "Linfoma de Burkitt, SOE",
        TIPOHIST %in% ("9689/3") ~ "Linfoma de células da zona marginal esplênica (C42.2)",
        TIPOHIST %in% ("9690/3") ~ "Linfoma folicular, SOE",
        TIPOHIST %in% ("9691/3") ~ "Linfoma folicular grau 2",
        TIPOHIST %in% ("9695/3") ~ "Linfoma folicular grau 1",
        TIPOHIST %in% ("9698/3") ~ "Linfoma folicular, grau 3",
        TIPOHIST %in% ("9699/3") ~ "Linfoma de zona marginal tipo células B, SOE",
        TIPOHIST %in% ("9700/3") ~ "Micose fungóide (C44.-)",
        TIPOHIST %in% ("9701/3") ~ "Síndrome de Sezary",
        TIPOHIST %in% ("9702/3") ~ "Linfoma de células T, maduras, SOE",
        TIPOHIST %in% ("9705/3") ~ "Linfoma angio-imunoblástico de células T",
        TIPOHIST %in% ("9708/3") ~ "Linfoma subcutâneo, tipo paniculite de células T",
        TIPOHIST %in% ("9709/3") ~ "Linfoma cutâneo de células T, SOE (C44.-)",
        TIPOHIST %in% ("9714/3") ~ "Linfoma anaplásico de grandes células, T e NULL",
        TIPOHIST %in% ("9716/3") ~ "Linfoma hepatoesplênica, tipo gama-delta",
        TIPOHIST %in% ("9717/3") ~ "Linfoma intestinal de células T",
        TIPOHIST %in% ("9718/3") ~ "Lesão linfoproliferativa cutânea primária de células T (CD30+) (C44.-)",
        TIPOHIST %in% ("9719/3") ~ "Linfoma nasal e tipo nasal de células T/NK",
        TIPOHIST %in% ("9727/3") ~ "Linfoma linfoblástico de células precursoras SOE",
        TIPOHIST %in% ("9728/3") ~ "Linfoma linfoblástico de células precursoras B",
        TIPOHIST %in% ("9729/3") ~ "Linfoma linfoblástico de células precursoras T",
        # 973 TUMORES DE PLASMÓCITOS
        TIPOHIST %in% ("9731/3") ~ "Plasmocitoma, SOE",
        TIPOHIST %in% ("9732/3") ~ "Mieloma múltiplo (C42.1)",
        TIPOHIST %in% ("9733/3") ~ "Leucemia de plasmocitos (C42.1)",
        TIPOHIST %in% ("9734/3") ~ "Plamocitoma extramedular",
        # 974 TUMORES DE MASTÓCITOS
        TIPOHIST %in% ("9740/3") ~ "Sarcoma de mastócitos",
        TIPOHIST %in% ("9741/3") ~ "Mastocitose maligna",
        TIPOHIST %in% ("9742/3") ~ "Leucemia de mastócitos (C42.1)",
        # 975 NEOPLASIAS DE HISTIÓCITOS E CÉLULAS LINFÓIDES ACESSÓRIAS
        TIPOHIST %in% ("9750/3") ~ "Histiocitose maligna",
        TIPOHIST %in% ("9754/3") ~ "Histiocitose de células de Langerhans, disseminada",
        TIPOHIST %in% ("9755/3") ~ "Sarcoma histiocítico",
        TIPOHIST %in% ("9756/3") ~ "Sarcoma de células de Langerhans",
        TIPOHIST %in% ("9757/3") ~ "Sarcoma de células dendríticas digitiforme",
        TIPOHIST %in% ("9758/3") ~ "Sarcoma de células dendríticas folicular",
        # 976 DOENÇAS IMUNOPROLIFERATIVAS
        TIPOHIST %in% ("9760/3") ~ "Doença imunoproliferativa, SOE",
        TIPOHIST %in% ("9761/3") ~ "Macroglobulinemia de Waldenstrom (C42.0)",
        TIPOHIST %in% ("9762/3") ~ "Doença de cadeia pesada, SOE",
        TIPOHIST %in% ("9764/3") ~ "Doença imunoproliferativa do intestino delgado (C17.-)",
        # 980-994 LEUCEMIAS
        # 980 LEUCEMIAS, SOE (C42.1)
        TIPOHIST %in% ("9800/3") ~ "Leucemia, SOE",
        TIPOHIST %in% ("9801/3") ~ "Leucemia aguda, SOE",
        TIPOHIST %in% ("9805/3") ~ "Leucemia aguda, bifenotípica",
        # 982-983 LEUCEMIAS LINFÓIDES (C42.1)
        TIPOHIST %in% ("9820/3") ~ "Leucemia linfóide, SOE",
        TIPOHIST %in% ("9823/3") ~ "Leucemia linfocítica crônica de células B/linfoma linfocítico de pequenas células",
        TIPOHIST %in% ("9826/3") ~ "Leucemia de células de Burkitt",
        TIPOHIST %in% ("9827/3") ~ "Leucemia de células T do adulto/Linfoma (HTL V-1+)",
        TIPOHIST %in% ("9831/1") ~ "Leucemia linfocítica, de células grandes tipo T granulares",
        TIPOHIST %in% ("9832/3") ~ "Leucemia prolinfocítica, SOE",
        TIPOHIST %in% ("9833/3") ~ "Leucemia prolinfocítica, tipo células B",
        TIPOHIST %in% ("9834/3") ~ "Leucemia prolinfocítica, tipo células T",
        TIPOHIST %in% ("9835/3") ~ "Leucemia linfoblástica de células precursoras, SOE",
        TIPOHIST %in% ("9836/3") ~ "Leucemia linfoblástica de células precursoras tipo B",
        TIPOHIST %in% ("9837/3") ~ "Leucemia linfoblástica de células T precursoras",
        # 984-993 LEUCEMIA MIELÓIDE (C42.1)
        TIPOHIST %in% ("9840/3") ~ "Leucemia mielóide aguda tipo M6",
        TIPOHIST %in% ("9860/3") ~ "Leucemia mielóide, SOE",
        TIPOHIST %in% ("9861/3") ~ "Leucemia mielóide aguda, SOE",
        TIPOHIST %in% ("9863/3") ~ "Leucemia mielóide crônica, SOE",
        TIPOHIST %in% ("9866/3") ~ "Leucemia promielocítica aguda,T (15;17) (q22;q11-12)",
        TIPOHIST %in% ("9867/3") ~ "Leucemia mielomonocítica aguda",
        TIPOHIST %in% ("9870/3") ~ "Leucemia basofílica aguda",
        TIPOHIST %in% ("9871/3") ~ "Leucemia mielóide aguda com eosinófilos anormais na medula",
        TIPOHIST %in% ("9872/3") ~ "Leucemia mielóide aguda, com diferenciação mínima",
        TIPOHIST %in% ("9873/3") ~ "Leucemia mielóide aguda sem maturação",
        TIPOHIST %in% ("9874/3") ~ "Leucemia mielóide aguda com maturação",
        TIPOHIST %in% ("9875/3") ~ "Leucemia crônica mielogênica, BCR/ABL positiva",
        TIPOHIST %in% ("9876/3") ~ "Leucemia mielóide crônica atípica, BCR/ABL negativa",
        TIPOHIST %in% ("9891/3") ~ "Leucemia monocítica aguda",
        TIPOHIST %in% ("9895/3") ~ "Leucemia mielóide aguda com displasia de multilinhagem",
        TIPOHIST %in% ("9896/3") ~ "Leucemia mielóide aguda, t(8;21)(q22;q22)",
        TIPOHIST %in% ("9897/3") ~ "Leucemia mielóide aguda, com anormalidades em 11q23",
        TIPOHIST %in% ("9910/3") ~ "Leucemia megacarioblástica aguda",
        TIPOHIST %in% ("9920/3") ~ "Leucemia mielóide aguda relacionada ao tratamento, SOE",
        TIPOHIST %in% ("9930/3") ~ "Sarcoma mielóide",
        TIPOHIST %in% ("9931/3") ~ "Panmielose aguda com mielofibrose (C42.1)",
        # 994 OUTRAS LEUCEMIAS (C42.1)
        TIPOHIST %in% ("9940/3") ~ "Leucemia tipo células pilosas (C42.1)",
        TIPOHIST %in% ("9945/3") ~ "Leucemia mielomonocítica crônica, SOE",
        TIPOHIST %in% ("9946/3") ~ "Leucemia mielomonocítica juvenil",
        TIPOHIST %in% ("9948/3") ~ "Leucemia de células tipo NK agressiva",
        # 995-996 ALTERAÇÕES MIELO PROLIFERATIVAS CRÔNICAS (C42.1)
        TIPOHIST %in% ("9950/3") ~ "Policitemia vera",
        TIPOHIST %in% ("9960/3") ~ "Doença mieloproliferativa crônica, SOE",
        TIPOHIST %in% ("9961/3") ~ "Mielosclerose com metaplasia mielóide",
        TIPOHIST %in% ("9962/3") ~ "Trombocitemia essencial",
        TIPOHIST %in% ("9963/3") ~ "Leucemia crônica neutrofílica",
        TIPOHIST %in% ("9964/3") ~ "Sindrome hipereosinofílico",
        # 997 OUTRAS ALTERAÇÕES HEMATOLÓGICAS
        TIPOHIST %in% ("9975/1") ~ "Doença mieloproliferativa, SOE",
        # 998 SÍNDROMES MIELODISPLÁSICAS (C42.1)
        TIPOHIST %in% ("9980/3") ~ "Anemia refratária",
        TIPOHIST %in% ("9982/3") ~ "Anemia refratária com sideroblastos",
        TIPOHIST %in% ("9983/3") ~ "Anemia refratária com excesso de blastos",
        TIPOHIST %in% ("9984/3") ~ "Anemia refratária com excesso de blastos, em transformação",
        TIPOHIST %in% ("9985/3") ~ "Citopenia refratária com displasia multilinhagem",
        TIPOHIST %in% ("9986/3") ~ "Síndrome mielodisplásica com deleção 5q (5q-)",
        TIPOHIST %in% ("9987/3") ~ "Síndrome mielodisplásica relacionada a terapia, SOE",
        TIPOHIST %in% ("9989/3") ~ "Síndrome mielodisplásica, SOE", # Não existe nenhum código depois desse
        TRUE ~ TIPOHIST
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

arrow::write_parquet(dadosFinal, "../outputs/dadosFinal.parquet")

# Dividindo o banco entre as 5 regiões do país
# Importante notar que esse processo retira NAs e também não contabiliza pacientes do exterior

dadosNorte <- dadosFinal |> 
  dplyr::filter(RegiaoResidencia == "Norte")
arrow::write_parquet(dadosNorte, "../outputs/dadosNorte.parquet")

dadosNordeste <- dadosFinal |> 
  dplyr::filter(RegiaoResidencia == "Nordeste")
arrow::write_parquet(dadosNordeste, "../outputs/dadosNordeste.parquet")

dadosCentroOeste <- dadosFinal |> 
  dplyr::filter(RegiaoResidencia == "Centro-Oeste")
arrow::write_parquet(dadosCentroOeste, "../outputs/dadosCentroOeste.parquet")

dadosSudeste <- dadosFinal |> 
  dplyr::filter(RegiaoResidencia == "Sudeste")
arrow::write_parquet(dadosSudeste, "../outputs/dadosSudeste.parquet")

dadosSul <- dadosFinal |> 
  dplyr::filter(RegiaoResidencia == "Sul")
arrow::write_parquet(dadosSul, "../outputs/dadosSul.parquet")

