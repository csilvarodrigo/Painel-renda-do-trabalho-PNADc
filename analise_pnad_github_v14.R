# ==============================================================================
# PNAD CONTÍNUA 3T/2026 — RENDA DO TRABALHO PRINCIPAL (18+ ANOS)
# Variáveis: VD4016 (Renda Habitual), VD4011 (COD), VD3004 (Escolaridade), V1028 (Peso)
# Grupos: Homem Branco, Mulher Branca, Homem Negro, Mulher Negra e Todos
#         ("Todos" inclui Amarelos, Indígenas e Ignorados)
# Pacote para Índice de Gini: DescTools::Gini(x, weights = w, na.rm = TRUE)
# Recortes: Brasil, UFs, Capitais e Regiões Metropolitanas (RM_RIDE)
# Saída: CSVs tabulados + Painel HTML responsivo (index.html) com mapas coropléticos
#         de Estados, Capitais e Regiões Metropolitanas (malhas IBGE via geobr)  [v14]
# ==============================================================================

# Instale os pacotes necessários caso ainda não possua:
# install.packages(c("PNADcIBGE", "data.table", "DescTools", "jsonlite", "geobr", "sf"))   # R >= 4.0

library(PNADcIBGE)
library(data.table)
library(DescTools)  # Para cálculo oficial do Índice de Gini ponderado: DescTools::Gini()
library(jsonlite)

# ==============================================================================
# 0. FUNÇÕES AUXILIARES PONDERADAS
# ==============================================================================
w_quantile <- function(x, w, probs = 0.5) {
  valid <- !is.na(x) & !is.na(w) & w > 0
  x <- as.numeric(x[valid])
  w <- as.numeric(w[valid])
  n <- length(x)
  if (n == 0) return(rep(NA_real_, length(probs)))
  if (n == 1) return(rep(x[1], length(probs)))
  
  ord <- order(x)
  x <- x[ord]
  w <- w[ord]
  
  # Posição acumulada centrada (interpolação linear ponderada contínua, evita saltos em degraus)
  cum_w <- (cumsum(w) - 0.5 * w) / sum(w)
  approx(x = cum_w, y = x, xout = probs, method = "linear", rule = 2, ties = mean)$y
}

w_mean <- function(x, w) {
  valid <- !is.na(x) & !is.na(w) & w > 0
  if (!any(valid)) return(NA_real_)
  sum(x[valid] * w[valid]) / sum(w[valid])
}

# Cálculo do Índice de Gini utilizando o pacote DescTools com peso amostral V1028
calc_gini_pacote <- function(x, w) {
  valid <- !is.na(x) & !is.na(w) & w > 0 & x >= 0
  if (sum(valid) < 2) return(NA_real_)
  round(as.numeric(DescTools::Gini(x[valid], weights = w[valid], na.rm = TRUE)), 4)
}

# ==============================================================================
# 1. DICIONÁRIO OFICIAL DA COD (Estrutura_Ocupacao_COD.xlsx)
#    10 Grandes Grupos (1º dígito) e 43 Subgrupos Principais (2 dígitos)
# ==============================================================================
dic_grande_grupo <- data.table(
  cod_gg = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "0"),
  grande_grupo_cod = c(
    "1 - Diretores e gerentes",
    "2 - Profissionais das ciências e intelectuais",
    "3 - Técnicos e profissionais de nível médio",
    "4 - Trabalhadores de apoio administrativo",
    "5 - Trabalhadores dos serviços, vendedores dos comércios e mercados",
    "6 - Trabalhadores qualificados da agropecuária, florestais, da caça e da pesca",
    "7 - Trabalhadores qualificados, operários e artesãos da construção, das artes mecânicas e outros ofícios",
    "8 - Operadores de instalações e máquinas e montadores",
    "9 - Ocupações elementares",
    "0 - Membros das forças armadas, policiais e bombeiros militares"
  )
)

dic_subgrupo_principal <- data.table(
  cod_sg = c(
    "11", "12", "13", "14",
    "21", "22", "23", "24", "25", "26",
    "31", "32", "33", "34", "35",
    "41", "42", "43", "44",
    "51", "52", "53", "54",
    "61", "62",
    "71", "72", "73", "74", "75",
    "81", "82", "83",
    "91", "92", "93", "94", "95", "96",
    "01", "02", "04", "05"
  ),
  subgrupo_principal_cod = c(
    "11 - Diretores executivos, dirigentes da administração pública e membros do poder executivo e legislativo",
    "12 - Dirigentes administrativos e comerciais",
    "13 - Dirigentes e gerentes de produção e operação",
    "14 - Gerentes de hotéis, restaurantes, comércios e outros serviços",
    "21 - Profissionais das ciências e da engenharia",
    "22 - Profissionais da saúde",
    "23 - Profissionais do ensino",
    "24 - Especialistas em organização da administração pública e de empresas",
    "25 - Profissionais de tecnologias da informação e comunicações",
    "26 - Profissionais em direito, em ciências sociais e culturais",
    "31 - Profissionais de nível médio das ciências e da engenharia",
    "32 - Profissionais de nível médio da saúde e afins",
    "33 - Profissionais de nível médio em operações financeiras e administrativas",
    "34 - Profissionais de nível médio de serviços jurídicos, sociais, culturais e afins",
    "35 - Técnicos de nível médio da tecnologia da informação e das comunicações",
    "41 - Escriturários",
    "42 - Trabalhadores de atendimento direto ao público",
    "43 - Trabalhadores de cálculos numéricos e encarregados do registro de materiais",
    "44 - Outros trabalhadores de apoio administrativo",
    "51 - Trabalhadores dos serviços pessoais",
    "52 - Vendedores",
    "53 - Trabalhadores dos cuidados pessoais",
    "54 - Trabalhadores dos serviços de proteção e segurança",
    "61 - Agricultores e trabalhadores qualificados da agropecuária",
    "62 - Trabalhadores florestais qualificados, pescadores e caçadores",
    "71 - Trabalhadores qualificados e operários da construção exclusive eletricistas",
    "72 - Trabalhadores qualificados e operários da metalurgia, da construção mecânica e afins",
    "73 - Artesãos e operários das artes gráficas",
    "74 - Trabalhadores especializados em eletricidade e eletrônica",
    "75 - Operários e oficiais de processamento de alimentos, da madeira, da confecção e afins",
    "81 - Operadores de instalações fixas e máquinas",
    "82 - Montadores",
    "83 - Condutores de veículos e operadores de equipamentos móveis pesados",
    "91 - Trabalhadores domésticos e outros trabalhadores de limpeza de interior de edifícios",
    "92 - Trabalhadores elementares da agropecuária, da pesca e florestais",
    "93 - Trabalhadores elementares da mineração, da construção, da indústria de transformação e do transporte",
    "94 - Ajudantes de preparação de alimentos",
    "95 - Trabalhadores ambulantes dos serviços e afins",
    "96 - Coletores de lixo e outras ocupações elementares",
    "01 - Oficiais das forças armadas",
    "02 - Graduados e praças das forças armadas",
    "04 - Policiais militares",
    "05 - Bombeiros militares"
  )
)

# ==============================================================================
# 2. DOWNLOAD / LEITURA DA PNAD CONTÍNUA 3T/2026 (INCLUINDO VD4011 E V4010)
# ==============================================================================
vars_pnad <- c(
  "UF", "Capital", "RM_RIDE", "V1028",
  "V2001", "V2005", "V2007", "V2009", "V2010",
  "V4009", "V4010", "V4012", "V4013", "V4014",
  "V4025", "V4028", "V4029", "V4032", "V4039",
  "V4040", "V4041", "VD3004", "VD4011", "VD4016"
)

if (file.exists("raw_pnadC3t2026.rds")) {
  raw_pnad3t2026 <- readRDS("raw_pnadC3t2026.rds")
  if (!("VD4011" %in% names(raw_pnad3t2026))) {
    tmp_vd4011 <- get_pnadc(
      year = 2026, quarter = 3,
      vars = c("VD4011"),
      defyear = 2026, labels = TRUE, design = FALSE
    )
    raw_pnad3t2026$VD4011 <- tmp_vd4011$VD4011
    rm(tmp_vd4011)
    saveRDS(raw_pnad3t2026, file = "raw_pnadC3t2026.rds")
  }
} else {
  raw_pnad3t2026 <- get_pnadc(
    year = 2026, quarter = 3,
    vars = vars_pnad,
    defyear = 2026, labels = TRUE, design = FALSE
  )
  saveRDS(raw_pnad3t2026, file = "raw_pnadC3t2026.rds")
}

dt <- as.data.table(raw_pnad3t2026)
rm(raw_pnad3t2026)
gc()

# Filtro: 18+ anos (V2009 >= 18), renda válida > 0 (VD4016) e peso V1028 > 0
# Preserva TODOS os grupos raciais (inclusive Amarela, Indígena e Ignorado) para "Todos"
dt <- dt[V2009 >= 18 & !is.na(VD4016) & VD4016 > 0 & !is.na(V1028) & V1028 > 0]

# Grupos específicos de Gênero/Raça
dt[, grupo_especifico := fcase(
  V2007 == "Homem"  & V2010 == "Branca",              "Homem Branco",
  V2007 == "Mulher" & V2010 == "Branca",              "Mulher Branca",
  V2007 == "Homem"  & V2010 %in% c("Preta", "Parda"), "Homem Negro",
  V2007 == "Mulher" & V2010 %in% c("Preta", "Parda"), "Mulher Negra",
  default = NA_character_
)]

# Escolaridade (VD3004)
dt[, escolaridade := fcase(
  VD3004 == "Sem instrução e menos de 1 ano de estudo", "Sem instrução",
  VD3004 == "Fundamental incompleto ou equivalente",    "Fundamental Incompleto",
  VD3004 == "Fundamental completo ou equivalente",      "Fundamental Completo",
  VD3004 == "Médio incompleto ou equivalente",          "Médio Incompleto",
  VD3004 == "Médio completo ou equivalente",            "Médio Completo",
  VD3004 == "Superior incompleto ou equivalente",       "Superior Incompleto",
  VD3004 == "Superior completo",                        "Superior Completo",
  default = NA_character_
)]

# ==============================================================================
# 3. MAPEAMENTO DE VD4011 E V4010 PARA GRANDE GRUPO E SUBGRUPO PRINCIPAL DA COD
# ==============================================================================
# Na PNAD Contínua:
# - V4010 traz o código de 4 dígitos do Grupo de Base da COD (ex: "1112", "0411", etc.)
# - VD4011 traz o agrupamento ocupacional derivado (11 categorias/grandes grupos)
# Usando V4010 (4 dígitos) + fallback em VD4011, obtemos com precisão exata tanto
# os 10 Grandes Grupos (1º dígito) quanto todos os 43 Subgrupos Principais (2 primeiros dígitos)!

dt[, v4010_pad := fifelse(
  !is.na(V4010) & trimws(as.character(V4010)) != "",
  sprintf("%04d", suppressWarnings(as.integer(gsub("[^0-9]", "", as.character(V4010))))),
  NA_character_
)]

dt[, cod_gg := substr(v4010_pad, 1, 1)]
dt[, cod_sg := substr(v4010_pad, 1, 2)]

# Fallback caso algum registro não tenha V4010 preenchido mas tenha VD4011
dt[is.na(cod_gg) & !is.na(VD4011), cod_gg := fcase(
  grepl("Diretores", VD4011, ignore.case = TRUE), "1",
  grepl("ciências e intelectuais", VD4011, ignore.case = TRUE), "2",
  grepl("nível médio", VD4011, ignore.case = TRUE), "3",
  grepl("apoio administrativo", VD4011, ignore.case = TRUE), "4",
  grepl("serviços, vendedores", VD4011, ignore.case = TRUE), "5",
  grepl("agropecuária", VD4011, ignore.case = TRUE), "6",
  grepl("construção|artes mecânicas", VD4011, ignore.case = TRUE), "7",
  grepl("Operadores de instalações", VD4011, ignore.case = TRUE), "8",
  grepl("elementares", VD4011, ignore.case = TRUE), "9",
  grepl("forças armadas|policiais", VD4011, ignore.case = TRUE), "0",
  default = NA_character_
)]

dt <- merge(dt, dic_grande_grupo, by = "cod_gg", all.x = TRUE)
dt <- merge(dt, dic_subgrupo_principal, by = "cod_sg", all.x = TRUE)

# Também mantemos a categoria original de VD4011 para referência
dt[, vd4011_original := trimws(as.character(VD4011))]

niveis_grupos <- c("Homem Branco", "Mulher Branca", "Homem Negro", "Mulher Negra", "Todos")
niveis_esc <- c(
  "Sem instrução", "Fundamental Incompleto", "Fundamental Completo",
  "Médio Incompleto", "Médio Completo", "Superior Incompleto", "Superior Completo"
)

# ==============================================================================
# 4. EMPILHAMENTO TERRITORIAL (BRASIL, UFs, CAPITAIS E RMs) + 5 GRUPOS
# ==============================================================================
montar_territorios <- function(base_dt) {
  cols_keep <- c(
    "V1028", "VD4016", "grupo_especifico", "escolaridade",
    "grande_grupo_cod", "subgrupo_principal_cod", "vd4011_original"
  )
  
  lista_terr <- list(
    copy(base_dt)[, c(list(Nivel_Territorial = "Brasil", Territorio = "Brasil"), .SD), .SDcols = cols_keep],
    copy(base_dt)[!is.na(UF) & as.character(UF) != "", c(list(Nivel_Territorial = "UF", Territorio = as.character(UF)), .SD), .SDcols = cols_keep],
    copy(base_dt)[!is.na(Capital) & as.character(Capital) != "", c(list(Nivel_Territorial = "Capital", Territorio = as.character(Capital)), .SD), .SDcols = cols_keep],
    copy(base_dt)[!is.na(RM_RIDE) & as.character(RM_RIDE) != "", c(list(Nivel_Territorial = "Região Metropolitana", Territorio = gsub("\\s+", " ", as.character(RM_RIDE))), .SD), .SDcols = cols_keep]
  )
  
  # Caso o objeto possua coluna de município (ex.: Municipio ou Município), inclui nível Município
  col_mun <- intersect(c("Municipio", "Município", "MUNICIPIO"), names(base_dt))
  if (length(col_mun) > 0) {
    nm_mun <- col_mun[1]
    t_mun <- copy(base_dt)[!is.na(get(nm_mun)) & as.character(get(nm_mun)) != "",
                           c(list(Nivel_Territorial = "Município", Territorio = as.character(get(nm_mun))), .SD),
                           .SDcols = cols_keep]
    lista_terr[[length(lista_terr) + 1]] <- t_mun
  }
  
  rbindlist(lista_terr, use.names = TRUE)
}

dt_terr <- montar_territorios(dt)

# 4 grupos específicos + "Todos" (sem excluir Amarela, Indígena e Ignorado)
dt_4g    <- dt_terr[!is.na(grupo_especifico)][, genero_raca := grupo_especifico]
dt_todos <- copy(dt_terr)[, genero_raca := "Todos"]

dt_analise <- rbindlist(list(dt_4g, dt_todos), use.names = TRUE)
dt_analise[, genero_raca := factor(genero_raca, levels = niveis_grupos)]

rm(dt_terr, dt_4g, dt_todos)
gc()

# ==============================================================================
# 5. TABULAÇÃO COM MÉDIA, MEDIANA, DECIS E GINI (VIA PACOTE DescTools)
# ==============================================================================
probs_dist <- c(0.10, 0.20, 0.25, 0.30, 0.40, 0.50, 0.60, 0.70, 0.75, 0.80, 0.90)
# Pontos extras (P5, P95, P99) usados pela calculadora de posicionamento para cobrir as pontas da distribuição
probs_extra <- c(0.05, 0.95, 0.99)

calc_metricas_e_gini <- function(dados, group_cols, extras = FALSE) {
  dados[, {
    qs <- w_quantile(VD4016, V1028, probs = c(probs_dist, probs_extra))
    res <- list(
      Pop_Ocupada_18mais = round(sum(V1028), 0),
      Amostra_N          = .N,
      Media              = round(w_mean(VD4016, V1028), 2),
      Gini               = calc_gini_pacote(VD4016, V1028),
      D1_P10             = round(qs[1], 2),
      D2_P20             = round(qs[2], 2),
      P25                = round(qs[3], 2),
      D3_P30             = round(qs[4], 2),
      D4_P40             = round(qs[5], 2),
      Mediana_D5         = round(qs[6], 2),
      D6_P60             = round(qs[7], 2),
      D7_P70             = round(qs[8], 2),
      P75                = round(qs[9], 2),
      D8_P80             = round(qs[10], 2),
      D9_P90             = round(qs[11], 2)
    )
    if (extras) res <- c(res, list(P05 = round(qs[12], 2), P95 = round(qs[13], 2), P99 = round(qs[14], 2)))
    res
  }, by = group_cols]
}

# 5A) Geral: Território x Gênero/Raça
tab_geral <- calc_metricas_e_gini(
  dt_analise,
  group_cols = c("Nivel_Territorial", "Territorio", "genero_raca"),
  extras = TRUE
)[order(Nivel_Territorial, Territorio, genero_raca)]

# 5B) Escolaridade: Território x Escolaridade x Gênero/Raça
dt_esc <- dt_analise[!is.na(escolaridade)]
dt_esc[, escolaridade := factor(escolaridade, levels = niveis_esc)]
tab_escolaridade <- calc_metricas_e_gini(
  dt_esc,
  group_cols = c("Nivel_Territorial", "Territorio", "escolaridade", "genero_raca")
)[order(Nivel_Territorial, Territorio, escolaridade, genero_raca)]

# 5C) COD - 10 Grandes Grupos: Território x Grande Grupo COD x Gênero/Raça
dt_gg <- dt_analise[!is.na(grande_grupo_cod)]
tab_grande_grupo <- calc_metricas_e_gini(
  dt_gg,
  group_cols = c("Nivel_Territorial", "Territorio", "grande_grupo_cod", "genero_raca")
)[order(Nivel_Territorial, Territorio, grande_grupo_cod, genero_raca)]

# 5D) COD - 43 Subgrupos Principais: Território x Subgrupo Principal COD x Gênero/Raça
dt_sg <- dt_analise[!is.na(subgrupo_principal_cod)]
tab_subgrupo <- calc_metricas_e_gini(
  dt_sg,
  group_cols = c("Nivel_Territorial", "Territorio", "subgrupo_principal_cod", "genero_raca")
)[order(Nivel_Territorial, Territorio, subgrupo_principal_cod, genero_raca)]

# Exportar planilhas CSV tabuladas
fwrite(tab_geral,        "pnad_3t2026_geral_genero_raca_gini.csv", sep = ";", dec = ",", bom = TRUE)
fwrite(tab_escolaridade, "pnad_3t2026_escolaridade_genero_raca_gini.csv", sep = ";", dec = ",", bom = TRUE)
fwrite(tab_grande_grupo, "pnad_3t2026_cod_10_grandes_grupos_gini.csv", sep = ";", dec = ",", bom = TRUE)
fwrite(tab_subgrupo,     "pnad_3t2026_cod_subgrupos_principais_gini.csv", sep = ";", dec = ",", bom = TRUE)

cat("Tabelas CSV exportadas com sucesso!\n")

# ==============================================================================
# 6. PREPARAÇÃO DOS DADOS PARA O PAINEL WEB (index.html) COM MAPA DO BRASIL
#    - Mapa interativo com Estados (UFs), Capitais e Regiões Metropolitanas clicáveis
#    - Seletor entre Renda Média e Renda Mediana no Mapa
#    - Filtro COD pelos 10 Grandes Grupos E pelos 43 Subgrupos Principais
# ==============================================================================
# Prepara dados geográficos (polígonos simplificados dos estados + coordenadas de capitais/RMs)
# Caso tenha o pacote geobr e sf instalados, baixa a malha oficial simplificada do IBGE;
# caso contrário, usa GeoJSON público leve do IBGE/GitHub diretamente no navegador.

# ==============================================================================
# 5.5 MALHAS GEOGRÁFICAS (IBGE via pacote geobr): UFs, Capitais e RMs/RIDEs
#     - Estados:  geobr::read_state()
#     - Capitais: geobr::read_municipality() filtrado pelos 27 códigos IBGE
#     - RMs:      geobr::read_metro_area() (união dos municípios de cada recorte)
#     Os polígonos são simplificados e embutidos no index.html (sem download no navegador).
#     Instale uma vez: install.packages(c("geobr", "sf"))
# ==============================================================================
library(sf)
suppressWarnings(sf_use_s2(FALSE))

baixar_malhas <- function() {
  rm_sf <- NULL
  for (ano in c(2024, 2022, 2018)) {   # usa o recorte metropolitano mais recente disponível
    rm_sf <- tryCatch(geobr::read_metro_area(year = ano, showProgress = FALSE), error = function(e) NULL)
    if (!is.null(rm_sf) && nrow(rm_sf) > 0) break
  }
  list(
    uf  = geobr::read_state(year = 2020, showProgress = FALSE),
    mun = geobr::read_municipality(code_muni = "all", year = 2020, showProgress = FALSE),
    rm  = rm_sf
  )
}
malhas <- baixar_malhas()

capitais <- data.table(
  chave = c("Porto Velho","Rio Branco","Manaus","Boa Vista","Belém","Macapá","Palmas","São Luís","Teresina",
            "Fortaleza","Natal","João Pessoa","Recife","Maceió","Aracaju","Salvador","Belo Horizonte","Vitória",
            "Rio de Janeiro","São Paulo","Curitiba","Florianópolis","Porto Alegre","Campo Grande","Cuiabá",
            "Goiânia","Brasília"),
  code_muni = c(1100205,1200401,1302603,1400100,1501402,1600303,1721000,2111300,2211001,
                2304400,2408102,2507507,2611606,2704302,2800308,2927408,3106200,3205309,
                3304557,3550308,4106902,4205407,4314902,5002704,5103403,5208707,5300108)
)
# Identifica a capital citada no nome do território (vale para "Município de X", "Região Metropolitana de X"...)
achar_capital <- function(txt) {
  hit <- capitais[vapply(chave, function(k) grepl(k, txt, fixed = TRUE), logical(1))]
  if (nrow(hit) == 0) NULL else hit[1]
}
terr_nivel <- function(nv) sort(unique(gsub("\\s+", " ", trimws(as.character(tab_geral[Nivel_Territorial == nv, Territorio])))))

# --- UFs ---
nomes_uf <- terr_nivel("UF")
g_uf <- malhas$uf[malhas$uf$name_state %in% nomes_uf, ]
g_uf <- st_sf(nome = as.character(g_uf$name_state), geometry = st_geometry(g_uf))

# --- Capitais (polígono do município) ---
cap_terr <- terr_nivel("Capital")
cap_cod  <- vapply(cap_terr, function(t) { h <- achar_capital(t); if (is.null(h)) NA_real_ else h$code_muni }, numeric(1))
idx_cap  <- match(cap_cod, as.numeric(malhas$mun$code_muni))
g_cap    <- st_sf(nome = cap_terr[!is.na(idx_cap)], geometry = st_geometry(malhas$mun)[idx_cap[!is.na(idx_cap)]])

# --- RMs/RIDEs: união dos municípios do recorte que contém a capital ---
rm_terr <- terr_nivel("Região Metropolitana")
lista_rm <- lapply(rm_terr, function(t) {
  h <- achar_capital(t); if (is.null(h)) return(NULL)
  grp <- unique(malhas$rm$name_metro[as.numeric(malhas$rm$code_muni) == h$code_muni])
  if (length(grp) == 0) return(NULL)
  geom <- suppressMessages(suppressWarnings(st_union(st_make_valid(st_geometry(malhas$rm[malhas$rm$name_metro == grp[1], ])))))
  st_sf(nome = t, geometry = geom)
})
lista_rm <- Filter(Negate(is.null), lista_rm)
g_rm <- if (length(lista_rm)) do.call(rbind, lista_rm) else NULL

# Converte para GeoJSON enxuto (WGS84, 3 casas decimais) e guarda um ponto interno (lon/lat) para os marcadores
para_geojson <- function(g, tol) {
  if (is.null(g) || nrow(g) == 0) return('{"type":"FeatureCollection","features":[]}')
  geom <- st_transform(st_geometry(g), 4326)
  geom <- suppressWarnings(st_simplify(st_make_valid(geom), dTolerance = tol, preserveTopology = TRUE))
  # st_make_valid/st_union podem devolver GEOMETRYCOLLECTION (que o Plotly não desenha): mantém só os polígonos
  geom <- suppressWarnings(st_cast(st_collection_extract(geom, "POLYGON"), "MULTIPOLYGON"))
  pts  <- suppressWarnings(st_coordinates(st_point_on_surface(geom)))
  out  <- st_sf(nome = g$nome, lon = round(pts[, 1], 3), lat = round(pts[, 2], 3), geometry = geom)
  tmp  <- tempfile(fileext = ".geojson")
  st_write(out, tmp, driver = "GeoJSON", delete_dsn = TRUE, quiet = TRUE)
  txt <- paste(readLines(tmp, warn = FALSE, encoding = "UTF-8"), collapse = "")
  # 3 casas decimais (~100 m) feitas no texto: o COORDINATE_PRECISION do GDAL degenera ilhas minúsculas em GeometryCollection
  gsub("(-?[0-9]+\\.[0-9]{3})[0-9]+", "\\1", txt, perl = TRUE)
}
json_geo_uf  <- para_geojson(g_uf,  0.02)
json_geo_cap <- para_geojson(g_cap, 0.004)
json_geo_rm  <- para_geojson(g_rm,  0.02)

cat(sprintf("Malhas: %d/%d UFs | %d/%d capitais | %d/%d RMs\n",
            nrow(g_uf), length(nomes_uf), nrow(g_cap), length(cap_terr), if (is.null(g_rm)) 0L else nrow(g_rm), length(rm_terr)))
faltam <- c(setdiff(nomes_uf, g_uf$nome), setdiff(cap_terr, g_cap$nome), setdiff(rm_terr, if (is.null(g_rm)) character() else g_rm$nome))
if (length(faltam)) warning("Territórios sem polígono (ficarão sem cor no mapa): ", paste(faltam, collapse = "; "))


json_geral <- toJSON(tab_geral, dataframe = "rows", auto_unbox = TRUE)
json_esc   <- toJSON(tab_escolaridade, dataframe = "rows", auto_unbox = TRUE)
json_gg    <- toJSON(tab_grande_grupo, dataframe = "rows", auto_unbox = TRUE)
json_sg    <- toJSON(tab_subgrupo[Amostra_N >= 3], dataframe = "rows", auto_unbox = TRUE)

# Template em string bruta r"-----( ... )-----": o conteúdo é copiado literalmente (sem escapar aspas ou barras).
html_template <- r"-----(
<!DOCTYPE html>
<html lang="pt-BR">
<head>
<meta charset="UTF-8" />
<meta name="viewport" content="width=device-width, initial-scale=1, viewport-fit=cover" />
<meta name="theme-color" content="#4E6258" />
<title>Dados de Renda e Desigualdade no Mercado de Trabalho Brasileiro</title>
<link rel="preconnect" href="https://fonts.googleapis.com" />
<link rel="preconnect" href="https://fonts.gstatic.com" crossorigin />
<link href="https://fonts.googleapis.com/css2?family=Roboto:wght@400;500;700&display=swap" rel="stylesheet" />
<script src="https://cdn.plot.ly/plotly-2.32.0.min.js"></script>
<style>
:root{
  --primary:#4E6258; --on-primary:#fff; --primary-container:#D2E8DA; --on-primary-container:#0B2016;
  --bg:#F6F8F5; --card:#fff; --on-surface:#191C1A; --on-variant:#414942; --outline:#717973; --outline-var:#C1C9C2;
  --r-sm:8px; --r-md:12px; --r-lg:16px;
  --e1:0 1px 2px rgba(0,0,0,.18),0 1px 3px 1px rgba(0,0,0,.08);
  --e2:0 2px 6px 2px rgba(0,0,0,.1),0 1px 2px rgba(0,0,0,.2);
  --gap:16px;
}
*{box-sizing:border-box}
html{-webkit-text-size-adjust:100%}
body{margin:0;background:var(--bg);color:var(--on-surface);font:400 16px/1.5 Roboto,system-ui,-apple-system,"Segoe UI",sans-serif;
  padding-left:env(safe-area-inset-left);padding-right:env(safe-area-inset-right)}
button,select{font:inherit;color:inherit}
:focus-visible{outline:3px solid #2F6FED;outline-offset:2px}
/* Header: app bar grande (Material 3) com gradiente, ilustração em selo e abas primárias sobre superfície clara */
.appbar{position:relative;isolation:isolate;overflow:hidden;color:var(--on-primary);padding-top:env(safe-area-inset-top);
  background:linear-gradient(135deg,#2E3E36 0%,#4E6258 52%,#6A8474 100%)}
.appbar::before{content:"";position:absolute;inset:0;z-index:-1;pointer-events:none;
  background:radial-gradient(circle 240px at 92% 35%,rgba(255,255,255,.22),transparent 70%),radial-gradient(circle 200px at 3% 115%,rgba(255,255,255,.10),transparent 70%)}
.appbar::after{content:"";position:absolute;z-index:-1;pointer-events:none;right:-70px;top:-90px;width:280px;height:280px;border-radius:50%;border:1px solid rgba(255,255,255,.16)}
.appbar-inner{max-width:1280px;margin:0 auto;padding:28px 16px 32px;display:flex;align-items:center;justify-content:space-between;gap:24px}
.appbar-text{min-width:0}
.appbar h1{margin:0;font-size:clamp(1.375rem,1rem + 1.6vw,2.125rem);line-height:1.2;font-weight:500;letter-spacing:-.01em;text-wrap:balance}
.appbar p{margin:8px 0 0;max-width:62ch;font-size:clamp(.875rem,.8rem + .3vw,1.0625rem);line-height:1.4;color:rgba(255,255,255,.86)}
.brand-badge{flex:none;display:grid;place-items:center;width:clamp(72px,9vw,112px);aspect-ratio:1;border-radius:28px;background:rgba(255,255,255,.94);
  box-shadow:0 8px 24px rgba(0,0,0,.24),inset 0 0 0 1px rgba(255,255,255,.7)}
.brand-badge:empty{display:none}
.brand-badge img{display:block;width:80%;height:auto}
.tabs{position:sticky;top:0;z-index:20;display:flex;overflow-x:auto;background:#fff;box-shadow:0 1px 0 var(--outline-var),var(--e1);scrollbar-width:none}
.tabs::-webkit-scrollbar{display:none}
.tab{position:relative;flex:1 0 auto;white-space:nowrap;min-height:52px;padding:0 20px;border:0;background:transparent;color:var(--on-variant);
  font-weight:500;font-size:.9375rem;letter-spacing:.1px;cursor:pointer;transition:background .15s,color .15s}
.tab::after{content:"";position:absolute;left:20px;right:20px;bottom:0;height:3px;border-radius:3px 3px 0 0;background:var(--primary);transform:scaleX(0);transition:transform .2s}
.tab[aria-selected=true]{color:var(--primary)}
.tab[aria-selected=true]::after{transform:scaleX(1)}
.tab:hover{background:rgba(78,98,88,.08)}
main{max-width:1280px;margin:0 auto;padding:var(--gap)}
.card{background:var(--card);border-radius:var(--r-lg);box-shadow:var(--e1);padding:16px;margin-bottom:var(--gap)}
.card h2{margin:0;font-size:1.0625rem;font-weight:500;color:var(--primary)}
.card-head{display:flex;flex-wrap:wrap;gap:12px;align-items:center;justify-content:space-between;margin-bottom:8px}
.hint{margin:0 0 8px;font-size:.8125rem;color:var(--on-variant)}
details.filters{background:var(--card);border-radius:var(--r-lg);box-shadow:var(--e1);margin-bottom:var(--gap)}
details.filters>summary{list-style:none;display:flex;align-items:center;gap:12px;min-height:56px;padding:8px 16px;cursor:pointer;font-weight:500}
details.filters>summary::-webkit-details-marker{display:none}
details.filters>summary .sel{flex:1;font-weight:400;color:var(--on-variant);font-size:.875rem;overflow:hidden;text-overflow:ellipsis;white-space:nowrap}
details.filters>summary::after{content:"";width:10px;height:10px;border-right:2px solid var(--on-variant);border-bottom:2px solid var(--on-variant);transform:rotate(45deg);transition:transform .2s}
details.filters[open]>summary::after{transform:rotate(-135deg)}
.filters-body{display:grid;grid-template-columns:repeat(auto-fit,minmax(220px,1fr));gap:16px;padding:4px 16px 16px}
.field label{display:block;font-size:.75rem;font-weight:500;letter-spacing:.4px;text-transform:uppercase;color:var(--on-variant);margin-bottom:4px}
.field label{white-space:nowrap;overflow:hidden;text-overflow:ellipsis}
#subFiltroBox{grid-column:1/-1}
.field select{width:100%;min-height:48px;padding:8px 12px;font-size:16px;background:#fff;border:1px solid var(--outline);border-radius:var(--r-sm)}
.field select:hover{border-color:var(--on-surface)}
.btn-text{min-height:48px;padding:0 16px;border:0;background:transparent;color:var(--primary);font-weight:500;border-radius:100px;cursor:pointer}
.btn-text:hover{background:rgba(78,98,88,.1)}
.btn-tonal{min-height:48px;padding:0 20px;border:0;background:var(--primary-container);color:var(--on-primary-container);font-weight:500;border-radius:100px;cursor:pointer}
.filters-actions{grid-column:1/-1;display:flex;justify-content:flex-end}
.seg{display:inline-flex;border:1px solid var(--outline);border-radius:100px;overflow:hidden}
.seg button{min-height:40px;padding:0 16px;border:0;border-right:1px solid var(--outline);background:transparent;cursor:pointer;font-size:.875rem;font-weight:500}
.seg button:last-child{border-right:0}
.seg button[aria-pressed=true]{background:var(--primary-container);color:var(--on-primary-container)}
.seg button[aria-pressed=true]::before{content:"\2713\00a0"}
.controls{display:flex;flex-wrap:wrap;gap:8px 16px;margin-bottom:8px}
.grid2{display:grid;grid-template-columns:minmax(0,1.3fr) minmax(0,1fr);gap:var(--gap)}
.plot{width:100%}
.plot-map{height:clamp(340px,62vh,580px)}
.plot-gini{height:300px}
.plot-decil{height:380px}
.js-plotly-plot .draglayer,.js-plotly-plot .nsewdrag{touch-action:pan-y!important}
.resumo{margin-top:8px;padding:12px 16px;border-radius:var(--r-md);background:var(--primary-container);color:var(--on-primary-container);font-size:.9375rem}
.resumo b{font-weight:700}
.kpis{display:grid;grid-template-columns:repeat(2,minmax(0,1fr));gap:12px;margin-bottom:var(--gap)}
.kpis .kpi:last-child:nth-child(odd){grid-column:1/-1}
.kpi{border-radius:var(--r-md);padding:14px 16px;color:#fff;box-shadow:var(--e1)}
.kpi .t{font-size:.75rem;font-weight:500;text-transform:uppercase;letter-spacing:.4px}
.kpi-vals{display:flex;margin:8px 0 10px}
.kv{flex:1;min-width:0;padding-right:12px}
.kv+.kv{padding:0 0 0 12px;border-left:1px solid rgba(255,255,255,.38)}
.kv .v{font-size:1.3rem;font-weight:700;line-height:1.2;white-space:nowrap}
.kv .l{margin-top:2px;font-size:.75rem;font-weight:500;letter-spacing:.4px;text-transform:uppercase;opacity:.92}
.kpi .g{display:inline-block;font-size:.8125rem;font-weight:500;background:rgba(0,0,0,.25);padding:2px 10px;border-radius:100px}
@media (min-width:1100px){.kpis{grid-template-columns:repeat(5,minmax(0,1fr))}.kpis .kpi:last-child:nth-child(odd){grid-column:auto}.kv .v{font-size:clamp(.95rem,.3rem + .75vw,1.35rem)}}
@media (max-width:599px){.kpis{grid-template-columns:1fr}}
.table-wrap{overflow-x:auto;-webkit-overflow-scrolling:touch}
table{width:100%;border-collapse:collapse;font-size:.875rem;min-width:620px}
th,td{padding:12px;text-align:right;border-bottom:1px solid var(--outline-var);white-space:nowrap}
th{font-size:.75rem;text-transform:uppercase;letter-spacing:.4px;color:var(--on-variant);background:#F1F4F0}
th:first-child,td:first-child{text-align:left;position:sticky;left:0;background:#fff;font-weight:500}
th:first-child{background:#F1F4F0}
.notice{margin:0;padding:24px;text-align:center;color:var(--on-variant)}
footer{max-width:1280px;margin:0 auto;padding:8px 16px calc(24px + env(safe-area-inset-bottom));font-size:.8125rem;color:var(--on-variant)}
[hidden]{display:none!important}

.cmp-controls{display:grid;grid-template-columns:repeat(4,minmax(0,1fr));gap:16px;margin:12px 0 16px}
#cmpBox5{grid-column:span 2}
.cmp-results{display:grid;grid-template-columns:repeat(2,minmax(0,1fr));gap:var(--gap);margin-bottom:var(--gap)}
.panel{border:1px solid var(--outline-var);border-radius:var(--r-md);padding:12px 16px;min-width:0}
.panel h3{margin:0 0 8px;font-size:.9375rem;font-weight:500;color:var(--on-variant)}
.plot-calc{height:300px}.plot-cmp{height:380px}
.big{font-size:2.5rem;line-height:1.1;font-weight:700;color:var(--primary)}
.big-sub{font-size:.875rem;color:var(--on-variant);margin-bottom:4px}
.pos-text p{margin:8px 0}
.affix{display:flex;align-items:center;min-height:48px;border:1px solid var(--outline);border-radius:var(--r-sm);background:#fff;padding:0 12px}
.affix:focus-within{outline:3px solid #2F6FED;outline-offset:2px}
.affix span{color:var(--on-variant);margin-right:8px}
.affix input{flex:1;min-width:0;border:0;outline:0;font-size:16px;background:transparent;min-height:46px;font-family:inherit}
.chips-box{margin-bottom:12px}
.chips-box .lbl{font-size:.75rem;font-weight:500;letter-spacing:.4px;text-transform:uppercase;color:var(--on-variant);margin-bottom:6px}
.chips-head{display:flex;justify-content:space-between;align-items:baseline;gap:12px;margin-bottom:4px}
.chips-head .lbl{margin:0}
.contador{font-size:.8125rem;font-weight:500;color:var(--on-variant);white-space:nowrap}
.chips-tools{display:flex;flex-wrap:wrap;align-items:center;gap:4px 12px;margin-bottom:8px}
.busca{flex:1 1 260px}
.busca input{width:100%;min-height:40px;padding:0 16px;border:1px solid var(--outline);border-radius:100px;font-size:16px;background:#fff;font-family:inherit}
.btn-sm{min-height:40px;padding:0 12px;font-size:.875rem}
.chips{display:grid;grid-template-columns:repeat(auto-fill,minmax(270px,1fr));gap:8px;max-height:300px;overflow-y:auto;padding:2px 6px 2px 2px}
.chips.compact{grid-template-columns:repeat(auto-fill,minmax(190px,1fr));max-height:none;overflow:visible}
.chip{display:flex;align-items:center;gap:10px;width:100%;min-height:44px;padding:8px 12px;border:1px solid var(--outline);border-radius:var(--r-sm);background:#fff;cursor:pointer;font-size:.875rem;line-height:1.3;text-align:left}
.chip::before{content:"";flex:none;width:1em;text-align:center;font-weight:700}
.chip:hover{background:#F1F4F0}
.chip[aria-pressed=true]{background:var(--primary-container);color:var(--on-primary-container);border-color:transparent}
.chip[aria-pressed=true]::before{content:"\2713"}
.chips-vazio{grid-column:1/-1;margin:8px 0;color:var(--on-variant);font-size:.875rem}
.chip .dot{width:10px;height:10px;border-radius:50%;flex:none}
.note{margin:8px 0 0;font-size:.8125rem;color:var(--on-variant)}
.stack{display:grid;gap:18px}
.stack-row{display:grid;grid-template-columns:150px minmax(0,1fr);gap:6px 16px;align-items:start}
.stack-row.ref{padding-bottom:18px;border-bottom:1px dashed var(--outline-var)}
.stack-label{padding-top:12px;font-size:.875rem;font-weight:500;line-height:1.3}
.stack-label small{display:block;margin-top:2px;font-size:.75rem;font-weight:400;color:var(--on-variant)}
.stack-label .dot-cat{display:inline-block;width:10px;height:10px;border-radius:50%;margin-right:8px}
.stack-bar{display:flex;gap:2px;height:44px;border-radius:0;overflow:hidden}
.stack-seg{display:flex;align-items:center;justify-content:center;min-width:0;overflow:hidden;border:0;border-radius:0;font-size:.8125rem;font-weight:700;white-space:nowrap}
.stack-legend{display:flex;flex-wrap:wrap;gap:6px 16px;margin:8px 0 0;padding:0;list-style:none;font-size:.8125rem;color:var(--on-variant)}
.stack-legend.top{margin:0}
.stack-legend li{display:inline-flex;align-items:center;gap:6px;max-width:100%}
.stack-legend i{width:10px;height:10px;border-radius:50%;flex:none}
.stack-legend b{color:var(--on-surface);font-weight:500}
.stack-vazio{margin:0;padding:12px 16px;border-radius:var(--r-md);background:#F1F4F0;color:var(--on-variant);font-size:.875rem}
#panelComp{margin-bottom:var(--gap)}
#compSub{margin:0 0 16px}
.cmp-card table{min-width:520px}
@media (max-width:959px){.cmp-controls{grid-template-columns:repeat(2,minmax(0,1fr))}.cmp-results{grid-template-columns:1fr}}
@media (max-width:599px){.stack-row{grid-template-columns:1fr}.stack-row>div:empty{display:none}.stack-label{padding-top:0}.stack-bar{height:40px}.appbar-inner{padding:20px 12px 24px;gap:12px}.brand-badge{width:64px;border-radius:20px}.tab{padding:0 16px}.chips{grid-template-columns:1fr;max-height:260px}.chips.compact{grid-template-columns:repeat(2,minmax(0,1fr))}.cmp-controls{grid-template-columns:1fr}#cmpBox5{grid-column:auto}.big{font-size:2rem}}
@media (max-width:959px){.grid2{grid-template-columns:1fr}}
@media (max-width:599px){
  :root{--gap:12px}
  main{padding:12px}
  .card{padding:12px;border-radius:var(--r-md)}
  .plot-map{height:min(78vh,520px)}
  .seg button{padding:0 12px}
}
@media (prefers-reduced-motion:reduce){*{transition:none!important}}
</style>
</head>
<body>
<header class="appbar">
  <div class="appbar-inner">
    <div class="appbar-text">
      <h1>Dados de Renda e Desigualdade no Mercado de Trabalho Brasileiro</h1>
      <p>Dados da PNADc IBGE 3T2026 · Rendimento habitual do trabalho principal</p>
    </div>
    <div class="brand-badge" aria-hidden="true">__LOGO_HTML__</div>
  </div>
</header>
<div class="tabs" role="tablist" aria-label="Modo de visualização">
  <button class="tab" role="tab" id="tabApr" aria-selected="true" aria-controls="viewApresentacao">Apresentação</button>
  <button class="tab" role="tab" id="tabCur" aria-selected="false" aria-controls="viewCurvas">Curvas de distribuição</button>
  <button class="tab" role="tab" id="tabCmp" aria-selected="false" aria-controls="viewComparativo">Comparativo</button>
</div>

<main>
  <noscript><p class="notice">Este painel precisa de JavaScript para exibir os dados.</p></noscript>
  <details class="filters" id="filtros" open>
    <summary><span>Filtros</span><span class="sel" id="resumoFiltros"></span></summary>
    <div class="filters-body">
      <div class="field"><label for="selNivel">Nível territorial</label><select id="selNivel"></select></div>
      <div class="field"><label for="selTerr">Território</label><select id="selTerr"></select></div>
      <div class="field"><label for="selDim">Segmentação</label>
        <select id="selDim">
          <option value="geral">Apenas gênero/raça</option>
          <option value="esc">Escolaridade</option>
          <option value="gg">Grandes grupos de ocupações</option>
          <option value="sg">Subgrupos de ocupações</option>
        </select></div>
      <div class="field" id="subFiltroBox" hidden><label id="subFiltroLabel" for="selSub">Categoria</label><select id="selSub"></select></div>
      <div class="field" id="grupoMapaBox"><label for="selGrupoMapa">Grupo exibido no mapa</label>
        <select id="selGrupoMapa">
          <option value="Todos">Todos</option><option value="Homem Branco">Homem Branco</option>
          <option value="Mulher Branca">Mulher Branca</option><option value="Homem Negro">Homem Negro</option>
          <option value="Mulher Negra">Mulher Negra</option>
        </select></div>
      <div class="filters-actions"><button class="btn-text" id="btnLimpar" type="button">Limpar filtros</button></div>
    </div>
  </details>

  <section id="viewApresentacao" role="tabpanel" aria-labelledby="tabApr">
    <div class="grid2">
      <div class="card">
        <div class="card-head"><h2 id="mapTitle">Mapa da renda</h2><button class="btn-text" id="btnBrasil" type="button" hidden>← Ver Brasil</button></div>
        <div class="controls">
          <div class="seg" role="group" aria-label="Nível do mapa">
            <button type="button" data-nivel="UF" aria-pressed="true">Estados</button>
            <button type="button" data-nivel="Capital" aria-pressed="false">Capitais</button>
            <button type="button" data-nivel="Região Metropolitana" aria-pressed="false">Regiões metropolitanas</button>
          </div>
          <div class="seg" role="group" aria-label="Medida de renda">
            <button type="button" data-metrica="Mediana_D5" aria-pressed="true">Mediana</button>
            <button type="button" data-metrica="Media" aria-pressed="false">Média</button>
          </div>
        </div>
        <p class="hint">Toque ou clique em uma área para selecionar. Em “Capitais” e “RMs”, toque em um estado cinza para ampliá-lo. Quanto mais escuro, maior a renda.</p>
        <div id="chartMap" class="plot plot-map" aria-label="Mapa coroplético da renda"></div>
        <div class="resumo" id="mapaResumo" aria-live="polite"></div>
      </div>
      <div class="card">
        <div class="card-head"><h2>Índice de Gini por grupo</h2></div>
        <p class="hint">0 representa a igualdade máxima e 1, a desigualdade máxima.</p>
        <div id="chartGini" class="plot plot-gini"></div>
      </div>
    </div>
  </section>

  <section id="viewCurvas" role="tabpanel" aria-labelledby="tabCur" hidden>
    <div class="kpis" id="cardsContainer"></div>
    <div class="card"><div class="card-head"><h2>Curva de distribuição por decis</h2></div><div id="chartDecil" class="plot plot-decil"></div></div>
    <div class="card"><div class="card-head"><h2>Média, Gini, mediana e decis</h2></div>
      <div class="table-wrap"><table id="dataTable" aria-label="Média, Gini, mediana e decis por grupo">
        <thead><tr><th>Grupo</th><th>Pop. ocupada</th><th>Média</th><th>Gini</th><th>D1</th><th>Mediana</th><th>D9</th></tr></thead>
        <tbody></tbody></table></div></div>
  </section>
  <section id="viewComparativo" role="tabpanel" aria-labelledby="tabCmp" hidden>
    <div class="card cmp-card" id="cardCalc">
      <div class="card-head"><h2>Calculadora de posicionamento</h2><button class="btn-text" id="btnLimparCalc" type="button">Limpar filtros</button></div>
      <p class="hint">Escolha o território, seu grupo de gênero/raça e informe sua renda mensal do trabalho principal para ver onde você está na distribuição.</p>
      <div class="cmp-controls">
        <div class="field"><label for="cmpNivel">Nível territorial</label><select id="cmpNivel"></select></div>
        <div class="field"><label for="cmpTerr">Território</label><select id="cmpTerr"></select></div>
        <div class="field"><label for="calcGrupo">Gênero/raça</label>
          <select id="calcGrupo"><option>Homem Branco</option><option>Mulher Branca</option><option>Homem Negro</option><option>Mulher Negra</option></select></div>
        <div class="field"><label for="calcRenda">Sua renda mensal</label>
          <div class="affix"><span>R$</span><input id="calcRenda" type="text" inputmode="decimal" autocomplete="off" placeholder="Ex.: 3.500,00" /></div></div>
      </div>
      <p class="resumo" id="calcVazio" aria-live="polite"></p>
      <div id="calcResultado" hidden>
        <div class="cmp-results">
          <div class="panel"><h3>Sua posição</h3><div class="pos-text" id="calcTexto" aria-live="polite"></div></div>
          <div class="panel"><h3>Percentil da sua renda em cada grupo</h3><div id="chartCalc" class="plot plot-calc"></div></div>
        </div>
        <div class="panel"><h3>Comparativo com outros grupos</h3>
          <div class="table-wrap"><table id="calcTabela" aria-label="Posição da renda informada em cada grupo">
            <thead><tr><th>Grupo</th><th>Posição com a sua renda</th><th>Mediana do grupo</th><th>Renda na mesma posição</th></tr></thead><tbody></tbody></table></div></div>
      </div>
      <p class="note">Estimativa por interpolação entre pontos conhecidos da distribuição (decis, quartis e extremos), com margem de alguns pontos percentuais. Considera o rendimento habitual do trabalho principal; não é uma medida oficial do IBGE.</p>
    </div>

    <div class="card cmp-card" id="cardComp">
      <div class="card-head"><h2>Comparador de distribuições</h2><button class="btn-text" id="btnLimparCmp" type="button">Limpar filtros</button></div>
      <p class="hint">Compare curvas de renda (decis) e médias entre grupos de gênero/raça, ou entre categorias de escolaridade e de ocupação.</p>
      <div class="cmp-controls">
        <div class="field"><label for="cmpNivel2">Nível territorial</label><select id="cmpNivel2"></select></div>
        <div class="field"><label for="cmpTerr2">Território</label><select id="cmpTerr2"></select></div>
        <div class="field"><label for="comparaPor">Comparar por</label>
          <select id="comparaPor"><option value="genero">Gênero/raça</option><option value="esc">Escolaridade</option>
            <option value="gg">Grandes grupos de ocupações</option><option value="sg">Subgrupos de ocupações</option></select></div>
        <div class="field"><label for="cmpSel4" id="cmpLabel4">Recorte</label><select id="cmpSel4"></select></div>
        <div class="field" id="cmpBox5"><label for="cmpSel5" id="cmpLabel5">Categoria</label><select id="cmpSel5"></select></div>
      </div>
      <div class="chips-box">
        <div class="chips-head"><div class="lbl" id="cmpChipsTitulo"></div><span class="contador" id="cmpContador" aria-live="polite"></span></div>
        <div class="chips-tools">
          <div class="busca" id="cmpBuscaBox" hidden><input id="cmpBusca" type="search" placeholder="Buscar categoria…" aria-label="Buscar categoria" autocomplete="off" /></div>
          <button class="btn-text btn-sm" id="cmpPadrao" type="button">Restaurar padrão</button>
          <button class="btn-text btn-sm" id="cmpDesmarcar" type="button">Desmarcar tudo</button>
        </div>
        <div class="chips" id="cmpChips" role="group" aria-labelledby="cmpChipsTitulo"></div>
      </div>
      <p class="note" id="cmpAviso" aria-live="polite"></p>
      <p class="resumo" id="cmpVazio" hidden></p>
      <div id="cmpResultado">
        <div class="cmp-results">
          <div class="panel"><h3>Curvas de distribuição (decis)</h3><div id="chartCmpCurvas" class="plot plot-cmp"></div></div>
          <div class="panel"><h3>Renda média</h3><div id="chartCmpMedia" class="plot"></div></div>
        </div>
        <div class="panel" id="panelComp"><h3>Composição dos ocupados por gênero/raça: todos × segmento</h3>
          <p class="note" id="compSub"></p>
          <div class="stack" id="compBarras"></div></div>
        <div class="panel"><h3>Resumo das séries</h3>
          <div class="table-wrap"><table id="cmpTabela" aria-label="Resumo das séries comparadas">
            <thead><tr><th>Série</th><th>Pop. ocupada</th><th>Média</th><th>Mediana</th><th>Gini</th></tr></thead><tbody></tbody></table></div></div>
      </div>
    </div>
  </section>
</main>
<footer>Fonte: PNADc/IBGE 3T2026 (VD4016, VD4011, V4010, VD3004, V2007, V2010, peso V1028). Malhas: IBGE via pacote geobr; a composição municipal das RMs pode diferir da usada pela PNAD.</footer>

<script>
const D_GERAL = __JSON_GERAL__;
const D_ESC   = __JSON_ESC__;
const D_GG    = __JSON_GG__;
const D_SG    = __JSON_SG__;
const GEO = { "UF": __GEO_UF__, "Capital": __GEO_CAP__, "Região Metropolitana": __GEO_RM__ };

const $ = id => document.getElementById(id);
const CORES = {"Homem Branco":"#0F2C59","Mulher Branca":"#D62828","Homem Negro":"#1E7F5C","Mulher Negra":"#C75B0A","Todos":"#3A3A3A"};
const ORDEM_GRUPOS = ["Homem Branco","Mulher Branca","Homem Negro","Mulher Negra","Todos"];
// Escala sequencial (ColorBrewer YlGn): quanto maior o valor, mais escura a cor.
const ESCALA = [[0,"#FFFFCC"],[0.25,"#C2E699"],[0.5,"#78C679"],[0.75,"#238443"],[1,"#004529"]];
const UF_SIGLA = {"Rondônia":"RO","Acre":"AC","Amazonas":"AM","Roraima":"RR","Pará":"PA","Amapá":"AP","Tocantins":"TO","Maranhão":"MA","Piauí":"PI","Ceará":"CE","Rio Grande do Norte":"RN","Paraíba":"PB","Pernambuco":"PE","Alagoas":"AL","Sergipe":"SE","Bahia":"BA","Minas Gerais":"MG","Espírito Santo":"ES","Rio de Janeiro":"RJ","São Paulo":"SP","Paraná":"PR","Santa Catarina":"SC","Rio Grande do Sul":"RS","Mato Grosso do Sul":"MS","Mato Grosso":"MT","Goiás":"GO","Distrito Federal":"DF"};
const SIGLA_UF = Object.fromEntries(Object.entries(UF_SIGLA).map(([k,v]) => [v,k]));

let MODO = "apresentacao", METRICA = "Mediana_D5", NIVEL_MAPA = "UF";

const fmtBRL = v => { if (v == null || isNaN(v)) return "—"; const n = Number(v), d = Math.abs(n - Math.round(n)) > 0.004;
  return "R$ " + n.toLocaleString("pt-BR", {minimumFractionDigits: d ? 2 : 0, maximumFractionDigits: 2}); };
const fmtReais = v => (v == null || isNaN(v)) ? "—" : "R$ " + Math.round(v).toLocaleString("pt-BR");   // cards: reais inteiros
const fmtInt  = v => (v == null || isNaN(v)) ? "—" : Math.round(v).toLocaleString("pt-BR");
const fmtGini = v => (v == null || isNaN(v)) ? "—" : Number(v).toFixed(4).replace(".", ",");
const nomeCurto = n => n.replace(/^(Município de|Região Metropolitana de|Região Administrativa Integrada de Desenvolvimento da)\s+/, "");
const semCodigo = s => String(s).replace(/^\s*\d+\s*[-–]\s*/, "");
const siglaDe = t => { const m = /\(([A-Z]{2})\)\s*$/.exec(t || ""); return m ? m[1] : null; };
const ehMobile = () => window.innerWidth < 600;

// Plotly sem barra de ferramentas, sem zoom/arrasto (não atrapalha a rolagem no celular)
const PLOT_CFG = {responsive:true, displayModeBar:false, displaylogo:false, scrollZoom:false, doubleClick:false, showTips:false};
const plotar = (id, data, layout) => Plotly.newPlot(id, data, Object.assign({
  dragmode:false, paper_bgcolor:"rgba(0,0,0,0)", plot_bgcolor:"rgba(0,0,0,0)",
  font:{family:"Roboto, system-ui, sans-serif", color:"#191C1A", size: ehMobile() ? 12 : 13}
}, layout), PLOT_CFG);

const selNivel = $("selNivel"), selTerr = $("selTerr"), selDim = $("selDim"), selSub = $("selSub"), selGrupoMapa = $("selGrupoMapa");
const niveisDisponiveis = [...new Set(D_GERAL.map(d => d.Nivel_Territorial))];
["Brasil","UF","Capital","Região Metropolitana","Município"].forEach(n => { if (niveisDisponiveis.includes(n)) selNivel.add(new Option(n, n)); });

// ---------- navegação / filtros ----------
function setModo(modo) {
  MODO = modo;
  $("tabApr").setAttribute("aria-selected", modo === "apresentacao");
  $("tabCur").setAttribute("aria-selected", modo === "curvas");
  $("tabCmp").setAttribute("aria-selected", modo === "comparativo");
  $("viewApresentacao").hidden = modo !== "apresentacao";
  $("viewCurvas").hidden = modo !== "curvas";
  $("viewComparativo").hidden = modo !== "comparativo";
  $("filtros").hidden = modo === "comparativo";   // o Comparativo tem seus próprios controles
  $("grupoMapaBox").hidden = modo !== "apresentacao";
  renderizar();
}
function limparFiltros() {
  selNivel.value = "Brasil"; selDim.value = "geral"; selGrupoMapa.value = "Todos";
  METRICA = "Mediana_D5"; NIVEL_MAPA = "UF"; marcarSegmentados();
  atualizarTerritorios("Brasil");
}
function marcarSegmentados() {
  document.querySelectorAll("[data-nivel]").forEach(b => b.setAttribute("aria-pressed", b.dataset.nivel === NIVEL_MAPA));
  document.querySelectorAll("[data-metrica]").forEach(b => b.setAttribute("aria-pressed", b.dataset.metrica === METRICA));
}
function atualizarTerritorios(alvo = null) {
  const terrs = [...new Set(D_GERAL.filter(d => d.Nivel_Territorial === selNivel.value).map(d => d.Territorio))].sort((a, b) => a.localeCompare(b, "pt-BR"));
  selTerr.innerHTML = "";
  terrs.forEach(t => selTerr.add(new Option(t, t)));
  if (alvo && terrs.includes(alvo)) selTerr.value = alvo;
  atualizarSubcategorias();
}
function atualizarSubcategorias() {
  const dim = selDim.value;
  if (dim === "geral") { $("subFiltroBox").hidden = true; renderizar(); return; }
  $("subFiltroBox").hidden = false;
  const cfg = {esc:[D_ESC,"escolaridade","Escolaridade"], gg:[D_GG,"grande_grupo_cod","Grandes grupos de ocupações"], sg:[D_SG,"subgrupo_principal_cod","Subgrupos de ocupações"]}[dim];
  $("subFiltroLabel").textContent = cfg[2];
  const prev = selSub.value;
  const cats = [...new Set(cfg[0].filter(d => d.Nivel_Territorial === selNivel.value && d.Territorio === selTerr.value).map(d => d[cfg[1]]))].filter(Boolean);
  selSub.innerHTML = "";
  ordenarCats(dim, cats);
  cats.forEach(c => selSub.add(new Option(semCodigo(c), c)));
  if (prev && cats.includes(prev)) selSub.value = prev;
  renderizar();
}
function filtrarFonteAtual(nivel = null, terr = null) {
  const dim = selDim.value, sub = selSub.value;
  let f = D_GERAL;
  if (dim === "esc") f = D_ESC.filter(d => d.escolaridade === sub);
  if (dim === "gg")  f = D_GG.filter(d => d.grande_grupo_cod === sub);
  if (dim === "sg")  f = D_SG.filter(d => d.subgrupo_principal_cod === sub);
  if (nivel) f = f.filter(d => d.Nivel_Territorial === nivel);
  if (terr)  f = f.filter(d => d.Territorio === terr);
  return f;
}
function obterLinhas() {
  const rows = filtrarFonteAtual(selNivel.value, selTerr.value);
  return ORDEM_GRUPOS.map(g => rows.find(r => r.genero_raca === g)).filter(Boolean);
}
function ufSelecionada() {
  const n = selNivel.value, t = selTerr.value;
  if (n === "UF") return t;
  if (n === "Capital" || n === "Região Metropolitana") return SIGLA_UF[siglaDe(t)] || null;
  return null;
}
function resumoDosFiltros() {
  const seg = selDim.value === "geral" ? "Gênero/raça" : (semCodigo(selSub.value || ""));
  $("resumoFiltros").textContent = nomeCurto(selTerr.value || "") + (seg ? " · " + seg : "");
}

// ---------- mapa ----------
function bbox(feats) {
  let x0 = 1e9, y0 = 1e9, x1 = -1e9, y1 = -1e9;
  const walk = a => { if (typeof a[0] === "number") { x0 = Math.min(x0,a[0]); x1 = Math.max(x1,a[0]); y0 = Math.min(y0,a[1]); y1 = Math.max(y1,a[1]); } else a.forEach(walk); };
  feats.forEach(f => { if (f.geometry && f.geometry.coordinates) walk(f.geometry.coordinates); });
  return [x0, y0, x1, y1];
}
function renderizarMapa() {
  if (MODO !== "apresentacao") return;
  const grupo = selGrupoMapa.value, nivel = NIVEL_MAPA, ufSel = ufSelecionada();
  const rotulo = METRICA === "Mediana_D5" ? "Renda mediana" : "Renda média";
  const camada = GEO[nivel];
  const rows = filtrarFonteAtual(nivel, null).filter(d => d.genero_raca === grupo && d[METRICA] != null);
  const val = new Map(rows.map(r => [r.Territorio, r[METRICA]]));
  const vs = [...val.values()];
  const zmin = vs.length ? Math.min(...vs) : 0, zmax = vs.length ? Math.max(...vs) : 1;   // escala nacional do nível
  let feats = camada.features.filter(f => val.has(f.properties.nome));
  if (nivel !== "UF" && ufSel) feats = feats.filter(f => SIGLA_UF[siglaDe(f.properties.nome)] === ufSel);
  const nomes = feats.map(f => f.properties.nome), z = nomes.map(n => val.get(n));
  const tip = n => "<b>" + nomeCurto(n) + "</b><br>" + rotulo + " (" + grupo + ")<br>" + fmtBRL(val.get(n));
  const textos = nomes.map(tip);

  $("mapTitle").textContent = (ufSel ? ufSel + " — " : "Brasil — ") + rotulo + " (" + grupo + ")";
  $("btnBrasil").hidden = !ufSel;

  const traces = [];
  if (nivel !== "UF") {   // fundo cinza: estados clicáveis para ampliar
    const u = GEO.UF.features.map(f => f.properties.nome);
    traces.push({type:"choropleth", name:"base", geojson:GEO.UF, featureidkey:"properties.nome", locations:u, z:u.map(() => 0),
      colorscale:[[0,"#D9DEDA"],[1,"#D9DEDA"]], showscale:false, customdata:u, hovertemplate:"%{customdata}<extra>toque para ampliar</extra>",
      marker:{line:{color:"#fff", width:1}}});
  }
  traces.push({type:"choropleth", name:"dados", geojson:{type:"FeatureCollection", features:feats}, featureidkey:"properties.nome",
    locations:nomes, z:z, zmin:zmin, zmax:zmax, colorscale:ESCALA, customdata:nomes, text:textos, hovertemplate:"%{text}<extra></extra>",
    marker:{line:{color:"#fff", width: nivel === "UF" ? 1 : 0.6}},
    colorbar:{orientation:"h", x:0.5, xanchor:"center", y:-0.02, yanchor:"top", len:0.9, thickness:12, outlinewidth:0,
      tickprefix:"R$ ", tickfont:{size:11}, title:{text:rotulo, side:"top", font:{size:12}}}});
  if (nivel !== "UF") {   // marcadores com a MESMA escala de cores (áreas pequenas ficam visíveis)
    traces.push({type:"scattergeo", name:"marcadores", mode:"markers", showlegend:false,
      lon:feats.map(f => f.properties.lon), lat:feats.map(f => f.properties.lat), customdata:nomes, text:textos, hovertemplate:"%{text}<extra></extra>",
      marker:{size: ehMobile() ? 12 : 14, color:z, colorscale:ESCALA, cmin:zmin, cmax:zmax, line:{color:"#191C1A", width:1.3}}});
  }
  const sel = selTerr.value;
  if (selNivel.value === nivel && nomes.includes(sel)) {   // contorno do território selecionado
    traces.push({type:"choropleth", name:"sel", geojson:{type:"FeatureCollection", features:feats.filter(f => f.properties.nome === sel)},
      featureidkey:"properties.nome", locations:[sel], z:[1], colorscale:[[0,"rgba(0,0,0,0)"],[1,"rgba(0,0,0,0)"]], showscale:false,
      hoverinfo:"skip", marker:{line:{color:"#191C1A", width:3}}});
  }
  const alvo = ufSel ? GEO.UF.features.filter(f => f.properties.nome === ufSel) : GEO.UF.features;
  const [x0,y0,x1,y1] = bbox(alvo), px = (x1-x0)*0.05, py = (y1-y0)*0.05;
  const mapDiv = $("chartMap");
  plotar(mapDiv, traces, {
    margin:{t:0, r:0, b:70, l:0},
    geo:{projection:{type:"mercator"}, lonaxis:{range:[x0-px, x1+px]}, lataxis:{range:[y0-py, y1+py]},
      visible:false, showland:false, showcountries:false, showcoastlines:false, showframe:false, bgcolor:"rgba(0,0,0,0)"}
  }).then(() => {
    if (mapDiv.removeAllListeners) mapDiv.removeAllListeners("plotly_click");
    mapDiv.on("plotly_click", ev => {
      const p = ev && ev.points && ev.points[0];
      const alvo = p && (p.customdata || p.location || (p.data.customdata || [])[p.pointNumber]);
      if (!alvo) return;
      selNivel.value = p.data.name === "base" ? "UF" : nivel;
      atualizarTerritorios(alvo);
    });
  });

  // resumo acessível do território selecionado + posição no ranking
  const r = $("mapaResumo");
  const selVal = val.get(sel);
  if (selNivel.value === nivel && selVal != null) {
    const ord = vs.slice().sort((a, b) => b - a), pos = ord.indexOf(selVal) + 1;
    r.innerHTML = "<b>" + nomeCurto(sel) + "</b> · " + rotulo + " (" + grupo + "): <b>" + fmtBRL(selVal) + "</b> · " + pos + "º de " + vs.length + " no recorte";
  } else {
    r.innerHTML = vs.length ? "Escala: de <b>" + fmtBRL(zmin) + "</b> a <b>" + fmtBRL(zmax) + "</b> · " + vs.length + " áreas com dados" : "Sem dados para este recorte.";
  }
}

// ---------- gráficos e tabela ----------
function renderizar() {
  resumoDosFiltros();
  const rows = obterLinhas();
  if (MODO === "apresentacao") {
    plotar("chartGini", [{type:"bar", orientation:"h", y:rows.map(r => r.genero_raca), x:rows.map(r => r.Gini),
      marker:{color:rows.map(r => CORES[r.genero_raca])}, text:rows.map(r => fmtGini(r.Gini)), textposition:"outside", cliponaxis:false,
      hovertemplate:"%{y}: %{text}<extra></extra>"}], {
      margin:{t:8, r:48, b:36, l:8},
      xaxis:{range:[0, 0.8], fixedrange:true, title:{text:"Índice de Gini"}, gridcolor:"#E1E6E1"},
      yaxis:{autorange:"reversed", automargin:true, fixedrange:true}
    });
    renderizarMapa();
  } else if (MODO === "curvas") {
    $("cardsContainer").innerHTML = rows.map(r => '<div class="kpi" style="background:' + CORES[r.genero_raca] + '"><div class="t">' + r.genero_raca + '</div>' +
      '<div class="kpi-vals"><div class="kv"><div class="v">' + fmtReais(r.Mediana_D5) + '</div><div class="l">Mediana</div></div>' +
      '<div class="kv"><div class="v">' + fmtReais(r.Media) + '</div><div class="l">Média</div></div></div>' +
      '<span class="g">Gini ' + fmtGini(r.Gini) + '</span></div>').join("");
    const lab = ["D1","D2","D3","D4","D5","D6","D7","D8","D9"], keys = ["D1_P10","D2_P20","D3_P30","D4_P40","Mediana_D5","D6_P60","D7_P70","D8_P80","D9_P90"];
    plotar("chartDecil", rows.map(r => ({type:"scatter", mode:"lines+markers", x:lab, y:keys.map(k => r[k]), name:r.genero_raca,
      line:{color:CORES[r.genero_raca], width:r.genero_raca === "Todos" ? 3.5 : 2.5, dash:r.genero_raca === "Todos" ? "dash" : "solid"}, marker:{size:7}})), {
      margin:{t:8, r:12, b:90, l: ehMobile() ? 62 : 72},
      xaxis:{fixedrange:true, title:{text:"Decis (D5 = mediana)"}}, yaxis:{fixedrange:true, tickprefix:"R$ ", separatethousands:true, gridcolor:"#E1E6E1"},
      legend:{orientation:"h", y:-0.28, x:0.5, xanchor:"center"}, hovermode:"x unified"
    });
    document.querySelector("#dataTable tbody").innerHTML = rows.map(r => '<tr><td><span style="display:inline-block;width:10px;height:10px;border-radius:50%;background:' + CORES[r.genero_raca] +
      ';margin-right:8px"></span>' + r.genero_raca + '</td><td>' + fmtInt(r.Pop_Ocupada_18mais) + '</td><td><b>' + fmtBRL(r.Media) + '</b></td><td><b>' + fmtGini(r.Gini) +
      '</b></td><td>' + fmtBRL(r.D1_P10) + '</td><td><b>' + fmtBRL(r.Mediana_D5) + '</b></td><td>' + fmtBRL(r.D9_P90) + '</td></tr>').join("");
  } else {
    renderizarComparativo();
  }
}

// ---------- Comparativo ----------
// Pontos da distribuição usados na calculadora (P05, P95 e P99 só existem se o R v9 os calculou)
const QUANT = [[5,"P05"],[10,"D1_P10"],[20,"D2_P20"],[25,"P25"],[30,"D3_P30"],[40,"D4_P40"],[50,"Mediana_D5"],[60,"D6_P60"],[70,"D7_P70"],[75,"P75"],[80,"D8_P80"],[90,"D9_P90"],[95,"P95"],[99,"P99"]];
const DE_GRUPO = {"Homem Branco":"dos homens brancos ocupados","Mulher Branca":"das mulheres brancas ocupadas","Homem Negro":"dos homens negros ocupados","Mulher Negra":"das mulheres negras ocupadas","Todos":"das pessoas ocupadas"};
const NIVEL_ROTULO = {"Brasil":"Brasil","UF":"Estado","Capital":"Capital","Região Metropolitana":"Região metropolitana"};
const ORDEM_ESC = ["Sem instrução","Fundamental Incompleto","Fundamental Completo","Médio Incompleto","Médio Completo","Superior Incompleto","Superior Completo"];
// escolaridade em ordem lógica; ocupações em ordem numérica do código
const ordenarCats = (dim, cats) => dim === "esc" ? cats.sort((a, b) => ORDEM_ESC.indexOf(a) - ORDEM_ESC.indexOf(b)) : cats.sort((a, b) => a.localeCompare(b, "pt-BR", {numeric:true}));
const PALETA = ["#0072B2","#D55E00","#009E73","#CC79A7","#E69F00","#56B4E9","#7A5195","#6B6B6B"];
const FONTES = {geral:[D_GERAL,null], esc:[D_ESC,"escolaridade"], gg:[D_GG,"grande_grupo_cod"], sg:[D_SG,"subgrupo_principal_cod"]};
const ROT_DIM = {esc:"Escolaridade", gg:"Grandes grupos de ocupações", sg:"Subgrupos de ocupações"};
const NIVEIS_CMP = ["Brasil","UF","Capital","Região Metropolitana"].filter(n => niveisDisponiveis.includes(n));
const MAX_CATS = 8;
const CMP = {nivel:"Brasil", terr:"Brasil", por:"genero", rec:"geral", cat:"", ref:"Todos", grupos:new Set(ORDEM_GRUPOS), cats:new Set(), busca:""};
const ptSort = (a, b) => a.localeCompare(b, "pt-BR", {numeric:true});
const DECIS_LAB = ["D1","D2","D3","D4","D5","D6","D7","D8","D9"], DECIS_KEY = ["D1_P10","D2_P20","D3_P30","D4_P40","Mediana_D5","D6_P60","D7_P70","D8_P80","D9_P90"];
const ptsCmp = (nivel, terr) => D_GERAL.filter(d => d.Nivel_Territorial === nivel && d.Territorio === terr);
const buscar = (dim, cat, g) => { const [src, col] = FONTES[dim];
  return src.find(d => d.Nivel_Territorial === CMP.nivel && d.Territorio === CMP.terr && d.genero_raca === g && (!col || d[col] === cat)); };
const dot = c => '<span style="display:inline-block;width:10px;height:10px;border-radius:50%;background:' + c + ';margin-right:8px"></span>';

// território compartilhado pelas duas subseções (os seletores ficam sincronizados)
const PARES_TERR = [["cmpNivel","cmpTerr"], ["cmpNivel2","cmpTerr2"]];
const terrsDoNivel = n => [...new Set(D_GERAL.filter(d => d.Nivel_Territorial === n).map(d => d.Territorio))].sort((a, b) => ptSort(nomeCurto(a), nomeCurto(b)));
function preencherTerritoriosCmp() {
  PARES_TERR.forEach(([a, b]) => {
    const sn = $(a), st = $(b);
    if (!sn.options.length) NIVEIS_CMP.forEach(n => sn.add(new Option(NIVEL_ROTULO[n], n)));
    sn.value = CMP.nivel;
    st.innerHTML = "";
    terrsDoNivel(CMP.nivel).forEach(t => st.add(new Option(nomeCurto(t), t)));
    st.value = CMP.terr;
  });
}
function aoMudarTerritorio() { preencherTerritoriosCmp(); prepararComparador(); renderizarCalculadora(); }

// ----- calculadora de posicionamento -----
function parseBRL(txt) {
  let s = String(txt || "").replace(/[^\d.,]/g, "");
  if (!s) return NaN;
  if (s.includes(",")) s = s.replace(/\./g, "").replace(",", ".");
  else if (/^\d{1,3}(\.\d{3})+$/.test(s)) s = s.replace(/\./g, "");
  return parseFloat(s);
}
function pontosQuantis(r) {
  const pts = [];
  QUANT.forEach(([p, k]) => { if (r[k] != null && !isNaN(r[k])) pts.push([p, +r[k]]); });
  for (let i = 1; i < pts.length; i++) if (pts[i][1] < pts[i-1][1]) pts[i][1] = pts[i-1][1];
  return pts;
}
function percentilDe(x, pts) {
  if (pts.length < 2) return null;
  if (x < pts[0][1]) return {tipo:"abaixo", p:pts[0][0]};
  if (x > pts[pts.length-1][1]) return {tipo:"acima", p:pts[pts.length-1][0]};
  const iguais = pts.filter(q => q[1] === x);       // várias pessoas com exatamente esse valor (ex.: salário mínimo)
  if (iguais.length) { const a = iguais[0][0], b = iguais[iguais.length-1][0]; return a === b ? {tipo:"exato", p:a} : {tipo:"faixa", a, b}; }
  for (let i = 1; i < pts.length; i++) {
    const [pa, va] = pts[i-1], [pb, vb] = pts[i];
    if (x > va && x < vb) return {tipo:"exato", p: pa + (pb - pa) * (x - va) / (vb - va)};
  }
  return null;
}
function rendaNoPercentil(pts, p) {
  if (pts.length < 2 || p < pts[0][0] || p > pts[pts.length-1][0]) return null;
  for (let i = 1; i < pts.length; i++) {
    const [pa, va] = pts[i-1], [pb, vb] = pts[i];
    if (p >= pa && p <= pb) return va + (vb - va) * (p - pa) / (pb - pa);
  }
  return null;
}
const pctNum = pc => !pc ? null : pc.tipo === "faixa" ? (pc.a + pc.b) / 2 : pc.p;
const rotuloPos = pc => !pc ? "—" : pc.tipo === "abaixo" ? "< " + pc.p + "º" : pc.tipo === "acima" ? "> " + pc.p + "º"
  : pc.tipo === "faixa" ? pc.a + "–" + pc.b + "º" : "≈ " + Math.min(99, Math.max(1, Math.round(pc.p))) + "º";
function frasePos(pc, x, g) {
  const ref = DE_GRUPO[g], v = "<b>" + fmtBRL(x) + "</b>";
  if (!pc) return "<p>Não há dados suficientes para este grupo neste território.</p>";
  if (pc.tipo === "exato")  return "<p>Sua renda de " + v + " é maior do que a de cerca de <b>" + Math.round(pc.p) + "%</b> " + ref + ".</p>";
  if (pc.tipo === "faixa")  return "<p>Sua renda de " + v + " está entre o <b>" + pc.a + "º e o " + pc.b + "º percentil</b> " + ref + ". Muitas pessoas recebem exatamente esse valor, então não dá para precisar mais a posição.</p>";
  if (pc.tipo === "abaixo") return "<p>Sua renda de " + v + " está <b>abaixo do " + pc.p + "º percentil</b> " + ref + ": pelo menos " + (100 - pc.p) + "% ganham mais.</p>";
  return "<p>Sua renda de " + v + " está <b>acima do " + pc.p + "º percentil</b> " + ref + ": você está entre os <b>" + (100 - pc.p) + "%</b> que mais ganham.</p>";
}
function renderizarCalculadora() {
  if (MODO !== "comparativo") return;
  const bruto = $("calcRenda").value, x = parseBRL(bruto), g = $("calcGrupo").value;
  const vazio = $("calcVazio"), res = $("calcResultado");
  if (!(x > 0)) {
    res.hidden = true; vazio.hidden = false;
    vazio.textContent = bruto.trim() ? "Digite um valor maior que zero (ex.: 3.500,00)." : "Digite sua renda mensal do trabalho principal para ver sua posição.";
    return;
  }
  vazio.hidden = true; res.hidden = false;
  const pos = ORDEM_GRUPOS.map(gr => { const r = ptsCmp(CMP.nivel, CMP.terr).find(d => d.genero_raca === gr), pts = r ? pontosQuantis(r) : [];
    return {g:gr, r, pts, pc: pts.length > 1 ? percentilDe(x, pts) : null}; });
  const eu = pos.find(o => o.g === g), pEu = pctNum(eu.pc);
  let html = '<div class="big">' + rotuloPos(eu.pc) + '</div><div class="big-sub">percentil entre ' + g.toLowerCase().replace("homem", "homens").replace("mulher", "mulheres").replace("branco", "brancos").replace("branca", "brancas").replace("negro", "negros").replace("negra", "negras") + '</div>' + frasePos(eu.pc, x, g);
  const outros = pos.filter(o => o.g !== "Todos" && o.pc).sort((a, b) => pctNum(b.pc) - pctNum(a.pc));
  if (outros.length > 1 && pctNum(outros[0].pc) !== pctNum(outros[outros.length-1].pc)) {
    const alto = outros[0], baixo = outros[outros.length-1];
    html += "<p><b>Com a mesma renda:</b> posição mais alta entre " + alto.g + " (" + rotuloPos(alto.pc) + ") e mais baixa entre " + baixo.g + " (" + rotuloPos(baixo.pc) + ").</p>";
  }
  $("calcTexto").innerHTML = html;
  plotar("chartCalc", [{type:"bar", orientation:"h", y:pos.map(o => o.g), x:pos.map(o => o.pc ? pctNum(o.pc) : 0),
    marker:{color:pos.map(o => CORES[o.g]), opacity:pos.map(o => o.g === g ? 1 : 0.5)}, text:pos.map(o => rotuloPos(o.pc)), textposition:"outside", cliponaxis:false,
    hovertemplate:"%{y}: %{text}<extra></extra>"}], {
    margin:{t:8, r:48, b:44, l:8}, xaxis:{range:[0, 105], fixedrange:true, ticksuffix:"º", title:{text:"Percentil (% do grupo com renda menor)"}, gridcolor:"#E1E6E1"},
    yaxis:{autorange:"reversed", automargin:true, fixedrange:true}});
  $("calcTabela").querySelector("tbody").innerHTML = pos.map(o => {
    const eq = (pEu != null && o.pts.length) ? (o.g === g ? x : rendaNoPercentil(o.pts, pEu)) : null;
    return "<tr><td>" + dot(CORES[o.g]) + o.g + (o.g === g ? " (seu grupo)" : "") + "</td><td><b>" + rotuloPos(o.pc) + "</b></td><td>" + (o.r ? fmtBRL(o.r.Mediana_D5) : "—") +
      "</td><td>" + (eq == null ? "—" : (eu.pc.tipo === "faixa" && o.g !== g ? "≈ " : "") + fmtBRL(eq)) + "</td></tr>"; }).join("");
}

// ----- comparador de distribuições -----
function categoriasDisp(dim, g) {
  const [src, col] = FONTES[dim];
  const cats = [...new Set(src.filter(d => d.Nivel_Territorial === CMP.nivel && d.Territorio === CMP.terr && d.genero_raca === g).map(d => d[col]))].filter(Boolean);
  return ordenarCats(dim, cats);
}
function padraoCats(dim, lista) {
  if (dim === "esc") return lista;
  const pop = c => { const r = buscar(dim, c, CMP.ref); return r ? r.Pop_Ocupada_18mais : 0; };
  const top = new Set(lista.slice().sort((a, b) => pop(b) - pop(a)).slice(0, 4));
  return lista.filter(c => top.has(c));
}
const normTxt = t => String(t).normalize("NFD").replace(/[\u0300-\u036f]/g, "").toLowerCase();
function prepararComparador() {
  const s4 = $("cmpSel4"), s5 = $("cmpSel5");
  CMP.busca = ""; $("cmpBusca").value = "";
  $("comparaPor").value = CMP.por;
  s4.innerHTML = ""; s5.innerHTML = "";
  if (CMP.por === "genero") {
    $("cmpLabel4").textContent = "Recorte";
    [["geral","Todos os ocupados"],["esc","Escolaridade"],["gg","Grande grupo de ocupações"],["sg","Subgrupo de ocupações"]].forEach(([v, t]) => s4.add(new Option(t, v)));
    s4.value = CMP.rec;
    const cats = CMP.rec === "geral" ? [] : categoriasDisp(CMP.rec, "Todos");
    $("cmpBox5").hidden = CMP.rec === "geral";
    $("cmpLabel5").textContent = ROT_DIM[CMP.rec] || "Categoria";
    cats.forEach(c => s5.add(new Option(semCodigo(c), c)));
    if (!cats.includes(CMP.cat)) CMP.cat = cats[0] || "";
    s5.value = CMP.cat;
  } else {
    $("cmpLabel4").textContent = "Gênero/raça";
    ORDEM_GRUPOS.forEach(g => s4.add(new Option(g, g)));
    s4.value = CMP.ref; $("cmpBox5").hidden = true;
    const lista = categoriasDisp(CMP.por, CMP.ref), sel = [...CMP.cats].filter(c => lista.includes(c));
    CMP.cats = new Set(sel.length ? sel : padraoCats(CMP.por, lista));
  }
  desenharChips();
  renderizarComparador();
}
function desenharChips() {
  const ehG = CMP.por === "genero", box = $("cmpChips");
  const itens = ehG ? ORDEM_GRUPOS : categoriasDisp(CMP.por, CMP.ref), sel = ehG ? CMP.grupos : CMP.cats;
  const ordem = itens.filter(i => sel.has(i));                      // a cor de cada categoria segue a ordem da seleção
  $("cmpChipsTitulo").textContent = ehG ? "Grupos na comparação" : "Categorias na comparação";
  $("cmpContador").textContent = ehG ? sel.size + " de " + itens.length + " selecionados" : sel.size + " de " + MAX_CATS + " selecionadas";
  $("cmpBuscaBox").hidden = ehG || itens.length <= 12;
  box.className = "chips" + (ehG || CMP.por === "esc" ? " compact" : "");
  const q = normTxt(CMP.busca.trim()), visiveis = itens.filter(i => !q || normTxt(semCodigo(i)).includes(q));
  box.innerHTML = "";
  if (!visiveis.length) { const p = document.createElement("p"); p.className = "chips-vazio"; p.textContent = "Nenhuma categoria encontrada para essa busca."; box.appendChild(p); return; }
  visiveis.forEach(i => {
    const on = sel.has(i), b = document.createElement("button"), d = document.createElement("span");
    b.type = "button"; b.className = "chip"; b.setAttribute("aria-pressed", on);
    d.className = "dot"; d.style.background = ehG ? CORES[i] : (on ? PALETA[ordem.indexOf(i) % PALETA.length] : "#9AA39C");
    b.append(d, document.createTextNode(ehG ? i : semCodigo(i)));
    b.addEventListener("click", () => alternarChip(i));
    box.appendChild(b);
  });
}
function restaurarPadraoChips() {
  if (CMP.por === "genero") CMP.grupos = new Set(ORDEM_GRUPOS);
  else CMP.cats = new Set(padraoCats(CMP.por, categoriasDisp(CMP.por, CMP.ref)));
  $("cmpAviso").textContent = ""; desenharChips(); renderizarComparador();
}
function desmarcarChips() {
  (CMP.por === "genero" ? CMP.grupos : CMP.cats).clear();
  $("cmpAviso").textContent = ""; desenharChips(); renderizarComparador();
}
// "Limpar filtros": volta ao Brasil; o território é compartilhado, então os dois cards reiniciam nele
function limparCalculadora() {
  $("calcRenda").value = ""; $("calcGrupo").selectedIndex = 0;
  CMP.nivel = "Brasil"; CMP.terr = "Brasil"; aoMudarTerritorio();
}
function limparComparador() {
  CMP.nivel = "Brasil"; CMP.terr = "Brasil";
  Object.assign(CMP, {por:"genero", rec:"geral", cat:"", ref:"Todos", grupos:new Set(ORDEM_GRUPOS), cats:new Set(), busca:""});
  aoMudarTerritorio();
}
function alternarChip(i) {
  const ehG = CMP.por === "genero", sel = ehG ? CMP.grupos : CMP.cats;
  if (sel.has(i)) sel.delete(i);
  else { if (!ehG && sel.size >= MAX_CATS) { $("cmpAviso").textContent = "Limite de " + MAX_CATS + " categorias: desmarque uma para incluir outra."; return; } sel.add(i); }
  desenharChips(); renderizarComparador();
}
function seriesCmp() {
  const out = [], falt = [];
  if (CMP.por === "genero") {
    ORDEM_GRUPOS.filter(g => CMP.grupos.has(g)).forEach(g => { const r = buscar(CMP.rec, CMP.cat, g); r ? out.push({nome:g, cor:CORES[g], r}) : falt.push(g); });
  } else {
    categoriasDisp(CMP.por, CMP.ref).filter(c => CMP.cats.has(c)).forEach((c, i) => { const r = buscar(CMP.por, c, CMP.ref); r ? out.push({nome:semCodigo(c), cor:PALETA[i % PALETA.length], r}) : falt.push(semCodigo(c)); });
  }
  return {out, falt};
}
function renderizarComparador() {
  if (MODO !== "comparativo") return;
  const {out, falt} = seriesCmp();
  $("cmpAviso").textContent = falt.length ? "Sem dados suficientes neste território para: " + falt.join("; ") + "." : "";
  $("cmpResultado").hidden = !out.length; $("cmpVazio").hidden = !!out.length;
  if (!out.length) { $("cmpVazio").textContent = "Selecione pelo menos um grupo ou categoria com dados para comparar."; return; }
  const curta = n => n.length > 34 ? n.slice(0, 33) + "…" : n;
  plotar("chartCmpCurvas", out.map(s => ({type:"scatter", mode:"lines+markers", x:DECIS_LAB, y:DECIS_KEY.map(k => s.r[k]), name:s.nome,
    text:DECIS_KEY.map(k => fmtBRL(s.r[k])), hovertemplate:"%{text}<extra>" + s.nome + "</extra>", line:{color:s.cor, width:2.5}, marker:{size:6}})), {
    margin:{t:8, r:12, b:48, l: ehMobile() ? 62 : 72}, showlegend:false, hovermode:"x unified",
    xaxis:{fixedrange:true, title:{text:"Decis (D5 = mediana)"}}, yaxis:{fixedrange:true, tickprefix:"R$ ", separatethousands:true, gridcolor:"#E1E6E1"}});
  $("chartCmpMedia").style.height = Math.max(260, 52 * out.length + 90) + "px";
  plotar("chartCmpMedia", [{type:"bar", orientation:"h", y:out.map((s, i) => i), x:out.map(s => s.r.Media), marker:{color:out.map(s => s.cor)},
    text:out.map(s => fmtBRL(s.r.Media)), textposition:"outside", cliponaxis:false, customdata:out.map(s => s.nome), hovertemplate:"%{customdata}<br>Média: %{text}<extra></extra>"}], {
    margin:{t:8, r:76, b:36, l:8}, xaxis:{fixedrange:true, gridcolor:"#E1E6E1", tickprefix:"R$ ", separatethousands:true, rangemode:"tozero"},
    yaxis:{autorange:"reversed", automargin:true, fixedrange:true, tickmode:"array", tickvals:out.map((s, i) => i), ticktext:out.map(s => curta(s.nome))}});
  $("cmpTabela").querySelector("tbody").innerHTML = out.map(s => "<tr><td>" + dot(s.cor) + curta(s.nome) + "</td><td>" + fmtInt(s.r.Pop_Ocupada_18mais) +
    "</td><td><b>" + fmtBRL(s.r.Media) + "</b></td><td>" + fmtBRL(s.r.Mediana_D5) + "</td><td>" + fmtGini(s.r.Gini) + "</td></tr>").join("");
  renderizarComposicao();
}
// ----- composição: duas barras empilhadas (gênero/raça × segmento) -----
const GRUPOS4 = ["Homem Branco","Mulher Branca","Homem Negro","Mulher Negra"];
const ROT_OUTROS = "Outros (amarelos, indígenas e não especificados)";
const CINZA = "#B8C1BA";
const fmtPct = v => v.toLocaleString("pt-BR", {minimumFractionDigits:1, maximumFractionDigits:1}) + "%";
const escHtml = t => String(t).replace(/[&<>"]/g, c => ({"&":"&amp;","<":"&lt;",">":"&gt;",'"':"&quot;"}[c]));
const abrevia = (n, m = 56) => n.length > m ? n.slice(0, m - 1) + "…" : n;
const corTexto = hex => { const n = parseInt(hex.slice(1), 16), L = (0.299 * (n >> 16) + 0.587 * ((n >> 8) & 255) + 0.114 * (n & 255)) / 255; return L > 0.62 ? "#191C1A" : "#fff"; };
const popDe = (dim, cat, g) => { const r = buscar(dim, cat, g); return r && r.Pop_Ocupada_18mais != null ? +r.Pop_Ocupada_18mais : null; };

function segsGenero(dim, cat) {                       // 4 grupos + "Outros" (amarelos, indígenas e não especificados) = 100%
  const itens = GRUPOS4.map(g => ({nome:g, cor:CORES[g], pop:popDe(dim, cat, g)})).filter(s => s.pop > 0);
  const soma = itens.reduce((a, s) => a + s.pop, 0), total = popDe(dim, cat, "Todos"), base = total && total >= soma ? total : soma;
  if (!base) return null;
  const segs = itens.map(s => ({...s, pct: s.pop / base * 100})), resto = base - soma;
  if (resto / base > 0.0005) segs.push({nome: itens.length < 4 ? "Outros e grupos sem dados" : ROT_OUTROS, cor:CINZA, pop:resto, pct:resto / base * 100});
  return segs;
}
function htmlBarra(rotulo, sub, segs, cor, fixa) {
  const rot = '<div class="stack-label">' + (cor ? '<i class="dot-cat" style="background:' + cor + '"></i>' : '') + escHtml(rotulo) + '<small>' + escHtml(sub) + '</small></div>';
  const cls = "stack-row" + (fixa ? " ref" : "");
  if (!segs) return '<div class="' + cls + '">' + rot + '<p class="stack-vazio">Sem dados suficientes para este recorte.</p></div>';
  const aria = segs.map(s => s.nome + " " + fmtPct(s.pct)).join("; ");
  const barra = segs.map(s => '<div class="stack-seg" style="flex:' + s.pct.toFixed(3) + ' 1 0;background:' + s.cor + ';color:' + corTexto(s.cor) + '" title="' + escHtml(s.nome) + ': ' + fmtPct(s.pct) + ' (' + fmtInt(s.pop) + ' pessoas)">' + (s.pct >= 7 ? Math.round(s.pct) + "%" : "") + '</div>').join("");
  const vals = segs.map(s => '<li title="' + escHtml(s.nome) + '"><i style="background:' + s.cor + '"></i><b>' + fmtPct(s.pct) + '</b></li>').join("");
  return '<div class="' + cls + '">' + rot + '<div><div class="stack-bar" role="img" aria-label="' + escHtml(rotulo + " por gênero/raça: " + aria) + '">' + barra + '</div><ul class="stack-legend">' + vals + '</ul></div></div>';
}
function renderizarComposicao() {
  const por = CMP.por, linhas = [], aviso = "Todas as barras são por gênero/raça, incluindo outros (amarelos, indígenas e não especificados).";
  let legenda, vazio = "";
  if (por === "genero") {
    if (CMP.rec === "geral") { legenda = "Composição de todos os ocupados por gênero/raça neste território."; vazio = "Escolha um recorte (escolaridade, grande grupo ou subgrupo) para comparar com a composição de um segmento."; }
    else { linhas.push({rot:abrevia(semCodigo(CMP.cat), 44), cor:"#4E6258", segs:segsGenero(CMP.rec, CMP.cat)}); legenda = "Barra fixa: todos os ocupados. Barra de comparação: ocupados em “" + abrevia(semCodigo(CMP.cat), 60) + "”. " + aviso; }
  } else {
    categoriasDisp(por, CMP.ref).filter(c => CMP.cats.has(c)).forEach((c, i) => linhas.push({rot:abrevia(semCodigo(c), 44), cor:PALETA[i % PALETA.length], segs:segsGenero(por, c)}));
    legenda = "Barra fixa: todos os ocupados. Demais barras: ocupados em cada categoria selecionada (a cor do ponto segue os chips acima). " + aviso;
  }
  $("compSub").textContent = legenda;
  const chave = [...GRUPOS4.map(g => [g, CORES[g]]), [ROT_OUTROS, CINZA]].map(([n, c]) => '<li><i style="background:' + c + '"></i>' + escHtml(n) + '</li>').join("");
  let html = '<div class="stack-row"><div></div><ul class="stack-legend top" aria-label="Legenda das cores">' + chave + '</ul></div>';
  html += htmlBarra("Todos os ocupados", "referência fixa", segsGenero("geral", null), null, true);
  html += linhas.map(l => htmlBarra(l.rot, "por gênero/raça", l.segs, l.cor, false)).join("");
  if (vazio) html += '<p class="stack-vazio">' + vazio + '</p>';
  $("compBarras").innerHTML = html;
}
function renderizarComparativo() { renderizarCalculadora(); renderizarComparador(); }
function iniciarComparativo() { preencherTerritoriosCmp(); prepararComparador(); }

// ---------- eventos ----------
$("tabCmp").addEventListener("click", () => setModo("comparativo"));
PARES_TERR.forEach(([a, b]) => {
  $(a).addEventListener("change", e => { CMP.nivel = e.target.value; CMP.terr = terrsDoNivel(CMP.nivel)[0]; aoMudarTerritorio(); });
  $(b).addEventListener("change", e => { CMP.terr = e.target.value; aoMudarTerritorio(); });
});
$("btnLimparCalc").addEventListener("click", limparCalculadora);
$("btnLimparCmp").addEventListener("click", limparComparador);
$("cmpPadrao").addEventListener("click", restaurarPadraoChips);
$("cmpDesmarcar").addEventListener("click", desmarcarChips);
$("cmpBusca").addEventListener("input", e => { CMP.busca = e.target.value; desenharChips(); });
$("calcRenda").addEventListener("input", renderizarCalculadora);
$("calcGrupo").addEventListener("change", renderizarCalculadora);
$("comparaPor").addEventListener("change", e => { CMP.por = e.target.value; CMP.cats = new Set(); prepararComparador(); });
$("cmpSel4").addEventListener("change", e => {
  if (CMP.por === "genero") { CMP.rec = e.target.value; CMP.cat = ""; } else { CMP.ref = e.target.value; CMP.cats = new Set(); }
  prepararComparador();
});
$("cmpSel5").addEventListener("change", e => { CMP.cat = e.target.value; renderizarComparador(); });
$("tabApr").addEventListener("click", () => setModo("apresentacao"));
$("tabCur").addEventListener("click", () => setModo("curvas"));
$("btnLimpar").addEventListener("click", limparFiltros);
$("btnBrasil").addEventListener("click", () => { selNivel.value = "Brasil"; atualizarTerritorios("Brasil"); });
document.querySelectorAll("[data-nivel]").forEach(b => b.addEventListener("click", () => { NIVEL_MAPA = b.dataset.nivel; marcarSegmentados(); renderizarMapa(); }));
document.querySelectorAll("[data-metrica]").forEach(b => b.addEventListener("click", () => { METRICA = b.dataset.metrica; marcarSegmentados(); renderizarMapa(); }));
selNivel.addEventListener("change", () => atualizarTerritorios());
selTerr.addEventListener("change", atualizarSubcategorias);
selDim.addEventListener("change", atualizarSubcategorias);
selSub.addEventListener("change", renderizar);
selGrupoMapa.addEventListener("change", renderizarMapa);

if (window.matchMedia("(max-width:719px)").matches) $("filtros").open = false;   // celular: filtros recolhidos
iniciarComparativo();
atualizarTerritorios();
</script>
</body>
</html>
)-----"

# Ilustração do header (group.png, 192 px otimizada, ~6 KB) embutida em base64: o index.html continua autossuficiente.
# Para trocar a imagem: converta o novo PNG com jsonlite::base64_enc(readBin(arquivo, "raw", file.size(arquivo))), quebre em linhas curtas
# (o console do R aceita no máximo 4094 caracteres por linha ao usar "Run") e cole abaixo.
logo_b64  <- paste0(
  "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"
)
logo_html <- sprintf('<img src="data:image/png;base64,%s" alt="" width="96" height="96" decoding="async">', logo_b64)

html_final <- sub("__JSON_GERAL__", json_geral, html_template, fixed = TRUE)
html_final <- sub("__JSON_ESC__", json_esc, html_final, fixed = TRUE)
html_final <- sub("__JSON_GG__", json_gg, html_final, fixed = TRUE)
html_final <- sub("__JSON_SG__", json_sg, html_final, fixed = TRUE)
html_final <- sub("__LOGO_HTML__", logo_html, html_final, fixed = TRUE)
html_final <- sub("__GEO_UF__",  json_geo_uf,  html_final, fixed = TRUE)
html_final <- sub("__GEO_CAP__", json_geo_cap, html_final, fixed = TRUE)
html_final <- sub("__GEO_RM__",  json_geo_rm,  html_final, fixed = TRUE)

writeLines(html_final, "index.html", useBytes = TRUE)
cat("Arquivo index.html atualizado com sucesso!\n")
