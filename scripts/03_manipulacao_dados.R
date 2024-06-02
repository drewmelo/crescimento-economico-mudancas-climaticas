### ================= SCRIPT 03 - MANIPULAÇÃO DOS DADOS =======================

## Manipulação dos dados (SCIMAGOJR) ------------------------------------------

# Junção da base de áreas de periódicos com a base principal
dados_area <- dados_area |>
                janitor::clean_names() |>
                dplyr::mutate(title = toupper(title)) |>
                dplyr::select(-1, -5, -7, -c(9:18), -c(21:22)) |>
                dplyr::right_join(dados, by = c("title" = "SO")) |>
                janitor::clean_names() |>
                # Atribuindo manualmente áreas para periódicos mais relevantes
                dplyr::mutate(areas = dplyr::case_when(
                  title == "SUSTAINABILITY (SWITZERLAND)" | title == "SUSTAINABILITY (SWITZERLAND) " ~ "Computer Science; Energy; Environmental Science; Social Sciences",
                  title == "ENVIRONMENT DEVELOPMENT AND SUSTAINABILITY"  ~ "Economics, Econometrics and Finance; Environmental Science; Social Sciences",
                  title == "RENEWABLE & SUSTAINABLE ENERGY REVIEWS" ~ "Energy",
                  title == "ENVIRONMENTAL & RESOURCE ECONOMICS" ~ "Economics, Econometrics and Finance; Environmental Science",
                  title == "ENVIRONMENTAL SCIENCE & POLICY" ~ "Environmental Science; Social Sciences",
                  title == "ECONOMIC RESEARCH-EKONOMSKA ISTRAZIVANJA " ~ "Economics, Econometrics and Finance",
                  title == "ENVIRONMENTAL SCIENCE AND POLLUTION RESEARCH INTERNATIONAL" ~ "Environmental Science; Medicine",
                  title == "ENERGY & ENVIRONMENT" ~ "Energy; Environmental Science",
                  title == "ENVIRONMENTAL SCIENCE AND TECHNOLOGY" ~ "Energy; Environmental Science",
                  title == "RESOURCES CONSERVATION AND RECYCLING" ~ "Economics, Econometrics and Finance; Environmental Science",
                  title == "ENERGY RESEARCH & SOCIAL SCIENCE" ~ "Energy; Social Sciences",
                  title == "OIL AND GAS JOURNAL" ~ "Energy",
                  title == "RESOURCE AND ENERGY ECONOMICS" ~ "Economics, Econometrics and Finance",
                  title == "ENERGY SOURCES PART B-ECONOMICS PLANNING AND POLICY" ~ "Chemical Engineering; Energy; Environmental Science",
                  TRUE ~ areas
                ))

## Manipulação dos dados das figuras ------------------------------------------

# Figura 2 --------------------------------------------------------------------
dados_figura2 <- dados_figura2 |>
                  janitor::clean_names()

# Figura 3 --------------------------------------------------------------------
dados_figura3 <- dados_figura3 |>
                  dplyr::rename(freq = Freq) |>
                  dplyr::mutate(region = tolower(region))

# Figura 4 --------------------------------------------------------------------
dados_figura4 <- dados_figura4 |>
                  janitor::clean_names() |>
                  # Identificando os maiores países em produções científicas
                  dplyr::group_by(country) |>
                  dplyr::summarize(total_article = sum(articles)) |>
                  dplyr::ungroup() |>
                  dplyr::arrange(dplyr::desc(total_article)) |>
                  dplyr::inner_join(readxl::read_excel("dados/dados_biblioshiny/countries_production.xlsx",
                                                       skip = 1) |>
                                      janitor::clean_names(), by = "country") |>
                  dplyr::mutate(
                    country = dplyr::case_match(country,
                                                "CHINA" ~ "China",
                                                "USA" ~ "Estados Unidos",
                                                "PAKISTAN" ~ "Paquistão",
                                                "TURKEY" ~ "Turquia",
                                                "AUSTRALIA" ~ "Austrália",
                                                "INDIA" ~ "Índia",
                                                "GERMANY" ~ "Alemanha",
                                                "UNITED KINGDOM" ~ "Reino Unido",
                                                .default = country),
                    ano_ref = 2018.3
                  )

# Figura 5 --------------------------------------------------------------------
dados_figura5 <- dados_figura5 |>
                  janitor::clean_names() |>
                  dplyr::mutate(from = tolower(from), to = tolower(to))

# Filtrando a frequência de conexões
dados_figura5 <- dados_figura5[dados_figura5$frequency > 2,
                               c('from','to',"frequency")]

# Gerando análises das conexões entre os países
collab_net <- network::network(dados_figura5,
                               matrix.type = 'edgelist',
                               directed = FALSE,  # esta será uma rede não direcionada
                               ignore.eval = FALSE  # confusamente, isso diz para incluir os pesos das arestas
)

# Juntando os dados latitudinais e longitudinais aos dados
collab_net %v% 'lon' <- sapply(network::network.vertex.names(collab_net),
                               function(name) {
                                 rawnodes[rawnodes$ID == name, ]$lon
                               }
                        )

collab_net %v% 'lat' <- sapply(network::network.vertex.names(collab_net),
                               function(name) {
                                 rawnodes[rawnodes$ID == name, ]$lat
                               }
                        )

# Figura 6 --------------------------------------------------------------------

# Realizando contagem de artigos por área
contagem_figura6 <- dados_area |>
                     tidyr::separate_rows(areas, sep = "; ") |>
                     dplyr::count(areas, py)

# Preparando plotagem da figura 6
dados_figura6 <- contagem_figura6 |>
                  dplyr::group_by(areas) |>
                  # Contagem dos artigos por ano - por área
                  dplyr::reframe(total_n = sum(n)) |>
                  dplyr::inner_join(contagem_figura6, by = "areas") |>
                  dplyr::relocate(total_n, .after = n) |>
                  tidyr::drop_na() |>
                  # Tradução do nome das principais áreas em destaque
                  dplyr::mutate(areas = dplyr::case_match(areas,
                                                          "Environmental Science" ~ "Ciência Ambiental",
                                                          "Social Sciences" ~ "Ciências Sociais",
                                                          "Economics, Econometrics and Finance" ~ "Economia, Econometria e Finanças",
                                                          "Energy" ~ "Energia",
                                                          "Business, Management and Accounting" ~ "Negócios, Gestão e Contabilidade",
                                                          "Engineering" ~ "Engenharia",
                                                          "Medicine" ~ "Medicina",
                                                          "Earth and Planetary Sciences" ~ "Ciências da Terra e Planetárias",
                                                          "Agricultural and Biological Sciences" ~ "Ciências Agrícolas e Biológicas",
                                                          .default = areas
                  ))

# Figura 7 --------------------------------------------------------------------

dados_figura7 <- dados_figura7 |>
                  head(15) |>
                  dplyr::rename(total_citacoes = TC,
                                media_tc = `Average Article Citations`,
                                pais = Country) |>
                  dplyr::mutate(pais = dplyr::case_match(
                    pais,
                    "CHINA" ~ "China",
                    "USA" ~ "Estados Unidos",
                    "UNITED KINGDOM" ~ "Reino Unido",
                    "AUSTRALIA" ~ "Austrália",
                    "TURKEY" ~ "Turquia",
                    "GERMANY" ~ "Alemanha",
                    "SPAIN" ~ "Espanha",
                    "NETHERLANDS" ~ "Países Baixos",
                    "INDIA" ~ "Índia",
                    "PAKISTAN" ~ "Paquistão",
                    "FRANCE" ~ "França",
                    "MALAYSIA" ~ "Malásia",
                    "AUSTRIA" ~ "Áustria",
                    "NORWAY" ~ "Noruega",
                    "ITALY" ~ "Itália",
                    # Para manter qualquer valor que não corresponda a nenhuma das condições
                    .default = pais
                  )) |>
                  dplyr::mutate(pais_fator = forcats::fct_reorder(pais, total_citacoes))

## Auxílio para análise das figuras --------------------------------------------

# Calculando a variação anual e média dos artigos (figura 4)
var_figura4 <- dados_figura4 |>
                dplyr::group_by(country) |>
                dplyr::mutate(variacao_artigo = articles - dplyr::lag(articles),
                              variacao_artigo_percentual = round(100 * (
                                articles - dplyr::lag(articles)) / dplyr::lag(articles), 2)
                ) |>
                dplyr::filter(country %in% c("China", "Estados Unidos", "Paquistão", "Turquia",
                                             "Austrália", "Índia", "Alemanha", "Reino Unido")) |>
                dplyr::arrange(year, country) |>
                tidyr::drop_na() |>
                dplyr::ungroup()

# Realizando contagem acumulada dos artigos (figura 6)
acum_figura6 <- dados_figura6 |>
                 dplyr::filter(areas %in% c("Ciência Ambiental", "Ciências Sociais", "Economia, Econometria e Finanças",
                                             "Energia", "Medicina")) |>
                 dplyr::group_by(areas) |>
                 dplyr::mutate(cum_n = cumsum(n))

## Resultados das tabelas -----------------------------------------------------

# Tabela 1 --------------------------------------------------------------------

# Gerando a análise dos periódicos
dados_tabela1 <- bibliometrix::Hindex(dados, field = "source", sep = ";") |>
                  purrr::pluck("H") |>
                  janitor::clean_names() |>
                  dplyr::arrange(dplyr::desc(h_index)) |>
                  dplyr::inner_join(dados_area, by = c("element" = "title")) |>
                  dplyr::distinct(element, .keep_all = TRUE) |>
                  dplyr::select(1:15) |>
                  dplyr::inner_join(readxl::read_excel("dados/dados_biblioshiny/most_relevant_sources.xlsx",
                                                       skip = 1),
                                    by = c("element" = "Sources")) |>
                  janitor::clean_names() |>
                  dplyr::select(element, h_index_x, tc_x,
                                np, py_start)

# Tabela 2 --------------------------------------------------------------------

# Gerando a análise das instituições
dados_tabela2 <- dplyr::right_join(dados_tabela2, dados,
                                   by = c("Paper" = "SR")) |>
                  janitor::clean_names() |>
                  dplyr::select(1:3, 5, py, au_un) |>
                  tidyr::separate_rows(au_un, sep = ";") |>
                  dplyr::distinct(au_un, paper, total_citations) |>
                  tidyr::drop_na() |>
                  dplyr::group_by(au_un) |>
                  dplyr::mutate(total_artigos_instituto = sum(total_citations)) |>
                  dplyr::inner_join(
                    readxl::read_excel("dados/dados_biblioshiny/artigos_totais_instituto.xlsx",
                                       skip = 1), by = c("au_un" = "Affiliation")) |>
                  dplyr::distinct(au_un, total_artigos_instituto, articles = Articles) |>
                  dplyr::arrange(dplyr::desc(total_artigos_instituto)) |>
                  dplyr::ungroup()

# Tabela 3 --------------------------------------------------------------------

# Gerando a análise dos autores
dados_tabela3 <- bibliometrix::Hindex(dados, field = "author", sep = ";") |>
                  purrr::pluck("H") |>
                  janitor::clean_names() |>
                  dplyr::arrange(dplyr::desc(h_index)) |>
                  dplyr::select(-c(g_index, m_index))
