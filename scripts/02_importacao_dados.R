### ================= SCRIPT 02 - IMPORTAÇÃO DOS DADOS =======================

## Convertendo os dados para data frame

# Scopus
a <- bibliometrix::convert2df("dados/dados_scopuswos/scopus.csv", dbsource = "scopus",
                              format = "csv")

# Web of Science
files <- list("dados/dados_scopuswos/savedrecs.txt",
              "dados/dados_scopuswos/savedrecs_1.txt",
              "dados/dados_scopuswos/savedrecs_2.txt",
              "dados/dados_scopuswos/savedrecs_3.txt",
              "dados/dados_scopuswos/savedrecs_4.txt",
              "dados/dados_scopuswos/savedrecs_5.txt",
              "dados/dados_scopuswos/savedrecs_6.txt",
              "dados/dados_scopuswos/savedrecs_7.txt",
              "dados/dados_scopuswos/savedrecs_8.txt",
              "dados/dados_scopuswos/savedrecs_9.txt",
              "dados/dados_scopuswos/savedrecs_10.txt")

# Criando uma lista para armazenar os data frames
df_list <- list()

for(i in seq_along(files)) {
  df_list[[i]] <- bibliometrix::convert2df(files[i], dbsource = "wos",
                                           format = "plaintext")
}

# Adicionando o dataframe 'a' na lista
df_list <- c(list(a), df_list)

# REMOÇÃO DOS ARTIGOS DUPLICADOS  ---------------------------------------------

scopus_wos <- do.call(bibliometrix::mergeDbSources, c(df_list,
                                                      list(remove.duplicated = T)
                                                      )
                      )

## MANIPULAÇÃO DOS DADOS ------------------------------------------------------

# Visualização da variável DT da base de dados
distinct(scopus_wos, DT) |>
  knitr::kable(row.names = F)

# Filtrando apenas artigos e papers
dados <- scopus_wos |>
  dplyr::filter(DT %in% c("ARTICLE"))

## RESULTADOS DA ANÁLISE DO PACOTE BIBLIOMETRIX -------------------------------

resultados <- bibliometrix::biblioAnalysis(dados)

# Sumarizando os resultados
dados_ds <- summary(object = resultados, k = 20)

# Plotando o gráfico
plots <- plot(resultados, k = 10)

## LIMPEZA DO GLOBAL ENVIRONMENT ----------------------------------------------

# Criação de objetos para excluir e manter no ambiente global
all_obj <- ls()

keep_obj <- c('dados', 'resultados', 'dados_ds', 'plots')

remov_obj <- setdiff(all_obj, keep_obj)

# Remoção dos objetos
rm(list = remov_obj)

rm(all_obj, keep_obj, remov_obj)

## IMPORTAÇÃO DOS DADOS GERADOS PELO BIBLIOMETRIX (FIGURAS) -------------------

# Figura 02
dados_figura2 <- plots$AnnualScientProd$data

# Figura 03
dados_figura3 <- readxl::read_excel("dados/dados_biblioshiny/countries_production_map.xlsx",
                                    skip = 1)
# Figura 04
dados_figura4 <- readxl::read_excel("dados/dados_biblioshiny/countries_production.xlsx",
                                    skip = 1)

# Figura 05
dados_figura5 <- readxl::read_excel("dados/dados_biblioshiny/collaboration_worldmap.xlsx",
                                    skip = 1)

# Figura 07
dados_figura7 <- readxl::read_excel("dados/dados_biblioshiny/most_cited_countries.xlsx",
                                    skip = 1)

# Figura 08
dados_figura8 <- readxl::read_excel("dados/dados_biblioshiny/trend_topics.xlsx",
                                    skip = 1)

## IMPORTAÇÃO DOS DADOS DO SCIMAGOJR ---------------------------------------

# Importação dos dados referente as áreas dos periódicos
dados_area <- read_csv2("dados/dados_scimagojr/area_pesquisa.csv")

