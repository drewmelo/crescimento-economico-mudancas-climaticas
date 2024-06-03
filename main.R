### ========================= SCRIPT PRINCIPAL ============================ ###

## SCRIPT 01 - PACOTES --------------------------------------------------------

# Carregando o script 01_pacotes.R
source("scripts/01_pacotes.R")

# Caso nunca tenha utilizado o pacote extrafont, necessário executar a função font_import()
# para a utlização da fonte Times New Roman -- obs: este processo pode demorar
#extrafont::font_import()
# Após isso, executar novamente o script '01_pacotes.R' acima

## SCRIPT 02 - IMPORTAÇÃO DOS DADOS -------------------------------------------

# Carregando o script 02_importacao_dados.R (pode demorar a compilar)
source("scripts/02_importacao_dados.R")

## SCRIPT 03 - MANIPULAÇÃO DOS DADOS ------------------------------------------

# Exportando os dados para xlsx para análise do biblioshiny
openxlsx::write.xlsx(dados, "dados/dados.xlsx", rowNames = F)

# Carregando o script 03_manipulacao_dados.R
source("scripts/03_manipulacao_dados.R")

### FIGURAS -------------------------------------------------------------------

## FIGURA 2 -------------------------------------------------------------------

# Plotagem do gráfico de linhas
figura_2 <- dados_figura2 |>
              ggplot2::ggplot(ggplot2::aes(x = year, y = freq)) +
              ggplot2::geom_line() +
              ggplot2::geom_area(fill = '#8ebaef', alpha = .3) +
              ggplot2::scale_y_continuous(breaks = scales::breaks_pretty()) +
              ggplot2::scale_x_continuous(limits = c(1978, 2024),
                                          breaks = seq(1978, 2024, 3)) +
              ggplot2::labs(x = "Ano", y = "Artigos Publicados") +
              ggthemes::theme_hc(base_family = "Times New Roman") +
              ggplot2::theme(
                axis.text = ggplot2::element_text(size = 15),
                axis.title = ggplot2::element_text(size = 18)
              )

# Salvando a figura 2 em /figuras
print(figura_2) |>
  ggplot2::ggsave(
    filename = "figuras/figura_2.png",
    width = 16,    # Largura do gráfico em polegadas
    height = 9,    # Altura do gráfico em polegadas
    dpi = 300      # Resolução do gráfico em DPI (dots per inch)
  )

## FIGURA 3 -------------------------------------------------------------------

# Importando o mapa mundi do pacote ggplot2
mapa <- ggplot2::map_data("world") |>
          dplyr::mutate(
            region = tolower(region)
         )

# Plotagem do mapa
figura_3 <- ggplot2::ggplot() +
              ggplot2::geom_map(data = mapa, map = mapa,
                                ggplot2::aes(long, lat, map_id = region),
                                color = "#6E7B7C", fill = "#D1DBDD",
                                linewidth = .1) +
              ggplot2::geom_map(data = dados_figura3, map = mapa,
                                ggplot2::aes(fill = freq, map_id = region),
                                color = "#7f7f7f", linewidth = .5) +
              ggplot2::scale_fill_gradient(low = "#87CEEB",
                                           high = "#104E8B",
                                           name = "Artigos",
                                           limits = c(0, 2800),
                                           breaks = seq(0, 2800, 500)) +
              ggplot2::scale_x_continuous(breaks = c()) +
              ggplot2::scale_y_continuous(breaks = c()) +
              ggplot2::xlab("") +
              ggplot2::ylab("") +
              ggthemes::theme_map(base_family = "Times New Roman") +
              ggplot2::theme(
                legend.position = c(.05, .25)
              )

# Salvando a figura 3 em /figuras
print(figura_3) |>
  ggplot2::ggsave(
    filename = "figuras/figura_3.png",
    width = 16,    # Largura do gráfico em polegadas
    height = 9,    # Altura do gráfico em polegadas
    dpi = 300      # Resolução do gráfico em DPI (dots per inch)
  )

## FIGURA 4 -------------------------------------------------------------------

# Rótulos para os países destacados
n_paises <- length(c("China", "Estados Unidos", "Reino Unido", "Paquistão", "Turquia",
              "Austrália", "Índia", "Alemanha"))

# Plotando a figura 4
figura_4 <- ggplot2::ggplot(data = dados_figura4 |>
                              dplyr::filter(total_article < 1300),
                            ggplot2::aes(year, articles, group = country)
            ) +
            ggplot2::geom_line(color = "grey75", linewidth = .6,
                               alpha = .5, show.legend = FALSE) +
            ggplot2::geom_line(
              data = dados_figura4 |>
                dplyr::filter(
                  country %in% c("China",
                                 "Estados Unidos",
                                 "Reino Unido",
                                 "Paquistão",
                                 "Turquia",
                                 "Austrália",
                                 "Índia",
                                 "Alemanha")
                ),
              ggplot2::aes(col = country), linewidth = .9, show.legend = FALSE
            ) +
            ggplot2::geom_segment(
              data = tibble::tibble(y = 0, x1 = 1990, x2 = 2024),
              ggplot2::aes(x = x1, xend = x2, y = y, yend = y),
              inherit.aes = FALSE,
              color = "grey60",
              size = .8
            ) +
            ggplot2::geom_vline(
              ggplot2::aes(xintercept = ano_ref),
              color = "grey40",
              linetype = "dotted",
              size = .8
            )

# Inserção de texto e legenda no gráfico
figura_4 <- figura_4 +
              ggplot2::annotate(
                "text", x = 2017.5, y = 630,
                label = "2018",
                family = "Times New Roman",
                size = 6,
                color = "grey40",
                hjust = .5,
                lineheight = 0.8
              ) +
              ggrepel::geom_text_repel(
                data = dados_figura4 |>
                  dplyr::filter(year == 2024) |>
                  dplyr::arrange(dplyr::desc(articles)) |>
                  head(8),
                ggplot2::aes(color = country, label = country),
                family = "Times New Roman",
                fontface = "bold",
                size = 5,
                direction = "y",
                xlim = c(2030, NA),
                hjust = 0,
                segment.size = .7,
                segment.alpha = .5,
                segment.linetype = "dotted",
                box.padding = .4,
                segment.curvature = -0.1,
                segment.ncp = 3,
                segment.angle = 20,
                max.overlaps = Inf,
                show.legend = FALSE
              ) +
              # Sistema de coordenadas
              ggplot2::coord_cartesian(
                clip = "off",
                ylim = c(0, 2800)
              ) +
              # Correção de escala do eixo x e y e cores
              ggplot2::scale_x_continuous(limits = c(1990, 2024),
                                          breaks = seq(1990, 2024, 3)) +
              ggplot2::scale_y_continuous(limits = c(0, 2800),
                                          breaks = scales::breaks_pretty()) +
              ggplot2::scale_color_manual(
                values = c(
                  rcartocolor::carto_pal(
                    n = n_paises, name = "Bold")[1:n_paises-1], "grey50"
                )
              ) +
              ggplot2::labs(y = "Artigos Publicados", x = "Ano") +
              beautyxtrar::theme_academic() +
              ggplot2::theme(
                axis.text = ggplot2::element_text(size = 15),
                axis.title = ggplot2::element_text(size = 18)
              )

# Salvando a figura 4 em /figuras
print(figura_4) |>
  ggplot2::ggsave(
    filename = "figuras/figura_4.png",
    width = 16,    # Largura do gráfico em polegadas
    height = 9,    # Altura do gráfico em polegadas
    dpi = 300      # Resolução do gráfico em DPI (dots per inch)
  )

## FIGURA 5 -------------------------------------------------------------------

# Abrir o dispositivo gráfico para salvar a figura 5
png(filename = "figuras/figura_5.png",
    width = 16,    # Largura do gráfico em polegadas
    height = 9,    # Altura do gráfico em polegadas
    units = "in",  # Unidades em polegadas
    res = 300)     # Resolução do gráfico em DPI (dots per inch)

# Plot do mapa como background
maps::map('world', fill = TRUE, col = '#E4E9EA', border = '#A8ACAD', lwd = .8)

# Definindo os limites do eixo x e y
xrange <- range(collab_net %v% 'lon', na.rm = TRUE)
yrange <- range(collab_net %v% 'lat', na.rm = TRUE)

xlim <- c(xrange[1] - 10, xrange[2] + 10)
ylim <- c(yrange[1] - 10, yrange[2] + 10)

# Plotagem da rede
figura_5 <- network::plot.network(collab_net,
                                  new = FALSE,
                                  coord = cbind(collab_net %v% 'lon',
                                                collab_net %v% 'lat'),
                                  edge.col = '#5c7dbf40',
                                  edge.label.cex = .1,
                                  vertex.cex = 0.7,
                                  vertex.col = '#5c7dbf',
                                  vertex.border = 'grey70',
                                  jitter = FALSE,
                                  xlim = xlim,
                                  ylim = ylim)


# Fechar o dispositivo gráfico
dev.off()

## FIGURA 6 -------------------------------------------------------------------

figura_6 <- ggplot2::ggplot(data = dados_figura6 |>
                              dplyr::filter(total_n < 700),
                            ggplot2::aes(py, n, group = areas)) +
            ggplot2::geom_line(color = "grey75",
                               linewidth = .6,
                               alpha = .5, show.legend = FALSE) +
            ggplot2::geom_line(
              data = dados_figura6 |>
                dplyr::filter(total_n > 700),
              ggplot2::aes(col = areas),
              linewidth = .9
            )

# Rótulos para as áreas destacadas
n_areas <- length(areas <- c("Ciência Ambiental",
                             "Ciências Sociais",
                             "Economia, Econometria e Finanças",
                             "Energia",
                             "Medicina"))

figura_6 <- figura_6 +
            # Sistema de coordenadas
            ggplot2::coord_cartesian(
              clip = "off",
              ylim = c(0, 610)
            ) +
            # Correção de escala do eixo x e y e cores
            ggplot2::scale_x_continuous(limits = c(1978, 2024),
                                        breaks = seq(1978, 2024, 3)) +
            ggplot2::scale_y_continuous(limits = c(0, 610),
                                        breaks = scales::breaks_pretty()) +
            ggplot2::scale_color_manual(
              values = c(
                rcartocolor::carto_pal(
                  n = n_areas, name = "Bold")[1:n_areas-1], "grey50"
              )
            ) +
            ggplot2::labs(y = "Artigos Publicados", x = "Ano", col = NULL) +
            beautyxtrar::theme_academic() +
            ggplot2::theme(
              legend.position = c(.13, .9),
              legend.text = ggplot2::element_text(size = 16),
              axis.text = ggplot2::element_text(size = 15),
              axis.title = ggplot2::element_text(size = 18)
            )

# Salvando a figura 6 em /figuras
print(figura_6) |>
  ggplot2::ggsave(
    filename = "figuras/figura_6.png",
    width = 16,    # Largura do gráfico em polegadas
    height = 9,    # Altura do gráfico em polegadas
    dpi = 300      # Resolução do gráfico em DPI (dots per inch)
  )

## FIGURA 7 -------------------------------------------------------------------

# Plotagem da figura 7a
figura_7a <- dados_figura7 |>
              ggplot2::ggplot(ggplot2::aes(x = total_citacoes,
                                           y = pais_fator,
                                           col = total_citacoes)) +
              ggplot2::geom_col(fill = "#77a3bf", col = "#688fa5",
                                width = .8, show.legend = FALSE) +
              ggplot2::scale_x_continuous(breaks = scales::breaks_pretty(n = 5)) +
              ggplot2::geom_text(
                data = dados_figura7 |>
                  dplyr::filter(total_citacoes >= 14000),
                ggplot2::aes(label = total_citacoes,
                             x = total_citacoes - 0.05 * max(
                               dados_figura7$total_citacoes)
                ),
                size = 6, show.legend = FALSE, col = 'gray97',
                family = "Times New Roman") +
              ggplot2::labs(x = "Total de Citações", y = "País") +
              beautyxtrar::theme_academic() +
              ggplot2::theme(
                legend.text = ggplot2::element_text(size = 16),
                axis.text = ggplot2::element_text(size = 15),
                axis.title = ggplot2::element_text(size = 18)
              )

# Salvando a figura 7a em /figuras
print(figura_7a) |>
  ggplot2::ggsave(
    filename = "figuras/figura_7a.png",
    width = 16,    # Largura do gráfico em polegadas
    height = 9,    # Altura do gráfico em polegadas
    dpi = 300      # Resolução do gráfico em DPI (dots per inch)
  )

# Plotagem da figura 7b
figura_7b <- dados_figura7 |>
              dplyr::mutate(pais_fator = forcats::fct_reorder(pais, media_tc)) |>
              ggplot2::ggplot(ggplot2::aes(x = media_tc, y = pais_fator,
                                           col = media_tc)) +
              ggplot2::geom_col(fill = "#77a3bf", col = "#688fa5",
                                width = .8, show.legend = FALSE) +
              ggplot2::scale_x_continuous(limits = c(0, 80),
                                          breaks = scales::breaks_pretty(n = 5)) +
              ggplot2::geom_text(data = dados_figura7 |>
                                   dplyr::filter(media_tc >= 50),
                                 ggplot2::aes(label = media_tc,
                                              x = media_tc - 0.05 * max(
                                                dados_figura7$media_tc)
                                 ),
                                 size = 6, show.legend = FALSE, col = 'gray97',
                                 family = "Times New Roman") +
              ggplot2::labs(x = "Média de Citações", y = "País") +
              beautyxtrar::theme_academic() +
              ggplot2::theme(
                legend.text = ggplot2::element_text(size = 16),
                axis.text = ggplot2::element_text(size = 15),
                axis.title = ggplot2::element_text(size = 18)
              )

# Salvando a figura 7b em /figuras
print(figura_7b) |>
  ggplot2::ggsave(
    filename = "figuras/figura_7b.png",
    width = 16,    # Largura do gráfico em polegadas
    height = 9,    # Altura do gráfico em polegadas
    dpi = 300      # Resolução do gráfico em DPI (dots per inch)
  )

## FIGURA 8 -------------------------------------------------------------------

# Plotagem da figura 8
figura_8 <- dados_figura8 |>
              ggplot2::ggplot(ggplot2::aes(y = reorder(item, year_med))) +
              ggalt::geom_dumbbell(aes(x = year_q1, xend = year_q3),
                                   color = "#C2C8DB",
                                   size = 1.8,
                                   dot_guide = FALSE,
                                   size_x = 1.3,
                                   size_xend = 1.3,
                                   colour_x = "#C2C8DB",
                                   colour_xend = "#C2C8DB",
                                   show.legend = FALSE) +
              ggplot2::geom_point(aes(x = year_med, y = item, size = freq),
                                  col = '#577B9B', alpha = .8,
                                  shape = 21, stroke = 1,
                                  fill = '#6187B0', show.legend = FALSE) +
              ggplot2::scale_size(range = c(2, 8)) +
              ggplot2::scale_x_continuous(limits = c(1992, 2024),
                                          breaks = scales::breaks_width(4)) +
              beautyxtrar::theme_xtra(base_family = "Times New Roman") +
              ggplot2::labs(x = NULL, y = "Palavras-Chave dos Autores") +
              ggplot2::theme(
                axis.text.y = ggplot2::element_text(size = 10,
                                                    hjust = 1,
                                                    vjust = 0.5),
                panel.grid.major = ggplot2::element_line(color = "gray90",
                                                         linewidth = .5),
                panel.grid.minor = ggplot2::element_line(color = "gray90",
                                                         linewidth = 0.25,
                                                         linetype = "dashed")
              )

# Salvando a figura 8 em /figuras
print(figura_8) |>
  ggplot2::ggsave(
    filename = "figuras/figura_8.png",
    width = 16,    # Largura do gráfico em polegadas
    height = 9,    # Altura do gráfico em polegadas
    dpi = 300      # Resolução do gráfico em DPI (dots per inch)
  )

### TABELAS -------------------------------------------------------------------

## Tabela 1 -------------------------------------------------------------------

# Preparando os dados da tabela
tabela_1 <- dados_tabela1 |>
              head(10) |>
              dplyr::rename(
                `Periódico` = element,
                `H Index` = h_index_x,
                `Total de Citações` = tc_x,
                `Total de Artigos` = np,
                `Ano Inicial de Publicação` = py_start
              )

# Criando o gráfico da tabela usando ggplot2
tabela_1 <- ggplot2::ggplot(tabela_1,
                            ggplot2::aes(x = "", y = Periódico)) +
            ggplot2::geom_blank() +
            ggplot2::theme_void() +
            ggplot2::annotation_custom(
              gridExtra::tableGrob(tabela_1)
            ) +
            ggplot2::ggtitle(
              "Tabela 1 - Desempenho dos principais periódicos científicos em termos de índice de produtividade acadêmica"
            ) +
            ggplot2::theme(
              plot.title = ggplot2::element_text(
                size = 14, face = "bold", hjust = 0.5, vjust = -.5
              )
            )

# Gerando o PDF em /tabelas
pdf("tabelas/tabela_1.pdf", height = 3.5, width = 14, family = "Times")
print(tabela_1)
dev.off()

## Tabela 2 -------------------------------------------------------------------

# Preparando os dados da tabela
tabela_2 <- dados_tabela2 |>
              head(10) |>
              dplyr::rename(
                `Instituição` = au_un,
                `Total de Citações` = total_artigos_instituto,
                `Total de Artigos` = articles
              )

# Criando o gráfico da tabela usando ggplot2
tabela_2 <- ggplot2::ggplot(tabela_2,
                            ggplot2::aes(x = "", y = Instituição)) +
            ggplot2::geom_blank() +
            ggplot2::theme_void() +
            ggplot2::annotation_custom(
              gridExtra::tableGrob(tabela_2)
            ) +
            ggplot2::ggtitle(
              "Tabela 2 - As dez instituições científicas mais influentes em termos de total de citações recebidas e total de artigos publicados"
            ) +
            ggplot2::theme(
              plot.title = ggplot2::element_text(
                size = 14, face = "bold", hjust = 0.5, vjust = -.5
              )
            )

#  Gerando o PDF em /tabelas
pdf("tabelas/tabela_2.pdf", height = 3.5, width = 14, family = "Times")
print(tabela_2)
dev.off()

## Tabela 3 -------------------------------------------------------------------

# Preparando os dados da tabela
tabela_3 <- dados_tabela3 |>
              head(10) |>
              dplyr::rename(
                `Autor` = element,
                `H Index` = h_index,
                `Total de Citações` = tc,
                `Total de Artigos` = np,
                `Ano Inicial de Publicação` = py_start
              )

# Criando o gráfico da tabela usando ggplot2
tabela_3 <- ggplot2::ggplot(tabela_3,
                            ggplot2::aes(x = "", y = Autor)) +
            ggplot2::geom_blank() +
            ggplot2::theme_void() +
            ggplot2::annotation_custom(
              gridExtra::tableGrob(tabela_3)
            ) +
            ggplot2::ggtitle(
              "Tabela 3 - Os dez autores mais influentes em termos de índice de produtividade acadêmica"
            ) +
            ggplot2::theme(
              plot.title = ggplot2::element_text(
                size = 14, face = "bold", hjust = 0.5, vjust = -.5
              )
            )

#  Gerando o PDF em /tabelas
pdf("tabelas/tabela_3.pdf", height = 3.5, width = 14, family = "Times")
print(tabela_3)
dev.off()

## Tabela 4 -------------------------------------------------------------------

# Preparando os dados da tabela
tabela_4 <- dados_tabela4 |>
              dplyr::select(-c(2:3, 7, 8)) |>
              head(10) |>
              dplyr::rename(
                `Documento` = Document,
                `LCS` = `Local Citations`,
                `GCS` = `Global Citations`,
                `Influência Local (%)` = `LC/GC Ratio (%)`
              )

# Criando o gráfico da tabela usando ggplot2
tabela_4 <- ggplot2::ggplot(tabela_4,
                            ggplot2::aes(x = "", y = Documento)) +
            ggplot2::geom_blank() +
            ggplot2::theme_void() +
            ggplot2::annotation_custom(
              gridExtra::tableGrob(tabela_4)
            ) +
            ggplot2::ggtitle(
              "Tabela 4 -  Principais documentos em termos de citação local e relevância para o campo de estudo"
            ) +
            ggplot2::theme(
              plot.title = ggplot2::element_text(
                size = 14, face = "bold", hjust = 0.5, vjust = -.5
              )
            )

#  Gerando o PDF em /tabelas
pdf("tabelas/tabela_4.pdf", height = 3.5, width = 14, family = "Times")
print(tabela_4)
dev.off()
