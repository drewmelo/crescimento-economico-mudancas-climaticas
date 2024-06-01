### ======================= SCRIPT PRINCIPAL ==============================

## SCRIPT 01 - PACOTES -----------------------------------------------------

# Carregando o script 01_pacotes.R
source("scripts/01_pacotes.R")

## SCRIPT 02 - IMPORTAÇÃO DOS DADOS ----------------------------------------

# Carregando o script 02_importacao_dados.R
source("scripts/02_importacao_dados.R")

## SCRIPT 03 - MANIPULAÇÃO DOS DADOS ----------------------------------------

# Carregando o script 03_manipulacao_dados.R
source("scripts/03_manipulacao_dados.R")

### FIGURAS -----------------------------------------------------------------

## FIGURA 2 ----------------------------------------------------------------

# Plotagem do gráfico de linhas
figura_2 <- dados_figura2 |>
              ggplot2::ggplot(ggplot2::aes(x = year, y = freq)) +
              ggplot2::geom_line() +
              ggplot2::geom_area(fill = '#8ebaef', alpha = .3) +
              ggplot2::scale_y_continuous(breaks = scales::breaks_pretty()) +
              ggplot2::scale_x_continuous(limits = c(1978, 2024),
                                          breaks = seq(1978, 2024, 3)) +
              ggplot2::labs(x = "Ano", y = "Artigos publicados") +
              ggthemes::theme_hc(base_family = "Times New Roman") +
              ggplot2::theme(
                axis.text = ggplot2::element_text(size = 15),
                axis.title = ggplot2::element_text(size = 18)
              )

# Visualização da figura
figura_2

# Salvando a figura 2 na pasta figuras
ggplot2::ggsave(
  filename = "figuras/figura_2.png",
  plot = figura_2,
  width = 16,    # Largura do gráfico em polegadas
  height = 9,    # Altura do gráfico em polegadas
  dpi = 300      # Resolução do gráfico em DPI (dots per inch)
)

## FIGURA 3 ---------------------------------------------------------------

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

# Visualização da figura
figura_3

# Salvando a figura 3 na pasta figuras
ggplot2::ggsave(
  filename = "figuras/figura_3.png",
  plot = figura_3,
  width = 16,    # Largura do gráfico em polegadas
  height = 9,    # Altura do gráfico em polegadas
  dpi = 300      # Resolução do gráfico em DPI (dots per inch)
)


## FIGURA 4 ---------------------------------------------------------------

# Rótulos para os países destacados
n <- length(c("China", "Estados Unidos", "Reino Unido", "Paquistão", "Turquia",
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
                  rcartocolor::carto_pal(n = n, name = "Bold")[1:n-1], "grey50"
                )
              ) +
              ggplot2::labs(y = "Artigos publicados", x = "Ano") +
              beautyxtrar::theme_academic() +
              ggplot2::theme(
                axis.text = ggplot2::element_text(size = 15),
                axis.title = ggplot2::element_text(size = 18)
              )

# Visualização da figura
figura_4

# Salvando a figura 3 na pasta figuras
ggplot2::ggsave(
  filename = "figuras/figura_4.png",
  plot = figura_4,
  width = 16,    # Largura do gráfico em polegadas
  height = 9,    # Altura do gráfico em polegadas
  dpi = 300      # Resolução do gráfico em DPI (dots per inch)
)

## FIGURA 5 ---------------------------------------------------------------

# Abrir o dispositivo gráfico para salvar a figura
png(filename = "figuras/figura_5.png",
    width = 16,    # Largura do gráfico em polegadas
    height = 9,    # Altura do gráfico em polegadas
    units = "in",  # Unidades em polegadas
    res = 300)     # Resolução do gráfico em DPI (dots per inch)

# Plot do mapa como background (compilar esta linha primeiro nesta seção)
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

