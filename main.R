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

## Figura 03 ----------------------------------------------------------------

# Plotagem do gráfico de linhas
dados_figura2 |>
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

## Figura 03 ---------------------------------------------------------------

# Importando o mapa mundi do pacote ggplot2
mapa <- ggplot2::map_data("world") |>
          dplyr::mutate(
            region = tolower(region)
         )

# Plotagem do mapa
ggplot2::ggplot() +
  ggplot2::geom_map(data = mapa, map = mapa, ggplot2::aes(long, lat, map_id = region),
                    color = "#6E7B7C", fill = "#D1DBDD", linewidth = .1) +
  ggplot2::geom_map(data = dados_figura3, map = mapa,
                    ggplot2::aes(fill = freq, map_id = region),
                    color = "#7f7f7f", linewidth = .5) +
  ggplot2::scale_fill_gradient(low = "#87CEEB", high = "#104E8B", name = "Artigos",
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

## Figura 04 ---------------------------------------------------------------


