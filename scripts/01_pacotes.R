### ======================= SCRIPT 01 - PACOTES ==============================

## Lista de pacotes necessários --------------------------------------------

pacotes <- c(
  # Pacotes do universo tidy
  "tidyverse",
  # Conversão e obtenção de dados bibliométricos
  "bibliometrix",
  # Customização do plot
  "ggalt",
  "scales",
  "rcartocolor",
  # Temas para gráficos
  "ggthemes",
  "beautyxtrar",
  # Pacotes para storytelling
  "showtext",
  "ggrepel",
  "ggtext",
  # Mapas
  "maps",
  # Rede - análise de ocorrência
  "network",
  # Manipulação de dados - formatação dos nomes de colunas
  "janitor",
  # Visualização das variáveis da base principal
  "knitr",
  # Importação de dados
  "readxl",
  # Fontes e tabelas
  "extrafont",
  "gridExtra"
)

# Verificar quais pacotes já estão instalados
pacotes_instalados <- pacotes %in% rownames(installed.packages())

## Instalar pacotes que não estão instalados ----------------------------------

# Instalar pacotes do CRAN que não estão instalados
if (any(!pacotes_instalados & pacotes != "beautyxtrar")) {
  install.packages(pacotes[!pacotes_instalados & pacotes != "beautyxtrar"])
}

# Instalar pacote beautyxtrar do GitHub se não estiver instalado
if (!"beautyxtrar" %in% rownames(installed.packages())) {
  if (!requireNamespace("devtools", quietly = TRUE)) {
    install.packages("devtools")
  }
  devtools::install_github("drewmelo/beautyxtrar", force = TRUE)
}

# Função para carregar pacotes silenciosamente e verificar carregamento
carregar_pacote <- function(pacote) {
  suppressPackageStartupMessages({
    suppressMessages({
      suppressWarnings({
        library(pacote, character.only = TRUE)
      })
    })
  })
}

# Carregar todos os pacotes e verificar sucesso
pacotes_carregados <- sapply(pacotes, function(pacote) {
  carregar_pacote(pacote)
  pacote %in% .packages()
})

# Imprimir resultado do carregamento
if (all(pacotes_carregados)) {
  print("Carregamento dos pacotes concluído com sucesso.")
} else {
  pacotes_falha <- pacotes[!pacotes_carregados]
  mensagem_falha <- paste("Falha ao carregar os seguintes pacotes:",
                          paste(pacotes_falha, collapse = ", "))
  print(mensagem_falha)
}

# Carregando fontes do dispositivo
extrafont::loadfonts(device = "win", quiet = TRUE)

# Verificar se a fonte "Times New Roman" está disponível
if ("Times New Roman" %in% extrafont::fonts()) {
  print("A fonte 'Times New Roman' foi carregada com sucesso.")
} else {
  print("A fonte 'Times New Roman' não está disponível.")
}
