Crescimento Econômico e Mudanças Climáticas: uma análise bibliométrica
================

[![Project Status:
Active](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

## Resumo

O conceito de desenvolvimento sustentável surgiu nos anos 1970 como
alternativa ao crescimento econômico desenfreado e ganhou relevância com
o relatório do Clube de Roma em 1972, destacando a necessidade de
equilibrar crescimento econômico, proteção ambiental e justiça social.
Este estudo visa mapeia a evolução da literatura sobre a interseção
entre crescimento econômico e mudanças climáticas, usando análise
bibliométrica de 5919 documentos na base Scopus e Web of Science
(1978-2024) com uso do software R. Os resultados mostram um crescimento
anual de 13,77% na produção científica, com a China liderando em
publicações, seguida por Estados Unidos e Reino Unido. As áreas
principais são Ciência Ambiental, Ciências Sociais, Economia, Energia e
Medicina. Instituições como Universidade Tsinghua e Academia Chinesa de
Ciências, e autores como Wang S., Bekun F. e Adebayo T., são destacados.
Revelando uma expansão rápida, colaboração internacional significativa e
identifica lacunas que necessitam de investigação, sublinhando a
importância da colaboração global para soluções sustentáveis.

**Palavras-chave:** Análise bibliométrica; Desenvolvimentos
sustentáveis; Economia; Crescimento econômico.

## Índice

1.  [Descrição dos Scripts](#descrição-dos-scripts) 1.1.
    [Compilação](#compilação)
2.  [Descrição dos Dados](#descrição-dos-dados)
3.  [Instalação](#instalação)

## Descrição dos Scripts

Este repositório contém uma variedade de arquivos de código e
diretórios. Em particular, a pasta **scripts** inclui três arquivos
programados em R, que são:

- `/01_pacotes.R`: Responsável por gerenciar pacotes necessários para o
  projeto. Este script verifica quais pacotes estão instalados, instala
  os que faltam, e carrega todos os pacotes de forma silenciosa. Além
  disso, carrega fontes do dispositivo e verifica se a fonte “Times New
  Roman” está disponível, imprimindo uma mensagem de sucesso ou falha.

- `/02_importacao_dados.R`: Caracterizado pela importação dos dados
  bibliométrico, tendo como objetivo a conversão dos arquivos de dados
  do **Scopus** e **Web of Science** (WoS) em data frames, remoção de
  artigos duplicados, e filtragem de artigos científicos. O script
  também realiza análises bibliométricas, gera resultados resumidos e
  gráficos, e limpa o ambiente global removendo objetos temporários.
  Além disso, ele importa dados adicionais de arquivos Excel e CSV para
  visualizações e análises mais detalhadas. É importante ressaltar que
  **a compilação deste script levará alguns minutos para ser concluída**
  (aproximadamente 3 a 5 minutos), dependendo do poder de processamento
  da sua máquina.

- `/03_manipulacao_dados.R`: Este script realiza manipulações nos dados
  do **Scimago Journal & Country Rank** (Scimagojr) e nas figuras a
  serem geradas a partir dos dados obtidos no tópico anterior. Ele junta
  a base de áreas de periódicos com a base principal, atribui áreas
  manualmente para periódicos relevantes, e limpa e renomeia colunas
  conforme necessário. Além disso, o script manipula os dados das
  figuras, como renomear colunas, identificar e filtrar os maiores
  países em produções científicas, ajustar os dados para plotagem de
  redes de colaboração entre países, realizar contagem acumulada de
  artigos por área, calcular variação anual de artigos e gerar análises
  de tabelas relacionadas a periódicos, instituições e autores.

### Compilação

Para executar os três scripts mencionados, abra o código principal,
denominado `main.R.` Este script principal carrega e executa
sequencialmente os trechos de códigos auxiliares necessários. Além
disso, o `main.R` gera figuras e tabelas a partir dos dados analisados,
exportando esses resultados para a pasta **figuras** (em formato *png*)
e **tabelas** (em formato *pdf*), onde estarão disponíveis como output
final.

## Descrição dos Dados

Todos os dados deste projeto estão localizados na pasta **dados**,
necessários para a construção dos resultados presentes no artigo. A
pasta está subdividida em três diretórios:

- `/dados_scopuswos/`: Esta pasta contém os dados obtidos das
  plataformas Scopus e Web of Science. Há um arquivo (`scopus.csv`)
  referente à Scopus e 12 arquivos em formato *txt* provenientes do WoS.

- `/dados_biblioshiny/`: No diretório `/dados/`, encontra-se o arquivo
  `dados.xlsx`, utilizado para gerar análises através da ferramenta
  *biblioshiny* do pacote *Biliometrix*. Os resultados dessas análises
  foram exportados diretamente para esta pasta, totalizando 9 arquivos,
  todos em formato *xlsx*.

- `/dados_scimagojr/`: Esta pasta contém dados da plataforma Scimagojr,
  utilizados para obter as áreas de pesquisa dos periódicos, estando
  denominado como `area_pesquisa.csv.`. A partir deste, foi possível
  realizar a junção dos dados do Scimagojr com a base principal (objeto
  denominado `dados`), utilizando os nomes dos periódicos como
  referência.

## Instalação

Para reproduzir os resultados deste projeto, você pode clonar este
repositório. Siga os passos abaixo:

1.  **Clone o repositório**:

    ``` sh
    git clone https://github.com/drewmelo/crescimento-economico-mudancas-climaticas.git
    ```

2.  **Navegue até o diretório do projeto**:

    ``` sh
    cd crescimento-economico-mudancas-climaticas
    ```
