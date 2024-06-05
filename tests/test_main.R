library(testthat)

test_that("Teste básico para o script main.R", {
  # Carrega o script main.R
  source("main.R")

  # Verifica se os arquivos de saída foram gerados corretamente
  expect_true(file.exists("figuras"))
  expect_true(file.exists("tabelas"))

  # Adicione mais testes conforme necessário para verificar se os resultados
  # produzidos pelo main.R estão corretos
})

