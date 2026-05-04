# Arquivo: 01-avaliacao-resolucao.R
# Integrante 1: Evelyn C.M. Leal
# Integrante 2: Isadora Faria Leal
# Integrante 3: Kerollayne Borges O.Sousa
# Integrante 4: Sara Pâmela Ferreira
# Data: 28/04/2026
# Objetivo: Resolução da Avaliação 1 — Introdução à Ciência de Dados


# Configurações globais -----------------------------------------------

options(digits = 5, scipen = 999)

# carrega os pacotes usados (Exercício 1)
library(here) # para usar caminhos relativos
library(tidyverse) # carrega o dplyr, readr, ggplot2, etc.
library(janitor) # para limpar os nomes das colunas

# Exercício 1 -----------------------------------------------------------

# importa o arquivo agencias.csv

# define o caminho relativo do arquivo usando a função here():
caminho_agencias <- here("dados/brutos/agencias.csv")
  
 # importa o arquivo com a função read_csv:
  dados_agencias <- read.csv(caminho_agencias)
  
  # inspeciona a estrutura do objeto
  glimpse(dados_agencias)
  
  # importa o arquivo credito_trimestral.csv
  
  # define o caminho relativo do arquivo usando a função here():
  caminho_credito <- here("dados/brutos/credito_trimestral.csv")
  
  # importa o arquivo com a função read_csv:
  dados_credito <-read.csv(caminho_credito)
  
  # inspeciona a estrutura do objeto
  glimpse(dados_credito)
  
  
  # Exercício 2 ----------------------------------------------------------

# 2.a)
  dados_agencias_plenas <- dados_agencias |> 
    filter(tipo_agencia == "Plena")
  
  glimpse(dados_agencias_plenas)
 

# 2.b)
  dados_agencias |> 
    select("nome_agencia", "cidade", "num_cooperados") |> 
    arrange(desc("num_cooperados"))
  
  view(dados_agencias)
         

# 2.c)
  dados_vendas_divinopolis <- dados_agencias |>
    filter(cidade == "Divinópolis", num_cooperados > 1000)

  view(dados_vendas_divinopolis)


# Exercício 3 ---------------------------------------------------------

# 3.a) pivot_longer
  

# reorganiza os dados de crédito em trimestre e volume_credito
dados_credito_longo <- dados_credito |> 
    pivot_longer(
      cols = c("T1", "T2", "T3", "T4"),
      names_to = "trimestre",
      values_to = "volume_crédito"
    )
  
  # 3.b) left_join
  
  # combina `dados_credito_longo`com `dados_agencias`
  dados_completos <- dados_credito_longo |> 
    left_join(dados_agencias, by = "codigo_agencia")
  
  glimpse(dados_completos)
  
  view(dados_completos)

  
  # Exercício 4 ---------------------------------------------------------

# cria dados_analise com credito_por_cooperado
dados_analise <- dados_completos |>  
    # converte as variaveis para fatores nominais 
    # e cria a variavel receita
    mutate(
      crédito_por_cooperado = as.factor (volume_crédito/num_cooperados),
    )
    
  glimpse(dados_analise)
  
  view(dados_analise)
  
  # resume por cidade e ordena por volume_total
  dados_analise |>
    group_by(cidade) |> 
    summarise(
      volume_total = sum(volume_crédito),
      media_dos_creditos_por_cooperado = mean(crédito_por_cooperado)
    ) |> 
    arrange(desc(volume_total))
  
  glimpse(dados_analise)
  
  

  
  # Resposta do Exercício 4:
  
  # Cidade com maior volume_total: Divinópolis
  # Cidade com maior media_dos_creditos_por_cooperado:
  
  
  
  # Exercício 5 ---------------------------------------------------------

# classifica nivel_credito e resume por tipo_agencia
resumo_por_tipo <- 
    