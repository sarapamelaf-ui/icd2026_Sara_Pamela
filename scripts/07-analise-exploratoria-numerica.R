# Arquivo: 07-analise-exploratoria-numerica.R
# Autor(a): Sara Pamela Ferreira
# Data: 15/06/2026
# Objetivos:
# 1. Carregar e inspecionar os dados de marketing
# 2. Calcular estatísticas amostrais de centro, posição e variabilidade
# 3. Interpretar a receita semanal com base nos dados observados

# 0. Pacotes e opções globais --------------------------------------------

# Opções de exibição numérica
options(digits = 5, scipen = 999)

# Pacotes usados nesta aula
library(here)
library(tidyverse)


# 1. Carregamento dos dados -----------------------------------------------

# Caminho do arquivo de dados limpos
caminho_dados <- here("dados/limpos/dados_marketing_limpos.rds")

# Leitura dos dados de marketing
dados_marketing <-  read_rds(caminho_dados)

# Estrutura dos dados carregados
glimpse(dados_marketing)


# 2. Inspeção inicial -----------------------------------------------------

# Dimensões da base 
dim(dados_marketing)

# Nomes das variaveis
names(dados_marketing)

# Primeiras linhas da base
head(dados_marketing, 15)

# Contagem de semanas por status de promoção
dados_marketing |> 
  count(status_promocao)


# 3. Valores típicos da receita -------------------------------------------

estatisticas_centro <- dados_marketing |>
  summarize(
    media = mean(receita_vendas),
    
    mediana = median(receita_vendas)
    
  )

estatisticas_centro


# 4. Resumo rápido da receita ---------------------------------------------

# Extrai a coluna receitas_vendas como um vetor

receita <- dados_marketing |> 
  pull(receita_vendas)

# Resumo amostras basico
summary(receita)


# 5. Quantis da receita ---------------------------------------------------

# Quantis amostrais selecionados
quantis_receita <- dados_marketing |> 
  summarize(
    #Minimo amostral e percentil 5
    p0 = quantile(receita_vendas, 0),
    p5 = quantile(receita_vendas, 0.05),
    
    #Quartis amostrais
    q1 = quantile(receita_vendas, 0.25),
    mediana = quantile(receita_vendas,0,50),
    q3 = quantile(receita_vendas, 0,75),
    
    # Percentil 95 e maximo amostral
    p95 = quantile(receita_vendas, 0.95),
    p100 = quantile(receita_vendas, 1)
  )


# 6. Variabilidade da receita ---------------------------------------------

estatistcas_variabilidade <-  dados_marketing |> 
  summarize(
    amplitude - max(receita_vendas) - min(receita_vendas),
    
    variancia = var(receita_vendas),
    
    desvio_padrao = sd(receita_vendas),
    
    iqr = IQR(receita_vendas)
  )
