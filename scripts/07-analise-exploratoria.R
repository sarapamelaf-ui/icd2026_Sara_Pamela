# Arquivo: 07-analise-exploratoria.R
# Autor(a): Sara Pamela Ferreoira
# Data: 25/05/2026
# Objetivos:
# 1. Calcular estatísticas descritivas amostrais.
# 2. Visualizar a distribuição empírica dos dados.
# 3. Comparar padrões entre grupos.

# 0. Configurações globais ---------------------------------------------

# Controla a forma como os valores numéricos aparecem no console.
options(digits = 5, scipen = 999)

# Carrega os pacotes usados para caminhos, manipulação e visualização.
library(here)
library(tidyverse)
library(tidyplots)

# Permite que os gráficos do tidyplots usem o espaço disponível.
tidyplots_options(width = NA, height = NA)

# 1  carregamento dos dados

caminho_dados <-  here("dados/limpos/dados_marketing_limpos.rds")

dados_marketing <-  read_rds(caminho_dados)

glimpse(dados_marketing)

# 2 Inspeção inicial ---------------------------

#Verificação tamanho dos dados: quantidade de linhas e colunas
dim(dados_marketing)

# Lista os nomes das variaveis disponiceis
names(dados_marketing)

# Mostra as primeiras linhas para uma inspeção rápida dos dados.
head(dados_marketing)

# Conta quantas semans aparecem em cada status de promoção
dados_marketing |> 
  count(status_promocao)


# 3. Primeira inspeção estatistica da receita -----------------------------

# Calcula estatisticas inicias da receita semanal de vendas.

inspecao_receita <-  dados_marketing |> 
  summarize(
    n = n(),
    minimo = min(receita_vendas),
    maximo = max(receita_vendas),
    amplitude = max(receita_vendas)-
      min(receita_vendas)
  )

# Mostra a tabela da inspeção o console
inspecao_receita


# 4. Distribuição empírica da receita -------------------------------------

# Visualiza como a receita se distribui nos dados oservados.
# tidyplot() inicia o gráfico e add_histogram() adiciona o histograma.
# O argumento bins controla a quantidade de classes do histograma
dados_marketing |> 
  tidyplot(x = receita_vendas) |> 
  add_histogram(bins = 20)


# 5. Valores típicos da receita -------------------------------------------

# Calcula média e mediana para descrever o centro da distribuição.
centro_receita <- dados_marketing |> 
  summarize(
    media = mean(receita_vendas),
    mediana = median(receita_vendas)
  )

centro_receita

x <- c(1,2, 1000)
mean(x)

median(x)
