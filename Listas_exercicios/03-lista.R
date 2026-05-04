# Arquivo: 03-lista.R
# Autor(a): Sara Pamela Ferreira
# Data: 14/04/2026
# Objetivo: Resolução da Lista de Exercícios 3

# Configurações globais --------------------------------------

# Configura o número de dígitos exibidos
options(digits = 5, scipen = 999)

# Carrega os pacotes necessários
library(here)
library(tidyverse)


# Exercício 1 ------------------------------------------------

## a 
caminho.csv <-  here("dados/brutos/receitas_trimestres.csv")

receitas_trimestres.csv <- read_csv(caminho.csv)

## b

glimpse(receitas_trimestres.csv)


## c

# Porque provavelmente ela foi criada para apresentação
# e não para análise


## d

# cria uma tibble no formato amplo
receitas <- tribble(
  ~produto, ~T1, ~T2, ~T3, ~T4,
  "Produto A", 50000, 55000, 60000, 65000,
  "Produto B", 30000, 32000, 35000, 37000,
  "Produto C", 20000, 22000, 25000, 27000
  
)
# exibe o objeto 
receitas

 # tranforma receitas para o for,ato longo
 receitas_longo <- receitas |>
   pivot_longer(
     cols = c("T1", "T2", "T3", "T4"),
     names_to = "trimestre",
     values_to = "receita"
   )
 
 # exibe o objeto
 receitas_longo
 
 glimpse(receitas_longo)
 
## e

 view(receitas_longo)


# Exercício 2 ------------------------------------------------


## a
caminho.csv <- here("dados/brutos/desempenho-empresa.csv")

dados_desempenho <-  read.csv(caminho.csv)  


## b

glimpse(dados_desempenho)

## c

## está misturado as colunas de receita e despesa

## d



## e



# Exercício 3 ------------------------------------------------


## a


## b


## c


## d


## e


## f


## g


## h


## i