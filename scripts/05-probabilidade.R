# Arquivo: 05-probabilidade.R
# Autor: Sara Pamela Ferreira
# Data: 11/05/2026
# Objetivos:
# 1. Praticar cálculos básicos de probabilidades
# 2. Praticar conceitos básicos de simulação de Monte Carlo

# Configuracoes globais -----------------------------------------------

# exibe números sem notação científica
options(digits = 5, scipen = 999)


# Pacotes usados ------------------------------------------------------

library(tidyverse)
library(probs) # instale esse pacote

# espaço amostral do lançamento de uma moeda 2 vezes
tosscoin(times = 3)

# Solução do exemplo 1 



# A função tosscoim simula o lançamento de moedas


# simula o lançamento de 3 moedas



# fixa a semente para reprodutibilidade
set.seed(123)

# cria um vetor de 1 até 6 (faces)
dado <- 1:6                                 

# define o n. de lançamentos do dado
n <- 10 

# simula os 10 lançamentos
lançamentos <- sample(dado, size = 10, replace = TRUE)
lançamentos

# calcula o valor médio dos 10 lançamentos
mean(lançamentos)  # média observada

# Repita o experimento aumentando o tamanho da amostra.
# Execute o código abaixo trocando n 
# pelos valores 100, 1.000, 10.000 e 100.000 
# e anote o valor médio obtido.

# fixa a semente para reprodutibilidade
set.seed(123)

# define o n. de lançamentos do dado
n <- 10 

# simula os n lançamentos
lançamentos <- sample(dado, size = 10, replace = TRUE)

# calcula o valor médio dos 10 lançamentos
mean(lançamentos)  # média observada