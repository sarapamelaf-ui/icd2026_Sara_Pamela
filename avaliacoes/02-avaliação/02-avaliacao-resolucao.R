# Arquivo: 02-avaliacao-resolucao.R
# Integrante 1: EVELYN C. M. LEAL
# Integrante 2: ISADORA LEAL FARIA
# Integrante 3: KEROLLAYNE BORGES
# Integrante 4: SARA PAMELA FERREIRA
# Data: 09/06/2026
# Objetivo: Resolução da Avaliação 2 — Introdução à Ciência de Dados


# Configurações globais ---------------------------------------
options(digits = 5, scipen = 999)

# Exercício 1 -------------------------------------------------

# a) 
# Um possivel espaço amostral é: S = {renova, não renova}
# Também seria aceitavel escrever:
# S = {renova contrato, não renova contrato}
# ou outra redação equivalente.


# b) 
#

# c) 
#

# d) 
#

# e) 
#

# f) 



# Exercício 2 -------------------------------------------------

# a) 
# X pode assumir dois valores:
# X = 12, quando o cupom é utilizado;
# X = 0, quando o cupom não é utilizado


# b) 
# P(X = 12) = 0,1

# c)
# P(X = 0) = 1 - 0,1 = 0,9


# d)
# valor esperado de X:
# E(X) = X1 * P(X = 12) + X2 * P(X = 0)
# E(X) = 12 * 0,1 + 0 * 0,9
# E(X) = R$ 1,20


# e)
# O custo esperado do uso do cupom é de R$ 1,20 por produto vendido.
# Isso não significa que cada produto terá custo de R$ 1,20
# Individualmente, o custo será R$ 0 ou R$ 12
# O valor esperado resume o custo médio por produto quando pensamos
# em muitos produtos vendidos sob as mesmas condições.



# Exercício 3 -------------------------------------------------

# Digite e execute o código necessário e escreva a 
# interpretação em comentários.


set.seed(123)

# item a)

lambda <- 10
capacidade <- 13
n_simulacoes <- 1000


# item b) Simula os valores de usando a função adequada do R
clientes <- rpois(n = n_simulacoes, lambda = lambda)
head(clientes)


# item c) calcula a média simulada de clientes por hora de pico
media_simulada <- mean(clientes)
media_simulada


# item d)  Calculo da proporção de valores 
# simulados em que a capacidade de 13 clientes foi excedida.
prop_acima_capacidade <- mean(clientes > capacidade)
prop_acima_capacidade


# item e) Valor do percentil 95 da contagem de clientes por hora de pico.
percentil_95 <- quantile(clientes, 0.95)
percentil_95




