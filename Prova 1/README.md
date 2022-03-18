# Introdução à Ciência de Dados com R - Prova 1
Aluno: João Victor Mendes Freire

RA: 758943

## Exercício 1
```R
# Exercício 1

# Função que calcula a expressão do exercício
calcula_expressao <- function(values) {
  a <- values[1]
  b <- values[2]
  c <- values[3]
  d <- values[4]
  return(a * b + c * d)
}

# Função que calcula a permutação de uma lista de valores e aplica uma função 'f'
# nesses valores. Retorna uma lista com todos resultados de aplicar 'f'
permutacao <- function(values, idx, f) {
  if (idx == length(values)) {
    return( c(f(values)) )
  }
  
  n <- length(values)
  res <- c()
  for (i in idx:n) {
    # troca elementos
    tmp <- values[i]
    values[i] <- values[idx]
    values[idx] <- tmp
    
    # Permutação com essa base
    res <- c(res, permutacao(values, idx+1, f))
    
    # desfaz troca
    tmp <- values[i]
    values[i] <- values[idx]
    values[idx] <- tmp
  }
  return(res)
}

# Função que resolve o exercício
calcula_maximo <- function(a, b, c, d) {
  values <- c(a, b, c, d)
  
  # Validação da entrada
  if (!is.numeric(values)) {
    print("Erro: Valores não numéricos informados")
    return(NULL)
  }
  
  # Gerando permutações e salvando máximo
  max_value <- max(permutacao(values, 1, calcula_expressao))
  
  return(max_value)
}
calcula_maximo(1, 2, 3, 4)

```

## Exercício 2
```R
# Exercício 2

library(tidyverse)

vgsales <- read.csv("/Users/joaovicmendes/Desktop/vgsales.csv")

# a) Recupere todos os jogos produzidos pela Nintendo que foram vendidos na década de 90
nintendo_games <- filter(vgsales, Publisher == 'Nintendo' & Year >= '1990' & Year < '2000')

# b) Recupere todos os jogos do gênero de Sports, ordenando o resultado em relação ao
# total de vendas em nível global
sports_games <- filter(vgsales, Genre == 'Sports') %>%
  arrange(desc(Global_Sales))

# c) Recupere todos os jogos da franquia Mario (existem várias variações desse jogo)
# lançados a partir de 1998; ordene os jogos de maneira que os mais recentes sejam apresentados primeiro
mario_games <- filter(vgsales, Year >= '1998' & str_detect(Name, 'Mario')) %>%
  arrange(desc(Year))

# d) Recupere a quantidade média de vendas na Europa para os jogos da plataforma Wii
# lançados entre 2008 e 2012, inclusive, por gênero e ano de lançamento do jogo
eu_sales <- filter(vgsales, Platform == 'Wii' & Year >= '2008' & Year <= '2012') %>%
  group_by(Year, Genre) %>%
  summarise("Average EU Sales" = mean(as.numeric(EU_Sales)))

# e) Mostre em um gráfico de sua escolha (linha ou coluna), a quantidade de jogos
# lançados em cada plataforma por ano
sales_platform <- count(vgsales, Year, Platform)

sales_platform_graph <- ggplot(data = sales_platform) +
  geom_line(mapping = aes(x = as.factor(Year), y = `n`, group = Platform, colour = Platform)) +
  scale_y_continuous(n.breaks = 12) +
  labs(x = "Ano", y = "Número de Lançamentos") +
  theme(axis.title = element_text(size=10), plot.title = element_text(size=12, face="bold"))

```
