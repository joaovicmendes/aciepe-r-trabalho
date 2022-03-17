# Exercício 5
library(tidyverse)
movies <- read.csv("/Users/joaovicmendes/git/aciepe-r-trabalho/Lista 1/imdb_top_1000.csv")

# a) filmes do gênero Drama
drama_movies <- filter(movies, str_detect(Genre, 'Drama'))

# b) filmes estrelados por Morgan Freeman ordenados por ano de lançamento
freeman_movies <- filter(movies, str_detect(paste0(Star1, Star2, Star3, Star4), 'Morgan Freeman')) %>%
  arrange(Released_Year)

# c) média dos filmes dirigidos por Christpher Nolan
nolan_movies_average <- filter(movies, Director == 'Christopher Nolan') %>%
  summarise("Média" = mean(IMDB_Rating))

# d) 5 melhores filmes (IMDB_Rating) estrelador por Brad Pitt
pitt_top_5_movies <- filter(movies, str_detect(paste0(Star1, Star2, Star3, Star4), 'Brad Pitt')) %>%
  arrange(desc(IMDB_Rating)) %>%
  head(5)

# e) ano de lançamento médio e número médio de votos dos filmes de Ação
action_movies <- filter(movies, str_detect(Genre, 'Action')) %>%
  summarise("Ano de Lançamento Médio" = mean(as.numeric(Released_Year)), "Média de votos" = mean(No_of_Votes))

# f) gráfico de linha com a quantidade de filmes de Aventura que foram lançados por ano
movies$Released_Year <- as.numeric(movies$Released_Year)
adventure_movies_count <- filter(movies, str_detect(Genre, 'Adventure')) %>%
  arrange(Released_Year) %>%
  count(Released_Year) %>%
  filter(!is.na(Released_Year))

ggplot(data = adventure_movies_count) +
  geom_line(mapping = aes(x = `Released_Year`, y = `n`)) +
  scale_x_continuous(n.breaks = 10, na.value = 0) +
  scale_y_continuous(n.breaks = 10) +
  labs(x = "Ano", y = "Quantidade") +
  theme(axis.title = element_text(size=10), plot.title = element_text(size=12, face="bold"))

# g) gráfico de barras com o número de votos que cada filme da franquia do Star Wars recebeu
starwars_movies <- filter(movies, str_detect(Series_Title, 'Star Wars')) %>%
  arrange(Released_Year)
  select(Series_Title, No_of_Votes)

ggplot(data = starwars_movies) +
  geom_bar(stat = "identity", position = position_dodge(), mapping = aes(x = `Series_Title`, y = `No_of_Votes`)) +
  labs(x = "Filme", y = "Votos") +
  theme(axis.title = element_text(size=10), plot.title = element_text(size=12, face="bold"))
