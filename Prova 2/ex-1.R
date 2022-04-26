library(tidyverse)

wine <- read.csv("/Users/joaovicmendes/Desktop/wine.csv")

# Exercício 1
# Utilizando uma suavização sobre o gráfico de acidez fixa x pH, é possível
# notar uma relação de linearidade entre o pH e a acidez do vinho.
ggplot(data = wine, aes(x = pH, y = fixed.acidity)) + geom_point() + geom_smooth()

# Ao utilizar o coeficiente de correlação, obtemos um valor r = -0.6829.
# Como |r| > 0.2, é possível considerar que existe uma alta correlação entre os atributos.
cor(wine$fixed.acidity, wine$pH)
