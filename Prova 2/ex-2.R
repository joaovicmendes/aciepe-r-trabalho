# Exercício 2
wine_filtered <- arrange(wine, desc(alcohol), desc(pH)) %>%
  select(quality, alcohol, pH)

ggplot(data = wine_filtered, aes(x = alcohol, y = pH, colour = quality)) + 
  geom_point() + xlim(8, 15) +
  labs(x = "Teor Alcoólico (%)", y = "pH")
