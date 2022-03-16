## Exercício 1
calculate_distances <- function(x1, y1, x2, y2) {
    sqrt((x2-x1)^2 + (y2-y1)^2)
}

# exemplo:
x1 = c(0, 1, 2, 3)
y1 = c(0, 1, 2, 3)

x2 = c(0, 0, 0, 0)
y2 = c(0, 0, 0, 0)

calculate_distances(x1, y1, x2, y2)
