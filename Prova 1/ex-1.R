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
