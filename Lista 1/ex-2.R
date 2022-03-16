## Exercício 2
minimun_coins_recursive <- function(amount, coins) {
  # Caso base, se acabaram as moedas, retorna vetor vazio
  if (length(coins) == 0) return( c() )
  
  # Calcula a quantia máxima da moeda atual
  curr_coin = coins[1]
  n <- amount %/% curr_coin 
  amount_left <- amount %% curr_coin
  curr_quantity <- c(n)
  
  # Recursão
  if (length(coins) != 1)
    remaining_quantities <- minimun_coins_recursive(amount_left, coins[ 2:length(coins) ])
  else
    remaining_quantities <- minimun_coins_recursive(amount_left, c())
  
  # Combina a lista resultante da recursão com o valor atual
  final_list <- append(curr_quantity, remaining_quantities)
  return(final_list)
}

minimun_coins <- function(amount) {
  if (amount != round(amount)) {
    print("Erro, parametro não é valor inteiro")
    return()
  }
  
  quantities <- minimun_coins_recursive(amount, c(100, 50, 10, 5, 2, 1))
  named_quantities <- list("100" = quantities[1][1],
                            "50" = quantities[2][1],
                            "10" = quantities[3][1],
                             "5" = quantities[4][1],
                             "2" = quantities[5][1],
                             "1" = quantities[6][1])
  return(named_quantities)
}

# Exemplo:
minimun_coins(103)
minimun_coins(4)
minimun_coins(27)
