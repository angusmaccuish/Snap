# Monte Carlo simulation to determine the probability of a pair occurring
# within the first k cards.
mc.pair.probability <- function(k, ranks=13, suits=4, rogues=0, iterations=10000) {
  cards <- rep(1:ranks, suits)
  if (rogues > 0) cards <- c(cards, -(1:rogues))
  count <- 0
  for (n in 1:iterations) {
    cards <- sample(cards)
    pair <- any(head(cards == c(tail(cards, -1), NA), k-1), na.rm=TRUE)
    if (pair) count <- count + 1
  }
  return (count/iterations)
}


# Suitable for use in an apply type function.
# Example usage:
#   k <- 1:26
#   results <- parallel::mclapply(k, mc.pair.simulation(iterations=100000, ranks=13, suits=2, debug=TRUE))
#   plot(k, unlist(results))
mc.pair.simulation <- function(iterations, ranks=13, suits=4, rogues=0, debug=FALSE) {
  return (function(k) { 
    if (debug) print(k)
    p <- mc.pair.probability(k, ranks, suits, rogues, iterations)
    return (p)
  })
}


# Monte Carlo simulation to determine the probability of a pair occurring at kth card first
mc.first.pair.probability <- function(k, ranks=13, suits=4, iterations=10000) {
  cards <- rep(1:ranks, suits)
  count <- 0
  for (n in 1:iterations) {
    cards <- sample(cards)
    card <- match(TRUE, head(cards == c(tail(cards, -1), NA), k-1))
    if (!is.na(card) && card == (k-1)) {
      count <- count + 1
    }
  }
  return (count/iterations)
}


# Suitable for use in an apply type function.
# Example usage:
#   k <- 1:26
#   results <- parallel::mclapply(k, mc.first.pair.simulation(iterations=100000, ranks=13, suits=2, debug=TRUE))
#   plot(k, unlist(results))
mc.first.pair.simulation <- function(iterations, ranks=13, suits=4, debug=FALSE) {
  return (function(k) { 
    if (debug) print(k)
    return (mc.first.pair.probability(k, ranks, suits, iterations)) }
  )
}
