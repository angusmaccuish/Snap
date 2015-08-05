###########################################################################
# Monte Carlo simulation to determine the probability of a pair occurring #
# within the first k cards.                                               #
###########################################################################
mc.pair.probability <- function(k, ranks=13, suits=4, iterations=10000) {
  cards <- rep(1:ranks, suits)
  count <- 0
  n <- 0
  pair.exists <- function(cards, k) { return (any(head(cards == c(tail(cards, -1), NA), k-1), na.rm=TRUE)) }
  while (n < iterations) {
    cards <- sample(cards)
    pair <- any(head(cards == c(tail(cards, -1), NA), k-1), na.rm=TRUE)
    if (pair.exists(cards, k)) count <- count + 1
    if (pair.exists(rev(cards), k)) count <- count + 1
    n <- n + 1
  }
  return (count/iterations/2)
}


#################################################################################
# Monte Carlo simulation as above but split over available cores.               #
# Useful for calculating accurate probability for one particular set of inputs. #
#################################################################################
mc.pair.probability.parallel <- function(k, ranks=13, suits=4, iterations=10000) {
  cores <- parallel::detectCores()
  if (cores > 1) {
    # split iterations between cores, with excess in final batch if required
    batch.size <- floor(iterations/cores)
    batches <- c(rep(batch.size, cores-1), iterations-(cores-1)*batch.size)
  }
  else {
    # No parallelism available
    batches <- c(iterations)
  }
  fun <- function(iterations) { return(mc.pair.probability(k, ranks, suits, iterations)) }
  results <- unlist(parallel::mclapply(batches, fun, mc.cores=cores))
  return (mean(results))
}


############################################################################################################
# Suitable for use in an apply type function.                                                              #
# Example usage:                                                                                           #
#   k <- 1:26                                                                                              #
#   results <- parallel::mclapply(k, mc.pair.simulation(iterations=100000, ranks=13, suits=2, debug=TRUE)) #
#   plot(k, unlist(results))                                                                               #
############################################################################################################
mc.pair.simulation <- function(iterations, ranks=13, suits=4, debug=FALSE) {
  return (function(k) { 
    if (debug) print(k)
    p <- mc.pair.probability(k, ranks, suits, iterations)
    return (p)
  })
}


#############################################################################################
# Monte Carlo simulation to determine the probability of a pair occurring at kth card first #
#############################################################################################
mc.first.pair.probability <- function(k, ranks=13, suits=4, iterations=10000) {
  cards <- rep(1:ranks, suits)
  count <- 0
  n <- 0
  while (n < iterations) {
    cards <- sample(cards)
    card <- match(TRUE, head(cards == c(tail(cards, -1), NA), k-1))
    if (!is.na(card) && card == (k-1)) {
      count <- count + 1
    }
    n <- n + 1
  }
  return (count/iterations)
}


#######################################################################
# Monte Carlo simulation to determine the mean location of first pair.#
# Use fact that reverse combination of the cards is equally likely to #
# occur, so use antithetic match from right direction as well as left.#
#######################################################################
mc.first.pair.mean.location <- function(ranks=13, suits=4, iterations=10000) {
  deck = ranks*suits
  cards <- rep(1:ranks, suits)
  sum <- 0
  n <- 0
  while (n < iterations) {
    cards <- sample(cards)
    card <- match(TRUE, cards == c(tail(cards, -1), NA))
    if (!is.na(card)) {
      first.from.left <- card+1
      first.from.right <- tail(deck - which(cards == c(NA, head(cards, -1))) + 2, n=1)
      sum <- sum + first.from.left + first.from.right
    }
    n <- n + 1
  }
  return (sum/iterations/2)
}


##################################################################################################################
# Suitable for use in an apply type function.                                                                    #
# Example usage:                                                                                                 #
#   k <- 1:26                                                                                                    #
#   results <- parallel::mclapply(k, mc.first.pair.simulation(iterations=100000, ranks=13, suits=2, debug=TRUE)) #
#   plot(k, unlist(results))                                                                                     #
##################################################################################################################
mc.first.pair.simulation <- function(iterations, ranks=13, suits=4, debug=FALSE) {
  return (function(k) { 
    if (debug) print(k)
    return (mc.first.pair.probability(k, ranks, suits, iterations)) }
  )
}
