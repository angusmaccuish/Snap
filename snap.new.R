########################################################################
#                                                                      #
#  Calculate the probability of a pair occurring in the first k cards  #
#  r - the number of ranks (e.g. 13)                                   #
#  s - the number of suits (e.g. 4)                                    #
#  k - the depth                                                       #
#                                                                      #
########################################################################
pair.probability <- function(r, s, k) {
  deck <- r*s
  at.least.p.pairs <- function(p) {
	probability <- function(tuple) {
      pair.permutations <- tuple[1]
      cards.used <- tuple[2]
      return (pair.permutations * prod((k-cards.used+1):(k-p)) / prod((deck-cards.used+1):deck))
	}
	contributions <- list(c(choose(r, p) * 2^p, 2*p)) ### This is the only 2n specific code now
	return (sapply(contributions, probability))
  }

  # Use the inclusion-exclusion principle to calculate the probability of a pair existing
  max.pairs <- floor((s-1)*k/s)
  p <- 1:max.pairs
  return (sum(sapply(p, function(p) { return ((-1)^(p+1) * at.least.p.pairs(p)) })))
}


# Test
print(pair.probability(13, 2, 26))