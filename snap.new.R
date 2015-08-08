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
      pair.permutations * prod((k-cards.used+1):(k-p)) / prod((deck-cards.used+1):deck)
	}
	
	two.suits <- function(r, s, p) { c(choose(r, p) * choose(s, 2)^p * factorial(2)^p, 2*p) }
	
    contributions <- switch(s,
	                        list(c(0, 0)),
	                        list(two.suits(r, s, p)),
	                        { 
		                      t <- 0:floor(k/3)
		                      f <- function(t) {
			                    s <- p-2*t
			                    cards <- 2*s + 3*t
			                    if (s >=0 && (s+t) <= r && cards <= k) {
				                  perms <- choose(r, s) * choose(r-s, t) * 6^(s+t)
				                  c(perms, cards)
			                    }
			                    else 
				                  c(0, 0)
		                      }
		                      lapply(t, f)
							})
	sapply(contributions, probability)
  }

  # Use the inclusion-exclusion principle to calculate the probability of a pair existing
  max.pairs <- floor((s-1)*k/s)
  p <- 1:max.pairs
  sum(sapply(p, function(p) { return ((-1)^(p+1) * at.least.p.pairs(p)) }))
}
