########################################################################
#                                                                      #
#  Calculate the probability of a pair occurring in the first k cards  #
#  r - the number of ranks (e.g. 13)                                   #
#  s - the number of suits (e.g. 4)                                    #
#  k - the depth                                                       #
#                                                                      #
########################################################################
pair.probability <- function(ranks, suits, k, fn=pairs) {
  deck <- ranks*suits
  at.least.p.pairs <- function(p) {
    probability <- function(tuple) {
      pair.permutations <- tuple[1]
      cards.used <- tuple[2]
      pair.permutations * prod((k-cards.used+1):(k-p)) / prod((deck-cards.used+1):deck)
    }
    sum(sapply(fn(ranks, suits, p), probability))
  }

  # Use the inclusion-exclusion principle to calculate the probability of a pair existing
  max.pairs <- floor((suits-1)*k/suits)
  if (max.pairs > 0) sum(sapply(1:max.pairs, function(p) { return ((-1)^(p+1) * at.least.p.pairs(p)) })) else 0
}

pairs <- function(ranks, suits, p) {
  one.suit <- function(r, p) { list(c(1,0)) }
	
  two.suits <- function(r, p) { list(c(choose(r, p) * choose(suits, 2)^p * factorial(2)^p, 2*p)) }
	
  three.suits <- function(r, p) {
    t.min = max(0, p-r)
    t.max = r
    t <- Filter(function(t) { p >= 2*t }, t.min:t.max)
    f <- function(t) {
      lapply(two.suits(r-t, p-2*t), function(x) { 
        c(x[1] * choose(r, t) * choose(suits, 3)^t * factorial(3)^t, x[2] + 3*t)
      })
    }
    unlist(lapply(t, f), recursive=FALSE)
  }
	
  four.suits <- function(r, p) {
    q.min = max(0, p-2*r)
    q.max = r
    q <- Filter(function(q) { p >= 3*q }, q.min:q.max)
    f <- function(q) {
      d.min = 0
      d.max = r-q
      d <- Filter(function(d) { p >= 3*q + 2*d }, d.min:d.max)
      f <- function(d) {
        lapply(three.suits(r-q-d, p-3*q-2*d), function(x) {
          c(x[1] * choose(r, q) * choose(suits, 4)^q * factorial(4)^q * choose(r-q, d) * 3^d * factorial(2)^(2*d), x[2] + 4*q + 4*d)
        })	
      }
      unlist(lapply(d, f), recursive=FALSE)
    }
    unlist(lapply(q, f), recursive=FALSE)
  }
	
  fn <- switch(suits, one.suit, two.suits, three.suits, four.suits)
  fn(ranks, p)
}


