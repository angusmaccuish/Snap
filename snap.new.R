########################################################################
#                                                                      #
#  Calculate the probability of a pair occurring in the first k cards  #
#  k - the depth                                                       #
#                                                                      #
########################################################################
pair.probability <- function(ranks, suits, k, jokers=0, fn=pairs) {
  deck <- ranks*suits + jokers
  at.least.p.pairs <- function(p) {
    probability <- function(tuple) {
      pair.permutations <- tuple[1]
      cards.used <- tuple[2]
      pair.permutations * prod((k-cards.used+1):(k-p)) / prod((deck-cards.used+1):deck)
    }
    sum(sapply(fn(ranks, suits, jokers, p), probability))
  }

  # Use the inclusion-exclusion principle to calculate the probability of a pair existing
  max.pairs <- floor((suits-1)*k/suits)
  if (max.pairs > 0) sum(sapply(1:max.pairs, function(p) { return ((-1)^(p+1) * at.least.p.pairs(p)) })) else 0
}

pairs <- function(ranks, suits, jokers, required.pairs) {
  two.suits <- function(ranks, jokers, pairs) {
	j.min <- (ranks < pairs) # not enough ranks left to make p pairs
	j.max <- (pairs > 0 && jokers >= 2) # if more than 2 jokers available and pairs required
	lapply(j.min:j.max, function(j) { 
      c(choose(ranks, pairs-j) * choose(suits, 2)^(pairs-j) * factorial(2)^(pairs-j) * factorial(2)^j, 2*pairs)
    })
  }
	
  three.suits <- function(ranks, jokers, pairs) {
    t.min <- max(0, pairs-ranks)
    t.max <- ranks
    t <- Filter(function(t) { pairs >= 2*t }, t.min:t.max)
    f <- function(t) {
      lapply(two.suits(ranks-t, jokers, pairs-2*t), function(x) { 
        c(x[1] * choose(ranks, t) * choose(suits, 3)^t * factorial(3)^t, x[2] + 3*t)
      })
    }
    unlist(lapply(t, f), recursive=FALSE)
  }
	
  four.suits <- function(ranks, jokers, pairs) {
    q.min <- max(0, pairs-2*ranks)
    q.max <- ranks
    q <- Filter(function(q) { pairs >= 3*q }, q.min:q.max)
    f <- function(q) {
      d.min <- 0
      d.max <- ranks-q
      d <- Filter(function(d) { pairs >= 3*q + 2*d }, d.min:d.max)
      f <- function(d) {
        lapply(three.suits(ranks-q-d, jokers, pairs-3*q-2*d), function(x) {
	      perms <- x[1] * choose(ranks, q) * choose(suits, 4)^q * factorial(4)^q * choose(ranks-q, d) * 3^d * factorial(2)^(2*d)
	      cards <- x[2] + 4*q + 4*d
          c(perms, cards)
        })	
      }
      unlist(lapply(d, f), recursive=FALSE)
    }
    unlist(lapply(q, f), recursive=FALSE)
  }
	
  fn <- switch(suits, NULL, two.suits, three.suits, four.suits)
  fn(ranks, jokers, required.pairs)
}


