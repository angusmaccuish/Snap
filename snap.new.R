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
  two.in.a.row <- function(ranks, jokers, pairs) {
	f <- function(j) { 
      list(c(choose(ranks, pairs-j) * choose(suits, 2)^(pairs-j) * factorial(2)^(pairs-j) * choose(jokers, 2)^j * factorial(2)^j, 2*pairs))
    }
	min.joker.pairs <- (ranks < pairs)
	max.joker.pairs <- (pairs > 0 && jokers >= 2)
    unlist(lapply(min.joker.pairs:max.joker.pairs, f), recursive=FALSE)
  }
	
  three.in.a.row <- function(ranks, jokers, pairs) {
	f <- function(j) {
      t.min <- max(0, pairs-2*j-ranks)
      t.max <- ranks
      t <- Filter(function(t) { pairs >= (2*t + 2*j) }, t.min:t.max)
      f <- function(t) {
        lapply(two.in.a.row(ranks-t, jokers-3*j, pairs-2*t-2*j), function(x) { 
	      perms <- x[1] * choose(ranks, t) * choose(suits, 3)^t * factorial(3)^t * choose(jokers, 3)^j * factorial(3)^j
          cards <- x[2] + 3*t + 3*j
          c(perms, cards)
        })
      }
      unlist(lapply(t, f), recursive=FALSE)
    }
    min.joker.triples <- (pairs >= 2 && jokers >= 3 && 2*ranks < pairs)
    max.joker.triples <- (pairs >= 2 && jokers >= 3) # limitation, max joker triplet=1 just now!!
    unlist(lapply(min.joker.triples:max.joker.triples, f), recursive=FALSE)
  }
	
  four.in.a.row <- function(ranks, jokers, pairs) {
    q.min <- max(0, pairs-2*ranks)
    q.max <- ranks
    q <- Filter(function(q) { pairs >= 3*q }, q.min:q.max)
    f <- function(q) {
      d.min <- 0
      d.max <- ranks-q
      d <- Filter(function(d) { pairs >= (3*q + 2*d) }, d.min:d.max)
      f <- function(d) {
        lapply(three.in.a.row(ranks-q-d, jokers, pairs-3*q-2*d), function(x) {
	      perms <- x[1] * choose(ranks, q) * choose(suits, 4)^q * factorial(4)^q * choose(ranks-q, d) * 3^d * factorial(2)^(2*d)
	      cards <- x[2] + 4*q + 4*d
          c(perms, cards)
        })	
      }
      unlist(lapply(d, f), recursive=FALSE)
    }
    unlist(lapply(q, f), recursive=FALSE)
  }
	
  fn <- switch(suits, NULL, two.in.a.row, three.in.a.row, four.in.a.row)
  fn(ranks, jokers, required.pairs)
}


