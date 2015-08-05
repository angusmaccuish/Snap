###################################################################################
# Calculate theoretical probability of a pair occurring within the first k cards. #
# Hardcoded for case of suits=2 case.                                             #
###################################################################################
pair.probability.with.2.suits <- function(k, ranks=13) {
  deck <- 2*ranks
  f <- function(s) {
    cards <- 2*s
    p <- (-1)^(s+1)
    p <- p * choose(ranks, s)
    p <- p * 2^s
    p <- p / prod((deck-cards+1):deck)
    p <- p * prod((k-cards+1):(k-s))
    return (p)
  }
  max <- floor(k/2)
  p <- if (max > 0) sum(sapply(1:max, f)) else 0
  return (p)
}


################################################################
# (Unconditional) Probability that the first pair occurs at k. #
# Hardcoded for case of suits=2 case.                          #
################################################################
first.pair.probability.with.2.suits <- function(k, ranks=13) {
  probability.of.no.pair.before.pair.at.k <- 0
  if (k==2) probability.of.no.pair.before.pair.at.k <- 1
  if (k>2) {
    probability.of.no.pair.before.pair.at.k <- 1 - pair.probability.with.2.suits(k-2, ranks-1)
  }
  probability.of.pair.at.k <- 1/(2*ranks-1)
  return (probability.of.no.pair.before.pair.at.k * probability.of.pair.at.k)
}


###################################################################################
# Calculate theoretical probability of a pair occurring within the first k cards. #
# Hardcoded for case of suits=3 case.                                             #
###################################################################################
pair.probability.with.3.suits <- function(k, ranks=13) {
  deck <- 3*ranks
  outer <- function(n) {
    t.min <- max(0, n-ranks) # the minimum number of triples
    t.max <- floor(n/2) # the maximum number of triples
    inner <- function(t) {
      s <- n-2*t
      cards <- 2*s+3*t
      q <- (-1)^(n+1)
      q <- q * choose(ranks, s)
      q <- q * choose(ranks-s, t)
      q <- q * 6^(s+t)
      q <- q / prod((deck-cards+1):deck)
      q <- q * prod((k-cards+1):(k-n))
      return (q)
    }
    return (sum(sapply(t.min:t.max, inner)))
  }
  n.max <- floor(2*k/3)
  p <- if (n.max > 0) sum(sapply(1:n.max, outer)) else 0
  return (p)
}


###################################################################################
# Calculate theoretical probability of a pair occurring within the first k cards. #
# Conditioned on a joker having been dealt before the k^th card.                  #
# Hardcoded for case of suits=3 case.                                             #
###################################################################################
pair.probability.with.3.suits.given.joker.already.dealt <- function(k, ranks=13) {
  deck <- 3*ranks
  outer <- function(n) {
    t.min <- max(0, n-ranks) # the minimum number of triples
    t.max <- floor(n/2) # the maximum number of triples
    inner <- function(t) {
      s <- n-2*t
      cards <- 2*s+3*t
      q <- (-1)^(n+1)
      q <- q * choose(ranks, s)
      q <- q * choose(ranks-s, t)
      q <- q * 6^(s+t)
      q <- q / prod((deck-cards+1):deck)
      q <- q * prod((k-cards):(k-n))
      q <- q * (k-n-1)/(k-n)/(k-1)
      return (q)
    }
    return (sum(sapply(t.min:t.max, inner)))
  }
  n.max <- floor(2*k/3)
  p <- if (n.max > 0) sum(sapply(1:n.max, outer)) else 0
  return (p)
}


################################################################
# (Unconditional) Probability that the first pair occurs at k. #
# Hardcoded for case of suits=3 case.                          #
################################################################
first.pair.probability.with.3.suits <- function(k, ranks=13) {
  deck <- 3*ranks
  probability.of.no.pair.before.pair.at.k <- 0
  if (k==2) probability.of.no.pair.before.pair.at.k <- 1
  if (k>2) {
    p1 <- pair.probability.with.3.suits.given.joker.already.dealt(k-2, ranks-1) * (k-3)/(deck-2)
    p2 <- 1/(deck-2)
    p3 <- pair.probability.with.3.suits(k-2, ranks-1) * (deck-k)/(deck-2)
    probability.of.no.pair.before.pair.at.k <- 1-(p1+p2+p3)
  }
  probability.of.pair.at.k <- 2/(deck-1)
  return (probability.of.no.pair.before.pair.at.k * probability.of.pair.at.k)
}


###################################################################################
# Calculate theoretical probability of a pair occurring within the first k cards. #
# Hardcoded for case of suits=4 case.                                             #
###################################################################################
pair.probability.with.4.suits <- function(k, ranks=13, with.joker=FALSE) {
  deck <- 4*ranks + with.joker
  outer <- function(n) {
    q.min <- 0 # TODO
    q.max <- floor(n/3) # the maximum number of quartets
    innerq <- function(q) {
      d.min <- 0 # TODO
      d.max <- floor((n-3*q)/2) # the maximum number of doublets
      innerd <- function(d) {
        t.min <- 0 # TODO
        t.max <- floor((n-3*q-2*d)/2) # the maximum number of triplets
        innert <- function(t) {
          s <- n-3*q-2*d-2*t
          cards <- 2*s+3*t+4*d+4*q
          p <- 0
          if (s >= 0 && s+t+d+q <= ranks && cards <= k) {
            p <- (-1)^(n+1)
            p <- p * choose(ranks, s)
            p <- p * choose(ranks-s, t)
            p <- p * choose(ranks-s-t, d)
            p <- p * choose(ranks-s-t-d, q)
            p <- p * 12^s
            p <- p * 24^t
            p <- p * 12^d
            p <- p * 24^q
            p <- p / prod((deck-cards+1):deck)
            p <- p * prod((k-cards+1):(k-n))
          }
          return (p)
        }
        return (sum(sapply(t.min:t.max, innert)))
      }
      return (sum(sapply(d.min:d.max, innerd)))
    }
    return (sum(sapply(q.min:q.max, innerq)))
  }
  n.max <- floor(3*k/4)
  p <- if (n.max > 0) sum(sapply(1:n.max, outer)) else 0
  return (p)
}


###################################################################################
# Calculate theoretical probability of a pair occurring within the first k cards. #
# In addition to the standard deck, 2 jokers are included (which can pair up)     #
# Hardcoded for case of suits=4 case.                                             #
###################################################################################
pair.probability.with.4.suits.and.pair.of.jokers <- function(k, ranks=13) {
  deck <- 4*ranks + 2
  outer <- function(n) {
    q.min <- 0
    q.max <- floor(n/3) # the maximum number of quartets
    innerq <- function(q) {
      d.min <- 0
      d.max <- floor((n-3*q)/2) # the maximum number of doublets
      innerd <- function(d) {
        t.min <- 0
        t.max <- floor((n-3*q-2*d)/2) # the maximum number of triplets
        innert <- function(t) {
          remaining <- n-3*q-2*d-2*t
          jokers.max <- (remaining > 0)
          innerj <- function(j) {
            s <- n-3*q-2*d-2*t-j
            cards <- 2*s+2*j+3*t+4*d+4*q
            p <- 0
            if (s >= 0 && s+t+d+q <= ranks && cards <= k) {
              p <- (-1)^(n+1)
              p <- p * choose(ranks, s)
              p <- p * choose(ranks-s, t)
              p <- p * choose(ranks-s-t, d)
              p <- p * choose(ranks-s-t-d, q)
              p <- p * 2^j
              p <- p * 12^s
              p <- p * 24^t
              p <- p * 12^d
              p <- p * 24^q
              p <- p / prod((deck-cards+1):deck)
              p <- p * prod((k-cards+1):(k-n))
              # The following two lines were suppose to implement the condition that
              # a Joker cannot reside at k. However, by removing these adjustments,
              # we seem to get the right answer (or something which is very close!)
              # for a range of different ranks. Comment out for now and try to understand later...
              # (without these, we are calculating the pair probability with 2xJokers with no restrictions)
              #if (j==0) p <- p * (1-2*(k-cards)/(k-n)/(deck-cards))
              #if (j==1) p <- p * (1-1/(k-n))
            }
            return (p)
          }
          return (sum(sapply(0:jokers.max, innerj)))
        }
        return (sum(sapply(t.min:t.max, innert)))
      }
      return (sum(sapply(d.min:d.max, innerd)))
    }
    return (sum(sapply(q.min:q.max, innerq)))
  }
  n.max <- floor(3*k/4)
  p <- if (n.max > 0) sum(sapply(1:n.max, outer)) else 0
  return (p)
}


################################################################
# (Unconditional) Probability that the first pair occurs at k. #
# Hardcoded for case of suits=4 case.                          #
################################################################
first.pair.probability.with.4.suits <- function(k, ranks=13) {
  deck <- 4*ranks
  p <- 0
  if (k==2) p <- 3/(deck-1)
  if (k==3) p <- (1-2/(deck-2))*3/(deck-1)
  if (k>3) {
	# This function represents our lack of a general algorithm to calculate
	# the probability of a pair with an arbitrary number of jokers.
	pair.probability.with.4.suits.and.jokers <- function(k, ranks, jokers) {
	  if (jokers > 2) throw("Only a maximum of 2 jokers supported")
	  p <- 0
	  if (jokers == 0) p <- pair.probability.with.4.suits(k, ranks)
	  if (jokers == 1) p <- pair.probability.with.4.suits(k, ranks, with.joker=TRUE)
	  if (jokers == 2) p <- pair.probability.with.4.suits.and.pair.of.jokers(k, ranks)
	  return (p)
	}
	first.pair.probability <- function(k, ranks, jokers=2) {
	  q <- (jokers+1)/(4*ranks-3+jokers)
	  d <- if (jokers > 0) first.pair.probability(k-1, ranks, jokers-1) else 0
	  p <- (1-pair.probability.with.4.suits.and.jokers(k-2, ranks-1, jokers)-d) * q
	  return (p)
	}
	p <- first.pair.probability(k, ranks)
  }
  return (p)
}

