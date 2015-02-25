# Calculate theoretical probability of a pair occurring within the first k cards.
# Hardcoded for case of suits=2 case.
pair.probability.with.2.suits <- function(k, ranks=13, with.rogue=FALSE) {
  f <- function(s) {
    p <- (-1)^(s+1)
    p <- p * choose(ranks, s)
    p <- p * 2^s
    p <- p * factorial(2*ranks-2*s) / factorial(2*ranks)
    p <- p * factorial(k-s) / factorial(k-2*s)
    if (with.rogue) p <- p * (1-2*s/k)
    return (p)
  }
  max <- floor(k/2)
  p <- if (max > 0) sum(sapply(1:max, f)) else 0
  if (with.rogue) {
    weight <- k/(2*ranks+1)
    p <- p * weight + pair.probability.with.2.suits(k, ranks, with.rogue=FALSE) * (1-weight)
  }
  return (p)
}


# Calculate theoretical probability of a pair occurring within the first k cards.
# Hardcoded for case of suits=3 case.
pair.probability.with.3.suits <- function(k, ranks=13, with.rogue=FALSE) {
  dk <- if (with.rogue) 1 else 0
  outer <- function(p) {
    t.min <- max(0, p-ranks)
    t.max <- floor((k-dk)/3)
    inner <- function(t) {
      q <- (-1)^(p+1)
      q <- q * choose(ranks, p-2*t)
      q <- q * choose(ranks-p+2*t, t)
      q <- q * 6^(p-t)
      q <- q / prod((3*ranks+t-2*p+1):(3*ranks))
      q <- q * prod((k-dk+t-2*p+1):(k-p))
      if (with.rogue) q <- q/k
      return (q)
    }
    return (sum(sapply(t.min:t.max, inner)))
  }
  p.max <- floor(2*(k-dk)/3)
  p <- if (p.max > 0) sum(sapply(1:p.max, outer)) else 0
  if (with.rogue) {
    weight <- k/(3*ranks+1)
    p <- p * weight + pair.probability.with.3.suits(k, ranks, with.rogue=FALSE) * (1-weight)
  }
  return (p)
}


# (Unconditional) Probability that the first pair occurs at k.
# Hardcoded for case of suits=2 case.
# BROKEN!!!
first.pair.probability.with.2.suits <- function(k, ranks=13) {
  return ((1 - pair.probability.with.2.suits(k-2, ranks-1))/(2*ranks-1))
}
