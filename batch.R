library("parallel")
source("snap.sim.R")

args <- sapply(commandArgs(trailingOnly = TRUE), as.integer)
ranks <- args[1]
suits <- args[2]
rogues <- args[3]
iterations <- args[4]
cores <- detectCores()

k <- 2:(ranks*suits)
simulation <- mc.pair.simulation(iterations, ranks, suits, rogues, debug=TRUE)
results <- unlist(mclapply(k, simulation, mc.cores=cores))

file <- sprintf("%s.%s.%s.%s.txt", ranks, suits, rogues, iterations)
write(results, file, ncolumns=1)
