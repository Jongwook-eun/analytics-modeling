library(FrF2)

set.seed(123)
design <- FrF2(nruns = 16, nfactors = 10, randomize = FALSE)

names(design) <- c("Yard", "Solar", "Gar", "Pool", "Sch",
                   "Kit", "Fire", "Smart", "Bal", "Base")

design
