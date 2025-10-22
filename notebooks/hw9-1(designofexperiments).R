### Question 12.2
# To determine the value of 10 different yes/no features to the market value of a house (large yard, solar roof, etc.), 
# a real estate agent plans to survey 50 potential buyers, showing a fictitious house with different combinations of features.  
# To reduce the survey size, the agent wants to show just 16 fictitious houses. Use R’s FrF2 function (in the FrF2 package) to find a fractional factorial design for this experiment: 
# what set of features should each of the 16 fictitious houses have?  Note: the output of FrF2 is “1” (include) or  “-1” (don’t include) for each feature.

library(FrF2)

set.seed(123)
design <- FrF2(nruns = 16, nfactors = 10, randomize = FALSE)

names(design) <- c("Yard", "Solar", "Gar", "Pool", "Sch",
                   "Kit", "Fire", "Smart", "Bal", "Base")

design

