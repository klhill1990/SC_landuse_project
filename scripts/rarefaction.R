#RAREFACTION

#load required packages

library(vegan)
library(tidyverse)

#import datasets

spmatrix <- read.csv("./data/species_matrix.csv")

spmatrix <- spmatrix[c(2:130)]
spmatrix <- spmatrix %>% replace(is.na(.), 0)

View(spmatrix)

S <- (ncol(spmatrix))
raremax <- 10

Srare <- rarefy(spmatrix, 1)
plot(129, 1, xlab = "Observed No. of Species", ylab = "Rarefied Number of Species")
abline(0, 1)
rarecurve(spmatrix, step = 20, sample = 0, col = "blue", cex = 0.6)
