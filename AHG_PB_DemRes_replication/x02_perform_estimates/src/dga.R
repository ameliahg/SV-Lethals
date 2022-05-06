# Authors:     AHG
# Maintainers: AHG, PB
# Copyright:   2018, HRDAG, GPL v2 or later
# ============================================
# replication/perform_estimates_03/src/dga.R

library(stringr)
library(tidyverse)
library(dga)
library(Rcapture)
library(doMC)
library(foreach)
library(diptest)


args <- list(
  input = './input/strata-list-es.csv',
  output = './output/mse-results.csv',
  output.postprobs = './output/post-probs.csv'
)

cc <- read_delim(args$input, delim='|', progress=F, col_types='cnnnnnnnnnnnnnnncn')
cc$sname <- paste(cc$stratum.type,cc$stratum,sep='-')
for (i in 1:nrow(cc)) cc$nk[i] <- sum(as.numeric(cc[i,2:16]))
cc <- filter(cc,nk>=200, nk<=10000)
print(dim(cc))

pp <- select(cc,sname,x0001,x0010,x0011,x0100,x0101,x0110,x0111,x1000,x1001,x1010,x1011,x1100,x1101,x1110,x1111,nk)

for (i in 1:1000){
  ppname <- paste('pp',i,sep='')
  pp[[ppname]] <- NA
}

print(dim(pp))

get_dga <- function(rownum,num.lists,max.multiplier,ml,sdl) {
  # args: row number in cc, number of lists (almost always 4 in SV), maximum plausible multiplier,
  # meanlog parameter for lognormal prior, sdlog parameter for lognormal prior
  overlaps <- c(0,as.numeric(cc[rownum,2:16]))
  nk <- sum(overlaps)
  Y <- array(overlaps, dim=c(2,2,2,2))
  max.missing <- max.multiplier*nk
  Nmissing <- c(1:max.missing)  # Nmissing is a sequence from 1 the largest plausible number of unobserved
  delta <- 1/(2^num.lists)      # this is the shape of the prior distribution

  if (is.na(ml)|is.na(sdl)){ # if mlog or sdlog are NA, just do BMA with the default prior
    weights <- bma.cr(Y, Nmissing, delta, graphs4)
  }

  else {
    priorN <- dlnorm(seq(0, max.multiplier, length.out=max.missing), meanlog=ml, sdlog=sdl)
    priorN <- priorN/sum(priorN)
    priorN <- log(priorN)
    weights <- bma.cr(Y, Nmissing, delta, graphs4,logprior=priorN)
  }
  # check which graph gets the most weight and how much weight it gets
  graph.wt <- rowSums(weights)
  top.graph <- which.max(graph.wt)
  top.wt <- graph.wt[top.graph]
  sat.model <- nrow(weights)
  if ((sat.model==top.graph) && (top.wt>.1)) {
    weights <- weights[1:(nrow(weights)-1),]
    # re-normalize
    weights <- weights/sum(weights)
    sat.renorm = "T"
  } else {
    sat.renorm = "F"
  }
  post_probs <- colSums(weights)
  m0s_sample_from_post <- sample(Nmissing, size=1000, replace=TRUE, prob=post_probs)
  nhat_sample_from_post <- m0s_sample_from_post + sum(Y)
  dt <- as.numeric(dip.test(nhat_sample_from_post))
  quants <- as.integer(quantile(nhat_sample_from_post, probs=c(0.025, 0.25, 0.5, 0.75, 0.975)))
  return(c(quants,sat.renorm,dt[1],dt[2],top.graph,nhat_sample_from_post))
}

# main

cc$dgalog.025 <- NA
cc$dgalog.250 <- NA
cc$dgalog.nhat <- NA
cc$dgalog.750 <- NA
cc$dgalog.975 <- NA
cc$sat.renormlog <- NA
cc$diptestlog <- NA
cc$diptest.pvallog <- NA
cc$topgraphlog <- NA

data(graphs4)

for (i in 1:nrow(cc)){
  print(paste(i,cc$sname[i],cc$nk[i],sep=':'))
  r2 <- get_dga(i,4,20,0.7,0.6)
  cc$dgalog.025[i] <- r2[1]
  cc$dgalog.250[i] <- r2[2]
  cc$dgalog.nhat[i] <- r2[3]
  cc$dgalog.750[i] <- r2[4]
  cc$dgalog.975[i] <- r2[5]
  cc$sat.renormlog[i] <- r2[6]
  cc$diptestlog[i] <- r2[7]
  cc$diptest.pvallog[i] <- r2[8]
  cc$topgraphlog[i] <- r2[9]
  pp[i,c(18:1017)] <- r2[c(10:1009)]
}

for (i in 20:ncol(cc)) cc[[i]] <- as.numeric(cc[[i]])

stopifnot(nrow(cc)==nrow(pp))

write.table(cc,file=args$output,row.names=FALSE,sep=',')
write.table(pp,file=args$output.postprobs,row.names=FALSE,sep=',')