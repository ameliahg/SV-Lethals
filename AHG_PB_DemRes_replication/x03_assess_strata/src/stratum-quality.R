# Authors:     AHG
# Maintainers: AHG 
# Copyright:   2019, HRDAG, GPL v2 or later
# ============================================
# replication/x04_assess_strata/src/stratum-quality.R

library(tidyverse)

dytable <- read_csv('hand/dytable.csv')
print(dytable)
mse <- read_csv('input/mse-results.csv')
print(mse)

dytable$sname <- gsub('8491','8492',dytable$sname,fixed=TRUE)
mse$sname <- paste(mse$stratum.type,mse$stratum,sep='-')
dytable <- filter(dytable,sname %in% mse$sname)

mse$questionable.diptest <- ifelse(mse$diptestlog > 0.02,1,0)
mse$bad.diptest <- ifelse(mse$diptestlog > 0.025,1,0)
# mse$big.nhat.disagreement <- ifelse(abs((mse$dga.nhat-mse$dgalog.nhat)/mse$nk)>1,1,0)
# above line is used for comparing default estimates to those w lognormal priors
mse$wideci <- ifelse(((mse$dgalog.975-mse$dgalog.025)/mse$nk)>5,1,0)
mse$small <- ifelse(mse$nk<350,1,0)
mse$zerocells <- 0

for (i in 1:nrow(mse)){
  mse$zerocells[i] <- length(which(mse[i,c(2:16)]==0))
}

dytable$stratsize <- 0
dytable$questionable.diptest <- 0
dytable$bad.diptest <- 0
# dytable$big.nhat.disagreement <- 0
dytable$wideci <- 0
dytable$bad <- NULL
dytable$small <- 0

n <- intersect(dytable$sname,mse$sname)
print(length(n))

for (f in n){
  r <- which(mse$sname==f)
  print(r)
  r2 <- which(dytable$sname==f)
  ss <- length(unique(dytable$dy[r2]))
  qdt <- ifelse(mse$questionable.diptest[r]==1,1,0)
  bdt <- ifelse(mse$bad.diptest[r]==1,1,0)
  # bnd <- ifelse(mse$big.nhat.disagreement[r]==1,1,0)
  wci <- ifelse(mse$wideci[r]==1,1,0)
  sm <- ifelse(mse$small[r]==1,1,0)
  dytable$stratsize[r2] <- ss
  dytable$questionable.diptest[r2] <- qdt
  dytable$bad.diptest[r2] <- bdt
  # dytable$big.nhat.disagreement[r2] <- bnd
  dytable$wideci[r2] <- wci
  dytable$small[r2] <- sm
}

dytable$quality <- dytable$questionable.diptest+dytable$bad.diptest+
  # dytable$big.nhat.disagreement+
  dytable$wideci+dytable$small


dysummary <- group_by(dytable,dy) %>% summarise(nstrat=n(),
                                                small=length(which(small==1)),
                                                qscore4=length(which(quality==4)),
                                                qscore3=length(which(quality==3)),
                                                qscore2=length(which(quality==2)),
                                                qscore1=length(which(quality==1)),
                                                qscore0=length(which(quality==0)))

mse$yrsum <- 0
mse$yrsum[mse$stratum %in% as.character(c(1983:1990))] <- 1
mse$yrsum[mse$stratum %in% c('1980s1','1980s2','1981s1','1981s2','1982s1','1982s2')] <- 1
mse$yrsum[mse$stratum=='9192'] <- 1
mse$yrsum[mse$sname=='period2.5-1980'] <- 0 # there are 2 1980 strata, Because Reasons

mse$deptsum <- 0
mse$deptsum[mse$stratum %in% c('MORAZAN','AHUACHAPAN','CABANAS','CHALATENANGO','CUSCATLAN','LA LIBERTAD',
                               'LA PAZ','SAN MIGUEL','SAN SALVADOR','SAN VICENTE','SANTA ANA','SONSONATE',
                               'USULUTAN')] <- 1


write.table(dytable,file='hand/dytable.csv',row.names=FALSE,sep=',')
write.table(dysummary,file='hand/dy-by-stratumquality.csv',row.names=FALSE,sep=',')
write.table(mse,file='output/mse-results.csv',row.names=FALSE,sep=',')

# ^ the tables in hand are used to inform custom stratifications (i.e. to create global sums multi-1 and multi-2).
# the resulting list(s) of strata to sum over are placed in a human-created file called HANDMADE-DYS-FOR-GLOBAL-SUMS.csv 
# and are linked to output so that these sums can be analyzed. other analysts may wish to change the way these custom
# global sums are created. the key requirement is that all observations (i.e., in our analysis, all DYs) get used and 
# no DYs are double-counted. it's a hard balancing act when also trying to consider stratum quality.
