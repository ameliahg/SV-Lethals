#!/usr/bin/env Rscript --vanilla

# Authors: AHG, PB
# Maintainer: PB
# Date: 9 Apr 2015 (orig; revisions through 2019)
# Copyright: GPL v2 or later; HRDAG, 2019
#
# create strata for SV estimation

library(Rcapture)
library(argparse)
library(tidyverse)

parser <- ArgumentParser()
parser$add_argument('--input.merged', type='character')
parser$add_argument('--output', type='character')
arguments <- parser$parse_args()

es <- read_csv(arguments$input.merged,col_types='nnnnncccc')

# date fields

es$year <- substr(es$date,1,4)
es$month <- substr(es$date,6,7)
print(table(es$year))
print(table(es$month))
es$year <- as.numeric(es$year)
es$month <- as.numeric(es$month)
es$semester <- ifelse(es$month>6,2,1)
es$semester[es$month==0] <- 0

# geo fields

es$dcode <- substr(es$geocode,1,nchar(es$geocode)-4)
geotrans <- tibble(dcode=as.character(c(1:14)),
                   dept=c('AHUACHAPAN','SANTA ANA','SONSONATE',
                          'CHALATENANGO','LA LIBERTAD','SAN SALVADOR',
                          'CUSCATLAN','LA PAZ','CABANAS',
                          'SAN VICENTE','USULUTAN','SAN MIGUEL',
                          'MORAZAN','LA UNION'))

es <- left_join(es,geotrans)

# cell

es$cell <- paste('x',es$in_CDHES,es$in_Rescate,es$in_UN_TRC,es$in_UN_TRC_2,sep='')

# GLOBALS

table.cols <- c(
   "stratum",
   "x0001", "x0010", "x0011", "x0100",
   "x0101", "x0110", "x0111", "x1000",
   "x1001", "x1010", "x1011", "x1100",
   "x1101", "x1110", "x1111")


# STRATUM DEFS PART 1:
# NEW VARS

es$yr.sm <- paste(es$year, es$semester, sep='s')
print(table(es$yr.sm))

es$dp.yr <- paste(es$dept,es$year, sep='.')
es$dp.sm <- paste(es$dept, es$yr.sm, sep='.')

es$period <- ifelse(es$year < 1984, '8083', '8492')
es$dp.pd <- paste(es$dept, es$period, sep='.')

es$period2 <- 8081
es$period2[es$year %in% c(1982:1983)] <- 8283
es$period2[es$year %in% c(1984:1985)] <- 8485
es$period2[es$year %in% c(1986:1987)] <- 8687
es$period2[es$year %in% c(1988:1989)] <- 8889
es$period2[es$year %in% c(1990:1992)] <- 9092

es$period2.5 <- es$year
es$period2.5[es$year %in% c(1981:1982)] <- 8182
es$period2.5[es$year %in% c(1983:1984)] <- 8384
es$period2.5[es$year %in% c(1985:1986)] <- 8586
es$period2.5[es$year %in% c(1987:1988)] <- 8788
es$period2.5[es$year %in% c(1989:1990)] <- 8990
es$period2.5[es$year %in% c(1991:1992)] <- 9192


es$period3 <- 8082
es$period3[es$year %in% c(1983:1985)] <- 8385
es$period3[es$year %in% c(1986:1988)] <- 8688
es$period3[es$year %in% c(1989:1992)] <- 8992

es$period4 <- 8085
es$period4[es$year %in% c(1986:1992)] <- 8692

es$pd2.dp <- paste(es$period2,es$dept,sep='.')
es$pd2.5.dp <- paste(es$period2.5,es$dept,sep='.')
es$pd3.dp <- paste(es$period3,es$dept,sep='.')
es$pd4.dp <- paste(es$period4,es$dept,sep='.')

es$region <- 'WEST'
es$region[es$dept %in% c('LA LIBERTAD','CHALATENANGO',"SAN SALVADOR",'CUSCATLAN')] <- 'WCENTRAL'
es$region[es$dept %in% c('CABANAS','SAN VICENTE','LA PAZ','USULUTAN')] <- 'ECENTRAL'
es$region[es$dept %in% c('SAN MIGUEL','LA UNION','MORAZAN')] <- 'EAST'

es$re.yr <- paste(es$region,es$year,sep='.')
es$re.pd <- paste(es$region,es$period,sep='.')
es$re.pd2 <- paste(es$region,es$period2,sep='.')
es$re.pd2.5 <- paste(es$region,es$period2.5,sep='.')
es$re.pd3 <- paste(es$region,es$period3,sep='.')
es$re.pd4 <- paste(es$region,es$period4,sep='.')

es$r2 <- 'WEST2'
es$r2[es$dept %in% c('LA LIBERTAD','CUSCATLAN','SAN SALVADOR')] <- 'SSREGION'
es$r2[es$dept %in% c('CHALATENANGO','CABANAS')] <- 'NCENTRAL'
es$r2[es$dept %in% c('LA PAZ','SAN VICENTE','USULUTAN')] <- 'SCENTRAL'
es$r2[es$dept %in% c('SAN MIGUEL','MORAZAN','LA UNION')] <- "EAST2"

es$re2.yr <- paste(es$r2,es$year,sep='.')
es$re2.pd <- paste(es$r2,es$period,sep='.')
es$re2.pd2 <- paste(es$r2,es$period2,sep='.')
es$re2.pd2.5 <- paste(es$r2,es$period2.5,sep='.')
es$re2.pd3 <- paste(es$r2,es$period3,sep='.')
es$re2.pd4 <- paste(es$r2,es$period4,sep='.')

es$r3 <- 'AHUA-SONS'
es$r3[es$dept %in% c('SANTA ANA', 'CHALATENANGO')] <- 'SANT-CHAL'
es$r3[es$dept %in% c('LA LIBERTAD','SAN SALVADOR')] <- 'LALI-SANS'
es$r3[es$dept %in% c('CUSCATLAN','CABANAS')] <- 'CUSC-CABA'
es$r3[es$dept %in% c('LA PAZ', 'SAN VICENTE')] <- 'LAPA-SANV'
es$r3[es$dept %in% c('USULUTAN','SAN MIGUEL')] <- 'USUL-SANM'
es$r3[es$dept %in% c('MORAZAN','LA UNION')] <- 'MORA-LAUN'

es$re3.yr <- paste(es$r3,es$year,sep='.')
es$re3.pd <- paste(es$r3,es$period,sep='.')
es$re3.pd2 <- paste(es$r3,es$period2,sep='.')
es$re3.pd2.5 <- paste(es$r3,es$period2.5,sep='.')
es$re3.pd3 <- paste(es$r3,es$period3,sep='.')
es$re3.pd4 <- paste(es$r3,es$period4,sep='.')

# STRATUM DEFS PART 2:
# DEFS USING VARS

strata.levels <- c('year',
                   'yr.sm',
                   'dept',
                   'dp.yr',
                   'dp.pd',
                   'period2',
                   'pd2.dp',
                   'period3',
                   'pd3.dp',
                   'period2.5',
                   'pd2.5.dp',
                   'pd4.dp',
                   'region',
                   're.yr',
                   're.pd',
                   're.pd2',
                   're.pd2.5',
                   're.pd3',
                   're.pd4',
                   'r2',
                   're2.yr',
                   're2.pd',
                   're2.pd2',
                   're2.pd2.5',
                   're2.pd3',
                   're2.pd4',
                   'r3',
                   're3.yr',
                   're3.pd',
                   're3.pd2',
                   're3.pd2.5',
                   're3.pd3',
                   're3.pd4')

strata.list <- NULL
fname <- 'output/strata-list-es.csv'
for (slevel in strata.levels) {
  with.s.recs <- !is.na(es[[slevel]]) # get recs with non-NA values on strata level
  with.s <- es[with.s.recs, ] # subset data: only non-NA values on strata level
  x <- as.data.frame(table(with.s[[slevel]], with.s$cell))
  x2 <- reshape(x,v.names='Freq', timevar='Var2', idvar='Var1', direction='wide')
  stopifnot(length(names(x2)) == 16) # check that all the cells were defined.
  names(x2) <- table.cols
  min.nk <- 200
  rm(x, with.s, with.s.recs)
  x2$stratum.type <- slevel
  print(strata.list)
  print(dim(strata.list))
  print(x2)
  print(dim(x2))
  if (is.null(strata.list)) {
    strata.list <- x2
  } 
  else {
    strata.list <- rbind(strata.list, x2)
    print(strata.list)
  }
}

strata.list$stratum <- as.character(strata.list$stratum)
ncols.for.nk <- ncol(strata.list)-1
for (i in 1:nrow(strata.list)) strata.list$nk[i] <- sum(strata.list[i,2:ncols.for.nk])
strata.list <- subset(strata.list,strata.list$nk>=min.nk)
print(sort(unique(strata.list$stratum)))
print(nrow(strata.list))
write.table(strata.list, file=fname, row.names=FALSE, sep='|')

## done.

