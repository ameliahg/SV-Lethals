#
# Authors:     PB
# Maintainers: PB
# Copyright:   2018, HRDAG, GPL v2 or later
# ============================================
# replication/x04_global_sums/src/sums.R

library(readr)
library(tools)
library(magrittr)
library(dplyr)
library(tibble)
library(ggplot2)


args <- list(log = 'output/sums.log',
             pp = 'input/post-probs.csv',
             mse = 'input/mse-results.csv',
             cust = 'input/HANDMADE-DYS-FOR-GLOBAL-SUMS.csv',
             output_sums = 'output/sums.rds',
             output_probs = 'output/summed-post-probs.csv.gz')


# sink(args$log)
# print(sessionInfo())

calc_summary <- function(strata, summedname) {

    output <- strata %>%
        select(starts_with('pp')) %>%
        summarize_all(funs(sum))

    CIs <- output %>% as.integer(.) %>% quantile(., probs=c(0.025, 0.5, 0.975))
    output$dgalog.nhat <- CIs[2]
    output$dgalog.025 <- CIs[1]
    output$dgalog.975 <- CIs[3]

    xcells <- strata %>% select(starts_with('x'))
    # NB we have changed nk to n_{*} / nstar
    output$sname <- summedname
    output$nstrata <- nrow(strata)
    output$nstar <- xcells %>% sum(.)

    # count row-wise 0s
    numz <- xcells %>% mutate(numz = rowSums(. == 0)) %>% select(numz)
    output$zerocells <- sum(numz)
    output$any.zero.cells <- sum(numz > 0)
    
    return(output)
}


get_preselect_strata <- function(args, stratanames, summedname) {
  
    strata <- suppressMessages(read_csv(args$pp)) %>%
        filter(sname %in% stratanames)
    
    if (nrow(strata) != length(stratanames)) {
      print(strata)
      print(sort(stratanames))

      stop(summedname, ': strata=', nrow(strata),
              ', len(stnames)=', length(stratanames))
    }
    
    

    return(calc_summary(strata, summedname))
}

ct <- 'cnnnnnnnnnnnnnnncncnnnnncnnnnnnnncnnnnnnnnnnn'

getmulti <- function(args, to_use, summedname) {
    stratanames <- suppressMessages(read_csv(args$cust)) %>%
        select_(to_use) %>%  unique(.) %>%
        t(.) %>% as.vector(.)
    # print(problems(stratanames))

    return(get_preselect_strata(args, stratanames, summedname))
}


getyrs <- function(args, summedname) {
    stratanames <- suppressMessages(read_csv(args$mse)) %>%
        filter(yrsum == 1) %>% select(sname) %>%
        t(.) %>% as.character(.)

    return(get_preselect_strata(args, stratanames, summedname))
}


get_by_stype <- function(args, stype) {

    stratanames <- suppressMessages(read_csv(args$mse)) %>%
        filter(stratum.type == stype) %>%
        select(sname) %>% t(.) %>% as.vector(.)

    return(get_preselect_strata(args, stratanames, stype))
}


#---main-----
print('starting')

#---initialize output with multitype2-----
output <- getmulti(args, 'use2', 'multi-2')
print('done with multi 2')

#---add custom rows-----
output <- bind_rows(output, getmulti(args, 'use', 'multi-1'))
print('done with multi 1')
output <- bind_rows(output, getyrs(args, 'years.custom'))
print('done with years.custom')

#---add rows for other stratum.types-----
stypes <- suppressMessages(read_csv(args$mse)) %>%
    select(stratum.type) %>%
    unique(.) %>% t(.) %>% as.character(.)

print('starting stypes')
for (stype in stypes) {
    print(stype)
    tmp <- get_by_stype(args, stype)
    if (is.na(tmp)) {
        next
    }
    else{
        output <- bind_rows(output, get_by_stype(args, stype))
        print(paste('done with',stype))
    }
}

svsums <- output %>% select(-matches('^pp\\d+'))
print(str(svsums))
saveRDS(svsums, args$output_sums)

postprobs <- output %>% select(matches('pp\\d+|sname'))
print(str(postprobs))
write_delim(postprobs, args$output_probs, delim='|')

message('done')

# done
