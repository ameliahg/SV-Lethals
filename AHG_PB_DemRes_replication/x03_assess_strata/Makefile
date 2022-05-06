#
# Authors:     PB
# Maintainers: PB, AHG
# Copyright:   2019, HRDAG, GPL v2 or later
# ============================================
# replication/x03_assess_strata/Makefile

.PHONY: all clean

all: output/mse-results.csv

clean:
	rm output/*
  
output/mse-results.csv: \
		src/stratum-quality.R \
		input/mse-results.csv 
	Rscript --no-init-file $<
  ## done.
  