#
# Authors:     PB
# Maintainers: PB
# Copyright:   2018, HRDAG, GPL v2 or later
# ============================================
# replication/x02_perform_estimates/Makefile
#
.PHONY: all

all: output/mse-results.csv

clean:
	rm output/*

output/mse-results.csv: \
		src/dga.R \
		input/strata-list-es.csv
	Rscript --no-init-file $<

# done.
