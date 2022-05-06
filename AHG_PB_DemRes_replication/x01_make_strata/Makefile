# Authors:     PB
# Maintainers: PB, AHG
# Copyright:   2019, HRDAG, GPL v2 or later
# ============================================
# replication/x01_make_strata/Makefile

.PHONY: all clean

all: output/strata-list-es.csv

clean:
	rm output/*

output/strata-list-es.csv: \
		src/make-strata.R \
		input/merged-records-anon.csv
	/usr/bin/env Rscript --vanilla $< \
		--input.merged=input/merged-records-anon.csv \
		--output=$@
## done.
