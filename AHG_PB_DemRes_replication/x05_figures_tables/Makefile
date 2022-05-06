#
# Authors:     AHG
# Maintainers: PB
# Copyright:   2018, HRDAG, GPL v2 or later
# ============================================
# replication/x05_figures_tables/Makefile

.PHONY: all clean

all: output/lethals-by-dept-dataset-log10.pdf

clean:
	rm output/*

# creates a bunch of other stuff as a side effect
output/lethals-by-dept-dataset-log10.pdf: \
		src/figs-tables.R \
		input/mse-results.csv \
		input/sv-lethals-anon.csv \
		input/merged-records-anon.csv \
		input/strata-list-es.csv \
		input/post-probs.csv \
		input/sums.rds \
		input/SLV_adm1.rds
	Rscript --no-init-file $<

# done.
