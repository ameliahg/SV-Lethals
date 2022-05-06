#
# Authors:     AHG
# Maintainers: PB
# Copyright:   2018, HRDAG, GPL v2 or later
# ============================================
# replication/x04_global_sums/Makefile

.PHONY: all clean

all: output/sums.rds

clean:
	rm output/*

# creates output/summed-post-probs.csv.gz as side effect
output/sums.rds: \
		src/sums.R \
		input/post-probs.csv \
		input/HANDMADE-DYS-FOR-GLOBAL-SUMS.csv \
		input/mse-results.csv
	Rscript --no-init-file $<

# done.
