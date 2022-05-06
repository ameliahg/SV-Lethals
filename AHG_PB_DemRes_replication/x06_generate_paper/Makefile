#
# Authors:     PB
# Maintainers: PB
# Copyright:   2018, HRDAG, GPL v2 or later
# ============================================
#
# SV/write/lethals/src/Makefile
#

all: output/es-lethals.pdf

clean:
	rm output/*

.PHONY: all clean


R_OPTS=--no-save --no-restore --no-init-file --no-site-file
KNIT="rmarkdown::render(input='$<', output_dir='output/')"

output/es-lethals.pdf: \
		src/es-lethals.Rmd
	-rm src/es-lethals.html src/es-lethals.pdf src/es-lethals.tex
	Rscript $(R_OPTS) -e $(KNIT)
	mv src/es-lethals.pdf output/
	-mv src/es-lethals.tex output/
	-rm -rf src/*fls src/*fdb_latexmk src/*aux src/*out

# done
