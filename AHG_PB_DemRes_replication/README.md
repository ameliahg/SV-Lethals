# Replication of "Civilian Killings and Disappearances During Civil War in El Salvador (1980-92)": Instructions #

## Introduction

This replication directory has five tasks, numbered in the order in which they should typically be run (x01, x02...x05). Together, these five tasks create all the analyses referenced in our article, and all the tables and figures included in the article. 

You will probably need to install several R packages. These include dga, diptest, tidyverse, and ggplot2. To install package X, or if any task exits with a "no package named X" error, do install.packages("X") in your R/RStudio window.

In each task, you will find three directories: input, output, and src. Input is where data goes before analysis, src contains the code for the analysis, and output is where the completed analysis (or transformed data, or what have you) goes. Here we describe both the general process for running any given task and the specific job of each task, as well as providing a list of files in each task.

Notice that matching and merging are not included in the replication package due to the nature of our data-sharing agreements and Hoover Green's human subjects protocols. It is possible to arrange to share non-anonymized data. Please contact us: ameliahoovergreen@gmail.com / pball@hrdag.org.


## To replicate one task

1. Open a Terminal window.
2. Navigate to the task you want to replicate using the cd command. For example, if you use a Mac, and you saved the folder "replication" on your Desktop, and you want to run the first task (x01_make_strata), open the Terminal and write

> cd ~/Desktop/replication/x01_make_strata

3. Now, in the same Terminal window, write

> make clean

4. Now type

> make

5. That's it. The task should run, barring things like missing R packages (on which, see above). If you have never used Makefiles before, you may want to check out one of these tutorials to understand how our file structure works. 

http://web.mit.edu/gnu/doc/html/make_2.html
https://www.cs.swarthmore.edu/~newhall/unixhelp/howto_makefiles.html


## What each task does

### x01_make_strata

#### Input: 
- Anonymized, merged records (merged-records-anon.csv)

#### Src: 
- Code to define strata and create overlap counts for each stratum (make-strata.R)

#### Output: 
- List of strata (strata-list-es.csv)

### x02_perform_estimates

NOTE: Depending on your machine, this task can take an hour or more to run.

#### Input: 
- List of strata created in x01 (strata-list-es.csv)

#### Src: 
- Code to perform multiple systems estimates for all strata (dga.R).

#### Output: 
- MSE results for each stratum (mse-results.csv); 
- Posterior probabilities for each stratum (post-probs.csv).

###  x03_assess_strata

#### Input: 
- MSE results for each stratum created in x02 (mse-results.csv)

#### Src: 
- Code to assess stratum quality (stratum-quality.R)

#### Hand: 
- This is an unusual directory; as the name implies, the data here (custom stratifications) are made BY HAND (HANDMADE-DYS-FOR-GLOBAL-SUMS.csv). This file is what we used to create custom stratifications; in it, each department-year stratum (the minimum size of stratum in this analysis) is assigned to one stratum. Sometimes those strata are the department-years themselves; other times, multiple department-years are merged into one stratum.

#### Output: 
- MSE results with diagnostics for each stratum (mse-results.csv)
- Handmade custom strata (HANDMADE-DYS-FOR-GLOBAL-SUMS.csv)

### x04_global_sums

#### Input: 
- MSE estimates with diagnostics created in x02 (mse-results.csv) 
- Posterior probabilities created in x02 (post-probs.csv)
- Custom stratifications for global sums created in x03 (HANDMADE-DYS-FOR-GLOBAL-SUMS.csv).

#### Src: 
- Code that sums over stratifications to create summed global posterior probabilities (sums.R)

#### Output: 
- Global sums and confidence intervals (sums.rds)
- Posterior probability draws for global sums (summed-post-probs.csv.gz)

### x05_figures_tables

#### Input:
- Anonymized merged records (merged-records-anon.csv)
- Anonymized version of pre-merge records (sv-lethals-anon.csv)
- List of strata created in x01 (strata-list-es.csv)
- MSE results with diagnostics created in x03 (mse-results.csv)
- Posterior probabilities created in x03 (post-probs.csv)
- Global sums created in x04 (sums.rds)
- Map data for El Salvador (SLV_adm1.rds)
- Summed posterior probabilities created in x04 (summed-post-probs.csv.gz)
- Population data (elsal-pop-data.csv)

#### Src: 
- Create all tables and figures for inclusion in the paper (figs-tables.R)
- Create multiple-figure layouts (multiplot.R)

#### Output:
- Figures (prior-comparison-fig.pdf, lethals-by-yr-raw-fig.pdf, lethals-by-yr-est-fig.pdf, lethals-by-dept-raw-fig.pdf, post-prob-multi2-fig.pdf, lethals-by-dept-est-fig.pdf, post-prob-violins-fig.pdf, post-prob-1981-fig.pdf)
- Tables (bydept-table.txt, bydept-detail-table.txt, year-est-table.txt, year-cor-table-spearman.txt, overlap-table.txt, dept-cor-table-spearman.txt, bydept-table.txt)




