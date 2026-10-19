### precompute.R -- knit the live vignettes ahead of time, so they ship with their results
##
## Vignettes that query the WildObs database cannot run on CRAN or in CI, which hold no
## API key. Each one is written as <name>.Rmd.orig and knitted here, locally, into
## <name>.Rmd with every result already in place; R CMD check and pkgdown then only
## render that file. Re-run after a public database rebuild so the results stay current.
##
## Every result comes through the public API, so it shows only what any key holder can
## download. The key is read from the environment and is never printed.
## Run from the package root: Rscript vignettes/precompute.R [name ...]

## Load libraries
library(here) # For project-relative paths so this runs on any machine

## the vignettes to knit: those named on the command line, else every .Rmd.orig
args <- commandArgs(trailingOnly = TRUE)
sources <- if (length(args) > 0) {
  here("vignettes", paste0(args, ".Rmd.orig"))
} else {
  list.files(here("vignettes"), pattern = "\\.Rmd\\.orig$", full.names = TRUE)
} # end sources condition

## a live vignette needs the key; stop before knitting half a vignette without one
if (!nzchar(Sys.getenv("WILDOBSR_API_KEY"))) {
  stop("WILDOBSR_API_KEY is not set, so precompute.R cannot run the live vignettes.\n",
       "Add your key to ~/.Renviron and restart R.")
} # end API key check

## load the package under development, so library(WildObsR) in a vignette finds this
## version rather than an older installed one
devtools::load_all(here(), quiet = TRUE, export_all = FALSE, helpers = FALSE,
                  attach_testthat = FALSE)

## knit inside vignettes/, so figure paths in the output are relative to it
old_wd <- setwd(here("vignettes"))
# and return to where we were, even if a vignette fails
on.exit(setwd(old_wd), add = TRUE)

# for each vignette source
for (src in sources) {
  # the knitted file sits beside it, without .orig
  out <- sub("\\.orig$", "", basename(src))
  message(sprintf("[%s] knitting %s", format(Sys.time(), "%H:%M"), out))
  # figures get the vignette's name as a prefix, so vignettes never overwrite each other
  knitr::opts_chunk$set(fig.path = paste0("figures/", sub("\\.Rmd$", "", out), "-"),
                        # a failing chunk stops the knit rather than shipping an error
                        error = FALSE)
  # run every chunk now and write the results into the .Rmd
  knitr::knit(basename(src), out, envir = new.env(), quiet = TRUE)
} # end per vignette
