## R CMD check results
There were no ERRORs, or WARNINGs.

There was 1 NOTE: 
```
checking CRAN incoming feasibility ... NOTE
  Maintainer: ‘Gavin Cotterill <gcotterill2@unl.edu>’
  
  New submission
```
## Resubmission
This is a resubmission. Changes to this version include:

* (possibly) invalid URL from 'codecov' badge in 'README.md'. 
I deleted/recreated 'codecov' repo connection and checked that reports are uploading. I updated the README badges and double checked links. I also checked that my format follows other CRAN packages (ggplot2). I re-ran checks to ensure no URL warnings:

```
devtools::check(
  manual = TRUE,
  remote = TRUE,
  incoming = TRUE
)

devtools::check_win_devel()
```

* Added R-CMD-check CI and badge

### Previous revisions
* Added new functions, tests and vignettes.

* Functions no longer print() information to console but use message(), warning() or stop().

* Examples no longer change par() settings. Vignettes store user's par settings at beginning and restore them at end.

* plot_simulated_graph() and plot_sampled_graph() no longer set.seed within function, but option exists for the user to provide their own (default = NULL), which serves as a reminder if they want node positions to match between their 'true' and 'sampled network'.

* Removed single quotes around the whole DESCRIPTION text.

* Within the text, now using single undirected quotes.

* Neither name of package or word 'package' in TITLE or DESCRIPTION.

* Currently no DOI, getting ready to resubmit manuscript.

* All SUGGESTs are currently available on CRAN.


