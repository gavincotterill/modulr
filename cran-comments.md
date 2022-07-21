## R CMD check results
There were no ERRORs, or WARNINGs.

There was 1 NOTE: 
* New Submission


## Resubmission
This is a resubmission. In this version I have:

* Added new functions, tests and vignettes.

* Functions no longer print() information to console but use message(), warning() or stop().

* Examples no longer change par() settings. Vignettes store user's par settings at beginning and restore them at end.

* plot_simulated_graph() and plot_sampled_graph() no longer set.seed within function, but option exists for the user to provide their own (default = NULL), which serves as a reminder if they want node positions to match between their 'true' and 'sampled network'.

* Removed single quotes around the whole DESCRIPTION text.

* Within the text, now using single undirected quotes.

* Neither name of package or word 'package' in TITLE or DESCRIPTION.

* Currently no DOI, getting ready to resubmit manuscript.

* All SUGGESTs are currently available on CRAN.


