
<!-- README.md is generated from README.Rmd. Please edit that file.
Build with

load_all()
rmarkdown::render("README.Rmd")

which builds the .html that can be viewed locally (but isn't pushed to GitHub;
GitHub uses README.md to make the page you see on GitHub). See pacea if want to
save figures.
-->

# hdiAnalysis

<!-- badges: start -->

[![R-CMD-check](https://github.com/andrew-edwards/hdiAnalysis/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/andrew-edwards/hdiAnalysis/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/andrew-edwards/hdiAnalysis/branch/main/graph/badge.svg)](https://app.codecov.io/gh/andrew-edwards/hdiAnalysis?branch=main)
![Visitors](https://api.visitorbadge.io/api/visitors?path=https%3A%2F%2Fgithub.com%2Fandrew-edwards%2FhdiAnalysis&label=VISITORS&countColor=%23263759&style=flat&labelStyle=lower)
<!-- badges: end -->

An R package for investigating the use of hdi in stock assessments

## Installation

Okay, if you’re convinced this package might be useful for you, then to
install the latest version just:

    install.packages("remotes")    # If you do not already have the "remotes" package

    remotes::install_github("andrew-edwards/hdiAnalysis")

If you get an error like

    Error in utils::download.file(....)

then the connection may be timing out (happens to us on the DFO
network). Try

    options(timeout = 1200)

and then try and install again. If you get a different error then post
an Issue or contact
<a href="mailto:andrew.edwards@dfo-mpo.gc.ca">Andy</a> for help.
