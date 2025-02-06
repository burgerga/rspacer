# Thanks to https://ropensci.org/blog/2019/12/08/precompute-vignettes/
knitr::knit("vignettes/rspacer.Rmd.in", output = "vignettes/rspacer.Rmd")
knitr::knit("vignettes/articles/create_document_from_html.Rmd.in",
            output = "vignettes/articles/create_document_from_html.Rmd")
