R -e "devtools::document()"
R -e "devtools::build_vignettes()"
R -e "rmarkdown::render('README.Rmd')"
R -e "devtools::build_site()"

R -e "styler::style_dir()"
R -e 'styler::style_dir(filetype = "Rmd")'
R -e "devtools::document()"
R -e "devtools::check()"

R -e "codemetar::write_codemeta()"
