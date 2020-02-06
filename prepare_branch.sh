R -e "devtools::document(); 
      devtools::build_vignettes(); 
      devtools::build_site();
      styler::style_dir();
      styler::style_dir(filetype = 'Rmd');
      devtools::document();
      devtools::check();
      codemetar::write_codemeta()"

if [[ README.Rmd -nt README.md ]]; then
  R -e "rmarkdown::render('README.Rmd');"
fi
