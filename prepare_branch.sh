R -e "devtools::document(); 
      devtools::build_vignettes(); 
      rmarkdown::render('README.Rmd'); 
      devtools::build_site();
      styler::style_dir();
      styler::style_dir(filetype = 'Rmd');
      devtools::document();
      devtools::check();
      codemetar::write_codemeta()"
