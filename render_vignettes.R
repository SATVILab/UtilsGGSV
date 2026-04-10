Sys.setenv(RSTUDIO_PANDOC='C:/Users/siane/AppData/Local/Programs/Quarto/bin/tools')
stopifnot(requireNamespace('rmarkdown', quietly=TRUE))
stopifnot(requireNamespace('knitr', quietly=TRUE))
dir.create('inst/doc', recursive=TRUE, showWarnings=FALSE)
tryCatch({
  rmarkdown::render('vignettes/UtilsGGSV.Rmd', output_format='rmarkdown::html_vignette', output_dir='inst/doc', clean=TRUE)
}, error=function(e){cat('ERROR1:', conditionMessage(e), '\n'); q(status=1)})
tryCatch({
  rmarkdown::render('vignettes/benchmarking-merges.Rmd', output_format='rmarkdown::html_vignette', output_dir='inst/doc', clean=TRUE)
}, error=function(e){cat('ERROR2:', conditionMessage(e), '\n'); q(status=1)})