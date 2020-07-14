if(!require(shiny) | !require(shinyIncubator) | !require(devtools) | !require(ggplot2)) {
	install.packages(c('devtools','ggplot2'), repos='http://cran.r-project.org')
	require(devtools)
	install_github('shiny', 'rstudio')
	install_github('shiny-incubator', 'rstudio')
	require(shiny)
	require(ggplot2)
	require(shinyIncubator)
}

shiny::runGitHub('ShinyApps', 'JHYip', subdir='POS.R')
