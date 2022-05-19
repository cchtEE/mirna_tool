pkg_list <- c("shiny", "shinyBS", "shinyjs", "shinythemes", "devtools", "DT", "ggplot2", "truncdist", "shinyalert", "shinydashboard", "devtools",
"shinyWidgets", "RCurl", "R.utils", "BiocManager", "knitr", "testthat", "rmarkdown", "highcharter", "magrittr",
"ggrepel", "magrittr", "survival", "rlang", "RColorBrewer", "igraph", "downloader", "visNetwork", "survminer",
)

for(pkg in pkg_list){
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE )
  }
}

#devtools::install_version('BiocManager', version = '1.30.10', repos = 'http://cran.us.r-project.org')

pkg_list_bioc <- c("org.Hs.eg.db", "GenomicRanges", "clusterProfiler", "enrichplot", "hpar")

for(pkg in pkg_list_bioc){
  if (!require(pkg, character.only = TRUE)) {
    BiocManager::install(pkg)
  }
}

#devtools::install_github("isglobal-brge/CTDquerier")