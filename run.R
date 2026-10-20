# Render the complete analysis in R. Usage: Rscript run.R [data_directory]
args <- commandArgs(trailingOnly=TRUE)
script_arg <- grep("^--file=",commandArgs(),value=TRUE)
project <- if(length(script_arg)) dirname(normalizePath(sub("^--file=","",script_arg[1]))) else getwd()
setwd(project)
local_lib <- file.path(project,".R-library")
if(dir.exists(local_lib)) .libPaths(c(local_lib,.libPaths()))
required <- c("data.table","fixest","ggplot2","rmarkdown","knitr","jsonlite")
missing <- required[!vapply(required,requireNamespace,logical(1),quietly=TRUE)]
if(length(missing)) stop("Missing R packages: ",paste(missing,collapse=", "),". Run install_dependencies.R first.")
if(!rmarkdown::pandoc_available()) {
  candidates <- c("/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools/aarch64","/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools/x86_64")
  found <- candidates[file.exists(file.path(candidates,"pandoc"))]
  if(length(found)) Sys.setenv(RSTUDIO_PANDOC=found[1])
}
if(!rmarkdown::pandoc_available()) stop("Pandoc is required. Render from RStudio or install Pandoc.")
data_dir <- normalizePath(if(length(args)) args[1] else "data/ca_electricity_data",mustWork=TRUE)
rmarkdown::render("analysis.Rmd",output_file="index.html",params=list(data_dir=data_dir),envir=new.env(parent=globalenv()),quiet=FALSE)
