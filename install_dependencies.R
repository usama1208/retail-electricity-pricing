# Install missing packages into a project-local library, leaving system libraries unchanged.
local_lib <- file.path(getwd(),".R-library")
dir.create(local_lib,showWarnings=FALSE)
.libPaths(c(local_lib,.libPaths()))
packages <- c("data.table","fixest","ggplot2","rmarkdown","knitr","jsonlite")
missing <- packages[!vapply(packages,requireNamespace,logical(1),quietly=TRUE)]
if(length(missing)) install.packages(missing,lib=local_lib,repos="https://cloud.r-project.org")
