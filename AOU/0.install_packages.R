# Install packages

packages <- c("tidyverse", "DiagrammeR", "DiagrammeRsvg", "bigrquery", "cmprsk",
              "conflicted", "data.table", "data.table", "devtools",
              "fastcmprsk", "ggplot2", "ggrepel", "lubridate","lubridate", 
              "mice", "pracma", "prodlim", "riskRegression", "rsvg", "survAUC",
              "survival", "survival", "tictoc", "timeROC", "data.table")

for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}
