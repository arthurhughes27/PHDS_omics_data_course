# Install all the R packages needed for the Omics practicals
# (genomics data analysis, multiplicity, supervised learning).
#
# How to use : open this file in RStudio and click "Source" (or run
# source("install_packages.R") in the console). It works on Windows, macOS and Linux.
# You need an internet connection, and the installation can take 15 to 30 minutes.
# You can run the script again at any time : packages already installed are skipped.

# ---- Packages ----

cran_packages <- c(
  "BiocManager",
  "caret",
  "dendextend",
  "dplyr",
  "flashClust",
  "futile.logger",
  "ggdendro",
  "ggplot2",
  "ggrepel",
  "glmnet",
  "here",
  "knitr",
  "pls",
  "ranger",
  "readxl",
  "rmarkdown",
  "stringr",
  "tidyr",
  "UpSetR",
  "VennDiagram"
)

bioc_packages <- c(
  "dearseq",
  "edgeR",
  "GEOquery",
  "limma",
  "pvca",
  "sva"
)

# ---- Settings ----

cat("R version :", R.version.string, "\n")
if (getRversion() < "4.1.0") {
  warning("Your version of R is old. Please install the latest version of R from ",
          "https://cran.r-project.org/ and run this script again.")
}

# Use a CRAN mirror if none is set
if (is.na(getOption("repos")["CRAN"]) || getOption("repos")["CRAN"] %in% c("", "@CRAN@")) {
  options(repos = c(CRAN = "https://cloud.r-project.org"))
}

# Some packages are large : allow slow downloads
options(timeout = max(600, getOption("timeout")))

# On Windows and macOS, use the ready-made (binary) packages and never ask to compile
if (.Platform$pkgType != "source") {
  options(install.packages.compile.from.source = "never")
}

installed <- function() rownames(installed.packages())

# ---- CRAN packages (including BiocManager, which installs the Bioconductor packages) ----

missing_cran <- setdiff(cran_packages, installed())
if (length(missing_cran) > 0) {
  cat("Installing CRAN packages :", paste(missing_cran, collapse = ", "), "\n")
  install.packages(missing_cran, dependencies = TRUE)
}

# ---- Bioconductor packages ----

if (!requireNamespace("BiocManager", quietly = TRUE)) {
  stop("The package BiocManager could not be installed. Please check your internet ",
       "connection and run this script again.")
}

missing_bioc <- setdiff(bioc_packages, installed())
if (length(missing_bioc) > 0) {
  cat("Installing Bioconductor packages :", paste(missing_bioc, collapse = ", "), "\n")
  tryCatch(
    BiocManager::install(missing_bioc, ask = FALSE, update = FALSE),
    error = function(e) message("Bioconductor installation failed : ", conditionMessage(e))
  )
}

# ---- Check that everything can be loaded ----

all_packages <- c(cran_packages, bioc_packages)
can_load <- vapply(all_packages, requireNamespace, logical(1), quietly = TRUE)

if (all(can_load)) {
  cat("\nAll", length(all_packages), "packages are installed and working. You are ready for the practicals.\n")
} else {
  cat("\nThe following packages could not be installed or loaded :\n  ",
      paste(all_packages[!can_load], collapse = ", "), "\n", sep = "")
  cat("\nRestart R (in RStudio : Session > Restart R) and run this script again. If the problem remains :\n",
      "  - Windows : install Rtools from https://cran.r-project.org/bin/windows/Rtools/\n",
      "  - macOS : run 'xcode-select --install' in the Terminal\n",
      "  - Linux : some packages need system libraries; read the error messages above\n",
      "and send me the messages shown above.\n", sep = "")
}
