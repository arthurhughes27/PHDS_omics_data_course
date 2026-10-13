# Generate the questions version of each practical from its solutions version.
#
# In a solutions file, wrap what students must not see in markers, each on its own line:
#   <!-- solution:code -->   ...   <!-- /solution -->
#       the region is replaced by an empty code chunk for the students to fill
#   <!-- solution:text -->   ...   <!-- /solution -->
#       the region is removed (e.g. answers)
#
# Run from the project root (open the .Rproj first) : source("scripts/make_questions.R")
# Only solutions files containing markers are processed. Do not edit the generated
# questions files by hand : edit the solutions file and run this script again.

library(here)

placeholder <- c("```{r}", "# Your code here", "```")
notice <- "<!-- This file is generated from the solutions file by scripts/make_questions.R : do not edit it directly. -->"

make_questions <- function(solutions_file) {
  lines <- readLines(solutions_file, encoding = "UTF-8", warn = FALSE)
  out <- character()
  region <- NULL   # NULL outside a solution region, otherwise "code" or "text"
  in_fence <- FALSE

  for (i in seq_along(lines)) {
    line <- lines[i]
    open <- regmatches(line, regexec("^<!-- solution:(code|text) -->\\s*$", line))[[1]]
    close <- grepl("^<!-- /solution -->\\s*$", line)

    if (length(open) > 0) {
      if (!is.null(region)) stop("Line ", i, ": solution region opened inside another one")
      region <- open[2]
      if (region == "code") out <- c(out, placeholder)
    } else if (close) {
      if (is.null(region)) stop("Line ", i, ": closing marker without an opening marker")
      region <- NULL
    } else if (is.null(region)) {
      if (grepl("^```", line)) in_fence <- !in_fence
      # Avoid stacking blank lines where regions were removed
      if (line == "" && !in_fence && length(out) > 0 && out[length(out)] == "") next
      out <- c(out, line)
    }
  }
  if (!is.null(region)) stop("A solution region is never closed")

  # Insert the notice after the YAML header, if there is one
  if (out[1] == "---") {
    end_yaml <- which(out == "---")[2]
    out <- append(out, c("", notice, ""), after = end_yaml)
  } else {
    out <- c(notice, "", out)
  }
  out
}

solutions_files <- list.files(here("practicals", "solutions"),
                              pattern = "_solutions\\.Rmd$", full.names = TRUE)

for (solutions_file in solutions_files) {
  if (!any(grepl("<!-- solution:", readLines(solutions_file, warn = FALSE)))) {
    message("Skipped (no markers) : ", basename(solutions_file))
    next
  }
  questions_file <- here("practicals", "questions",
                         sub("_solutions\\.Rmd$", "_questions.Rmd", basename(solutions_file)))
  writeLines(make_questions(solutions_file), questions_file, useBytes = TRUE)
  message("Written : ", questions_file)
}
