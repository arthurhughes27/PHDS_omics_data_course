# Generate the questions version of each practical from its solutions version.
#
# In a solutions file, wrap what students must not see in markers, each on its own line:
#   <!-- solution:code -->   ...   <!-- /solution -->
#       the region is replaced by an empty code chunk for the students to fill
#   <!-- solution:text -->   ...   <!-- /solution -->
#       the region is removed (e.g. answers)
#   <!-- solution:code provides=object_name -->   ...   <!-- /solution -->
#       same as solution:code (the region contains one code chunk), and the code is kept
#       for the checkpoint marker below
#   <!-- checkpoint:object_name -->
#       replaced by a chunk, hidden in the knitted output, which runs the code of the
#       regions providing object_name, but only if the student has not created that
#       object. The code given to students after a task can then always run.
#
# Run from the project root (open the .Rproj first) : source("scripts/make_questions.R")
# Only solutions files containing markers are processed. Do not edit the generated
# questions files by hand : edit the solutions file and run this script again.

library(here)

placeholder <- c("```{r}", "# Your code here", "```")
notice <- "<!-- This file is generated from the solutions file by scripts/make_questions.R : do not edit it directly. -->"

# Lets the questions file be knitted before all the tasks are completed : errors from
# the missing student code are shown in the output instead of stopping the knit
knit_option <- c("```{r, include = FALSE}",
                 "knitr::opts_chunk$set(error = TRUE)",
                 "```")

# The code inside the first chunk of a region
chunk_body <- function(region_lines) {
  start <- grep("^```\\{r", region_lines)[1]
  end <- grep("^```\\s*$", region_lines)
  end <- end[end > start][1]
  if (is.na(start) || is.na(end)) stop("A 'provides' region must contain a code chunk")
  region_lines[(start + 1):(end - 1)]
}

# Hidden chunk which creates `name` if the student has not
setup_chunk <- function(name, body) {
  while (body[length(body)] == "") body <- body[-length(body)]   # drop trailing blank lines
  c("```{r, include = FALSE}",
    paste0("# Setup, not part of the tasks : creates `", name, "` if you have not created it"),
    paste0("if (!exists(\"", name, "\")) {"),
    ifelse(body == "", "", paste0("  ", body)),
    "}",
    "```")
}

make_questions <- function(solutions_file) {
  lines <- readLines(solutions_file, encoding = "UTF-8", warn = FALSE)
  out <- character()
  region <- NULL           # NULL outside a solution region, otherwise "code" or "text"
  region_lines <- character()
  provides <- NULL
  provided <- list()       # code of the regions providing each object
  in_fence <- FALSE

  for (i in seq_along(lines)) {
    line <- lines[i]
    open <- regmatches(line, regexec("^<!-- solution:(code|text)( provides=(\\w+))? -->\\s*$", line))[[1]]
    close <- grepl("^<!-- /solution -->\\s*$", line)
    checkpoint <- regmatches(line, regexec("^<!-- checkpoint:(\\w+) -->\\s*$", line))[[1]]

    if (length(checkpoint) > 0) {
      if (!is.null(region)) stop("Line ", i, ": checkpoint inside a solution region")
      name <- checkpoint[2]
      if (is.null(provided[[name]])) stop("Line ", i, ": no region provides ", name)
      out <- c(out, setup_chunk(name, provided[[name]]))
    } else if (length(open) > 0) {
      if (!is.null(region)) stop("Line ", i, ": solution region opened inside another one")
      region <- open[2]
      provides <- if (open[4] == "") NULL else open[4]
      region_lines <- character()
      if (region == "code") out <- c(out, placeholder)
    } else if (close) {
      if (is.null(region)) stop("Line ", i, ": closing marker without an opening marker")
      if (!is.null(provides)) {
        provided[[provides]] <- c(provided[[provides]], chunk_body(region_lines), "")
      }
      region <- NULL
      provides <- NULL
    } else if (!is.null(region)) {
      region_lines <- c(region_lines, line)
    } else {
      if (grepl("^```", line)) in_fence <- !in_fence
      # Avoid stacking blank lines where regions were removed
      if (line == "" && !in_fence && length(out) > 0 && out[length(out)] == "") next
      out <- c(out, line)
    }
  }
  if (!is.null(region)) stop("A solution region is never closed")

  # Insert the notice and the knitr option after the YAML header, if there is one
  if (out[1] == "---") {
    end_yaml <- which(out == "---")[2]
    out <- append(out, c("", notice, "", knit_option, ""), after = end_yaml)
  } else {
    out <- c(notice, "", knit_option, "", out)
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
