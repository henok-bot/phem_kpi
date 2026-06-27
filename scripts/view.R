#!/usr/bin/env Rscript

# ----------------------------------------------------------------------------
# view.R  —  browse the raw KPI files directly inside Positron
#
# Source it once at the start of an interactive session:
#     source("scripts/view.R")
#
# Then use:
#     kpi_files()                       # list all source xlsx/docx
#     sheets("Benishangul")             # list sheets in a workbook (fuzzy name)
#     vx("Benishangul")                 # open 1st sheet in the Data Explorer
#     vx("Benishangul", "Data")         # open a sheet (fuzzy sheet name)
#     vx("Benishangul", 2)              # open the 2nd sheet
#     vd("DOC-2025")                    # open a .docx as text in the editor
#
# `vx()` opens Positron's interactive Data Explorer (sort/filter/scroll).
# Fuzzy matching means you only type enough of the name to be unique.
# ----------------------------------------------------------------------------

suppressMessages({
  library(readxl)
  library(officer)
})

# directory holding the raw source files (default: raw/, searched recursively
# so all period folders are covered). Override with options(kpi.dir = "...").
KPI_DIR <- getOption("kpi.dir", if (dir.exists("raw")) "raw" else ".")

.kpi_all <- function(ext = "(xlsx|xls|docx)") {
  list.files(KPI_DIR, pattern = paste0("\\.", ext, "$"),
             full.names = TRUE, ignore.case = TRUE, recursive = TRUE)
}

# fuzzy pick: match `pattern` against file basenames, must be unique
.pick <- function(pattern, ext = "(xlsx|xls|docx)") {
  f <- .kpi_all(ext)
  if (is.numeric(pattern)) return(f[[pattern]])
  hit <- f[grepl(pattern, basename(f), ignore.case = TRUE)]
  if (length(hit) == 0) stop("No file matches '", pattern, "'. Try kpi_files().")
  if (length(hit) > 1) {
    stop("'", pattern, "' matches several files:\n  ",
         paste(basename(hit), collapse = "\n  "),
         "\nBe more specific.")
  }
  hit
}

# --- public functions -------------------------------------------------------

kpi_files <- function() {
  f <- .kpi_all()
  cat("Source files in '", normalizePath(KPI_DIR), "':\n", sep = "")
  for (x in f) cat("  -", basename(x), "\n")
  invisible(f)
}

sheets <- function(file) {
  f <- .pick(file, "(xlsx|xls)")
  s <- excel_sheets(f)
  cat("Sheets in", basename(f), ":\n")
  for (i in seq_along(s)) cat("  ", i, ") ", s[i], "\n", sep = "")
  invisible(s)
}

# view an xlsx sheet in Positron's Data Explorer
vx <- function(file, sheet = 1, raw = TRUE) {
  f  <- .pick(file, "(xlsx|xls)")
  sh <- excel_sheets(f)
  if (is.character(sheet)) {
    m <- sh[grepl(sheet, sh, ignore.case = TRUE)]
    if (length(m) == 0) stop("No sheet matches '", sheet, "'. Have: ",
                             paste(sh, collapse = ", "))
    sheet <- m[[1]]
  } else {
    sheet <- sh[[sheet]]
  }
  df <- read_excel(f, sheet = sheet,
                   col_names = !raw, .name_repair = "minimal")
  df <- as.data.frame(df)
  label <- paste0(substr(basename(f), 1, 20), " | ", sheet)
  message("Opening: ", basename(f), "  ::  ", sheet,
          "  (", nrow(df), " x ", ncol(df), ")")
  get("View")(df, title = label)   # Positron Data Explorer
  invisible(df)
}

# view a .docx as text in the Positron editor (uses the converted .md if present)
vd <- function(file) {
  f   <- .pick(file, "docx")
  stem <- tools::file_path_sans_ext(basename(f))
  md  <- file.path("converted", paste0(gsub("[^A-Za-z0-9._-]+", "_", stem), ".md"))
  if (file.exists(md)) {
    message("Opening converted markdown: ", md,
            "  (press the Preview button to render)")
    utils::file.edit(md)
    return(invisible(md))
  }
  # fallback: print text to console
  s <- docx_summary(read_docx(f))
  txt <- s$text[s$content_type == "paragraph" & nzchar(trimws(s$text))]
  cat(txt, sep = "\n\n")
  invisible(txt)
}

message("KPI viewers loaded. Try: kpi_files()  |  sheets(\"Afar\")  |  vx(\"Afar\")  |  vd(\"DOC\")")
