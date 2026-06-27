#!/usr/bin/env Rscript

# ----------------------------------------------------------------------------
# convert.R  —  reusable converter for KPI source files
#
# Converts:
#   .xlsx / .xls  ->  one CSV per sheet  +  one combined Markdown per workbook
#   .docx         ->  one Markdown file (paragraphs + tables, in order)
#
# It does NOT clean or reshape anything — it gives you a faithful, readable
# view of each file so you can browse from the terminal/IDE and guide analysis.
#
# Usage:
#   Rscript scripts/convert.R                  # convert ./ -> ./converted
#   Rscript scripts/convert.R <input>          # file or dir -> ./converted
#   Rscript scripts/convert.R <input> <outdir> # custom output dir
# ----------------------------------------------------------------------------

suppressMessages({
  library(readxl)
  library(officer)
  library(knitr)
})

# ---- helpers ---------------------------------------------------------------

# make a string safe for use as a folder/file name
slug <- function(x) {
  x <- gsub("[^A-Za-z0-9._-]+", "_", x)
  x <- gsub("_+", "_", x)
  gsub("^_|_$", "", x)
}

# tidy a data frame for markdown: NA -> "", strip newlines that break tables
md_clean <- function(df) {
  df[] <- lapply(df, function(col) {
    col <- as.character(col)
    col[is.na(col)] <- ""
    col <- gsub("[\r\n]+", " ", col)
    trimws(col)
  })
  df
}

# ---- xlsx ------------------------------------------------------------------

convert_xlsx <- function(path, outdir) {
  stem    <- tools::file_path_sans_ext(basename(path))
  sub     <- file.path(outdir, slug(stem))
  dir.create(sub, showWarnings = FALSE, recursive = TRUE)
  sheets  <- excel_sheets(path)
  md_lines <- c(paste0("# ", stem), "",
                paste0("Source: `", basename(path), "`  ",
                       "| Sheets: ", length(sheets)), "")

  for (sh in sheets) {
    # Preview only: cap rows so phantom sheets (e.g. an Addis workbook declaring
    # 1,048,576 rows) don't hang the converter. Real KPI sheets are well under
    # this. Trailing all-blank rows are trimmed below.
    PREVIEW_MAX <- 5000L
    df <- tryCatch(
      read_excel(path, sheet = sh, col_names = FALSE, .name_repair = "minimal",
                 n_max = PREVIEW_MAX),
      error = function(e) NULL)
    if (!is.null(df) && nrow(df) > 0) {
      blankrow <- apply(df, 1, function(r) all(is.na(r) | trimws(as.character(r)) == ""))
      lastreal <- suppressWarnings(max(which(!blankrow)))
      if (is.finite(lastreal)) df <- df[seq_len(lastreal), , drop = FALSE] else df <- df[0, ]
    }
    sh_slug <- slug(sh)
    if (is.null(df) || nrow(df) == 0) {
      md_lines <- c(md_lines, paste0("## ", sh), "", "_(empty sheet)_", "")
      next
    }
    # generic column names A, B, C ... for the raw dump
    names(df) <- make.unique(rep("", ncol(df)))
    names(df) <- paste0("c", seq_len(ncol(df)))

    csv_path <- file.path(sub, paste0(sh_slug, ".csv"))
    write.csv(df, csv_path, row.names = FALSE, na = "")

    md_lines <- c(md_lines,
                  paste0("## ", sh),
                  paste0("_", nrow(df), " rows x ", ncol(df), " cols",
                         " | CSV: `", basename(sub), "/", basename(csv_path), "`_"),
                  "",
                  knitr::kable(md_clean(df), format = "pipe"),
                  "")
  }

  md_path <- file.path(outdir, paste0(slug(stem), ".md"))
  writeLines(md_lines, md_path)
  list(stem = stem, type = "xlsx", sheets = sheets,
       md = md_path, csv_dir = sub)
}

# ---- docx ------------------------------------------------------------------

convert_docx <- function(path, outdir) {
  stem <- tools::file_path_sans_ext(basename(path))
  doc  <- read_docx(path)
  s    <- docx_summary(doc)                 # ordered content (paragraphs + table cells)
  s    <- s[order(s$doc_index), ]
  md_lines <- c(paste0("# ", stem), "",
                paste0("Source: `", basename(path), "`"), "")

  i <- 1
  n <- nrow(s)
  while (i <= n) {
    row <- s[i, ]
    if (row$content_type == "paragraph") {
      txt <- ifelse(is.na(row$text), "", row$text)
      if (nzchar(trimws(txt))) md_lines <- c(md_lines, trimws(txt), "")
      i <- i + 1
    } else if (row$content_type == "table cell") {
      # gather the whole table sharing this doc_index
      di    <- row$doc_index
      block <- s[s$doc_index == di & s$content_type == "table cell", ]
      wide  <- reshape(block[, c("row_id", "cell_id", "text")],
                       idvar = "row_id", timevar = "cell_id",
                       direction = "wide")
      wide  <- wide[order(wide$row_id), , drop = FALSE]
      wide$row_id <- NULL
      names(wide) <- sub("^text\\.", "col", names(wide))
      md_lines <- c(md_lines, knitr::kable(md_clean(wide), format = "pipe"), "")
      i <- i + nrow(block)
    } else {
      i <- i + 1
    }
  }

  md_path <- file.path(outdir, paste0(slug(stem), ".md"))
  writeLines(md_lines, md_path)
  list(stem = stem, type = "docx", sheets = NA, md = md_path, csv_dir = NA)
}

# ---- driver ----------------------------------------------------------------

# incremental: the .md output is our marker. Skip a file when its .md already
# exists and is newer than the source, so re-runs only convert changed/new files.
out_md_path <- function(path, outdir)
  file.path(outdir, paste0(slug(tools::file_path_sans_ext(basename(path))), ".md"))

convert_one <- function(path, outdir, force = FALSE) {
  ext <- tolower(tools::file_ext(path))
  md  <- out_md_path(path, outdir)
  if (!force && file.exists(md) &&
      file.mtime(md) >= file.mtime(path)) {
    message("up to date, skipping: ", basename(path))
    sheets <- if (ext %in% c("xlsx", "xls"))
      tryCatch(excel_sheets(path), error = function(e) NA) else NA
    return(list(stem = tools::file_path_sans_ext(basename(path)),
                type = if (ext == "docx") "docx" else "xlsx",
                sheets = sheets, md = md, csv_dir = NA, skipped = TRUE))
  }
  message("converting: ", basename(path))
  tryCatch({
    if (ext %in% c("xlsx", "xls"))      convert_xlsx(path, outdir)
    else if (ext == "docx")             convert_docx(path, outdir)
    else { message("  skipped (unsupported): ", ext); NULL }
  }, error = function(e) { message("  ERROR: ", e$message); NULL })
}

main <- function() {
  args   <- commandArgs(trailingOnly = TRUE)
  force  <- "--force" %in% args          # reconvert everything, ignore timestamps
  args   <- setdiff(args, "--force")
  input  <- if (length(args) >= 1) args[[1]] else if (dir.exists("raw")) "raw" else "."
  outdir <- if (length(args) >= 2) args[[2]] else "converted"
  dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

  files <- if (dir.exists(input)) {
    list.files(input, pattern = "\\.(xlsx|xls|docx)$",
               full.names = TRUE, ignore.case = TRUE, recursive = TRUE)
  } else input

  results <- Filter(Negate(is.null), lapply(files, convert_one, outdir = outdir, force = force))
  nskip <- sum(vapply(results, function(r) isTRUE(r$skipped), logical(1)))
  if (nskip) message(nskip, " file(s) already up to date (use --force to redo).")

  # build an INDEX.md so there is a single place to start browsing
  idx <- c("# Converted files — index", "",
           paste0("_Generated ", as.character(Sys.Date()),
                  " from ", length(results), " file(s)._"), "")
  for (r in results) {
    if (identical(r$type, "xlsx")) {
      idx <- c(idx, paste0("- **", r$stem, "** (xlsx) — [",
                           basename(r$md), "](", basename(r$md), ")"),
               paste0("  - sheets: ", paste(r$sheets, collapse = ", ")))
    } else {
      idx <- c(idx, paste0("- **", r$stem, "** (docx) — [",
                           basename(r$md), "](", basename(r$md), ")"))
    }
  }
  writeLines(idx, file.path(outdir, "INDEX.md"))
  message("\nDone. ", length(results), " file(s) -> ", outdir,
          "/  (start at INDEX.md)")
}

main()
