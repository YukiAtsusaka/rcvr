# tests/test_read_cvr.R
# Run with: testthat::test_file("tests/test_read_cvr.R")
#       or: source("tests/test_read_cvr.R")

library(testthat)
library(readr)
library(dplyr)

source("R/read_cvr.R")

# ── Shared fixtures ──────────────────────────────────────────────────────────

# Minimal identifiers table:
#   row 1 – matched file with a real election_id
#   row 2 – matched file whose election_id is NA
ids <- tibble::tribble(
  ~source_file,                    ~year, ~election_type, ~prm_party, ~juris,       ~state, ~office,       ~dist,       ~election_id,
  "minneapolis_2021_mayor.csv",    2021L, "general",       NA,        "Minneapolis", "MN",  "Mayor",       "At_Large",  "MN_2021_G_Minneapolis_At_Large_Mayor",
  "Alaska_20221108_HouseDistrict1.tab", NA_integer_, "general", NA,  "Alaska",      "AK",  NA_character_, "1",         NA_character_
)

# Minimal CVR data (two ballot rows)
cvr_rows <- tibble::tibble(
  ballot_id  = c(1L, 2L),
  first      = c("CandidateA", "CandidateB"),
  second     = c("CandidateB", NA_character_)
)

write_ids  <- function(path) write_csv(ids, path)
write_cvr  <- function(path) write_csv(cvr_rows, path)

# ── Tests ────────────────────────────────────────────────────────────────────

test_that("election_id is correct when source_file matches with a non-NA id", {
  tmp_dir  <- tempdir()
  cvr_path <- file.path(tmp_dir, "minneapolis_2021_mayor.csv")
  id_path  <- file.path(tmp_dir, "cvr_identifiers.csv")

  write_cvr(cvr_path)
  write_ids(id_path)

  result <- read_cvr(cvr_path, identifiers = id_path)

  expect_equal(result$election_id, rep("MN_2021_G_Minneapolis_At_Large_Mayor", 2))
})

test_that("election_id is NA when source_file matches but identifier is NA", {
  tmp_dir  <- tempdir()
  cvr_path <- file.path(tmp_dir, "Alaska_20221108_HouseDistrict1.tab")
  id_path  <- file.path(tmp_dir, "cvr_identifiers.csv")

  write_cvr(cvr_path)
  write_ids(id_path)

  result <- read_cvr(cvr_path, identifiers = id_path)

  expect_true(all(is.na(result$election_id)))
})

test_that("election_id is NA when file is not in identifiers", {
  tmp_dir  <- tempdir()
  cvr_path <- file.path(tmp_dir, "unknown_election.csv")
  id_path  <- file.path(tmp_dir, "cvr_identifiers.csv")

  write_cvr(cvr_path)
  write_ids(id_path)

  result <- read_cvr(cvr_path, identifiers = id_path)

  expect_true(all(is.na(result$election_id)))
})

test_that("election_id is the first column", {
  tmp_dir  <- tempdir()
  cvr_path <- file.path(tmp_dir, "minneapolis_2021_mayor.csv")
  id_path  <- file.path(tmp_dir, "cvr_identifiers.csv")

  write_cvr(cvr_path)
  write_ids(id_path)

  result <- read_cvr(cvr_path, identifiers = id_path)

  expect_equal(names(result)[1], "election_id")
})

test_that("all original CVR columns are retained", {
  tmp_dir  <- tempdir()
  cvr_path <- file.path(tmp_dir, "minneapolis_2021_mayor.csv")
  id_path  <- file.path(tmp_dir, "cvr_identifiers.csv")

  write_cvr(cvr_path)
  write_ids(id_path)

  result <- read_cvr(cvr_path, identifiers = id_path)

  expect_true(all(c("ballot_id", "first", "second") %in% names(result)))
})

test_that("all original CVR rows are retained", {
  tmp_dir  <- tempdir()
  cvr_path <- file.path(tmp_dir, "minneapolis_2021_mayor.csv")
  id_path  <- file.path(tmp_dir, "cvr_identifiers.csv")

  write_cvr(cvr_path)
  write_ids(id_path)

  result <- read_cvr(cvr_path, identifiers = id_path)

  expect_equal(nrow(result), nrow(cvr_rows))
})

test_that("directory prefix in file path is ignored for lookup", {
  tmp_dir  <- tempdir()
  sub_dir  <- file.path(tmp_dir, "subdir")
  dir.create(sub_dir, showWarnings = FALSE)

  cvr_path <- file.path(sub_dir, "minneapolis_2021_mayor.csv")
  id_path  <- file.path(tmp_dir, "cvr_identifiers.csv")

  write_cvr(cvr_path)
  write_ids(id_path)

  result <- read_cvr(cvr_path, identifiers = id_path)

  expect_equal(result$election_id[1], "MN_2021_G_Minneapolis_At_Large_Mayor")
})
