# Collect file names from CVR datasets on Harvard Dataverse

doi <- "doi:10.7910/DVN/AMK8PJ"

url <- paste0(
  "https://dataverse.harvard.edu/api/datasets/:persistentId/",
  "?persistentId=", doi
)

response <- jsonlite::fromJSON(url)

file_names <- response$data$latestVersion$files$label

saveRDS(file_names, "data-raw/cvr_file_names.rds")

cat("Collected", length(file_names), "file names\n")
