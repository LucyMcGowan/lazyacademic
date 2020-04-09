#' Make .bib files for papers
#'
#' This function will create .bib files in the Hugo Academic style for papers
#' specified in an Excel spreadsheet, CSV file, or Google Sheet.
#'
#' @param data The location of the file containing your data. Can be a path to a
#'   local Excel/CSV file, or the URL of a Google Sheet.
#' @param sheet_name Defines the sheet from which to read information on talks.
#'   Default is "talks"
#' @param dir The directory you would like the markdown documents to be saved in
#' @param folder Create each record within it's own folder in the main directory
#'   (TRUE), or simply create the files (FALSE). Default is TRUE
#' @param create Logicial. If the specified directory doesn't exists, should it
#'   be created? Default is TRUE.
#' @export
make_paper_bib <- function(data,
                           dir = "./static/files/citations",
                           sheet_name = "papers",
                           folder = FALSE,
                           create = TRUE) {

  # Get import file type and import data
  if (tools::file_ext(data) %in% c("csv","xlsx", "xls")) {
    d <- rio::import(data, which = sheet_name)
  } else {
    googlesheets4::sheets_deauth()
    d <- googlesheets4::read_sheet(data, sheet = sheet_name)
  }

  # Read bibtex column
  bib <- d$bib

  # file_base <- purrr::map_chr(d$bib, ~ strsplit(.x, '[{,]')[[1]][2])
  # file_name <- glue::glue("{dir}/{file_base}.bib")

  # Check if specified directory exists, if not, create it

  if (dir.exists(dir)) {
    message("Editing directory: ", dir)
  } else {
    if (create == TRUE) {
      message("Creating directory: ", dir)
      dir.create(file.path(dir), recursive = TRUE)
    } else {
      stop("Specified directory doesn't exist: ", dir,
           "\n  Use the `create = TRUE` argument to create the directory.")
    }
  }

  # Create filenames
  file_base <- purrr::map_chr(d$bib, ~ strsplit(.x, '[{,]')[[1]][2])

  if (folder == TRUE) {
    file_folder <- glue::glue("{dir}/{file_base} ")

    for (folder_no in 1:length(file_folder)) {

      if (!dir.exists(file_folder[folder_no])) {
        if (create == TRUE) {
          dir.create(file.path(file_folder[folder_no]), recursive = TRUE)
        } else {
          stop(
            "Specified directory doesn't exist: ",
            file_folder[folder_no],
            "\n  Use the `create = TRUE` argument to create the directory."
          )
        }

      }
    }

    file_name <- glue::glue("{dir}/{file_base}/{file_base}.bib")
  } else {
    file_name <- glue::glue("{dir}/{file_base}.bib")
  }

  # Provide messages about changes
  for (file in 1:length(file_name)) {
    if (file.exists(file_name[file])) {
      message(paste0("Updating existing citation: ", file_base[file]))
    } else {
      message("Creating new citation: ", file_base[file])
    }
  }

  purrr::walk2(bib, file_name, writeLines)
}
