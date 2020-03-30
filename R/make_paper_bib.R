#' Make .bib files for papers
#'
#' This function will create .bib files in the Hugo Academic style for papers
#' specified in a Google Sheet.
#'
#' @param dir The directory you would like the markdown documents to be saved in
#' @param sheet_id Your Google Sheet id
#' @export
make_paper_bib <- function(sheet_id, dir = "./static/files/citations") {


  googlesheets4::sheets_deauth()
  d <- googlesheets4::read_sheet(sheet_id)

  bib <- d$bib

  file_base <- purrr::map_chr(d$bib, ~ strsplit(.x, '[{,]')[[1]][2])
  file_name <- glue::glue("{dir}/{file_base}.bib")

  if(!file.exists(dir)){
    stop("Specified directory (",dir,") does not exist")
  }
  purrr::walk2(bib, file_name, writeLines)
}
