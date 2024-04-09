q_render_ <- function(input, package_dir, minimal_eval, ...) {
  # read input file
  input_str <- readLines(input)
  to_inject <- glue::glue(
    '
  ```{r, echo=FALSE, message=FALSE}
  litr:::setup("{{package_dir}}", {{minimal_eval}})
  ```
  ',
    .open = "{{",
    .close = "}}"
  )
  
  # inject litr::setup code after the 2nd --- of the YAML front matter
  yaml_end_idx <- which(input_str == "---")[2]
  changed_str <-
    c(input_str[1:yaml_end_idx], to_inject, input_str[(yaml_end_idx + 1):length(input_str)])
  # write modified code to a tmp file
  tmp_file <- "litr_render_tmp.qmd"
  writeLines(changed_str, tmp_file)
  # pass tmp file to quarto_render
  out <- quarto::quarto_render(tmp_file, ...)
  # clean up files after quarto finishes rendering
  # rename the output file to match the name of the original file
  fs::file_delete(tmp_file)
  output_file_base_name <- fs::path_ext_remove(tmp_file)
  input_file_base_name <- fs::path_ext_remove(input)
  # This assumes the output is always an html file, will need to do something different for bookdown
  fs::file_move(paste0(output_file_base_name, ".html"),
                paste0(input_file_base_name, ".html"))
  # rename the files directory as well
  # apparently there isn't a built in command to do this in R?
  system(paste(
    "mv",
    paste0(output_file_base_name, "_files"),
    paste0(input_file_base_name, "_files")
  ))
}
