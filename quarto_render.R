q_render_ <- function(input, package_dir, minimal_eval, ...) {
  # read input file
  input_str <- readLines(input)
  to_inject <- glue::glue(
    '
  ```{r, include=FALSE}
  litr:::setup("{{package_dir}}", {{minimal_eval}})
  ```
  ',
    .open = "{{",
    .close = "}}"
  )
  
  # inject litr::setup code after the 2nd --- of the YAML front matter if it's there
  yaml_marks <- which(input_str == "---")
  if (length(yaml_marks) >= 2) {
    yaml_end_idx <- which(input_str == "---")[2]
    changed_str <- c(input_str[1:yaml_end_idx], 
                     to_inject, 
                     input_str[(yaml_end_idx + 1):length(input_str)])
  } else {
    stop(litr:::make_noticeable('add yaml header that includes at least the following:
---
params:
  package_name: "{package_dir}"
  package_parent_dir: "."
---'))
  }
  
  # create a tmp directory and copy everything from current directory into it
  # - we want all files there since if it's a book there may be multiple qmd files and other resources; also if the R package already exists we want the litr-hash checks to compare against it
  # - we want to do everything in a new directory rather than modifying the name of the input file so that the html files will have the correct path
  tmpdir <- tempfile()
  fs::dir_create(tmpdir)
  cat(tmpdir)
  fs::dir_copy(".", tmpdir)
  # delete hidden files:
  fs::file_delete(list.files(all.files=TRUE, pattern = "^\\.[^\\.]"))

  # write modified code to file in there
  tmp_file <- file.path(tmpdir, input)
  writeLines(changed_str, tmp_file)

  # pass tmp file to quarto_render
  out <- quarto::quarto_render(tmp_file, ...)
  # clean up files after quarto finishes rendering
  # remove the modified qmd file and then move the output out of tmpdir
  fs::file_delete(tmp_file)
  fs::dir_copy(tmpdir, ".", overwrite = TRUE)
  fs::dir_delete(tmpdir)
  
  # add to DESCRIPTION file the version of litr used to create package:
  litr:::write_version_to_description(package_dir)
  
  # add litr hash so we can tell later if package files were manually edited:
  litr:::write_hash_to_description(package_dir)
}
