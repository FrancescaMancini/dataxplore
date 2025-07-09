# Helper function to detect markdown function depending on whether app is hosted on Posit or we are developing using devtools
get_markdown_path <- function(file_name, dev){

if (dev){

    file_path = file.path("inst/rmd", file_name)
} else{

    file_path = system.file("rmd", file_name, package = "dataxplore")
}

return(file_path)
}