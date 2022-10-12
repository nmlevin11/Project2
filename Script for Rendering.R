#Code for rendering file 
rmarkdown::render("Project2_Group_L._updated.Rmd", 
                  output_file = "README.md",
                  output_format = "github_document",
                  output_options = list(toc=TRUE,toc_depth=1,toc_float=TRUE))

