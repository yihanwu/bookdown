local({
  knitr_string <- '```{r %s} \n knitr::include_graphics("%s")\n```\n'
  html_string <- '![%s]("%s")\n'
  # rstudio checks
  if(rstudioapi::isAvailable("0.99.1111")) {
    context <- tryCatch(rstudioapi::getSourceEditorContext(), error = function(e) NULL)
  }
  if((exists("context") && is.null(context)) || rstudioapi::isAvailable("0.99.796")) {
    context <- rstudioapi::getActiveDocumentContext()
  } else stop(
    "The use of this addin requires RStudio 0.99.796 or newer (your version is ", rstudioapi::versionInfo()$version, ").")
  # start ui
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Insert Image", left = miniUI::miniTitleBarCancelButton(),
                           right = miniUI::miniTitleBarButton("done", "Done", primary = TRUE)),
    miniUI::miniContentPanel(
      shiny::actionButton("image_select", "Select Image"),
      shiny::actionButton("go", "Insert"),
      shiny::radioButtons("path_type", label = "Path Type", choices = list("Relative to Home", "Absolute", "Relative to Project"), selected = "Relative to Home", inline = T),
      shiny::radioButtons("format", label = "Format", choices = list("knitr", "markdown", "path only"), selected = "knitr", inline = T),
      # doesn't produce the formatted text, has all the line endings and such
      shiny::verbatimTextOutput("text_indicator")
    ) # content panel
  ) # end ui
  server <- function(input, output, session) {
    text_path <- shiny::eventReactive(input$image_select, {
      text <- as.character(rstudioapi::selectFile(filter = "*"))
      text
    })
    file_name <- shiny::eventReactive(text_path(), {
      basename(text_path())
    })
    file_dir <- shiny::eventReactive(text_path(), {
      dirname(text_path())
    })
    project_dir <- shiny::eventReactive(text_path(), {
      rstudioapi::getActiveProject()
    })
    # take out the puncuation that would throw off knitr chunks
    chunk_name <- shiny::reactive({
      chunk_name <- gsub("[^[:alnum:]]+", "-", file_name())
      # replace the possible dash that may occur rarely at the start of the name
      if(substring(gsub("[^[:alnum:]]+", "-", chunk_name)[[1]], 1, 1) == "-"){
        substr(chunk_name, start = 1, stop = 1)<-" "
      }
      chunk_name
    })
    directory <- shiny::reactive({
      if(input$path_type == "Absolute"){
        paste0(file_dir(), "/")
      } else if (input$path_type == "Relative to Home"){
        paste0(gsub(path.expand("~"), "~", file_dir()), "/")
      } else if (input$path_type == "Relative to Project"){
        dir_path <- gsub(rstudioapi::getActiveProject(), "", file_dir())
        # if the file is on the level of the project directoy, append a "." to the path
        if(nchar(dir_path)==0){
          dir_path <- as.character("./")
        } else {
          # don't forget the back slash at the end
          dir_path <- paste0(dir_path, "/")
        }
        if(substr(dir_path, start = 1, stop = 1) == "/"){
          dir_path <- substr(dir_path, start = 2, stop = nchar(dir_path))
        }
        dir_path
      }
    })
    # format the file according to the selected format
    out_text <- shiny::reactive({
      if(input$format == "knitr"){
        snippet <- sprintf(knitr_string, chunk_name(), paste0(directory(), file_name()))
      } else if(input$format == "markdown"){
        snippet <- sprintf(html_string, chunk_name(), paste0(directory(), file_name()))
      } else if(input$format == "path only"){
        snippet <- paste0(directory(), file_name())
      }
      snippet
    })
    # preview the code snippet
    output$text_indicator<-shiny::renderPrint({
      #chunk_name()
      #file_name()
      #paste0(directory(), "/")
      #as.character(rstudioapi::getActiveProject())
      #project_dir()
      out_text()
      #text_path()
      #knitr_string
    })
    # actually insert into editor
    shiny::observeEvent(input$go, {
      rstudioapi::insertText(text = out_text(), id = context$id)
    })
    # stop app, different than putting it under go, may insert multiple
    shiny::observeEvent(input$done, {
      shiny::stopApp()
    })
    
  }
  viewer <- shiny::dialogViewer("Add image to markdown", height = 300)
  shiny::runGadget(ui, server, viewer = viewer)
})



