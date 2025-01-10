# To test in regular R session:
# shiny::shinyAppDir("apps/src/cvd-emulator")
#
# To deploy:
# shinylive::export("apps/src/cvd-emulator", "apps/bin/cvd-emulator")
# system("cp apps/src/cvd-emulator/*.css apps/bin/cvd-emulator/")
# system("cp -r apps/src/cvd-emulator/images apps/bin/cvd-emulator/")

#library(shiny)
#library(shinyjs)
library(colorspace)


# Define server logic to read selected file ----
server <- shinyServer(function(input, output, session) {
  
  
  values <- shiny::reactiveValues(emulated = NULL)
  
  output$file_info <- shiny::renderText(paste("Choose an image (jpg or png) from your computer."))
  
  # ----------------------------------------------------------------
  # Package version information
  # ----------------------------------------------------------------
  output$filebox <- shiny::renderUI({
    shiny::fileInput("file", label=h2("Upload Image"),
                     multiple = FALSE, width = "100%",
                     accept = c("image/png","image/jpeg"))
  })
  
  # ----------------------------------------------------------------
  # Dark mode on/off
  # ----------------------------------------------------------------
  shiny::observeEvent(input$darkmode, {
    if ( ! input$darkmode ) {
      shinyjs::removeClass(selector = "body", class = "darkmode")
    } else {
      shinyjs::addClass(selector = "body", class = "darkmode")
    }
  })
  
  # ----------------------------------------------------------------
  # Status observer
  # ----------------------------------------------------------------
  shiny::observe({
    output$status <- shiny::renderText({paste("Please upload an image first.",
                                              "The conversion can take a few seconds, so please be patient.")})
    for ( type in c("orig","desaturate","deutan","protan","tritan") ) showInit(type,output)
  })
  
  
  # ----------------------------------------------------------------
  # Keyboard key bindings
  # ----------------------------------------------------------------
  shiny::observe({
    if ( is.null(input$key_pressed) ) return()
    # Switch a/s/d/f/g
    if        ( input$key_pressed == 65 ) { # a
      selected = "Original"
    } else if ( input$key_pressed == 83 ) { # s
      selected = "Desaturated"
    } else if ( input$key_pressed == 68 ) { # d
      selected = "Deuteranope"
    } else if ( input$key_pressed == 70 ) { # f
      selected = "Protanope"
    } else if ( input$key_pressed == 71 ) { # g
      selected = "Tritanope"
    } else if ( input$key_pressed == 72 ) { # h
      selected = "All"
    } else { return() }
    
    shiny::updateTabsetPanel(session, "maintabs", selected = selected)
  })
  
  shiny::observe({ if ( !is.null(input$file) ) values$emulated <- NULL })
  
  # ----------------------------------------------------------------
  # File observer: does basically everything
  # ----------------------------------------------------------------
  shiny::observe({
    
    if ( !is.null(values$emulated) ) return(FALSE)
    
    # Info
    if ( ! is.null(input$file) ) {
      output$status <- shiny::renderText({"File uploaded, starting conversion ..."})
      shiny::updateTabsetPanel(session, "maintabs", selected = "Original")
      output$filebox <- shiny::renderUI({
        shiny::fileInput("file", label=h2("Upload Image"),
                         multiple = FALSE, width = "100%",
                         accept = c("image/png","image/jpeg"))
      })
    }
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    # Is a data.frame containing one row (multiple file upload
    # is not allowed),
    # Columns: name, size, type (mime), datapath (temporary file location)
    shiny::req(input$file)
    
    ## Identify file type (file suffix)
    file     <- input$file$datapath[1]
    filename <- input$file$name[1]
    is_img   <- colorspace:::check_image_type(filename)
    
    # If is either png or jpeg: go ahead
    if ( is_img$png | is_img$jpg ) { 
      if ( is_img$png ) {
        img <- try(png::readPNG(file))
      } else {
        img <- try(jpeg::readJPEG(file))
      }
      
      # Broken image or no PNG/JPEG
      if ( "try-error" %in% class(img) ) {
        tmp <- paste("Sorry, image could not have been read. Seems that the file you",
                     "uploaded was no png/jpg/jpeg file or a broken file. Please check your file",
                     "and try again!")
        output$status <- shiny::renderText({tmp})
        return(FALSE)
      }
      
      # Resize image if width > 800
      if ( dim(img)[2] > 800 ) img <- resizeImage(img,800)
      ##if ( dim(img)[2] > 800 && Sys.getenv('SHINY_PORT') != "" ) img <- resizeImage(img,800)
      
      # Show uploaded image
      show(file, "orig", output)
      # Convert
      for ( type in c("desaturate", "deutan", "protan", "tritan") )
        show(img, type, output, input$severity/100.)
      
      # Delete uploaded file
      file.remove( file )
      rm(list=c("img","is_img","file","filename"))
      output$status <- shiny::renderText({"Image successfully converted."})
      values$emulated <- TRUE
      
      return(TRUE)
    } else {
      tmp <- paste("Uploaded image has had unknown file name extension!",
                   "Please upload png, jpg or jpeg (not case sensitive).")
      output$status <- shiny::renderText({tmp})
      return(FALSE)
    }
  })
  
}) # End of shinyServer


# Resize the image (neares neighbor interpolation/approximation)
resizeImage = function(im, w.out=600, h.out) {
  
  # Dimensions of the input image 
  din <- list("height"=dim(im)[1],"width"=dim(im)[2],"layers"=dim(im)[3])
  # If h.out is missing: proportional scaling
  if ( missing(h.out) ) h.out <- round(din$height/(din$width/w.out))
  # Create empty array to store resized image
  out = array(0,c(h.out,w.out,din$layers))
  # Compute ratios -- final number of indices is n.out, spaced over range of 1:n.in
  w_ratio = din$width  / w.out
  h_ratio = din$height / h.out
  # Do resizing -- select appropriate indices
  for ( l in 1:din$layers )
    out[,,l] <- im[ floor(h_ratio* 1:h.out), floor(w_ratio* 1:w.out), l]
  
  return(out)
}


# Function which converts the image and displays it in the shiny app
show <- function(img, type, output, severity = 1.0) {
  
  if ( is.character(img) ) {
    tmp <- img
    rm  <- FALSE
  } else {
    # Create temporary file name
    tmp <- tempfile(sprintf("hclconvert_%s",type),fileext=".png")
    # Convert image
    colorspace:::cvd_image(img, type, tmp, severity = severity) 
    rm <- TRUE
  }
  
  # Base64-encode file
  #txt <- RCurl::base64Encode(readBin(tmp, "raw", file.info(tmp)[1, "size"]), "txt")
  txt <- base64enc::base64encode(readBin(tmp, "raw", file.info(tmp)[1, "size"]))
  # Create inline image, save & open html file in browser 
  html <- sprintf('data:image/png;base64,%s', txt)
  # Rendering images
  output[[sprintf("image%s",type)]]     <- shiny::renderUI({ img(src = html) })
  output[[sprintf("all_image%s",type)]] <- shiny::renderUI({ img(src = html) })
  # Delete temporary file
  if ( rm ) file.remove(tmp)
}

# Show images when loading the app (included in images)
showInit <- function( type, output ) {
  output[[sprintf("image%s",type)]]     <- shiny::renderUI({img(src = sprintf("../images/rainbow_%s.png",type))})
  output[[sprintf("all_image%s",type)]] <- shiny::renderUI({img(src = sprintf("../images/rainbow_%s.png",type))})
}




desc <- paste("Please select a file from your disc which you want",
              "to convert. Only PNG/JPG/JPEG files are allowed.")

# Define UI for data upload app ----
ui <- shinyUI(bootstrapPage(
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "../cvdemulator.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "../cvdemulator_darkmode.css")
  ),
  tags$script("
   $(document).ready(function() {
        $(document).on(\"keyup\", function(event) {
            console.log(event.which);
            Shiny.onInputChange(\"key_pressed\", event.which);
            event.stopPropagation();
        });
   });
   "),
  shinyjs::useShinyjs(),
  
  tabsetPanel(id = "maintabs",
              # Upload or control panel
              tabPanel("Upload", icon = icon("cog", lib = "font-awesome"),
                       
                       # Severity slider
                       column(4, class = "col-severity",
                              withTags(div(class = "fa-large",
                                           icon("eye-slash", lib = "font-awesome"))),
                              h2("Severity"),
                              sliderInput("severity", label = NULL, min = 0, max = 100, step = 1, value = 100,
                                          width = "100%"),
                              p(paste("Choose the level of severity of color vision deficiency,",
                                      "where 100% means maximum deficiency and",
                                      "0% means no deficiency at all. This value is",
                                      "applied directly after image upload and cannot be changed afterwards.",
                                      "If you want to check the same image with a different severity level you",
                                      "need to re-upload the image."))
                       ),
                       
                       # Input: file upload
                       column(4, class = "col-file",
                              withTags(div(class = "fa-large", icon("image", lib = "font-awesome"))),
                              uiOutput("filebox"),
                              htmlOutput("file_info") # Will be filled with the file upload element.
                       ),
                       
                       # Status information
                       column(4, class = "col-status",
                              withTags(div(class = "fa-large", icon("spinner", lib = "font-awesome"))),
                              h2("Status"),
                              textOutput("status"),
                              h2("Tip"),
                              p(paste("You can use the keys \"a\", \"s\", \"d\", \"f\", \"g\", \"h\"",
                                      "to navigate trough the different tabs.")),
                              h3("Dark Mode"),
                              checkboxInput("darkmode", "Activate dark mode (check figures on black background).", value = FALSE, width = NULL)
                       )
              ),
              
              
              # Horizontal line ----
              
              tabPanel("Original",
                       withTags(div(class = "img-single",
                                    uiOutput("imageorig"),
                                    h4("Original image as uploaded."))
                       )
              ),
              tabPanel("Desaturated",
                       withTags(div(class = "img-single",
                                    uiOutput("imagedesaturate"),
                                    h4("Reduction in chroma until no color is left (\"total color blindness\").",
                                       "Full desaturation yields to a pure grayscale image (only differences",
                                       "in luminance left).")
                       ))
              ),
              tabPanel("Deuteranope",
                       withTags(div(class = "img-single",
                                    uiOutput("imagedeutan"),
                                    h4(paste("Most common color vision deficiency (6% of males/0.4% of females).",
                                             "Mutated green pigment which yeilds reduced sensitivity of the green",
                                             "area of the spectrum, also known as \"red-green color blindness\"."))
                       ))
              ),
              tabPanel("Protanope",
                       withTags(div(class = "img-single",
                                    uiOutput("imageprotan"),
                                    h4("Mutated red pigment and less able to discriminate colors. Less common",
                                       "than deuteranomaly (1% of males, 0.01% of females). The deficiency also",
                                       "yields a darkening of red colors such that red colors can appear nearly black.")
                       ))
              ),
              tabPanel("Tritanope",
                       withTags(div(class = "img-single",
                                    uiOutput("imagetritan"),
                                    h4("Mutated blue pigment which makes it difficult to distingiush between",
                                       "blueish and greenish hues, as well as between yellowish and reddish hues.",
                                       "tritanomaly is also known as \"blue-yellow color blindness\" but is relatively",
                                       "rare (0.01% for both, males and females).")
                       ))
              ),
              tabPanel("All",
                       withTags(div(class="img-all",
                                    div(class = "img-container", uiOutput("all_imageorig"), h4("Original")),
                                    div(class = "img-container", uiOutput("all_imagedesaturate"), h4("Desaturated")),
                                    div(class = "img-container", uiOutput("all_imagedeutan"), h4("Deuteranope")),
                                    div(class = "img-container", uiOutput("all_imageprotan"), h4("Protanope")),
                                    div(class = "img-container", uiOutput("all_imagetritan"), h4("Tritanope"))
                       ))
              ),
              tabPanel("Info", class = "info-tab", icon = icon("info-circle", lib = "font-awesome"),
                       htmlOutput("appInfo"),
                       includeHTML("info.html")
              ),
              selected = "Upload"
  )
  
)) # End of shinyUI


shinyApp(ui, server)

