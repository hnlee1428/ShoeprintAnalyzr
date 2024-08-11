# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.

library(tidyverse)
library(imager)
library(shiny)
library(shinyjs)
library(DT)
library(spatstat)
library(sp)
library(sf)
library(raster)
library(fasterize)
library(magick)
library(lattice)
library(gridExtra)
library(xlsx)
library(shinycssloaders)
library(imagefx)
library(zip)

## Uncomment (remove the heading '#'s) the below code (install.packages ~ "xlsx"))to install the following packages; 
## copy and paste it to R console and hit Enter for installation.
# install.packages(c("tidyverse",
#                    "imager",
#                    "shiny",
#                    "shinyjs",
#                    "DT",
#                    "spatstat",
#                    "sp",
#                    "sf",
#                    "raster",
#                    "fasterize",
#                    "magick",
#                    "lattice",
#                    "gridExtra",
#                    "shinycssloaders",
#                    "imagefx",
#                    "zip",
#                    "xlsx"))



## NOTE 1
## When you see an error as follows:
## Error: package or namespace load failed for ‘xlsx’:
##   .onLoad failed in loadNamespace() for 'rJava', details:
##   call: fun(libname, pkgname)
## error: JAVA_HOME cannot be determined from the Registry,
## Install Java on your computer, restart R, and then install the package 'xlsx' by running 'install.packages("xlsx") in R console. 
## To install Java, please see the instruction here: https://www.java.com/en/download/help/download_options.html


## NOTE 2
## When you see an error like "Error in : vector memory limit of 18.0 Gb reached, see mem.maxVSize()":
## Run "usethis::edit_r_environ()" in R console, and then a tab opens up in R script panel.
## Add "R_MAX_VSIZE=50Gb" to the first line of the tab and save it, which increases the memory limit to 50 Gb including physical and virtual memory.
## Restart R and run Shiny app again.

# Allow to handle upto 100 MB
options(shiny.maxRequestSize = 100 * 1024^2)

# To create a temporary directory 
temp_directory <- file.path(tempdir(), as.integer(Sys.Date()))
dir.create(temp_directory)

# User Interface for application
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
  .column_w_bar {
      border-right-color: #B2BEB5;
      border-right-width: 2px;
      border-right-style: dotted;
}",
                    ".irs-grid-text {font-size: 12px}",
                    'body {
      font-size: 17px;
   }',
                    "
             .btn-file {
             color: #003A70;
             background-color:#fff; 
             font-size: 15px; 
             }

             .progress-bar {
             background-color: #40B4E5;
             }

             "))),
  tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #40B4E5; font-size: 12px}")),
  tags$style(HTML(".js-irs-1 .irs-to,.js-irs-1 .irs-from, .js-irs-1 .irs-bar {background: #40B4E5}; font-size: 12px")),
  tags$style(HTML(".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .irs-bar {background: #40B4E5}; font-size: 12px")),
  tags$style(HTML(".js-irs-3 .irs-single, .js-irs-3 .irs-bar-edge, .js-irs-3 .irs-bar {background: #40B4E5}; font-size: 12px")),
  tags$style(HTML(".js-irs-4 .irs-single, .js-irs-4 .irs-bar-edge, .js-irs-4 .irs-bar {background: #40B4E5}; font-size: 12px")),
  tags$style(HTML(".js-irs-5 .irs-single, .js-irs-5 .irs-bar-edge, .js-irs-5 .irs-bar {background: #40B4E5}; font-size: 12px")),
  fluidRow(
    column(12, align = "center",
           h2("Welcome to Shoeprintanalyzr!"))
  ),
  fluidRow(
    column(12, class = 'column_w_bar',
           fluidRow(
             column(6, align = "center",
                    h3(icon("shoe-prints"), "Questioned impression", icon("shoe-prints"), 
                       style="color: #fff; background-color: rgba(255, 0, 0, 0.8); border-color: #fff")),
             column(6, align = "center",
                    h3(icon("shoe-prints"), "Reference impession", icon("shoe-prints"), 
                       style="color: #fff; background-color: rgba(0, 0, 255, 0.8); border-color: #fff"))
           ),
           fluidRow(
             column(3,align = "left", class = 'column_w_bar',
                    fileInput(inputId = "file2",
                              label = tags$span(icon("upload"), "Upload a shoeprint (png, jpg, tiff)"), 
                              accept = c(".png", ".jpg", ".tiff")),
                    div(fileInput(inputId = "file2_supp", 
                                  label = tags$span(icon("upload"), "Optional: processing info (txt)"), 
                                  accept = c(".txt")), style="color: #555759;"),
                    numericInput(inputId = "loaded2_zoom_percent", 
                                 label = tags$span(
                                   icon("magnifying-glass"), "Image Viewing Scale (%)", 
                                   tags$i(class = "glyphicon glyphicon-question-sign", 
                                          style = "color:#0072B2;",
                                          title = "To expedite image processing, the image on the right is displayed at the reduced scale you've chosen. Larger values may extend processing time but reduce discrepancies in your processed image when viewed at its original scale. min = 1%, max = 100%.")), 
                                 min = 1, max = 100, value = 20),
                    helpText("Note: Viweing scale should be set prior to cropping.
                             If you need to change the scale while/after cropping, update the scale, and then reset/redo cropping."),
                    hr(),
                    h3(icon("tools"), "Image Processing Tools", style="color: #fff; background-color: #555759"),
                    p(tags$span(
                      strong("Crop the shoeprint portion"), 
                      tags$i(class = "glyphicon glyphicon-question-sign", 
                             style = "color:#0072B2;",
                             title = "To select the outsole portion only and crop out the background, follow these steps:
1. Click on 'START'.
2. Place boundary points by clicking on the image—your points will be displayed in blue.
3. Click on 'END' once the boundary is set—your points will turn red.

You can use the 'PAUSE', 'BACKWARD', and 'UNDO' buttons to pause point collection, delete the last point, and delete all points (or undo the cropping process), respectively."))),
                    actionButton("selectForeground2", "START", 
                                 style="color: #fff; background-color: #003A70; border-color: #003A70; font-size:15px"),
                    actionButton("pauseCropping2", "", icon("pause"),
                                 style="color: #003A70; background-color: #fff; border-color: #fff; font-size:15px"),
                    actionButton("revertCropping2", "", icon("backward-step"),
                                 style="color: #003A70; background-color: #fff; border-color: #fff; font-size:15px"),
                    actionButton("resetCropping2", "", icon("arrow-rotate-left"),
                                 style="color: #003A70; background-color: #fff; border-color: #fff; font-size:15px"),
                    actionButton("cropBackground2", "END", 
                                 style="color: #fff; background-color: #003A70; border-color: #003A70; font-size:15px"),
                    br(),
                    checkboxInput(inputId = "makeitgray2",
                                  label = tags$span(
                                    strong("Grayscale"), 
                                    tags$i(class = "glyphicon glyphicon-question-sign", 
                                           style = "color:#0072B2;",
                                           title = "Image is converted to grayscale.")),
                                  value = FALSE),
                    checkboxInput(inputId = "preinvert2",
                                  label = tags$span(
                                    strong("Invert"), " (before thresholding)", 
                                    tags$i(class = "glyphicon glyphicon-question-sign", 
                                           style = "color:#0072B2;",
                                           title = "Inversion is to transform darker pixels to brighter and brighter pixels to darker. The inversion process is applied to the grayscale version of your image before the thresholding is applied.")),
                                  value = FALSE),
                    sliderInput(inputId = "Threshold2", 
                                label = tags$span(
                                  "Threshold", 
                                  tags$i(class = "glyphicon glyphicon-question-sign", 
                                         style = "color:#0072B2;",
                                         title = "Any values outside the selected range are set to white where 0 indicates the darkest intensity (black) and 1 indicates the brightest intensity (white).")),
                                min = 0, max = 1, value = c(0, 1)),
                    actionButton("resetall2", "Reset grayscale, inversion, and thresholding",
                                 style="color: #fff; background-color: #003A70; border-color: #003A70; font-size:15px"),
                    br(), br(), 
                    p(tags$span(
                      strong("Threshold Aid Plot"), 
                      tags$i(class = "glyphicon glyphicon-question-sign", 
                             style = "color:#0072B2;",
                             title = "Choose a region to examine pixel intensities by dragging on the GRAYSCALE image on the right. Following this, a level plot will pop up to depict the intensity values across the selected area."))),
                    plotOutput("image_brush_info2")
             ),
             
             
             column(3, align = "left", class = 'column_w_bar',
                    br(), br(),
                    shinycssloaders::withSpinner(verbatimTextOutput("loaded2_info"), proxy.height = "50px"),
                    plotOutput("plot2",
                               dblclick = "coord_questioned",
                               click = "plot2_click",
                               brush = brushOpts(id = "plot2_brush",
                                                 resetOnNew = TRUE),
                               height = "700px")),
             column(3,align = "left", class = 'column_w_bar', 
                    fileInput(inputId = "file1", 
                              label = tags$span(icon("upload"), "Upload a shoeprint (png, jpg, tiff)"), 
                              accept = c(".png", ".jpg", ".tiff")),
                    div(fileInput(inputId = "file1_supp", 
                                  label = tags$span(icon("upload"), "Optional: processing info (txt)"), 
                                  accept = c(".txt")), style="color: #555759;"),
                    numericInput(inputId = "loaded1_zoom_percent", 
                                 label = tags$span(
                                   icon("magnifying-glass"), "Image Viewing Scale (%)", 
                                   tags$i(class = "glyphicon glyphicon-question-sign", 
                                          style = "color:#0072B2;",
                                          title = "To expedite image processing, the image on the right is displayed at the reduced scale you've chosen. Larger values may extend processing time but reduce discrepancies in your processed image when viewed at its original scale. min = 1%, max = 100%.")), 
                                 min = 1, max = 100, value = 20),
                    helpText("Note: Viweing scale should be set prior to cropping.
                             If you need to change the scale while/after cropping, update the scale, and then reset/redo cropping."),
                    hr(),
                    h3(icon("tools"), "Image Processing Tools", style="color: #fff; background-color: #555759"),
                    p(tags$span(
                      strong("Crop the shoeprint portion"), 
                      tags$i(class = "glyphicon glyphicon-question-sign", 
                             style = "color:#0072B2;",
                             title = "To select the outsole portion only and crop out the background, follow these steps:
1. Click on 'START'.
2. Place boundary points by clicking on the image—your points will be displayed in blue.
3. Click on 'END' once the boundary is set—your points will turn red.

You can use the 'PAUSE', 'BACKWARD', and 'UNDO' buttons to pause point collection, delete the last point, and delete all points (or undo the cropping process), respectively."))),
                    actionButton("selectForeground", "START", 
                                 style="color: #fff; background-color: #003A70; border-color: #003A70; font-size:15px"),
                    actionButton("pauseCropping", "", icon("pause"),
                                 style="color: #003A70; background-color: #fff; border-color: #fff; font-size:15px"),
                    actionButton("revertCropping", "", icon("backward-step"),
                                 style="color: #003A70; background-color: #fff; border-color: #fff; font-size:15px"),
                    actionButton("resetCropping", "", icon("arrow-rotate-left"),
                                 style="color: #003A70; background-color: #fff; border-color: #fff; font-size:15px"),
                    actionButton("cropBackground", "END", 
                                 style="color: #fff; background-color: #003A70; border-color: #003A70; font-size:15px"),
                    checkboxInput(inputId = "makeitgray",
                                  label = tags$span(
                                    strong("Grayscale"), 
                                    tags$i(class = "glyphicon glyphicon-question-sign", 
                                           style = "color:#0072B2;",
                                           title = "Image is converted to grayscale.")),
                                  value = FALSE),
                    checkboxInput(inputId = "preinvert",
                                  label = tags$span(
                                    strong("Invert"), " (before thresholding)", 
                                    tags$i(class = "glyphicon glyphicon-question-sign", 
                                           style = "color:#0072B2;",
                                           title = "Inversion is to transform darker pixels to brighter and brighter pixels to darker. The inversion process is applied to the grayscale version of your image before the thresholding is applied.")),
                                  value = FALSE),
                    sliderInput(inputId = "Threshold1", 
                                label = tags$span(
                                  "Threshold", 
                                  tags$i(class = "glyphicon glyphicon-question-sign", 
                                         style = "color:#0072B2;",
                                         title = "Any values outside the selected range are set to white where 0 indicates the darkest intensity (black) and 1 indicates the brightest intensity (white).")),
                                min = 0, max = 1, value = c(0, 1)),
                    actionButton("resetall", "Reset grayscale, inversion, and thresholding",
                                 style="color: #fff; background-color: #003A70; border-color: #003A70; font-size:15px"),
                    br(), br(),
                    p(tags$span(
                      strong("Threshold Aid Plot"), 
                      tags$i(class = "glyphicon glyphicon-question-sign", 
                             style = "color:#0072B2;",
                             title = "Choose a region to examine pixel intensities by dragging on the GRAYSCALE image on the right. Following this, a level plot will pop up to depict the intensity values across the selected area."))),
                    plotOutput("image_brush_info1")
             ),
             column(3, align = "left", class = 'column_w_bar',
                    br(), br(),
                    shinycssloaders::withSpinner(verbatimTextOutput("loaded1_info"), proxy.height = "50px"),
                    plotOutput("plot1",
                               dblclick = "coord_ref",
                               click = "plot1_click",
                               #hover = hoverOpts(id = "plot1_hover", delayType = "throttle"),
                               brush = brushOpts(id = "plot1_brush",
                                                 resetOnNew = TRUE),
                               height = "700px")
             )),
           fluidRow(hr()))
    
  ),
  fluidRow(
    column(12, align = "center",
           h3(icon("gears"), "Parameters for Alignment Computation",
              style="color: #fff; background-color: #555759"))
  ),
  fluidRow(column(3, align = "center", 
                  sliderInput(inputId = "Angle_range", 
                              label = "Range of rotation angle to search", 
                              min = -45, max = 45, value = c(-30, 30))),
           column(3, align = "left", 
                  helpText("Note: An optimal rotation angle for the questioned image is sought within the selected range. 
                           Shortening the range could result in a shorter duration.")),
           column(2, align = "center",
                  selectInput(inputId = "Entered_scale_down_factor",
                              label = "Downscale factor",
                              choices = list("1/1", "1/2", "1/4", "1/8", "1/16", "1/32"),
                              selected = "1/8")),
           column(4, align = "left", 
                  helpText("Note: Larger values can improve alignment at the cost of increased processing time, 
                           whereas smaller values speed up the process but may compromise alignment."))),
  fluidRow(
    column(6, align = "center"),
    column(6, align = "center",
           verbatimTextOutput("downscale_info"))
  ),
  fluidRow(hr()),
  fluidRow(column(12, align = "center",
                  actionButton("submit", icon("flag-checkered"), label = "Process automated alignment!",
                               style="color: #fff; background-color: #CF0A2C; border-color: #CF0A2C; font-size: 19px"))),
  #actionButton(inputId = "ok", label = "Stop computation"), verbatimTextOutput("ok_output"),
  hr(),
  fluidRow(
    column(12, align = "center",
           h3("Alignment results from processed images",
              style="color: #fff; background-color: #555759"))
  ),
  fluidRow(
    
    column(3, class = 'column_w_bar',
           h4(icon("circle-info"), "Automated alignment information"),
           shinycssloaders::withSpinner(textOutput(outputId = "transform_information"), proxy.height = "50px"),
           br(),br(),br(),br(),br(),br(),
           h4(icon("square-poll-horizontal"), "Similarity between aligned images"),
           shinycssloaders::withSpinner(textOutput(outputId = "similarity_information"), proxy.height = "50px"),
           br(),br(),br(),br(),br(),br()
           # br(),br(),
           # h4("Adjust the rotation and translation of the aligned questioned image if needed."),
           # sliderInput(inputId = "Rotate", 
           #             label = "Rotation angle", 
           #             min = -5, max = 5, value = 0),
           # sliderInput(inputId = "x_shift", 
           #             label = "Horizontal shift", 
           #             min = -100, max = 100, value = 0),
           # sliderInput(inputId = "y_shift", 
           #             label = "Vertical shift", 
           #             min = -100, max = 100, value = 0),
           # actionButton("resetpostadjust", "Reset the post-adjustment",
           #              style="color: #fff; background-color: #003A70; border-color: #003A70; font-size:15px")
    ),
    column(3, align = "center",class = 'column_w_bar',
           h4(tags$span(
             icon("image"), "Questioned (aligned)", 
             tags$i(class = "glyphicon glyphicon-question-sign", 
                    style = "color:#0072B2;",
                    title = "Your pre-processed questioned impression is aligned with the pre-processed reference and displayed, with pixel intensity represented by orange colors."))),
           shinycssloaders::withSpinner(plotOutput("plot5",
                                                   height = "700px"), proxy.height = "50px")),
    column(3, align = "center", class = 'column_w_bar',
           h4(tags$span(
             icon("image"), "Reference", 
             tags$i(class = "glyphicon glyphicon-question-sign", 
                    style = "color:#0072B2;",
                    title = "Your pre-processed reference impression is displayed, with pixel intensity represented by blue colors."))),
           shinycssloaders::withSpinner(plotOutput("plot4",
                                                   height = "700px"), proxy.height = "50px")),
    column(3, align = "center",
           h4(tags$span(
             icon("image"), "Overlay (aligned)", 
             tags$i(class = "glyphicon glyphicon-question-sign", 
                    style = "color:#0072B2;",
                    title = "Overlay of pre-processed reference (blue) and aligned questioned (orange) impressions is displayed."))),
           shinycssloaders::withSpinner(plotOutput("plot6",
                                                   height = "700px"), proxy.height = "50px"))),
  hr(),
  fluidRow(
    column(3, align = "center",
           h3(icon("download"), "Download all results",
              style="color: #fff; background-color: #555759"),
           downloadButton(outputId = "download_btn", label = "Download", icon = icon("file-download"),
                          style="color: #fff; background-color: #CF0A2C; border-color: #CF0A2C; font-size:18px")
    ),
    column(9, align = "center",
           h3("Original images and their overlay after alignment",
              style="color: #fff; background-color: #555759"))
  ),
  fluidRow(
    column(3, align = "center"),
    column(3, align = "center",class = 'column_w_bar',
           h4(tags$span(
             icon("image"), "Questioned",
             tags$i(class = "glyphicon glyphicon-question-sign", 
                    style = "color:#0072B2;",
                    title = "Your original questioned impression is displayed."))),
           shinycssloaders::withSpinner(plotOutput("plot8",
                                                   height = "700px"), proxy.height = "50px")),
    column(3, align = "center",class = 'column_w_bar',
           h4(tags$span(
             icon("image"), "Reference", 
             tags$i(class = "glyphicon glyphicon-question-sign", 
                    style = "color:#0072B2;",
                    title = "Your original reference impression is displayed."))),
           shinycssloaders::withSpinner(plotOutput("plot7",
                                                   height = "700px"), proxy.height = "50px")),
    column(3, align = "center",class = 'column_w_bar',
           h4(tags$span(
             icon("image"), "Overlay (aligned)", 
             tags$i(class = "glyphicon glyphicon-question-sign", 
                    style = "color:#0072B2;",
                    title = "Overlay of original reference and aligned questioned impressions is displayed."))),
           shinycssloaders::withSpinner(plotOutput("plot9",
                                                   height = "700px"), proxy.height = "50px"))),
  hr()
  
)


# Compute the MD5 hash of the character string as the unique ID
generate_unique_id <- function(char_string) {
  hash <- digest::digest(char_string, algo = "md5", serialize = FALSE)
  return(hash)
}

# Check OS
get_os <- function() {
  if (grepl("darwin", version$os)) {
    return("mac64")
  } else if (grepl("linux", version$os)) {
    return("lin64")
  } else {
    return("win64")
  }
}

# Several functions used in server 
wrapper_f <- function(ref_input, questioned_input, 
                      ref_org, questioned_org,
                      scale_down_factor = 1, 
                      set.angles = seq(-30, 30, by = 0.5), 
                      padding_before_rotation = FALSE){
  
  
  progress <- shiny::Progress$new()
  # Make sure it closes when we exit this reactive, even if there's an error
  on.exit(progress$close())
  s_time <- Sys.time()
  
  progress$set(message = "Alignment in progress", detail = "processing prints in original scale and matching size..", value = 0.1)
  
  ref_org <- magick2cimg(ref_org); questioned_org <- magick2cimg(questioned_org)
  
  
  ref_input <- magick2cimg(ref_input) %>% 
    rm.alpha() %>% 
    grayscale()
  questioned_input <- magick2cimg(questioned_input) %>% 
    rm.alpha() %>% 
    grayscale()
  
  ### Image size to ensure no cropping after maximum rotation
  if(dim(ref_org)[4]>1){
    white1.r <- "white"
  }else{
    white1.r <- 1
  }
  if(dim(questioned_org)[4]>1){
    white1.q <- "white"
  }else{
    white1.q <- 1
  }
  # 
  # if(padding_before_rotation == TRUE){
  #   d1 <- dim(ref_input)
  #   d2 <- dim(questioned_input)
  #   
  #   max.angle <- max(abs(set.angles))*pi/180
  #   
  #   max.r.w <- as.integer(cos(max.angle) * (d1[1] + 5) + cos(pi/2-max.angle) * (d1[2] + 5)) - d1[1]
  #   if(mod(max.r.w, 2)>0){max.r.w <- max.r.w + 1}
  #   max.r.h <- as.integer(sin(max.angle) * (d1[1] + 5) + sin(pi/2-max.angle) * (d1[2] + 5)) - d1[2]
  #   if(mod(max.r.h, 2)>0){max.r.h <- max.r.h + 1}
  #   
  #   max.q.w <- as.integer(cos(max.angle) * (d2[1] + 5) + cos(pi/2-max.angle) * (d2[2] + 5)) - d2[1]
  #   if(mod(max.q.w, 2)>0){max.q.w <- max.q.w + 1}
  #   max.q.h <- as.integer(sin(max.angle) * (d2[1] + 5) + sin(pi/2-max.angle) * (d2[2] + 5)) - d2[2]
  #   if(mod(max.q.h, 2)>0){max.q.h <- max.q.h + 1}
  #   
  #   ref_input <- ref_input %>%
  #     pad(max.r.w, "x", pos = 0, val = 1) %>%
  #     pad(max.r.h, "y", pos = 0, val = 1)
  #   questioned_input <- questioned_input %>%
  #     pad(max.q.w, "x", pos = 0, val = 1) %>%
  #     pad(max.q.h, "y", pos = 0, val = 1)
  #   
  #   ref_org <- ref_org %>%
  #     pad(max.r.w, "x", pos = 0, val = white1.r) %>%
  #     pad(max.r.h, "y", pos = 0, val = white1.r)
  #   questioned_org <- questioned_org %>%
  #     pad(max.q.w, "x", pos = 0, val = white1.q) %>%
  #     pad(max.q.h, "y", pos = 0, val = white1.q)
  #   
  #   padding_info <- list(r = list(w = max.r.w/2, h = max.r.h/2, direction = "both"),
  #                        q = list(w = max.q.w/2, h = max.q.h/2, direction = "both"))
  #   
  #   
  # }else{
  #   padding_info <- list(r = list(w = 0, h = 0, direction = "both"),
  #                        q = list(w = 0, h = 0, direction = "both"))
  # }
  
  
  # dx <- org.dim.r$width - org.dim.q$width
  # dy <- org.dim.r$height - org.dim.q$height
  # 
  # mod1 <- 32-mod(max(org.dim.r$width, org.dim.q$width), 32); if(mod1==32){mod1 <- 0}
  # mod2 <- 32-mod(max(org.dim.r$height, org.dim.q$height), 32); if(mod2==32){mod2 <- 0}
  # 
  # if(dx>0){
  #   questioned_input <- questioned_input %>%
  #     pad(dx + mod1, "x", pos = 1, val = 1)
  #   
  #   if(mod1 > 0){
  #     ref_input <- ref_input %>%
  #       pad(mod1, "x", pos = 1, val = 1)
  #   }
  # }else{
  #   ref_input <- ref_input %>%
  #     pad(abs(dx) + mod1, "x", pos = 1, val = 1)
  #   
  #   if(mod1 > 0){
  #     questioned_input <- questioned_input %>%
  #       pad(mod1, "x", pos = 1, val = 1)
  #   }
  # }
  # 
  # if(dy>0){
  #   questioned_input <- questioned_input %>%
  #     pad(dy + mod2, "y", pos = 1, val = 1)
  #   
  #   if(mod2 > 0){
  #     ref_input <- ref_input %>%
  #       pad(mod2, "y", pos = 1, val = 1)
  #   }
  # }else{
  #   
  #   ref_input <- ref_input %>%
  #     pad(abs(dy) + mod2, "y", pos = 1, val = 1)
  #   
  #   if(mod2 > 0){
  #     questioned_input <- questioned_input %>%
  #       pad(mod2, "y", pos = 1, val = 1)
  #     
  #   }
  # }
  
  
  
  # Ensure that the size of inputs are identical to each other, which is multiple of 32 for integer size.
  d1 <- dim(ref_input)
  d2 <- dim(questioned_input)
  
  mod1 <- 32-mod(max(d1[1], d2[1]), 32); if(mod1==32){mod1 <- 0}
  mod2 <- 32-mod(max(d1[2], d2[2]), 32); if(mod2==32){mod2 <- 0}
  dx <- d1[1]-d2[1]
  dy <- d1[2]-d2[2]
  
  
  if(dx>0){
    questioned_input <- questioned_input %>%
      pad(dx + mod1, "x", pos = 1, val = 1)
    questioned_org <- questioned_org %>%
      pad(dx + mod1, "x", pos = 1, val = white1.q)
    
    if(mod1 > 0){
      ref_input <- ref_input %>%
        pad(mod1, "x", pos = 1, val = 1)
      ref_org <- ref_org %>%
        pad(mod1, "x", pos = 1, val = white1.r)
    }
  }else{
    ref_input <- ref_input %>%
      pad(abs(dx) + mod1, "x", pos = 1, val = 1)
    ref_org <- ref_org %>%
      pad(abs(dx) + mod1, "x", pos = 1, val = white1.r)
    if(mod1 > 0){
      questioned_input <- questioned_input %>%
        pad(mod1, "x", pos = 1, val = 1)
      questioned_org <- questioned_org %>%
        pad(mod1, "x", pos = 1, val = white1.q)
    }
  }
  
  if(dy>0){
    questioned_input <- questioned_input %>%
      pad(dy + mod2, "y", pos = 1, val = 1)
    questioned_org <- questioned_org %>%
      pad(dy + mod2, "y", pos = 1, val = white1.q)
    
    if(mod2 > 0){
      ref_input <- ref_input %>%
        pad(mod2, "y", pos = 1, val = 1)
      ref_org <- ref_org %>%
        pad(mod2, "y", pos = 1, val = white1.r)
    }
  }else{
    
    ref_input <- ref_input %>%
      pad(abs(dy) + mod2, "y", pos = 1, val = 1)
    ref_org <- ref_org %>%
      pad(abs(dy) + mod2, "y", pos = 1, val = white1.r)
    
    if(mod2 > 0){
      questioned_input <- questioned_input %>%
        pad(mod2, "y", pos = 1, val = 1)
      questioned_org <- questioned_org %>%
        pad(mod2, "y", pos = 1, val = white1.q)
    }
  }
  # new.d1 <- dim(ref_input); new.d2 <- dim(questioned_input)
  # 
  # padding_info$r$w <- c(padding_info$r$w, new.d1[1]-d1[1])
  # padding_info$r$h <- c(padding_info$r$h, new.d1[2]-d1[2])
  # padding_info$r$direction <- c(padding_info$r$direction, "east")
  # 
  # padding_info$q$w <- c(padding_info$q$w, new.d2[1]-d2[1])
  # padding_info$q$h <- c(padding_info$q$h, new.d2[2]-d2[2])
  # padding_info$q$direction <- c(padding_info$r$direction, "south")
  
  
  cx.32multiple <- dim(questioned_input)[1]/2; cy.32multiple <- dim(questioned_input)[2]/2;
  cx.32multiple.downscale <- cx.32multiple*scale_down_factor; cy.32multiple.downscale <- cy.32multiple*scale_down_factor;
  
  scale_down_factor
  d1 <- Sys.time()-s_time; d1.t <- round(d1, 2); d1.u <- units(d1)
  progress$inc(0.1, detail = paste("reducing images..(", d1.t, " ", d1.u, ").", sep = ""))
  
  # To compute the best alignment
  angles_seq <- set.angles
  if(scale_down_factor == 1/1){
    ref_reduced <- ref_input
  }else{
    if(scale_down_factor == 1/2){
      ref_reduced <- ref_input %>%
        resize_halfXY()
    }else{
      if(scale_down_factor == 1/4){
        ref_reduced <- ref_input %>%
          resize_halfXY() %>%
          resize_halfXY()
      }else{
        if(scale_down_factor == 1/8){
          ref_reduced <- ref_input %>%
            resize_halfXY() %>%
            resize_halfXY() %>%
            resize_halfXY()
        }else{
          if(scale_down_factor == 1/16){
            ref_reduced <- ref_input %>%
              resize_halfXY() %>%
              resize_halfXY() %>%
              resize_halfXY() %>%
              resize_halfXY()
          }else{
            if(scale_down_factor == 1/32){
              ref_reduced <- ref_input %>%
                resize_halfXY() %>%
                resize_halfXY() %>%
                resize_halfXY() %>%
                resize_halfXY() %>%
                resize_halfXY()
            }}}}}}
  
  if(scale_down_factor == 1/1){
    questioned_reduced <- questioned_input
  }else{
    if(scale_down_factor == 1/2){
      questioned_reduced <- questioned_input %>%
        resize_halfXY()
    }else{
      if(scale_down_factor == 1/4){
        questioned_reduced <- questioned_input %>%
          resize_halfXY() %>%
          resize_halfXY()
      }else{
        if(scale_down_factor == 1/8){
          questioned_reduced <- questioned_input %>%
            resize_halfXY() %>%
            resize_halfXY() %>%
            resize_halfXY()
        }else{
          if(scale_down_factor == 1/16){
            questioned_reduced <- questioned_input %>%
              resize_halfXY() %>%
              resize_halfXY() %>%
              resize_halfXY() %>%
              resize_halfXY()
          }else{
            if(scale_down_factor == 1/32){
              questioned_reduced <- questioned_input %>%
                resize_halfXY() %>%
                resize_halfXY() %>%
                resize_halfXY() %>%
                resize_halfXY() %>%
                resize_halfXY()
            }}}}}}
  
  # d1 <- dim(ref_reduced)
  # d2 <- dim(questioned_reduced)
  # 
  # dx <- d1[1]-d2[1]
  # dy <- d1[2]-d2[2]
  # 
  # questioned_reduced <- questioned_reduced[,,1,1]
  # ref_reduced <- ref_reduced[,,1,1]
  # if(dx>0){
  #   questioned_reduced <- rbind(questioned_reduced, matrix(1, dx,  d2[2]))
  # }else{
  #   ref_reduced <- rbind(ref_reduced, matrix(1, abs(dx), d1[2]))
  # }
  # 
  # new.d1 <- nrow(ref_reduced)
  # new.d2 <- nrow(questioned_reduced)
  # 
  # if(dy>0){
  #   questioned_reduced <- cbind(questioned_reduced, matrix(1, new.d2, dy))
  # }else{
  #   ref_reduced <- cbind(ref_reduced, matrix(1, new.d1, abs(dy)))
  # }
  # 
  # ref_matrix<- ref_reduced
  # 
  # questioned_reduced <- as.cimg(questioned_reduced)
  
  ref_matrix<- ref_reduced[,,1,1]
  res <- list()
  tempfile.list <- tempfile(paste("res_", 1:length(angles_seq), sep = ""), fileext = ".rds")
  for(j in 1:length(angles_seq)){
    
    rotated_img <- rotate.shift.over.white.bg.f(img = questioned_reduced, 
                                                theta = angles_seq[j], 
                                                cx.set = cx.32multiple.downscale, cy.set = cy.32multiple.downscale,
                                                shift.x = 0,
                                                shift.y = 0)
    
    
    mat.rotated <- rotated_img[,,1,1]
    
    auto.cor.mat <- imagefx::xcorr3d(ref_matrix, mat.rotated)
    
    # res[[j]] <- auto.cor.mat 
    saveRDS(auto.cor.mat,
            tempfile.list[[j]])
    
    res[[j]] <- auto.cor.mat[1:2]
    
    
    d1 <- Sys.time()-s_time; d1.t <- round(d1, 2); d1.u <- units(d1)
    progress$inc(0.4/length(angles_seq), detail = paste("computing the best alignment..(", round(100*j/length(angles_seq)), "%; ",  d1.t, " ", d1.u, ").", sep = ""))
  }
  
  d1 <- Sys.time()-s_time; d1.t <- round(d1, 2); d1.u <- units(d1)
  progress$inc(0.1, detail = paste("applying the best alignment..(", d1.t, " ", d1.u, ").", sep = ""))
  
  each.res <- res
  each.max.idx <- each.res %>%
    map(~.x$max.corr) %>%
    unlist()
  each.max.idx <- which.max(each.max.idx)
  each.angle <- set.angles[each.max.idx]
  each.shift <- as.vector(each.res[[each.max.idx]]$max.shifts)
  
  
  trans_img <- rotate.shift.over.white.bg.f(img = questioned_input, 
                                            theta = each.angle, 
                                            cx.set = cx.32multiple, cy.set = cy.32multiple,
                                            shift.x = -each.shift[1]*(1/scale_down_factor),
                                            shift.y = -each.shift[2]*(1/scale_down_factor))
  
  
  trans_org <- rotate.shift.over.white.bg.f(img = questioned_org,
                                            theta = each.angle,
                                            cx.set = cx.32multiple, cy.set = cy.32multiple,
                                            shift.x = -each.shift[1]*(1/scale_down_factor),
                                            shift.y = -each.shift[2]*(1/scale_down_factor))
  
  
  transform_info <- list(Angle = each.angle,
                         X = -each.shift[1]*(1/scale_down_factor),
                         Y = -each.shift[2]*(1/scale_down_factor))
  
  ## Colorizing the aligned images
  d1 <- Sys.time()-s_time; d1.t <- round(d1, 2); d1.u <- units(d1)
  progress$inc(0.1, detail = paste("Colorizing images..(", d1.t, " ", d1.u, ").", sep = ""))
  
  
  ## Processed images; temporary padding and colorizing.
  
  ref_blue <- ref_input %>%
    add.color();
  ref_blue[,,1,3] <- 1
  questioned_orange <- trans_img %>%
    add.color();
  questioned_orange[,,1,1] <- 1
  
  overlayed_blue_orange <- ref_blue + questioned_orange
  
  
  
  # ref_org_magick <- ref_org %>%
  #   mirror('x') %>%
  #   cimg2magick()
  # trans_org_magick <- trans_org %>%
  #   mirror('x') %>%
  #   cimg2magick()
  # overlayed_org <- ref_org_magick %>%
  #   image_composite(trans_org_magick, operator = "dissolve",compose_args = "80%",
  #                   gravity="northwest") %>%
  #   magick2cimg()
  
  rr <- ref_org %>%
    rm.alpha()
  if(dim(rr)[4]==1){
    rr <- add.color(rr)
  }
  qq <- trans_org %>%
    rm.alpha()
  if(dim(qq)[4]==1){
    qq <- add.color(qq)
  }
  # qq <- questioned_org %>%
  #   rm.alpha()
  # if(dim(qq)[4]==1){
  #   qq <- add.color(qq)
  # }
  overlayed_org <- rr + qq
  
  ## cross-correlation for alignment location info
  
  d1 <- Sys.time()-s_time; d1.t <- round(d1, 2); d1.u <- units(d1)
  progress$inc(0.1, detail = paste("computing diagnosis..(", d1.t, " ", d1.u, ").", sep = ""))
  
  display.idx <- seq(max(1, each.max.idx-2), min(each.max.idx+2, length(angles_seq)), 1)
  res <- display.idx %>%
    map(~readRDS(tempfile.list[[.x]]))
  unlink(tempfile.list)
  
  yy <- list()
  for(i in 1:length(display.idx)){
    xx <- res[[i]]$corr.mat
    xx[xx<=quantile(xx, 0.99)] <- NA
    yy[[i]] <- as.cimg(xx) %>% 
      mirror('x') 
  }
  yy.df <- yy %>%
    map2(display.idx,
         ~cbind(as.data.frame(.x), rotation_angle = as.factor(paste(angles_seq[.y], 
                                                                    "\u00B0", sep = "")))) %>% 
    map(~.x %>%
          mutate(x = x - round(median(x)),
                 y = -(y - round(median(y))))) %>%
    map_dfr(~.x)
  align_info_p <- list(max.cross.corr_angle = data.frame(angle = angles_seq,
                                                         max_corr = each.res %>%
                                                           map(~.x$max.corr) %>%
                                                           unlist()),
                       cross.corr_map_angle = lattice::levelplot(value ~ x*y | rotation_angle, data=yy.df, 
                                                                 xlab = 'shift_x', ylab = 'shift_y',
                                                                 ylim = c(max(yy.df$y), min(yy.df$y)),
                                                                 layout = c(length(display.idx), 1),
                                                                 main = paste("Top 1% of cross-correlation by angles; scale = ", 
                                                                              scale_down_factor, ".", sep = "")))
  rm(res)
  
  
  d1 <- Sys.time()-s_time; d1.t <- round(d1, 2); d1.u <- units(d1)
  progress$inc(0.1, detail = paste("computing similarity..(", d1.t, " ", d1.u, ").", sep = ""))
  
  
  aligned_cor <- cor(crop.bbox(trans_img, trans_img < 1),
                     crop.bbox(ref_input, trans_img < 1))
  
  
  
  # return(list(ref_images = list(gray = ref_input,
  #                               org_padded = ref_org),
  #             questioned_images = list(gray = questioned_input,
  #                                      aligned_gray = trans_img,
  #                                      org_padded = questioned_org,
  #                                      aligned_org_padded = trans_org),
  #             #padding_info = padding_info,
  #             transformed_info = transform_info,
  #             colored_res = list(ref = ref_blue,
  #                                questioned = questioned_orange, 
  #                                overlayed = ref_blue/2 + questioned_orange/2),
  #             overlayed_org = rr + qq,
  #             similarity_cor = aligned_cor,
  #             align_info_p = align_info_p))
  
  return(list(ref_images = list(gray = ref_input,
                                org_padded = ref_org),
              questioned_images = list(gray = questioned_input,
                                       aligned_gray = trans_img,
                                       aligned_org = trans_org,
                                       org_padded = questioned_org),
              transformed_info = transform_info,
              colored_res = list(ref = ref_blue,
                                 questioned = questioned_orange, 
                                 overlayed = overlayed_blue_orange),
              overlayed_org= overlayed_org,
              similarity_cor = aligned_cor,
              align_info_p = align_info_p))
  
  
} 


rotate.shift.over.white.bg.f <- function(img, theta = 0, cx.set, cy.set, shift.x = 0, shift.y = 0){
  # img.d <- dim(img)
  # img.w <- img.d[1]; img.h <- img.d[2]
  if(theta != 0){
    # img.cx <- as.integer(img.w/2); img.cy <- as.integer(img.h/2)
    # white.bg <- 1-(as.cimg(matrix(NA, img.w, img.h)) %>% 
    #                  imrotate(theta, cx = img.cx,
    #                           cy = img.cy,
    #                           boundary = 0))
    # white.bg[is.na(white.bg)] <- 0
    # 
    # if(img.d[4]>1){white.bg <- add.color(white.bg)}
    # 
    # img <- white.bg + img %>% 
    #   imrotate(theta, cx = img.cx,
    #            cy = img.cy,
    #            boundary = 0)
    img <- 1-((1-img)%>%
                imrotate(theta,
                         cx = cx.set,
                         cy = cy.set,
                         boundary = 0))
    
    
  }
  
  
  
  if(shift.x != 0 | shift.y != 0){
    
    # img.d <- dim(img)
    # white.bg <- 1-(as.cimg(matrix(NA, img.d[1], img.d[2])) %>% 
    #                  imshift(shift.x, shift.y, 0)); white.bg[is.na(white.bg)] <- 0
    # 
    # 
    # if(img.d[4]>1){white.bg <- add.color(white.bg)}
    # 
    # img <- white.bg + img %>% 
    #   imshift(shift.x, shift.y, 0)
    
    
    img <- 1-((1-img) %>%
                imshift(shift.x, shift.y, 0))
  }
  return(img)
}


ggplot_with_points <- function(loaded_img, clicked_data, point_col = "blue") {
  # To display an image with the clicked coordinates using ggplot2
  displayed_img <- loaded_img +
    geom_point(data = clicked_data, aes(x = .data$x_values,
                                        y = .data$y_values), col = point_col) +
    geom_path(data = clicked_data, aes(x = .data$x_values,
                                       y = .data$y_values), color = point_col, linetype = 1)
  if(nrow(clicked_data)>2){
    if(point_col != "red"){
      displayed_img <- displayed_img +
        geom_path(data = clicked_data[c(nrow(clicked_data), 1),], 
                  aes(x = .data$x_values,
                      y = .data$y_values), 
                  color = point_col, linetype = 2)
    }else{
      displayed_img <- displayed_img +
        geom_path(data = clicked_data[c(nrow(clicked_data), 1),], 
                  aes(x = .data$x_values,
                      y = .data$y_values), 
                  color = point_col, linetype = 1)
    }
  }
  return(displayed_img)
}


image_ggplot_modified <- function(image){
  
  # To display an image with the clicked coordinates using ggplot2
  info <- image_info(image)
  ggplot2::ggplot(data.frame(x = 0, y = 0), ggplot2::aes_string("x", "y")) + 
    ggplot2::geom_blank() + ggplot2::theme_void() + 
    ggplot2::coord_fixed(expand = FALSE, xlim = c(0, info$width), 
                         ylim = c(info$height, 0)) + 
    ggplot2::annotation_raster(image, 0, info$width, info$height, 0) + NULL
}

determine.display.scale.f <- function(x){
  info <- dim(x)
  # To display reduced images in the user interface
  if(info[2] > 5000){
    
    return(x %>%
             resize_halfXY() %>%
             resize_halfXY() %>%
             resize_halfXY())
    
  }else{
    if(info[2] > 2500){
      return(x %>%
               resize_halfXY() %>%
               resize_halfXY())
    }else{
      if(info[2] > 1000){
        return(x %>%
                 resize_halfXY())
      }else{
        return(x)
      }}}}



# Server logic 
server <- function(input, output, session) {
  ########### To load FILE1 and create its downscale for faster display #################  
  loaded1 <- reactive({
    file <- input$file1
    ext <- tools::file_ext(file$datapath)
    
    req(file)
    validate(need(ext %in% c("png", "jpg", "jpeg", "PNG", "JPG", "JPEG", "tiff", "tif", "TIFF", "TIF"), "Please upload a png, jpeg, tiff file."))
    img1 <- image_read(file$datapath) 
    return(img1)
  }) %>%
    bindCache(input$file1, cache = "app") %>%
    bindEvent(input$file1)
  
  
  loaded1_process_info <- reactive({
    file <- input$file1_supp
    ext <- tools::file_ext(file$datapath)
    
    req(file)
    validate(need(ext %in% c("txt"), "Please upload a txt file."))
    info <- read.table(file$datapath, header = TRUE)
    return(info)
  })
  
  
  # Output the info of FILE1
  
  loaded1_info_reactive <- eventReactive(input$file1,{
    
    print(image_info(loaded1()))
  })
  output$loaded1_info <- renderPrint({
    
    loaded1_info_reactive()
    
  })
  
  #######################################################################################  
  
  ########### To load FILE2 and create its downscale for faster display #################  
  loaded2 <- reactive({
    file <- input$file2
    ext <- tools::file_ext(file$datapath)
    
    req(file)
    validate(need(ext %in% c("png", "jpg", "jpeg", "PNG", "JPG", "JPEG", "tiff", "tif", "TIFF", "TIF"), "Please upload a png, jpeg, tiff file."))
    img1 <- image_read(file$datapath) 
    return(img1)
  }) %>%
    bindCache(input$file2, cache = "app") %>%
    bindEvent(input$file2)
  
  loaded2_process_info <- reactive({
    file <- input$file2_supp
    ext <- tools::file_ext(file$datapath)
    
    req(file)
    validate(need(ext %in% c("txt"), "Please upload a txt file."))
    info <- read.table(file$datapath, header = TRUE)
    return(info)
  })
  
  # Output the info of FILE2
  loaded2_info_reactive <- eventReactive(input$file2,{
    print(loaded2())
  })
  output$loaded2_info <- renderPrint({
    loaded2_info_reactive()
  })
  
  
  #######################################################################################  
  
  
  ########### Interactive settings to edit FILE1 #################
  v <- reactiveValues(crop.img = FALSE,
                      imgclick.x = NULL,
                      imgclick.y = NULL,
                      stop.cri = 0,
                      trace.crop = TRUE,
                      complete.crop = FALSE,
                      use.metadata = FALSE,
                      stop.alignment = 0)
  observeEvent(input$file1_supp,{
    v$use.metadata <- TRUE
  })
  observeEvent(input$file1,{
    v$crop.img <- FALSE
    v$imgclick.x <- NULL
    v$imgclick.y <- NULL
    v$stop.cri <- 0
    v$trace.crop <- TRUE
    v$complete.crop <- FALSE
    enable("pauseCropping")
    enable("selectForeground")
    disable("resetCropping")
    enable("cropBackground")
    
    image_data$single_click <- data.frame(x_values=NA_real_, y_values = NA_real_)
    
    
    updateCheckboxInput(session, "makeitgray", value = FALSE)
    updateCheckboxInput(session, "preinvert", value = FALSE)
    updateSliderInput(session, "Threshold1", value = c(0,1))
    
  })
  
  ### Reset all edits
  observeEvent(input$resetall,{
    updateCheckboxInput(session, "makeitgray", value = FALSE)
    updateCheckboxInput(session, "preinvert", value = FALSE)
    updateSliderInput(session, "Threshold1", value = c(0,1))
  })
  
  ### Begin cropping
  observeEvent(input$selectForeground, {
    v$crop.img <- TRUE
    v$trace.crop <- TRUE
    v$complete.crop <- FALSE
    v$stop.cri <- 0
    disable("selectForeground")
    enable("pauseCropping")
    enable("resetCropping")
    enable("cropBackground")
  })
  ### Pause tracing
  observeEvent(input$pauseCropping, {
    v$trace.crop <- FALSE
    v$complete.crop <- FALSE
    disable("pauseCropping")
    enable("selectForeground")
    enable("resetCropping")
    enable("cropBackground")
  })
  ### Complete cropping
  observeEvent(input$cropBackground, {
    v$trace.crop <- FALSE
    v$complete.crop <- TRUE
    disable("pauseCropping")
    enable("selectForeground")
    enable("resetCropping")
    disable("cropBackground")
    
  })
  ### Reset tracing
  observeEvent(input$resetCropping, {
    v$crop.img <- FALSE
    v$imgclick.x <- NULL
    v$imgclick.y <- NULL
    v$stop.cri <- 0
    v$trace.crop <- TRUE
    v$complete.crop <- FALSE
    enable("pauseCropping")
    enable("selectForeground")
    disable("resetCropping")
    enable("cropBackground")
    
    image_data$single_click <- data.frame(x_values=NA_real_, y_values = NA_real_)
  })
  
  observe({
    if(input$makeitgray == FALSE & (input$preinvert == TRUE | min(input$Threshold1) > 0 | max(input$Threshold1) < 1)){
      updateCheckboxInput(session, "makeitgray", value = TRUE)
    }
  })
  
  #   observeEvent(input$submit,{
  #     v$stop.alignment <- 0
  #   })
  # 
  #   observeEvent(input$ok,{
  #     v$stop.alignment <- input$ok
  #   })
  # 
  # 
  #   output$ok_output <- renderPrint({
  # 
  # paste(input$ok, ", ", v$stop.alignment)
  # 
  #   })
  ### Append clicked coordinates
  observeEvent(input$plot1_click, {
    if(v$crop.img == TRUE){
      if(v$trace.crop == TRUE & v$stop.cri < 50){
        v$imgclick.x <- c(v$imgclick.x, round(as.numeric(input$plot1_click$x)))
        v$imgclick.y <- c(v$imgclick.y, round(as.numeric(input$plot1_click$y)))
        v$stop.cri <- length(v$imgclick.x)
      }
    }
    
  })
  
  ############################################################
  ########### Interactive settings to edit FILE2 #############
  u <- reactiveValues(crop.img = FALSE,
                      imgclick.x = NULL,
                      imgclick.y = NULL,
                      stop.cri = -99,
                      trace.crop = TRUE,
                      complete.crop = FALSE)
  
  observeEvent(input$file2,{
    u$crop.img <- FALSE
    u$imgclick.x <- NULL
    u$imgclick.y <- NULL
    u$stop.cri <- 0
    u$trace.crop <- TRUE
    u$complete.crop <- FALSE
    enable("pauseCropping")
    enable("selectForeground")
    disable("resetCropping")
    enable("cropBackground") 
    
    
    image_data2$single_click <- data.frame(x_values=NA_real_, y_values = NA_real_)
    
    
    updateCheckboxInput(session, "makeitgray2", value = FALSE)
    updateCheckboxInput(session, "preinvert2", value = FALSE)
    updateSliderInput(session, "Threshold2", value = c(0, 1))
  })
  
  
  ### Reset all edits
  observeEvent(input$resetall2,{
    updateCheckboxInput(session, "makeitgray2", value = FALSE)
    updateCheckboxInput(session, "preinvert2", value = FALSE)
    updateSliderInput(session, "Threshold2", value = c(0, 1))
  })
  
  ### Begin cropping
  observeEvent(input$selectForeground2, {
    u$crop.img <- TRUE
    u$trace.crop <- TRUE
    u$complete.crop <- FALSE
    u$stop.cri <- 0
    disable("selectForeground2")
    enable("pauseCropping2")
    #  enable("resetCropping2")
    enable("cropBackground2")
  })
  ### Pause tracing
  observeEvent(input$pauseCropping2, {
    u$trace.crop <- FALSE
    u$complete.crop <- FALSE
    disable("pauseCropping2")
    enable("selectForeground2")
    enable("resetCropping2")
    enable("cropBackground2")
  })
  ### Complete cropping
  observeEvent(input$cropBackground2, {
    u$trace.crop <- FALSE
    u$complete.crop <- TRUE
    disable("pauseCropping2")
    enable("selectForeground2")
    enable("resetCropping2")
    disable("cropBackground2")
    
    disable("cropBackground2")
  })
  ### Reset tracing
  observeEvent(input$resetCropping2, {
    u$crop.img <- FALSE
    u$imgclick.x <- NULL
    u$imgclick.y <- NULL
    u$stop.cri <- 0
    u$trace.crop <- TRUE
    u$complete.crop <- FALSE
    enable("pauseCropping")
    enable("selectForeground")
    disable("resetCropping")
    enable("cropBackground")
    
    image_data2$single_click <- data.frame(x_values=NA_real_, y_values = NA_real_)
  })
  
  observe({
    if(input$makeitgray2 == FALSE & (input$preinvert2 == TRUE | min(input$Threshold2) > 0 | max(input$Threshold2) < 1)){
      updateCheckboxInput(session, "makeitgray2", value = TRUE)
    }
  })
  ### Append clicked coordinates
  observeEvent(input$plot2_click, {
    if(u$crop.img == TRUE){
      if(u$trace.crop == TRUE & u$stop.cri < 50){
        u$imgclick.x <- c(u$imgclick.x, round(as.numeric(input$plot2_click$x)))
        u$imgclick.y <- c(u$imgclick.y, round(as.numeric(input$plot2_click$y)))
        u$stop.cri <- length(u$imgclick.x)
      }
    }
    
  })
  ########################################################
  
  
  
  ########### The coordinates to define shoe portion of FILE1 #############
  image_data <- shiny::reactiveValues()
  image_data$single_click <- data.frame(x_values=NA_real_, y_values = NA_real_)
  
  observeEvent({input$plot1_click}, {
    if(v$crop.img == TRUE){
      image_data$single_click <- data.frame(x_values=v$imgclick.x, y_values = v$imgclick.y)
    }
  })
  
  observeEvent({input$revertCropping}, {
    if(nrow(image_data$single_click)>0 & v$complete.crop != TRUE){
      image_data$single_click <- image_data$single_click[-nrow(image_data$single_click),]
      v$imgclick.x <- v$imgclick.x[-length(v$imgclick.x)]
      v$imgclick.y <- v$imgclick.y[-length(v$imgclick.y)]
    }
    if(length(v$imgclick.x)==0){
      image_data$single_click <- data.frame(x_values=NA_real_, y_values = NA_real_)
      v$imgclick.x <- NULL
      v$imgclick.y <- NULL
    }
  })
  #########################################################################
  
  
  ########### The coordinates to define shoe portion of FILE2 #############
  image_data2 <- shiny::reactiveValues()
  image_data2$single_click <- data.frame(x_values=NA_real_, y_values = NA_real_)
  
  observeEvent({input$plot2_click}, {
    if(u$crop.img == TRUE){
      image_data2$single_click <- data.frame(x_values=u$imgclick.x, y_values = u$imgclick.y)
    }
  })
  
  observeEvent({input$revertCropping2}, {
    if(nrow(image_data2$single_click)>0 & u$complete.crop != TRUE){
      image_data2$single_click <- image_data2$single_click[-nrow(image_data2$single_click),]
      u$imgclick.x <- u$imgclick.x[-length(u$imgclick.x)]
      u$imgclick.y <- u$imgclick.y[-length(u$imgclick.y)]
    }
    if(length(u$imgclick.x)==0){
      image_data2$single_click <- data.frame(x_values=NA_real_, y_values = NA_real_)
      u$imgclick.x <- NULL
      u$imgclick.y <- NULL
    }
  })
  #########################################################################
  
  
  ########### To plot FILE1 being edited in a reduced scale #############
  loaded1_processed_plot <- reactive({
    before_crop <- loaded1() %>%
      image_scale(geometry = paste(input$loaded1_zoom_percent, "%", sep = ""))
    img_info <- image_info(before_crop)
    
    
    if(is.null(input$file1_supp)){
      if(input$makeitgray == TRUE & img_info$colorspace != "Gray"){
        
        before_crop <- before_crop %>%
          image_convert(colorspace = "gray")
        
      }
      if(input$preinvert == TRUE){ # faster computation
        if(img_info$colorspace != "Gray"){
          before_crop <- before_crop %>%
            image_convert(colorspace = "gray") %>%
            image_negate()
        }else{
          before_crop <- before_crop %>%
            image_negate()
        }
      }
      
      
      if(min(input$Threshold1)!=0 | max(input$Threshold1) != 1){
        if(img_info$colorspace != "Gray"){
          
          before_crop <- before_crop %>%
            image_convert(colorspace = "gray")
          
          if(max(input$Threshold1)!=1){
            before_crop <- before_crop %>%
              image_threshold(type = "white",
                              threshold = paste(max(input$Threshold1)*100, "%", sep = ""))
          }
          
          if(min(input$Threshold1)!=0){
            lower.thresholding1 <- before_crop %>%
              image_threshold(type = "black",
                              threshold = paste(min(input$Threshold1)*100, "%", sep = ""))
            
            lower.thresholding2 <- before_crop %>%
              image_threshold(type = "white",
                              threshold = paste(min(input$Threshold1)*100-0.000001, "%", sep = "")) %>%
              image_threshold(type = "black",
                              threshold = paste(min(input$Threshold1)*100-0.000001, "%", sep = ""))
            
            before_crop <- image_composite(lower.thresholding1, lower.thresholding2, "minus") %>%
              image_negate() 
            rm(lower.thresholding1);rm(lower.thresholding2)
          }
          
          
          
          
        }else{
          
          if(max(input$Threshold1)!=1){
            before_crop <- before_crop %>%
              image_threshold(type = "white",
                              threshold = paste(max(input$Threshold1)*100, "%", sep = ""))
          }
          
          if(min(input$Threshold1)!=0){
            lower.thresholding1 <- before_crop %>%
              image_threshold(type = "black",
                              threshold = paste(min(input$Threshold1)*100, "%", sep = ""))
            
            lower.thresholding2 <- before_crop %>%
              image_threshold(type = "white",
                              threshold = paste(min(input$Threshold1)*100-0.000001, "%", sep = "")) %>%
              image_threshold(type = "black",
                              threshold = paste(min(input$Threshold1)*100-0.000001, "%", sep = ""))
            
            before_crop <- image_composite(lower.thresholding1, lower.thresholding2, "minus") %>%
              image_negate() 
            rm(lower.thresholding1);rm(lower.thresholding2)
          }
          
        }}
      
      
      if(v$complete.crop == TRUE){
        p1 <- rbind(c(0,0),
                    c(0, img_info$height),
                    c(img_info$width, img_info$height),
                    c(img_info$width, 0),
                    c(0,0))
        
        hole <- cbind(v$imgclick.x, v$imgclick.y); hole <- rbind(hole, hole[1,])
        
        p1 <- list(p1, hole)
        pols <- st_sf(value = 1,
                      geometry = st_sfc(st_polygon(p1)))
        r <- raster(pols, res = 1)
        r <- fasterize(pols, r, field = "value", fun="sum")
        mask1 <- (!is.na(matrix(r, nrow = dim(r)[2], dim(r)[1], byrow = F))) %>%
          as.cimg() %>%
          cimg2magick() %>%
          image_flip() %>%
          image_flop()
        
        
        after_crop <- image_composite(before_crop, image_transparent(mask1, "black"), "over")
        
        return(list(ggplot_img = image_ggplot_modified(after_crop),
                    magick_img = after_crop,
                    mask = mask1,
                    mask_xy = hole))
      }else{
        
        return(list(ggplot_img = image_ggplot_modified(before_crop),
                    magick_img = before_crop,
                    mask = NULL,
                    mask_xy = NULL))
      }
    }else{
      cond <- loaded1_process_info()
      
      if(unique(cond$grayscale) == TRUE & img_info$colorspace != "Gray"){
        before_crop <- before_crop %>%
          image_convert(colorspace = "gray")
      }
      
      if(unique(cond$invert) == TRUE){ # faster computation
        if(img_info$colorspace != "Gray"){
          before_crop <- before_crop %>%
            image_convert(colorspace = "gray") %>%
            image_negate()
        }else{
          before_crop <- before_crop %>%
            image_negate()
        }
      }
      
      
      # if(unique(cond$threshold)<100){
      #   if(img_info$colorspace != "Gray"){
      #     before_crop <- before_crop %>%
      #       image_convert(colorspace = "gray") %>%
      #       image_threshold(type = "white",
      #                       threshold = paste(unique(cond$threshold), "%", sep = ""))%>%
      #       image_threshold(type = "black",
      #                       threshold = paste(unique(cond$threshold), "%", sep = ""))
      #   }else{
      #     before_crop <- before_crop %>%
      #       image_threshold(type = "white",
      #                       threshold = paste(unique(cond$threshold), "%", sep = ""))%>%
      #       image_threshold(type = "black",
      #                       threshold = paste(unique(cond$threshold), "%", sep = ""))
      #   }}
      
      if(unique(cond$threshold_lower)!=0 | unique(cond$threshold_upper) != 1){
        if(img_info$colorspace != "Gray"){
          
          before_crop <- before_crop %>%
            image_convert(colorspace = "gray")
          
          if(unique(cond$threshold_upper)!=1){
            before_crop <- before_crop %>%
              image_threshold(type = "white",
                              threshold = paste(unique(cond$threshold_upper)*100, "%", sep = ""))
          }
          
          if(unique(cond$threshold_lower)!=0){
            lower.thresholding1 <- before_crop %>%
              image_threshold(type = "black",
                              threshold = paste(unique(cond$threshold_lower)*100, "%", sep = ""))
            
            lower.thresholding2 <- before_crop %>%
              image_threshold(type = "white",
                              threshold = paste(unique(cond$threshold_lower)*100-0.000001, "%", sep = "")) %>%
              image_threshold(type = "black",
                              threshold = paste(unique(cond$threshold_lower)*100-0.000001, "%", sep = ""))
            
            before_crop <- image_composite(lower.thresholding1, lower.thresholding2, "minus") %>%
              image_negate() 
            rm(lower.thresholding1);rm(lower.thresholding2)
          }
          
        }else{
          
          if(unique(cond$threshold_upper)!=1){
            before_crop <- before_crop %>%
              image_threshold(type = "white",
                              threshold = paste(unique(cond$threshold_upper)*100, "%", sep = ""))
          }
          
          if(unique(cond$threshold_lower)!=0){
            lower.thresholding1 <- before_crop %>%
              image_threshold(type = "black",
                              threshold = paste(unique(cond$threshold_lower)*100, "%", sep = ""))
            
            lower.thresholding2 <- before_crop %>%
              image_threshold(type = "white",
                              threshold = paste(unique(cond$threshold_lower)*100-0.000001, "%", sep = "")) %>%
              image_threshold(type = "black",
                              threshold = paste(unique(cond$threshold_lower)*100-0.000001, "%", sep = ""))
            
            before_crop <- image_composite(lower.thresholding1, lower.thresholding2, "minus") %>%
              image_negate() 
            rm(lower.thresholding1);rm(lower.thresholding2)
          }
          
        }}
      
      
      if(!is.null(cond$x) & !is.null(cond$y)){
        p1 <- rbind(c(0,0),
                    c(0, img_info$height),
                    c(img_info$width, img_info$height),
                    c(img_info$width, 0),
                    c(0,0))
        
        # hole <- cbind(as.numeric(cond$x), as.numeric(cond$y))
        hole <- cbind(sapply(as.numeric(cond$x), function(x) as.numeric(as.integer(x * input$loaded1_zoom_percent/100))),
                      sapply(as.numeric(cond$y), function(x) as.numeric(as.integer(x * input$loaded1_zoom_percent/100)))) ;  hole <- rbind(hole, hole[1,])
        
        p1 <- list(p1, hole)
        pols <- st_sf(value = 1,
                      geometry = st_sfc(st_polygon(p1)))
        r <- raster(pols, res = 1)
        r <- fasterize(pols, r, field = "value", fun="sum")
        mask1 <- (!is.na(matrix(r, nrow = dim(r)[2], dim(r)[1], byrow = F))) %>%
          as.cimg() %>%
          cimg2magick() %>%
          image_flip() %>%
          image_flop()
        
        
        after_crop <- image_composite(before_crop, image_transparent(mask1, "black"), "over")
        
        return(list(ggplot_img = image_ggplot_modified(after_crop),
                    magick_img = after_crop,
                    mask = mask1,
                    mask_xy = hole))
      }else{
        
        return(list(ggplot_img = image_ggplot_modified(before_crop),
                    magick_img = before_crop,
                    mask = NULL,
                    mask_xy = NULL))
        
      }
    }
  }) %>%
    bindCache(input$file1, input$makeitgray, input$preinvert, input$Threshold1, v$complete.crop, input$loaded1_zoom_percent,image_data$single_click,
              input$file1_supp, cache = "app")
  
  
  loaded1_processed <- reactive({
    
    
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    
    progress$set(message = "Processing Reference print..", value = 0.3)
    s_time <- Sys.time() 
    before_crop <- loaded1() 
    img_info <- image_info(before_crop)
    
    
    if(is.null(input$file1_supp)){
      if(input$makeitgray == TRUE & img_info$colorspace != "Gray"){
        
        before_crop <- before_crop %>%
          image_convert(colorspace = "gray")
        
      }
      if(input$preinvert == TRUE){ # faster computation
        if(img_info$colorspace != "Gray"){
          before_crop <- before_crop %>%
            image_convert(colorspace = "gray") %>%
            image_negate()
        }else{
          before_crop <- before_crop %>%
            image_negate()
        }
      }
      
      
      # if(input$Threshold1<100){
      if(min(input$Threshold1)!=0 | max(input$Threshold1) != 1){
        if(img_info$colorspace != "Gray"){
          # before_crop <- before_crop %>%
          #   image_convert(colorspace = "gray") %>%
          #   image_threshold(type = "white",
          #                   threshold = paste(input$Threshold1, "%", sep = ""))%>%
          #   image_threshold(type = "black",
          #                   threshold = paste(input$Threshold1, "%", sep = ""))
          
          before_crop <- before_crop %>%
            image_convert(colorspace = "gray")
          
          if(max(input$Threshold1)!=1){
            before_crop <- before_crop %>%
              image_threshold(type = "white",
                              threshold = paste(max(input$Threshold1)*100, "%", sep = ""))
          }
          
          if(min(input$Threshold1)!=0){
            lower.thresholding1 <- before_crop %>%
              image_threshold(type = "black",
                              threshold = paste(min(input$Threshold1)*100, "%", sep = ""))
            
            lower.thresholding2 <- before_crop %>%
              image_threshold(type = "white",
                              threshold = paste(min(input$Threshold1)*100-0.000001, "%", sep = "")) %>%
              image_threshold(type = "black",
                              threshold = paste(min(input$Threshold1)*100-0.000001, "%", sep = ""))
            
            before_crop <- image_composite(lower.thresholding1, lower.thresholding2, "minus") %>%
              image_negate() 
            rm(lower.thresholding1);rm(lower.thresholding2)
          }
          
        }else{
          # before_crop <- before_crop %>%
          #   image_threshold(type = "white",
          #                   threshold = paste(input$Threshold1, "%", sep = ""))%>%
          #   image_threshold(type = "black",
          #                   threshold = paste(input$Threshold1, "%", sep = ""))
          
          if(max(input$Threshold1)!=1){
            before_crop <- before_crop %>%
              image_threshold(type = "white",
                              threshold = paste(max(input$Threshold1)*100, "%", sep = ""))
          }
          
          if(min(input$Threshold1)!=0){
            lower.thresholding1 <- before_crop %>%
              image_threshold(type = "black",
                              threshold = paste(min(input$Threshold1)*100, "%", sep = ""))
            
            lower.thresholding2 <- before_crop %>%
              image_threshold(type = "white",
                              threshold = paste(min(input$Threshold1)*100-0.000001, "%", sep = "")) %>%
              image_threshold(type = "black",
                              threshold = paste(min(input$Threshold1)*100-0.000001, "%", sep = ""))
            
            before_crop <- image_composite(lower.thresholding1, lower.thresholding2, "minus") %>%
              image_negate() 
            rm(lower.thresholding1);rm(lower.thresholding2)
          }
          
        }}
      
      # if(input$Threshold1>0){
      #   before_crop <- magick2cimg(before_crop)
      #   
      #   if(img_info$colorspace != "Gray"){
      #     before_crop <- grayscale(before_crop)
      #     before_crop[before_crop<input$Threshold1] <- 0; before_crop[before_crop>0] <- 1
      #   }else{
      #     before_crop[before_crop<input$Threshold1] <- 0; before_crop[before_crop>0] <- 1
      #   }
      #   
      #   before_crop <- before_crop %>%
      #     mirror('x') %>%
      #     cimg2magick()
      # }
      
      if(v$complete.crop == TRUE){
        
        d1 <- Sys.time()-s_time; d1.t <- round(d1, 2); d1.u <- units(d1)
        progress$inc(0.4, detail = paste("Cropping..(", d1.t, " ", d1.u, ").", sep = ""))
        
        p1 <- rbind(c(0,0),
                    c(0, img_info$height),
                    c(img_info$width, img_info$height),
                    c(img_info$width, 0),
                    c(0,0))
        
        # hole <- cbind(v$imgclick.x, v$imgclick.y); hole <- rbind(hole, hole[1,])
        
        hole <- cbind(sapply(v$imgclick.x, function(x) as.numeric(as.integer(min(c(x * 100/input$loaded1_zoom_percent, img_info$width))))),
                      sapply(v$imgclick.y, function(x) as.numeric(as.integer(min(c(x * 100/input$loaded1_zoom_percent, img_info$height)))))) ;  hole <- rbind(hole, hole[1,])
        
        p1 <- list(p1, hole)
        pols <- st_sf(value = 1,
                      geometry = st_sfc(st_polygon(p1)))
        r <- raster(pols, res = 1)
        r <- fasterize(pols, r, field = "value", fun="sum")
        mask1 <- (!is.na(matrix(r, nrow = dim(r)[2], dim(r)[1], byrow = F))) %>%
          as.cimg() %>%
          cimg2magick() %>%
          image_flip() %>%
          image_flop()
        
        
        after_crop <- image_composite(before_crop, image_transparent(mask1, "black"), "over")
        
        
        d1 <- Sys.time()-s_time; d1.t <- round(d1, 2); d1.u <- units(d1)
        progress$inc(0.3, detail = paste("Complete..(", d1.t, " ", d1.u, ").", sep = ""))
        
        return(list(ggplot_img = image_ggplot_modified(after_crop),
                    magick_img = after_crop,
                    mask = mask1,
                    mask_xy = hole))
        
      }else{
        
        
        d1 <- Sys.time()-s_time; d1.t <- round(d1, 2); d1.u <- units(d1)
        progress$inc(0.7, detail = paste("Complete..(", d1.t, " ", d1.u, ").", sep = ""))
        
        return(list(ggplot_img = image_ggplot_modified(before_crop),
                    magick_img = before_crop,
                    mask = NULL,
                    mask_xy = NULL))
      }
    }else{
      cond <- loaded1_process_info()
      
      if(unique(cond$grayscale) == TRUE & img_info$colorspace != "Gray"){
        before_crop <- before_crop %>%
          image_convert(colorspace = "gray")
      }
      
      if(unique(cond$invert) == TRUE){ # faster computation
        if(img_info$colorspace != "Gray"){
          before_crop <- before_crop %>%
            image_convert(colorspace = "gray") %>%
            image_negate()
        }else{
          before_crop <- before_crop %>%
            image_negate()
        }
      }
      
      # if(unique(cond$threshold)<100){
      #   if(img_info$colorspace != "Gray"){
      #     before_crop <- before_crop %>%
      #       image_convert(colorspace = "gray") %>%
      #       image_threshold(type = "white",
      #                       threshold = paste(unique(cond$threshold), "%", sep = "")) %>%
      #       image_threshold(type = "black",
      #                       threshold = paste(unique(cond$threshold), "%", sep = ""))
      #   }else{
      #     before_crop <- before_crop %>%
      #       image_threshold(type = "white",
      #                       threshold = paste(unique(cond$threshold), "%", sep = ""))%>%
      #       image_threshold(type = "black",
      #                       threshold = paste(unique(cond$threshold), "%", sep = ""))
      #   }}
      
      if(unique(cond$threshold_lower)!=0 | unique(cond$threshold_upper) != 1){
        if(img_info$colorspace != "Gray"){
          
          before_crop <- before_crop %>%
            image_convert(colorspace = "gray")
          
          if(unique(cond$threshold_upper)!=1){
            before_crop <- before_crop %>%
              image_threshold(type = "white",
                              threshold = paste(unique(cond$threshold_upper)*100, "%", sep = ""))
          }
          
          if(unique(cond$threshold_lower)!=0){
            lower.thresholding1 <- before_crop %>%
              image_threshold(type = "black",
                              threshold = paste(unique(cond$threshold_lower)*100, "%", sep = ""))
            
            lower.thresholding2 <- before_crop %>%
              image_threshold(type = "white",
                              threshold = paste(unique(cond$threshold_lower)*100-0.000001, "%", sep = "")) %>%
              image_threshold(type = "black",
                              threshold = paste(unique(cond$threshold_lower)*100-0.000001, "%", sep = ""))
            
            before_crop <- image_composite(lower.thresholding1, lower.thresholding2, "minus") %>%
              image_negate() 
            rm(lower.thresholding1);rm(lower.thresholding2)
          }
          
        }else{
          
          if(unique(cond$threshold_upper)!=1){
            before_crop <- before_crop %>%
              image_threshold(type = "white",
                              threshold = paste(unique(cond$threshold_upper)*100, "%", sep = ""))
          }
          
          if(unique(cond$threshold_lower)!=0){
            lower.thresholding1 <- before_crop %>%
              image_threshold(type = "black",
                              threshold = paste(unique(cond$threshold_lower)*100, "%", sep = ""))
            
            lower.thresholding2 <- before_crop %>%
              image_threshold(type = "white",
                              threshold = paste(unique(cond$threshold_lower)*100-0.000001, "%", sep = "")) %>%
              image_threshold(type = "black",
                              threshold = paste(unique(cond$threshold_lower)*100-0.000001, "%", sep = ""))
            
            before_crop <- image_composite(lower.thresholding1, lower.thresholding2, "minus") %>%
              image_negate() 
            rm(lower.thresholding1);rm(lower.thresholding2)
          }
          
        }}
      
      if(!is.null(cond$x) & !is.null(cond$y)){
        p1 <- rbind(c(0,0),
                    c(0, img_info$height),
                    c(img_info$width, img_info$height),
                    c(img_info$width, 0),
                    c(0,0))
        
        hole <- cbind(as.numeric(cond$x), as.numeric(cond$y))
        
        
        p1 <- list(p1, hole)
        pols <- st_sf(value = 1,
                      geometry = st_sfc(st_polygon(p1)))
        r <- raster(pols, res = 1)
        r <- fasterize(pols, r, field = "value", fun="sum")
        mask1 <- (!is.na(matrix(r, nrow = dim(r)[2], dim(r)[1], byrow = F))) %>%
          as.cimg() %>%
          cimg2magick() %>%
          image_flip() %>%
          image_flop()
        
        
        after_crop <- image_composite(before_crop, image_transparent(mask1, "black"), "over")
        
        return(list(ggplot_img = image_ggplot_modified(after_crop),
                    magick_img = after_crop,
                    mask = mask1,
                    mask_xy = hole))
      }else{
        
        return(list(ggplot_img = image_ggplot_modified(before_crop),
                    magick_img = before_crop,
                    mask = NULL,
                    mask_xy = NULL))
        
      }
    }
  }) %>%
    bindCache(input$file1, input$makeitgray, input$preinvert, input$Threshold1, v$complete.crop, input$loaded1_zoom_percent, image_data$single_click,
              input$file1_supp, cache = "app")
  
  plot1.f <- function(){
    if(v$complete.crop == TRUE){
      return(ggplot_with_points(loaded1_processed_plot()$ggplot_img,
                                image_data$single_click,
                                point_col = "red"))
    }else{
      if((is.null(v$imgclick.x) & is.null(v$imgclick.y))){
        return(ggplot_with_points(loaded1_processed_plot()$ggplot_img,
                                  image_data$single_click,
                                  point_col = "green"))
      }else{
        
        return(ggplot_with_points(loaded1_processed_plot()$ggplot_img,
                                  image_data$single_click,
                                  point_col = "blue"))
      }
    }
    
  }
  
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  observeEvent(input$coord_ref, {
    brush <- input$plot1_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymax, brush$ymin)
      
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  }) 
  
  output$plot1 <- renderPlot({
    if (!is.null(ranges$x) & !is.null(ranges$y)){
      plot1.f() +
        coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)
    }else{
      plot1.f()
    }
  })
  
  output$image_brush_info1 <- renderPlot({
    img <- loaded1_processed_plot()$magick_img %>%
      magick2cimg()
    out <- input$plot1_brush#v_help$last_coord # input$plot1_brush
    if(dim(img)[4] == 1 & !is.null(out)){
      img <- img %>%
        imsub(x>=out$xmin & x <= out$xmax)%>%
        imsub(y>=out$ymin & y <= out$ymax)
      
      lattice::levelplot(value ~ x*y ,
                         data=img %>% mirror('y') %>% as.data.frame(),
                         col.regions = gray(0:255/256),
                         scales=list(x = list(draw = FALSE),
                                     y = list(draw = FALSE)),
                         aspect = dim(img)[2]/dim(img)[1])
    }
  })
  
  
  
  #######################################################################
  
  
  ########### To plot FILE2 being edited in a reduced scale #############
  loaded2_processed_plot <- reactive({
    before_crop <- loaded2() %>%
      image_scale(geometry = paste(input$loaded2_zoom_percent, "%", sep = ""))
    img_info <- image_info(before_crop)
    
    if(is.null(input$file2_supp)){
      if(input$makeitgray2 == TRUE & img_info$colorspace != "Gray"){
        before_crop <- before_crop %>%
          image_convert(colorspace = "gray")
      }
      
      if(input$preinvert2 == TRUE){ # faster computation
        if(img_info$colorspace != "Gray"){
          before_crop <- before_crop %>%
            image_convert(colorspace = "gray") %>%
            image_negate()
        }else{
          before_crop <- before_crop %>%
            image_negate()
        }
      }
      
      
      if(min(input$Threshold2)!=0 | max(input$Threshold2)!= 1){
        if(img_info$colorspace != "Gray"){
          before_crop <- before_crop %>%
            image_convert(colorspace = "gray")
          
          if(max(input$Threshold2)!=1){
            before_crop <- before_crop %>%
              image_threshold(type = "white",
                              threshold = paste(max(input$Threshold2)*100, "%", sep = ""))
          }
          
          if(min(input$Threshold2)!=0){
            lower.thresholding1 <- before_crop %>%
              image_threshold(type = "black",
                              threshold = paste(min(input$Threshold2)*100, "%", sep = ""))
            
            lower.thresholding2 <- before_crop %>%
              image_threshold(type = "white",
                              threshold = paste(min(input$Threshold2)*100-0.000001, "%", sep = "")) %>%
              image_threshold(type = "black",
                              threshold = paste(min(input$Threshold2)*100-0.000001, "%", sep = ""))
            
            before_crop <- image_composite(lower.thresholding1, lower.thresholding2, "minus") %>%
              image_negate() 
            rm(lower.thresholding1);rm(lower.thresholding2)
          }
          
        }else{
          
          if(max(input$Threshold2)!=1){
            before_crop <- before_crop %>%
              image_threshold(type = "white",
                              threshold = paste(max(input$Threshold2)*100, "%", sep = ""))
          }
          
          if(min(input$Threshold2)!=0){
            lower.thresholding1 <- before_crop %>%
              image_threshold(type = "black",
                              threshold = paste(min(input$Threshold2)*100, "%", sep = ""))
            
            lower.thresholding2 <- before_crop %>%
              image_threshold(type = "white",
                              threshold = paste(min(input$Threshold2)*100-0.000001, "%", sep = "")) %>%
              image_threshold(type = "black",
                              threshold = paste(min(input$Threshold2)*100-0.000001, "%", sep = ""))
            
            before_crop <- image_composite(lower.thresholding1, lower.thresholding2, "minus") %>%
              image_negate() 
            rm(lower.thresholding1);rm(lower.thresholding2)
          }
        }}
      
      
      
      if(u$complete.crop == TRUE){
        p1 <- rbind(c(0,0),
                    c(0, img_info$height),
                    c(img_info$width, img_info$height),
                    c(img_info$width, 0),
                    c(0,0))
        
        hole <- cbind(u$imgclick.x, u$imgclick.y); hole <- rbind(hole, hole[1,])
        
        p1 <- list(p1, hole)
        pols <- st_sf(value = 1,
                      geometry = st_sfc(st_polygon(p1)))
        r <- raster(pols, res = 1)
        r <- fasterize(pols, r, field = "value", fun="sum")
        mask1 <- (!is.na(matrix(r, nrow = dim(r)[2], dim(r)[1], byrow = F))) %>%
          as.cimg() %>%
          cimg2magick() %>%
          image_flip() %>%
          image_flop()
        
        
        after_crop <- image_composite(before_crop, image_transparent(mask1, "black"), "over")
        
        return(list(ggplot_img = image_ggplot_modified(after_crop),
                    magick_img = after_crop,
                    mask = mask1,
                    mask_xy = hole))
      }else{
        
        return(list(ggplot_img = image_ggplot_modified(before_crop),
                    magick_img = before_crop, 
                    mask = NULL,
                    mask_xy = NULL))
      }
    }else{
      cond <- loaded2_process_info()
      
      if(unique(cond$grayscale) == TRUE & img_info$colorspace != "Gray"){
        before_crop <- before_crop %>%
          image_convert(colorspace = "gray")
      }
      
      if(unique(cond$invert) == TRUE){ # faster computation
        if(img_info$colorspace != "Gray"){
          before_crop <- before_crop %>%
            image_convert(colorspace = "gray") %>%
            image_negate()
        }else{
          before_crop <- before_crop %>%
            image_negate()
        }
      }
      
      
      if(unique(cond$threshold_lower)!=0 | unique(cond$threshold_upper) != 1){
        if(img_info$colorspace != "Gray"){
          
          before_crop <- before_crop %>%
            image_convert(colorspace = "gray")
          
          if(unique(cond$threshold_upper)!=1){
            before_crop <- before_crop %>%
              image_threshold(type = "white",
                              threshold = paste(unique(cond$threshold_upper)*100, "%", sep = ""))
          }
          
          if(unique(cond$threshold_lower)!=0){
            lower.thresholding1 <- before_crop %>%
              image_threshold(type = "black",
                              threshold = paste(unique(cond$threshold_lower)*100, "%", sep = ""))
            
            lower.thresholding2 <- before_crop %>%
              image_threshold(type = "white",
                              threshold = paste(unique(cond$threshold_lower)*100-0.000001, "%", sep = "")) %>%
              image_threshold(type = "black",
                              threshold = paste(unique(cond$threshold_lower)*100-0.000001, "%", sep = ""))
            
            before_crop <- image_composite(lower.thresholding1, lower.thresholding2, "minus") %>%
              image_negate() 
            rm(lower.thresholding1);rm(lower.thresholding2)
          }
          
        }else{
          
          if(unique(cond$threshold_upper)!=1){
            before_crop <- before_crop %>%
              image_threshold(type = "white",
                              threshold = paste(unique(cond$threshold_upper)*100, "%", sep = ""))
          }
          
          if(unique(cond$threshold_lower)!=0){
            lower.thresholding1 <- before_crop %>%
              image_threshold(type = "black",
                              threshold = paste(unique(cond$threshold_lower)*100, "%", sep = ""))
            
            lower.thresholding2 <- before_crop %>%
              image_threshold(type = "white",
                              threshold = paste(unique(cond$threshold_lower)*100-0.000001, "%", sep = "")) %>%
              image_threshold(type = "black",
                              threshold = paste(unique(cond$threshold_lower)*100-0.000001, "%", sep = ""))
            
            before_crop <- image_composite(lower.thresholding1, lower.thresholding2, "minus") %>%
              image_negate() 
            rm(lower.thresholding1);rm(lower.thresholding2)
          }
          
        }}
      
      if(!is.null(cond$x) & !is.null(cond$y)){
        p1 <- rbind(c(0,0),
                    c(0, img_info$height),
                    c(img_info$width, img_info$height),
                    c(img_info$width, 0),
                    c(0,0))
        
        # hole <- cbind(as.numeric(cond$x), as.numeric(cond$y))
        
        hole <- cbind(sapply(as.numeric(cond$x), function(x) as.numeric(as.integer(x * input$loaded2_zoom_percent/100))),
                      sapply(as.numeric(cond$y), function(x) as.numeric(as.integer(x * input$loaded2_zoom_percent/100)))) ;  hole <- rbind(hole, hole[1,])
        
        p1 <- list(p1, hole)
        pols <- st_sf(value = 1,
                      geometry = st_sfc(st_polygon(p1)))
        r <- raster(pols, res = 1)
        r <- fasterize(pols, r, field = "value", fun="sum")
        mask1 <- (!is.na(matrix(r, nrow = dim(r)[2], dim(r)[1], byrow = F))) %>%
          as.cimg() %>%
          cimg2magick() %>%
          image_flip() %>%
          image_flop()
        
        
        after_crop <- image_composite(before_crop, image_transparent(mask1, "black"), "over")
        
        return(list(ggplot_img = image_ggplot_modified(after_crop),
                    magick_img = after_crop,
                    mask = mask1,
                    mask_xy = hole))
      }else{
        
        return(list(ggplot_img = image_ggplot_modified(before_crop),
                    magick_img = before_crop,
                    mask = NULL,
                    mask_xy = NULL))
        
      }
      
    }
  }) %>%
    bindCache(input$file2, input$makeitgray2, input$preinvert2, input$Threshold2, u$complete.crop, input$loaded2_zoom_percent, image_data2$single_click,
              input$file2_supp, cache = "app")
  
  
  loaded2_processed <- reactive({
    
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    
    progress$set(message = "Processing Questioned print..", value = 0.3)
    s_time <- Sys.time()
    
    before_crop <- loaded2() 
    img_info <- image_info(before_crop)
    
    if(is.null(input$file2_supp)){
      if(input$makeitgray2 == TRUE & img_info$colorspace != "Gray"){
        before_crop <- before_crop %>%
          image_convert(colorspace = "gray")
      }
      
      if(input$preinvert2 == TRUE){ # faster computation
        if(img_info$colorspace != "Gray"){
          before_crop <- before_crop %>%
            image_convert(colorspace = "gray") %>%
            image_negate()
        }else{
          before_crop <- before_crop %>%
            image_negate()
        }
      }
      
      
      if(min(input$Threshold2)!=0 | max(input$Threshold2) != 1){
        if(img_info$colorspace != "Gray"){
          # before_crop <- before_crop %>%
          #   image_convert(colorspace = "gray") %>%
          #   image_threshold(type = "white",
          #                   threshold = paste(input$Threshold2, "%", sep = ""))%>%
          #   image_threshold(type = "black",
          #                   threshold = paste(input$Threshold2, "%", sep = ""))
          
          
          before_crop <- before_crop %>%
            image_convert(colorspace = "gray")
          
          if(max(input$Threshold2)!=1){
            before_crop <- before_crop %>%
              image_threshold(type = "white",
                              threshold = paste(max(input$Threshold2)*100, "%", sep = ""))
          }
          
          if(min(input$Threshold2)!=0){
            lower.thresholding1 <- before_crop %>%
              image_threshold(type = "black",
                              threshold = paste(min(input$Threshold2)*100, "%", sep = ""))
            
            lower.thresholding2 <- before_crop %>%
              image_threshold(type = "white",
                              threshold = paste(min(input$Threshold2)*100-0.000001, "%", sep = "")) %>%
              image_threshold(type = "black",
                              threshold = paste(min(input$Threshold2)*100-0.000001, "%", sep = ""))
            
            before_crop <- image_composite(lower.thresholding1, lower.thresholding2, "minus") %>%
              image_negate() 
            rm(lower.thresholding1);rm(lower.thresholding2)
          }
        }else{
          # before_crop <- before_crop %>%
          #   image_threshold(type = "white",
          #                   threshold = paste(input$Threshold2, "%", sep = ""))%>%
          #   image_threshold(type = "black",
          #                   threshold = paste(input$Threshold2, "%", sep = ""))
          
          
          if(max(input$Threshold2)!=1){
            before_crop <- before_crop %>%
              image_threshold(type = "white",
                              threshold = paste(max(input$Threshold2)*100, "%", sep = ""))
          }
          
          if(min(input$Threshold2)!=0){
            lower.thresholding1 <- before_crop %>%
              image_threshold(type = "black",
                              threshold = paste(min(input$Threshold2)*100, "%", sep = ""))
            
            lower.thresholding2 <- before_crop %>%
              image_threshold(type = "white",
                              threshold = paste(min(input$Threshold2)*100-0.000001, "%", sep = "")) %>%
              image_threshold(type = "black",
                              threshold = paste(min(input$Threshold2)*100-0.000001, "%", sep = ""))
            
            before_crop <- image_composite(lower.thresholding1, lower.thresholding2, "minus") %>%
              image_negate() 
            rm(lower.thresholding1);rm(lower.thresholding2)
          }
        }}
      
      if(u$complete.crop == TRUE){
        
        
        d1 <- Sys.time()-s_time; d1.t <- round(d1, 2); d1.u <- units(d1)
        progress$inc(0.4, detail = paste("Cropping..(", d1.t, " ", d1.u, ").", sep = ""))
        
        p1 <- rbind(c(0,0),
                    c(0, img_info$height),
                    c(img_info$width, img_info$height),
                    c(img_info$width, 0),
                    c(0,0))
        
        #hole <- cbind(u$imgclick.x, u$imgclick.y); hole <- rbind(hole, hole[1,])
        
        hole <- cbind(sapply(u$imgclick.x, function(x) as.numeric(as.integer(min(c(x * 100/input$loaded2_zoom_percent, img_info$width))))),
                      sapply(u$imgclick.y, function(x) as.numeric(as.integer(min(c(x * 100/input$loaded2_zoom_percent, img_info$height)))))) ;  hole <- rbind(hole, hole[1,])
        
        
        
        p1 <- list(p1, hole)
        pols <- st_sf(value = 1,
                      geometry = st_sfc(st_polygon(p1)))
        r <- raster(pols, res = 1)
        r <- fasterize(pols, r, field = "value", fun="sum")
        mask1 <- (!is.na(matrix(r, nrow = dim(r)[2], dim(r)[1], byrow = F))) %>%
          as.cimg() %>%
          cimg2magick() %>%
          image_flip() %>%
          image_flop()
        
        
        after_crop <- image_composite(before_crop, image_transparent(mask1, "black"), "over")
        
        
        d1 <- Sys.time()-s_time; d1.t <- round(d1, 2); d1.u <- units(d1)
        progress$inc(0.3, detail = paste("Complete..(", d1.t, " ", d1.u, ").", sep = ""))
        
        return(list(ggplot_img = image_ggplot_modified(after_crop),
                    magick_img = after_crop,
                    mask = mask1,
                    mask_xy = hole))
      }else{
        
        
        d1 <- Sys.time()-s_time; d1.t <- round(d1, 2); d1.u <- units(d1)
        progress$inc(0.7, detail = paste("Complete..(", d1.t, " ", d1.u, ").", sep = ""))
        return(list(ggplot_img = image_ggplot_modified(before_crop),
                    magick_img = before_crop, 
                    mask = NULL,
                    mask_xy = NULL))
      }
    }else{
      cond <- loaded2_process_info()
      
      if(unique(cond$grayscale) == TRUE & img_info$colorspace != "Gray"){
        before_crop <- before_crop %>%
          image_convert(colorspace = "gray")
      }
      
      if(unique(cond$invert) == TRUE){ # faster computation
        if(img_info$colorspace != "Gray"){
          before_crop <- before_crop %>%
            image_convert(colorspace = "gray") %>%
            image_negate()
        }else{
          before_crop <- before_crop %>%
            image_negate()
        }
      }
      
      # if(unique(cond$threshold)<100){
      #   if(img_info$colorspace != "Gray"){
      #     before_crop <- before_crop %>%
      #       image_convert(colorspace = "gray") %>%
      #       image_threshold(type = "white",
      #                       threshold = paste(unique(cond$threshold), "%", sep = ""))%>%
      #       image_threshold(type = "black",
      #                       threshold = paste(unique(cond$threshold), "%", sep = ""))
      #   }else{
      #     before_crop <- before_crop %>%
      #       image_threshold(type = "white",
      #                       threshold = paste(unique(cond$threshold), "%", sep = "")) %>%
      #       image_threshold(type = "black",
      #                       threshold = paste(unique(cond$threshold), "%", sep = ""))
      #   }}
      
      if(unique(cond$threshold_lower)!=0 | unique(cond$threshold_upper) != 1){
        if(img_info$colorspace != "Gray"){
          
          before_crop <- before_crop %>%
            image_convert(colorspace = "gray")
          
          if(unique(cond$threshold_upper)!=1){
            before_crop <- before_crop %>%
              image_threshold(type = "white",
                              threshold = paste(unique(cond$threshold_upper)*100, "%", sep = ""))
          }
          
          if(unique(cond$threshold_lower)!=0){
            lower.thresholding1 <- before_crop %>%
              image_threshold(type = "black",
                              threshold = paste(unique(cond$threshold_lower)*100, "%", sep = ""))
            
            lower.thresholding2 <- before_crop %>%
              image_threshold(type = "white",
                              threshold = paste(unique(cond$threshold_lower)*100-0.000001, "%", sep = "")) %>%
              image_threshold(type = "black",
                              threshold = paste(unique(cond$threshold_lower)*100-0.000001, "%", sep = ""))
            
            before_crop <- image_composite(lower.thresholding1, lower.thresholding2, "minus") %>%
              image_negate() 
            rm(lower.thresholding1);rm(lower.thresholding2)
          }
          
        }else{
          
          if(unique(cond$threshold_upper)!=1){
            before_crop <- before_crop %>%
              image_threshold(type = "white",
                              threshold = paste(unique(cond$threshold_upper)*100, "%", sep = ""))
          }
          
          if(unique(cond$threshold_lower)!=0){
            lower.thresholding1 <- before_crop %>%
              image_threshold(type = "black",
                              threshold = paste(unique(cond$threshold_lower)*100, "%", sep = ""))
            
            lower.thresholding2 <- before_crop %>%
              image_threshold(type = "white",
                              threshold = paste(unique(cond$threshold_lower)*100-0.000001, "%", sep = "")) %>%
              image_threshold(type = "black",
                              threshold = paste(unique(cond$threshold_lower)*100-0.000001, "%", sep = ""))
            
            before_crop <- image_composite(lower.thresholding1, lower.thresholding2, "minus") %>%
              image_negate() 
            rm(lower.thresholding1);rm(lower.thresholding2)
          }
          
        }}
      
      if(!is.null(cond$x) & !is.null(cond$y)){
        p1 <- rbind(c(0,0),
                    c(0, img_info$height),
                    c(img_info$width, img_info$height),
                    c(img_info$width, 0),
                    c(0,0))
        
        hole <- cbind(as.numeric(cond$x), as.numeric(cond$y))
        
        p1 <- list(p1, hole)
        pols <- st_sf(value = 1,
                      geometry = st_sfc(st_polygon(p1)))
        r <- raster(pols, res = 1)
        r <- fasterize(pols, r, field = "value", fun="sum")
        mask1 <- (!is.na(matrix(r, nrow = dim(r)[2], dim(r)[1], byrow = F))) %>%
          as.cimg() %>%
          cimg2magick() %>%
          image_flip() %>%
          image_flop()
        
        
        after_crop <- image_composite(before_crop, image_transparent(mask1, "black"), "over")
        
        return(list(ggplot_img = image_ggplot_modified(after_crop),
                    magick_img = after_crop,
                    mask = mask1,
                    mask_xy = hole))
      }else{
        
        return(list(ggplot_img = image_ggplot_modified(before_crop),
                    magick_img = before_crop,
                    mask = NULL,
                    mask_xy = NULL))
        
      }
      
    }
  }) %>%
    bindCache(input$file2, input$makeitgray2, input$preinvert2, input$Threshold2, u$complete.crop, input$loaded2_zoom_percent, image_data2$single_click,
              input$file2_supp, cache = "app")
  
  
  plot2.f <- function(){
    if(u$complete.crop == TRUE){
      return(ggplot_with_points(loaded2_processed_plot()$ggplot_img, 
                                image_data2$single_click,
                                point_col = "red"))
    }else{
      if((is.null(u$imgclick.x) & is.null(u$imgclick.y))){
        return(ggplot_with_points(loaded2_processed_plot()$ggplot_img, 
                                  image_data2$single_click,
                                  point_col = "green"))
      }else{
        
        return(ggplot_with_points(loaded2_processed_plot()$ggplot_img, 
                                  image_data2$single_click,
                                  point_col = "blue"))
      }
    }
  }
  
  
  ranges2 <- reactiveValues(x = NULL, y = NULL)
  
  observeEvent(input$coord_questioned, {
    brush <- input$plot2_brush
    if (!is.null(brush)) {
      ranges2$x <- c(brush$xmin, brush$xmax)
      ranges2$y <- c(brush$ymax, brush$ymin)
      
    } else {
      ranges2$x <- NULL
      ranges2$y <- NULL
    }
  }) 
  
  output$plot2 <- renderPlot({
    if (!is.null(ranges2$x) & !is.null(ranges2$y)){
      plot2.f() +
        coord_cartesian(xlim = ranges2$x, ylim = ranges2$y, expand = FALSE)
    }else{
      plot2.f()
    }
  })
  
  output$image_brush_info2 <- renderPlot({
    img <- loaded2_processed_plot()$magick_img %>%
      magick2cimg()
    out <- input$plot2_brush#v_help$last_coord # input$plot1_brush
    if(dim(img)[4] == 1 & !is.null(out)){
      img <- img %>%
        imsub(x>=out$xmin & x <= out$xmax)%>%
        imsub(y>=out$ymin & y <= out$ymax)
      
      lattice::levelplot(value ~ x*y ,
                         data=img %>% mirror('y') %>% as.data.frame(),
                         col.regions = gray(0:255/256),
                         scales=list(x = list(draw = FALSE),
                                     y = list(draw = FALSE)),
                         aspect = dim(img)[2]/dim(img)[1])
    }
  })
  #######################################################################
  
  
  ########### Downscale factor for alignment #############
  # downscale_reactive <- eventReactive(input$submit,{
  # 
  #   paste("During alignment, your images will be reduced by ",
  #         input$Entered_scale_down_factor,
  #         "(", eval(parse(text =input$Entered_scale_down_factor)) *100, "%).", sep = "")
  # })
  downscale_reactive <- reactive({
    input$Entered_scale_down_factor
    
    return(paste("During alignment, your images will be reduced by ",
                 input$Entered_scale_down_factor,
                 "(", eval(parse(text =input$Entered_scale_down_factor)) *100, "%).", sep = ""))
  })
  output$downscale_info <- renderPrint({
    cat(downscale_reactive())
  })
  # output$downscale_info <- renderPrint({
  #   cat(paste("During alignment, your images will be reduced by ",
  #              input$Entered_scale_down_factor, 
  #              "(", eval(parse(text =input$Entered_scale_down_factor)) *100, "%).", sep = ""))
  # })
  ########################################################
  
  
  
  ########### To find the best alignment between the edited FILE1 and FILE2 #############
  aligned_res <- reactive({
    
    res <- wrapper_f(loaded1_processed()$magick_img,
                     loaded2_processed()$magick_img,
                     loaded1(),
                     loaded2(),
                     scale_down_factor = eval(parse(text = input$Entered_scale_down_factor)),
                     seq(min(input$Angle_range), max(input$Angle_range), by = 0.5))
    
    return(list(processed_res = res,
                temporary_dir = temp_directory))
    
    
  }) %>%
    bindCache(input$file1, input$makeitgray, input$preinvert, input$Threshold1, v$complete.crop,
              input$file2, input$makeitgray2, input$preinvert2, input$Threshold2, u$complete.crop,
              input$Entered_scale_down_factor, cache = "app") %>%
    bindEvent(input$submit)
  
  
  
  # The transformation information
  transform_information <- function(){
    transformed <- aligned_res()$processed_res$transformed_info
    return(paste("By the automated alignment, the questioned image was rotated by ",
                 transformed$Angle, " degrees and shifted with horizontal shift = ", transformed$X, " and ",
                 "vertical shift = ", -transformed$Y, ".", sep = "")) # -Y because the origin (0,0) is located at top-left corner, but in common sense, origin is at bottom-left corner.
  }
  output$transform_information <- renderText({
    if(is.character(transform_information())){
      print(transform_information())
    }
  })
  
  # The similarity score between the aligned images
  similarity_information <- function(){
    similarity_score <- aligned_res()$processed_res$similarity_cor
    return(paste("The Pearson correlation coefficient between two pre-processed images is ",
                 round(similarity_score, 4), ".", sep = ""))
  }
  output$similarity_information <- renderText({
    if(is.character(similarity_information())){
      print(similarity_information())
    }
  })
  
  #######################################################################################
  
  output$plot4 <- renderPlot({
    
    par(mar = c(0,0,0,0))
    if(is.numeric(aligned_res()$processed_res$similarity_cor)){
      progress <- shiny::Progress$new()
      # Make sure it closes when we exit this reactive, even if there's an error
      on.exit(progress$close())
      
      progress$set(message = "Plotting in progress..", value = 0)
      
      plot(aligned_res()$processed_res$colored_res$ref %>%
             determine.display.scale.f(), axes = FALSE)
      
      progress$inc(0.25)
      
    }
    
  })
  output$plot5 <- renderPlot({
    par(mar = c(0,0,0,0))
    if(is.numeric(aligned_res()$processed_res$similarity_cor)){
      progress <- shiny::Progress$new()
      # Make sure it closes when we exit this reactive, even if there's an error
      on.exit(progress$close())
      
      progress$set(message = "Plotting in progress..", value = .25)
      plot(aligned_res()$processed_res$colored_res$questioned %>%
             determine.display.scale.f(), axes = FALSE)
      
      progress$inc(0.25)
    }
  })
  output$plot6 <- renderPlot({
    par(mar = c(0,0,0,0))
    if(is.numeric(aligned_res()$processed_res$similarity_cor)){
      progress <- shiny::Progress$new()
      # Make sure it closes when we exit this reactive, even if there's an error
      on.exit(progress$close())
      
      progress$set(message = "Plotting in progress..", value = .5)
      plot(aligned_res()$processed_res$colored_res$overlayed %>%
             determine.display.scale.f(), axes = FALSE)
      
      progress$inc(0.25)
      
    }
  })
  #######################################################################################
  
  
  # ########### To plot the alignment results from the original FILE1 and FILE2 #############
  
  output$plot7 <- renderPlot({
    if(!is.null(input$file1)){
      plot(loaded1())
    }
  })
  
  # output$plot7 <- plotlyOutput({
  #   tmp <- ggplot_with_points(image_ggplot_modified(loaded1()), 
  #                      as.data.frame(x_value = 0, y_value = 0),
  #                      point_col = "green")
  #   plotly::toWebGL(tmp)
  #   
  # })
  # 
  output$plot8 <- renderPlot({
    if(!is.null(input$file2)){
      plot(loaded2())
    }
  })
  
  output$plot9 <- renderPlot({
    par(mar = c(0,0,0,0))
    if(is.numeric(aligned_res()$processed_res$similarity_cor)){
      progress <- shiny::Progress$new()
      # Make sure it closes when we exit this reactive, even if there's an error
      on.exit(progress$close())
      
      progress$set(message = "Plotting almost done..", value = .75)
      # plot(aligned_res()$processed_res$overlayed_org %>%
      #        determine.display.scale.f(), axes = FALSE)
      
      plot(aligned_res()$processed_res$overlayed_org %>%
             determine.display.scale.f(), axes = FALSE)
      progress$inc(0.25)
    }
    
    
  })
  
  
  #########################################################################################
  
  
  
  ########### To download the edited FILE1 and FILE2, and alignment results #############
  rm.extension <- function(x){
    x.split <- strsplit(x, "\\.")[[1]]
    x.split <- x.split[-length(x.split)]
    return(paste(x.split, collapse = "."))
  }
  
  
  output$download_btn <- downloadHandler(
    filename = function(){
      paste("ShoeprintAnalyzr", format(Sys.time(),'_%Y%m%d_%H%M%S'), ".zip", sep = "")
    },
    content = function(file){
      
      
      time_now <- format(Sys.time(),'tmp_%Y%m%d_%H%M%S')
      if(get_os() != "win64"){
        temp_directory_time <- paste(temp_directory, "/", time_now, sep = "")
        dir.create(temp_directory_time)
        original_f <- paste(temp_directory_time, "/", "Original", sep = ""); dir.create(original_f)
        processed_f <- paste(temp_directory_time, "/", "Processed", sep = ""); dir.create(processed_f)
        mask_f <- paste(temp_directory_time, "/", "Mask", sep = ""); dir.create(mask_f)
      }else{
        temp_directory_time <- paste(temp_directory, "\\", time_now, sep = "")
        dir.create(temp_directory_time)
        original_f <- paste(temp_directory_time, "\\", "Original", sep = ""); dir.create(original_f)
        processed_f <- paste(temp_directory_time, "\\", "Processed", sep = ""); dir.create(processed_f)
        mask_f <- paste(temp_directory_time, "\\", "Mask", sep = ""); dir.create(mask_f)
      }
      
      progress <- shiny::Progress$new()
      # Make sure it closes when we exit this reactive, even if there's an error
      on.exit(progress$close())
      
      progress$set(message = "Exporting shoeprints..", value = 0.1)
      
      
      img.list <- list(aligned_res()$processed_res$colored_res$ref,
                       aligned_res()$processed_res$colored_res$questioned,
                       aligned_res()$processed_res$colored_res$overlayed,
                       aligned_res()$processed_res$ref_images$org_padded,
                       aligned_res()$processed_res$questioned_images$org_padded,
                       aligned_res()$processed_res$questioned_images$aligned_org,
                       aligned_res()$processed_res$overlayed_org)
      
      filename.list <- list(paste("blue_", rm.extension(input$file1$name), sep = ""),
                            paste("orange_", rm.extension(input$file2$name), sep = ""),
                            "overlay_blue_orange",
                            paste("original_", rm.extension(input$file1$name), sep = ""),
                            paste("original_", rm.extension(input$file2$name), sep = ""),
                            paste("original_aligned_", rm.extension(input$file2$name), sep = ""),
                            "overlay_original")
      
      file.folder.list <- c(rep(processed_f, 3),
                            rep(original_f, 4))
      for(i in 1:length(filename.list)){
        imager::save.image(img.list[[i]],
                           file.path(file.folder.list[[i]], paste(filename.list[[i]], ".jpeg", sep = "")),
                           quality = 0.7)
        progress$inc(0.5/length(filename.list), 
                     detail = paste("(", round(100*i/length(filename.list)), "%)..", sep = ""))
      }
      
      progress$inc(0.1, message = "Exporting binary masks..", detail = NULL)
      if(!is.null(loaded1_processed()$mask)){
        image_write(loaded1_processed()$mask,
                    file.path(mask_f, paste("m_", rm.extension(input$file1$name), ".jpeg", sep = "")))
      }
      if(!is.null(loaded2_processed()$mask)){
        image_write(loaded2_processed()$mask,
                    file.path(mask_f, paste("m_", rm.extension(input$file2$name), ".jpeg", sep = "")))
      }
      
      progress$inc(0.1, message = "Exporting diagnostic plots..", detail = NULL)
      
      pdf(file.path(temp_directory_time, "Diagnose_reduced_scale.pdf"))
      plot(aligned_res()$processed_res$align_info_p$max.cross.corr_angle,
           xlab = "rotation angle",
           ylab = "maximum cross-correlation in a reduced scale")
      axis(1, at = seq(min(aligned_res()$processed_res$align_info_p$max.cross.corr_angle$angle),
                       max(aligned_res()$processed_res$align_info_p$max.cross.corr_angle$angle),
                       by = 5), las=1)
      
      print(aligned_res()$processed_res$align_info_p$cross.corr_map_angle)
      
      dev.off()
      
      transformed <- aligned_res()$processed_res
      
      
      progress$inc(0.1, message = "Exporting alignment information..", detail = NULL)
      
      
      if(!is.null(input$file1_supp)){
        cond <- loaded1_process_info()
        gr <- unique(cond$grayscale)
        ir <- unique(cond$invert)
        tlr <- unique(cond$threshold_lower)
        tur <- unique(cond$threshold_upper)
        rm(cond)
      }else{
        gr <- input$makeitgray
        ir <- input$preinvert
        tlr <- min(input$Threshold1)
        tur <- max(input$Threshold1)
      }
      if(!is.null(input$file2_supp)){
        cond <- loaded2_process_info() 
        gq <- unique(cond$grayscale)
        iq <- unique(cond$invert)
        tlq <- unique(cond$threshold_lower)
        tuq <- unique(cond$threshold_upper)
        rm(cond)
      }else{
        gq <- input$makeitgray2
        iq <- input$preinvert2
        tlq <- min(input$Threshold2)
        tuq <- max(input$Threshold2)
      }
      
      out_report <- data.frame(date = Sys.Date(),
                               task_id = generate_unique_id(paste(input$file1$name,
                                                                  input$file2$name,
                                                                  time_now,
                                                                  Sys.getpid(), sep = "")),
                               reference = input$file1$name,
                               questioned = input$file2$name,
                               grayscale_R= gr,
                               invert_R = ir,
                               threshold_lower_R = tlr,
                               threshold_upper_R = tur,
                               grayscale_Q= gq,
                               invert_Q = iq,
                               threshold_lower_Q = tlq,
                               threshold_upper_Q = tuq,
                               rotation = transformed$transformed_info$Angle,
                               shift_x = transformed$transformed_info$X,
                               shift_y = -transformed$transformed_info$Y,
                               pearson_corr = round(transformed$similarity_cor, 4),
                               downscale_factor = eval(parse(text = input$Entered_scale_down_factor))) %>%
        mutate(shift_x_downscale = transformed$transformed_info$X * downscale_factor,
               shift_y_downscale = -transformed$transformed_info$Y * downscale_factor,
               downscale_factor = as.character(MASS::fractions(downscale_factor)))
      
      
      xlsx::write.xlsx(out_report,
                       file = file.path(temp_directory_time, "alignment_metadata.xlsx"),
                       sheetName = "pair_info",
                       col.names = TRUE,
                       row.names = FALSE)
      
      
      if(!is.null(loaded1_processed()$mask_xy)){
        xlsx::write.xlsx(loaded1_processed()$mask_xy %>%
                           as.data.frame() %>%
                           rename(x = V1, y = V2),
                         file = file.path(temp_directory_time, "alignment_metadata.xlsx"),
                         sheetName = "crop_info_R",
                         col.names = TRUE,
                         row.names = FALSE,
                         append = TRUE)
        
        write.table(loaded1_processed()$mask_xy %>%
                      as.data.frame() %>%
                      rename(x = V1, y = V2) %>%
                      mutate(grayscale= gr,
                             invert = ir,
                             threshold_lower = tlr,
                             threshold_upper = tur),
                    file = file.path(temp_directory_time, paste("p_info_", rm.extension(input$file1$name), ".txt", sep = "")),
                    row.names = FALSE,
                    col.names = TRUE)
      }
      
      
      if(!is.null(loaded2_processed()$mask_xy)){
        xlsx::write.xlsx(loaded2_processed()$mask_xy %>%
                           as.data.frame() %>%
                           rename(x = V1, y = V2),
                         file = file.path(temp_directory_time, "alignment_metadata.xlsx"),
                         sheetName = "crop_info_Q",
                         col.names = TRUE,
                         row.names = FALSE,
                         append = TRUE)
        
        write.table(loaded2_processed()$mask_xy %>%
                      as.data.frame() %>%
                      rename(x = V1, y = V2) %>%
                      mutate(grayscale= gq,
                             invert = iq,
                             threshold_lower = tlq,
                             threshold_upper = tuq),
                    file = file.path(temp_directory_time, paste("p_info_", rm.extension(input$file2$name), ".txt", sep = "")),
                    row.names = FALSE,
                    col.names = TRUE)
      }
      
      
      progress$inc(0.1, message = "Zipping everything..")
      
      
      zip::zip(
        zipfile = file,
        files = dir(temp_directory_time),
        root = temp_directory_time
      )
      
      
      unlink(temp_directory_time, recursive = TRUE) # Remove the files saved temporarily.
      
      
      progress$inc(0.05, message = "Complete!", detail = NULL)
    },
    contentType = "application/zip"
    
  )
  #######################################################################################
}


# Run the application 
shinyApp(ui, server)



