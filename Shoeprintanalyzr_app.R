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
library(openxlsx) # library(xlsx) was replaced.
library(shinycssloaders)
library(imagefx)
library(zip)
library(pracma)

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
# 
# # To create a temporary directory 
temp_directory <- file.path(tempdir(), as.integer(Sys.Date()))
dir.create(temp_directory)
cat(temp_directory)
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
                              accept = c("image/png", "image/jpeg", "image/jpg", ".png", ".jpg", "jpeg", ".tiff")),
                    div(fileInput(inputId = "file2_supp", 
                                  label = tags$span(icon("upload"), "Optional: processing info (txt)"), 
                                  accept = c(".txt")), style="color: #555759;"),
                    numericInput(inputId = "loaded2_zoom_percent", 
                                 label = tags$span(
                                   icon("magnifying-glass"), "Image Viewing Scale (%)", 
                                   tags$i(class = "glyphicon glyphicon-question-sign", 
                                          style = "color:#0072B2;",
                                          title = "To expedite image processing, the image on the right is displayed at the reduced scale you've chosen. Larger values may extend processing time but reduce discrepancies in your processed image when viewed at its original scale. min = 1%, max = 100%.")), 
                                 min = 1, max = 100, value = 10),
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
                              accept = c("image/png", "image/jpeg", "image/jpg", ".png", ".jpg", "jpeg", ".tiff")),
                    div(fileInput(inputId = "file1_supp", 
                                  label = tags$span(icon("upload"), "Optional: processing info (txt)"), 
                                  accept = c(".txt")), style="color: #555759;"),
                    numericInput(inputId = "loaded1_zoom_percent", 
                                 label = tags$span(
                                   icon("magnifying-glass"), "Image Viewing Scale (%)", 
                                   tags$i(class = "glyphicon glyphicon-question-sign", 
                                          style = "color:#0072B2;",
                                          title = "To expedite image processing, the image on the right is displayed at the reduced scale you've chosen. Larger values may extend processing time but reduce discrepancies in your processed image when viewed at its original scale. min = 1%, max = 100%.")), 
                                 min = 1, max = 100, value = 10),
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
                                                   width = "90%", height = "auto"), proxy.height = "50px")),
    column(3, align = "center",class = 'column_w_bar',
           h4(tags$span(
             icon("image"), "Reference", 
             tags$i(class = "glyphicon glyphicon-question-sign", 
                    style = "color:#0072B2;",
                    title = "Your original reference impression is displayed."))),
           shinycssloaders::withSpinner(plotOutput("plot7",
                                                   width = "90%", height = "auto"), proxy.height = "50px")),
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

xcorr3d_optimized <- function(img1 = NULL, img2, precomputed_img1_sd_fft = NULL){
  # Handle input cases: either img1 or precomputed values
  if (!is.null(img1)) {
    img1 <- img1 - mean(img1)
    img1_sd <- sd(as.vector(img1))
    precomputed_img1_fft <- Conj(fft(img1))
  } else if (!is.null(precomputed_img1_sd_fft)) {
    img1_sd <- precomputed_img1_sd_fft$sd
    precomputed_img1_fft <- precomputed_img1_sd_fft$fft
  } else {
    stop("Either img1 or precomputed_img1_sd_fft must be provided.")
  }
  
  # Subtract mean from img2
  img2 <- img2 - mean(img2)
  
  # Compute FFT and cross-correlation
  IMG2 <- fft(img2)
  R <- precomputed_img1_fft * IMG2
  r.shift <- fft(R, inverse = TRUE) / length(R)
  r <- imagefx::fftshift(Re(r.shift))
  
  # Compute normalization factor
  img2_sd <- sd(as.vector(img2))
  norm_factor <- length(img2) * img1_sd * img2_sd
  r.norm <- r / norm_factor
  
  # Find maximum correlation
  max.cor <- max(r.norm)
  max_index <- which.max(r.norm)
  max_inds_abs <- arrayInd(max_index, dim(r.norm)) - (dim(r.norm) / 2)
  
  # Adjust for zero frequency offsets
  zero.freq <- ifelse(dim(r.norm) %% 2 == 0, -1, -1.5)
  max.inds <- max_inds_abs + zero.freq
  max.inds <- max.inds[1, ]  # Handle multiple peaks
  
  # Return results
  return.list <- list(
    max.shifts = max.inds,
    max.corr = max.cor,
    corr.mat = r.norm
  )
  return(return.list)
}


add.color.f <- function(x){
  x_temp <- abind::abind(x, x, along = 4);
  x <- abind::abind(x_temp, x, along = 4); rm(x_temp);gc()
  return(x)
}


rm.extension <- function(x){
  x.split <- strsplit(x, "\\.")[[1]]
  x.split <- x.split[-length(x.split)]
  return(paste(x.split, collapse = "."))
}


downscale_image <- function(img, factor) {
  for (i in seq_len(log2(1/factor))) img <- img %>% resize_halfXY()
  return(img)
}

rotate_shift_image <- function(img, theta, dx, dy) {
  if(theta!=0){
    info <- image_info(img)
    img_width <- info$width
    img_height <- info$height
    if(theta>0){
      w <- as.integer(cos(theta)*img_width + img_height*sin(theta)); w <- w + w%%2 + 2
      h <- as.integer(sin(theta)*img_width + img_height*cos(theta)); h <- h + w%%2 + 2
    }else{
      w <- as.integer(cos(theta)*img_height + img_width*sin(theta)); w <- w + w%%2 + 2
      h <- as.integer(sin(theta)*img_height + img_width*cos(theta)); h <- h + w%%2 + 2
    }
    img <- img %>%
      image_extent(paste0(w,'x',h), color = "white"); gc()
    rotated <- image_rotate(img, theta); rm(img); gc()
    
    cx <- w/2
    cy <- h/2
    
    # Compute top-left crop coordinates
    x_crop <- as.integer(max(0, cx - img_width / 2))
    y_crop <- as.integer(max(0, cy - img_height / 2))
    
    # Crop the image
    rotated <- image_crop(rotated, sprintf("%dx%d+%d+%d", img_width, img_height, x_crop, y_crop)); gc()
  }else{
    rotated <- img; gc()
  }
  if(dx != 0 | dy != 0){
    # Get image dimensions
    info <- image_info(rotated)
    img_width <- info$width
    img_height <- info$height
    
    # Compute cropping coordinates
    crop_x <- ifelse(dx >= 0, 0, -dx)  # Crop from left if shifting right, from right if shifting left
    crop_y <- ifelse(dy >= 0, 0, -dy)  # Crop from top if shifting down, from bottom if shifting up
    crop_w <- img_width - abs(dx)  # Remaining width
    crop_h <- img_height - abs(dy) # Remaining height
    
    # Ensure valid cropping dimensions
    if (crop_w <= 0 || crop_h <= 0) {
      stop("Shift is too large; no image remains after cropping.")
    }
    
    # Crop the part that remains visible after the shift
    cropped <- image_crop(rotated, sprintf("%dx%d+%d+%d", crop_w, crop_h, crop_x, crop_y)); rm(rotated); gc()
    
    # Determine gravity based on shift direction
    gravity <- case_when(
      dx >= 0 & dy >= 0 ~ "SouthEast",  # Shift right & down
      dx >= 0 & dy < 0  ~ "NorthEast",  # Shift right & up
      dx < 0 & dy >= 0  ~ "SouthWest",  # Shift left & down
      dx < 0 & dy < 0  ~ "NorthWest"   # Shift left & up
    )
    
    # Extend back to original size with correct alignment
    shifted <- image_extent(cropped, sprintf("%dx%d", img_width, img_height), gravity = gravity, color = "white"); rm(cropped); gc()
    
    return(shifted)
  }else(return(rotated))
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

is_local <- function() {
  # If running in RStudio (interactive local session)
  if (interactive() && Sys.getenv("RSTUDIO") == "1") {
    return(TRUE)
  }
  
  # If running on shinyapps.io (or other deployment)
  if (!is.null(Sys.getenv("SHINY_PORT")) && Sys.getenv("SHINY_PORT") != "") {
    return(FALSE)
  }
  
  return(TRUE)  # Default to local if no environment is detected
}


# Server logic 
server <- function(input, output, session) {
  # # Memory cleanup when session ends
  # onStop(function() {
  #   print("Restarting R to free up memory.")
  #   .rs.restartR()  # Restarts the R session
  # }) # For local Shiny app with RSTUDIO
  # 
  # session$onSessionEnded(function() {
  #   # print("Session ended: Restarting app...")
  #   rsconnect::restartApp("ShoeprintAnalyzr")
  # }) # For deployed Shiny app
  if (is_local()) {
    onStop(function() {
      print("Restarting R to free up memory.")
      if (exists(".rs.restartR", mode = "function")) {
        .rs.restartR()  # Restart only if in RStudio
      } else {
        print("Restart function not available.")
      }
    })
  } else {
    session$onSessionEnded(function() {
      print("Session ended: Restarting app...")
      rsconnect::restartApp("ShoeprintAnalyzr")
    })
  }
  
  
  ########### To load FILE1 and create its downscale for faster display ################# 
  # Output the info of FILE1
  cached_img1 <- reactiveVal(); cached_info1 <- reactiveVal(); cached_view1 <- reactiveVal()
  loaded1_info_reactive <- observeEvent(input$file1,{
    file <- input$file1
    
    req(file)
    ext <- tolower(tools::file_ext(file$datapath))
    
    # Check MIME type
    mimetype <- mime::guess_type(file$datapath)
    
    validate(
      need(ext %in% c("png", "jpg", "jpeg", "tiff", "tif"), "Please upload a png, jpeg, tiff file."),
      need(mimetype %in% c("image/png", "image/jpeg", "image/tiff"), "Invalid MIME type.")
    )
    
    img1 <- magick::image_read(file$datapath) #  try(magick::image_read(file$datapath), silent = TRUE)
    R_viewing_reduced <- tempfile(pattern = "R", tmpdir =  temp_directory, fileext = ".png")
    cached_view1(R_viewing_reduced)
    image_write(img1 %>%
                  image_scale(geometry = "25%"), path = R_viewing_reduced); gc()
    cached_img1(file$datapath); cached_info1(magick::image_info(img1))
    rm(img1);gc()
  })
  
 
  
  loaded1_process_info <- reactive({
    file <- input$file1_supp
    ext <- tools::file_ext(file$datapath)
    
    req(file)
    validate(need(ext %in% c("txt"), "Please upload a txt file."))
    info <- read.table(file$datapath, header = TRUE)
    return(info)
  })
  
  output$loaded1_info <- renderPrint({
    req(cached_info1())
    print(cached_info1())
    
  })
  
  #######################################################################################  
  # Output the info of FILE1
  cached_img2 <- reactiveVal(); cached_info2 <- reactiveVal(); cached_view2 <- reactiveVal()
  loaded2_info_reactive <- observeEvent(input$file2,{
    file <- input$file2
    
    req(file)
    ext <- tolower(tools::file_ext(file$datapath))
    
    # Check MIME type
    mimetype <- mime::guess_type(file$datapath)
    
    validate(
      need(ext %in% c("png", "jpg", "jpeg", "tiff", "tif"), "Please upload a png, jpeg, tiff file."),
      need(mimetype %in% c("image/png", "image/jpeg", "image/tiff"), "Invalid MIME type.")
    )
    
    img2 <- magick::image_read(file$datapath) #  try(magick::image_read(file$datapath), silent = TRUE)
    Q_viewing_reduced <- tempfile(pattern = "Q", tmpdir =  temp_directory, fileext = ".png")
    cached_view2(Q_viewing_reduced)
    image_write(img2 %>%
                  image_scale(geometry = "25%"), path = Q_viewing_reduced); gc()
    cached_img2(file$datapath); cached_info2(magick::image_info(img2))
    rm(img2);gc()
  })
  loaded2_process_info <- reactive({
    file <- input$file2_supp
    ext <- tools::file_ext(file$datapath)
    
    req(file)
    validate(need(ext %in% c("txt"), "Please upload a txt file."))
    info <- read.table(file$datapath, header = TRUE)
    return(info)
  })
  
  # Output the info of FILE2
  output$loaded2_info <- renderPrint({
    req(cached_info2())
    print(cached_info2())
    
  })
  #######################################################################################  
  
  match_dim <- reactive({
    req(input$file1, input$file2)  # Ensure both files exist
    
    ref_info <- cached_info1()
    d1 <- c(ref_info$width, ref_info$height)
    questioned_info <- cached_info2()
    d2 <- c(questioned_info$width, questioned_info$height)
    
    mod1 <- 32 - mod(max(d1[1], d2[1]), 32)
    if (mod1 == 32) mod1 <- 0
    mod2 <- 32 - mod(max(d1[2], d2[2]), 32)
    if (mod2 == 32) mod2 <- 0
    
    dx <- d1[1] - d2[1]
    dy <- d1[2] - d2[2]
    
    image_name <-c(paste("original_", c(rm.extension(input$file1$name),
                                        rm.extension(input$file2$name)), ".png", sep = ""),
                   paste("original_reduced_", rm.extension(input$file1$name), ".png", sep = ""),
                   paste("original_aligned_", rm.extension(input$file2$name), ".png", sep = ""),
                   c("overlay_original.png", "overlay_original_reduced.png"),
                   paste("mask_", c(rm.extension(input$file1$name),
                                    rm.extension(input$file2$name)), ".png", sep = ""),
                   paste("processed_", c(rm.extension(input$file1$name),
                                         rm.extension(input$file2$name)), ".png", sep = ""),
                   paste("processed_aligned_", rm.extension(input$file2$name), ".png", sep = ""),
                   paste("reduced_", c(rm.extension(input$file1$name),
                                       rm.extension(input$file2$name)), ".rds", sep = ""),
                   paste("blue_", rm.extension(input$file1$name), ".png", sep = ""),
                   paste("orange_", rm.extension(input$file2$name), ".png", sep = ""),
                   c("R_blue_reduced.png", "Q_orange_aligned_reduced.png"),
                   c("overlay_blue_orange.png", "overlay_blue_orange_reduced.png"))
    temp_file <- as.list(file.path(temp_directory, image_name)); names(temp_file) <- c("R_original", "Q_original", "R_original_reduced",
                                                                                       "Q_original_aligned",
                                                                                       "overlay_original", "overlay_original_reduced",
                                                                                       "R_mask", "Q_mask",
                                                                                       "R_processed", "Q_processed", "Q_processed_aligned",
                                                                                       "R_reduced_matrix", "Q_reduced_img",
                                                                                       "R_blue", "Q_orange_aligned",
                                                                                       "R_blue_reduced", "Q_orange_aligned_reduced",
                                                                                       "overlay_processed", "overlay_processed_reduced")
    
    # Update reactive list
    list(mod1 = mod1, mod2 = mod2,
         dx = dx, dy = dy,
         temp_file = temp_file)
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
    req(cached_img1()) 
    before_crop <- magick::image_read(cached_img1()) %>% # loaded1() %>%
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
            rm(lower.thresholding1);rm(lower.thresholding2);gc()
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
            rm(lower.thresholding1);rm(lower.thresholding2);gc()
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
          image_flop(); rm(r); gc()
        
        
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
            rm(lower.thresholding1);rm(lower.thresholding2);gc()
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
            rm(lower.thresholding1);rm(lower.thresholding2);gc()
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
          image_flop() ; rm(r); gc()
        
        
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
  
  universal_matched_info <- reactiveVal(list())
  loaded1_processed <- reactive({
    
    req(cached_img1()); req(cached_info1())
    
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    
    progress$set(message = "Processing Reference print..", value = 0.3)
    s_time <- cached_time()# Sys.time()
    before_crop <- magick::image_read(cached_img1()) #loaded1()
    img_info <- cached_info1()
    
    
    
    pre_computed_info <- match_dim()
    dx <- pre_computed_info$dx; dy <- pre_computed_info$dy;
    mod1 <- pre_computed_info$mod1; mod2 <- pre_computed_info$mod2;
    temp_file <- pre_computed_info$temp_file
    
    
    ref_org <- before_crop
    if(dx>0){
      if(mod1 > 0){
        ref_org <- image_extent(ref_org, 
                                paste0(img_info$width + mod1, "x", img_info$height), 
                                gravity = "west", color = "white")}}else{
                                  ref_org <- image_extent(ref_org, 
                                                          paste0(img_info$width + abs(dx) + mod1, "x", img_info$height), 
                                                          gravity = "west", color = "white")}; gc()
    new.width <- image_info(ref_org)$width - img_info$width
    if(dy>0){
      if(mod2 > 0){
        ref_org <- image_extent(ref_org, 
                                paste0(img_info$width + new.width, "x", img_info$height + mod2), 
                                gravity = "north", color = "white")}}else{
                                  ref_org <- image_extent(ref_org, 
                                                          paste0(img_info$width + new.width, "x", img_info$height + abs(dy) + mod2), 
                                                          gravity = "north", color = "white")}; gc()
    
    image_write(ref_org, path = temp_file$R_original)
    image_write(ref_org %>%
                  image_scale(geometry = "25%"), path = temp_file$R_original_reduced)
    rm(ref_org); gc()
    
    
    
    
    if(is.null(input$file1_supp)){
      if(img_info$colorspace != "Gray"){
        
        before_crop <- before_crop %>%
          image_convert(colorspace = "gray")
        
      }
      if(input$preinvert == TRUE){ # faster computation
        
        before_crop <- before_crop %>%
          image_negate()
        
      }
      
      
      # if(input$Threshold1<100){
      if(min(input$Threshold1)!=0 | max(input$Threshold1) != 1){
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
          rm(lower.thresholding1);rm(lower.thresholding2);gc()
        }
        
      }
      
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
          image_flop(); rm(r); gc()
        
        
        # after_crop <- image_composite(before_crop, image_transparent(mask1, "black"), "over") %>%
        #   magick2cimg() %>%
        #   rm.alpha() %>% 
        #   grayscale(); rm(before_crop); gc()
        
        after_crop <- image_composite(before_crop, image_transparent(mask1, "black"), "over") %>%
          image_convert(colorspace = "gray"); rm(before_crop); gc()
        
        
        
        
        image_write(mask1, path = temp_file$R_mask)
        rm(mask1); gc()
        
        
        if(dx>0){
          if(mod1 > 0){
            after_crop <- image_extent(after_crop, 
                                       paste0(img_info$width + mod1, "x", img_info$height), 
                                       gravity = "west", color = "white")}}else{
                                         after_crop <- image_extent(after_crop, 
                                                                    paste0(img_info$width + abs(dx) + mod1, "x", img_info$height), 
                                                                    gravity = "west", color = "white")}; gc()
        new.width <- image_info(after_crop)$width - img_info$width
        if(dy>0){
          if(mod2 > 0){
            after_crop <- image_extent(after_crop, 
                                       paste0(img_info$width + new.width, "x", img_info$height + mod2), 
                                       gravity = "north", color = "white")}}else{
                                         after_crop <- image_extent(after_crop, 
                                                                    paste0(img_info$width + new.width, "x", img_info$height + abs(dy) + mod2), 
                                                                    gravity = "north", color = "white")}; gc()
        
        
        scale_down_factor <- eval(parse(text = input$Entered_scale_down_factor))
        info <- image_info(after_crop); universal_matched_info(list(cx.32multiple = info$width / 2,
                                                                    cy.32multiple = info$height / 2,
                                                                    cx.32multiple.downscale = info$width / 2 * scale_down_factor,
                                                                    cy.32multiple.downscale = info$height / 2 * scale_down_factor))
        
        
        ref_reduced <- if(scale_down_factor < 1){
          image_resize(after_crop, paste0(info$width*scale_down_factor, "x", info$height*scale_down_factor)) %>%
            magick2cimg()
        }else{after_crop %>%
            magick2cimg()}; 
        saveRDS(ref_reduced[,,1,1], file = temp_file$R_reduced_matrix); rm(ref_reduced); gc()
        
        
        
        image_write(after_crop, path = temp_file$R_processed)
        
        # Create a white image of the same size
        img_white <- image_blank(info$width, info$height, color = "white"); gc()
        
        # Use CopyRedCompositeOp to replace the Red channel in the target image
        after_crop <- after_crop %>%
          image_convert(colorspace = "sRGB")
        
        after_crop <- image_composite(after_crop, img_white, operator = "CopyBlue"); rm(img_white); gc()
        # Converting white to transparency should be done before resizing to avoid reduced image with black spots
        image_write(after_crop  %>%
                      image_scale(geometry = "25%"), path = temp_file$R_blue_reduced); gc()
        # after_crop <- after_crop %>%
        #   image_transparent("white"); gc()
        image_write(after_crop, path = temp_file$R_blue);
        rm(after_crop); gc()
        
        
        
        return(list(img = temp_file$R_processed,
                    mask = temp_file$R_mask,
                    mask_xy = hole))
      }else{
        
        
        after_crop <- before_crop; rm(before_crop); gc()
        
        if(dx>0){
          if(mod1 > 0){
            after_crop <- image_extent(after_crop, 
                                       paste0(img_info$width + mod1, "x", img_info$height), 
                                       gravity = "west", color = "white")}}else{
                                         after_crop <- image_extent(after_crop, 
                                                                    paste0(img_info$width + abs(dx) + mod1, "x", img_info$height), 
                                                                    gravity = "west", color = "white")}; gc()
        new.width <- image_info(after_crop)$width - img_info$width
        if(dy>0){
          if(mod2 > 0){
            after_crop <- image_extent(after_crop, 
                                       paste0(img_info$width + new.width, "x", img_info$height + mod2), 
                                       gravity = "north", color = "white")}}else{
                                         after_crop <- image_extent(after_crop, 
                                                                    paste0(img_info$width + new.width, "x", img_info$height + abs(dy) + mod2), 
                                                                    gravity = "north", color = "white")}; gc()
        
        
        scale_down_factor <- eval(parse(text = input$Entered_scale_down_factor))
        info <- image_info(after_crop);  universal_matched_info(list(cx.32multiple = info$width / 2,
                                                                     cy.32multiple = info$height / 2,
                                                                     cx.32multiple.downscale = info$width / 2 * scale_down_factor,
                                                                     cy.32multiple.downscale = info$height / 2 * scale_down_factor))
        
        
        ref_reduced <- if(scale_down_factor < 1){
          image_resize(after_crop, paste0(info$width*scale_down_factor, "x", info$height*scale_down_factor)) %>%
            magick2cimg()
        }else{after_crop %>%
            magick2cimg()}; 
        
        saveRDS(ref_reduced[,,1,1], file = temp_file$R_reduced_matrix); rm(ref_reduced); gc()
        
        image_write(after_crop, path = temp_file$R_processed)
        
        # Create a white image of the same size
        img_white <- image_blank(info$width, info$height, color = "white"); gc()
        
        # Use CopyRedCompositeOp to replace the Red channel in the target image
        after_crop <- after_crop %>%
          image_convert(colorspace = "sRGB")
        
        after_crop <- image_composite(after_crop, img_white, operator = "CopyBlue"); rm(img_white); gc()
        # Converting white to transparency should be done before resizing to avoid reduced image with black spots
        image_write(after_crop  %>%
                      image_scale(geometry = "25%"), path = temp_file$R_blue_reduced); gc()
        # after_crop <- after_crop %>%
        #   image_transparent("white"); gc()
        image_write(after_crop, path = temp_file$R_blue);
        rm(after_crop); gc()
        
        
        return(list(img = temp_file$R_processed,
                    mask = NULL,
                    mask_xy = NULL))
      }
    }else{
      cond <- loaded1_process_info()
      
      if(img_info$colorspace != "Gray"){
        before_crop <- before_crop %>%
          image_convert(colorspace = "gray")
      }
      
      if(unique(cond$invert) == TRUE){ # faster computation
        
        before_crop <- before_crop %>%
          image_negate()
        
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
          rm(lower.thresholding1);rm(lower.thresholding2);gc()
        }
        
      }
      
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
          image_flop(); rm(r); gc()
        
        
        # after_crop <- image_composite(before_crop, image_transparent(mask1, "black"), "over") %>%
        #   magick2cimg() %>%
        #   rm.alpha() %>% 
        #   grayscale(); rm(before_crop); gc()
        
        after_crop <- image_composite(before_crop, image_transparent(mask1, "black"), "over") %>%
          image_convert(colorspace = "gray"); rm(before_crop); gc()
        
        
        image_write(mask1, path = temp_file$R_mask)
        rm(mask1); gc()
        
        
        if(dx>0){
          if(mod1 > 0){
            after_crop <- image_extent(after_crop, 
                                       paste0(img_info$width + mod1, "x", img_info$height), 
                                       gravity = "west", color = "white")}}else{
                                         after_crop <- image_extent(after_crop, 
                                                                    paste0(img_info$width + abs(dx) + mod1, "x", img_info$height), 
                                                                    gravity = "west", color = "white")}; gc()
        new.width <- image_info(after_crop)$width - img_info$width
        if(dy>0){
          if(mod2 > 0){
            after_crop <- image_extent(after_crop, 
                                       paste0(img_info$width + new.width, "x", img_info$height + mod2), 
                                       gravity = "north", color = "white")}}else{
                                         after_crop <- image_extent(after_crop, 
                                                                    paste0(img_info$width + new.width, "x", img_info$height + abs(dy) + mod2), 
                                                                    gravity = "north", color = "white")}; gc()
        
        scale_down_factor <- eval(parse(text = input$Entered_scale_down_factor))
        info <- image_info(after_crop);  universal_matched_info(list(cx.32multiple = info$width / 2,
                                                                     cy.32multiple = info$height / 2,
                                                                     cx.32multiple.downscale = info$width / 2 * scale_down_factor,
                                                                     cy.32multiple.downscale = info$height / 2 * scale_down_factor))
        
        ref_reduced <- if(scale_down_factor < 1){
          image_resize(after_crop, paste0(info$width*scale_down_factor, "x", info$height*scale_down_factor)) %>%
            magick2cimg()
        }else{after_crop %>%
            magick2cimg()}; 
        saveRDS(ref_reduced[,,1,1], file = temp_file$R_reduced_matrix); rm(ref_reduced); gc()
        
        image_write(after_crop, path = temp_file$R_processed)
        
        # Create a white image of the same size
        img_white <- image_blank(info$width, info$height, color = "white"); gc()
        
        # Use CopyRedCompositeOp to replace the Red channel in the target image
        after_crop <- after_crop %>%
          image_convert(colorspace = "sRGB")
        
        after_crop <- image_composite(after_crop, img_white, operator = "CopyBlue"); rm(img_white); gc()
        # Converting white to transparency should be done before resizing to avoid reduced image with black spots
        image_write(after_crop  %>%
                      image_scale(geometry = "25%"), path = temp_file$R_blue_reduced); gc()
        # after_crop <- after_crop %>%
        #   image_transparent("white"); gc()
        image_write(after_crop, path = temp_file$R_blue);
        rm(after_crop); gc()
        
        return(list(img = temp_file$R_processed,
                    mask = temp_file$R_mask,
                    mask_xy = hole))
      }else{
        after_crop <- before_crop; rm(before_crop); gc()
        
        if(dx>0){
          if(mod1 > 0){
            after_crop <- image_extent(after_crop, 
                                       paste0(img_info$width + mod1, "x", img_info$height), 
                                       gravity = "west", color = "white")}}else{
                                         after_crop <- image_extent(after_crop, 
                                                                    paste0(img_info$width + abs(dx) + mod1, "x", img_info$height), 
                                                                    gravity = "west", color = "white")}; gc()
        new.width <- image_info(after_crop)$width - img_info$width
        if(dy>0){
          if(mod2 > 0){
            after_crop <- image_extent(after_crop, 
                                       paste0(img_info$width + new.width, "x", img_info$height + mod2), 
                                       gravity = "north", color = "white")}}else{
                                         after_crop <- image_extent(after_crop, 
                                                                    paste0(img_info$width + new.width, "x", img_info$height + abs(dy) + mod2), 
                                                                    gravity = "north", color = "white")}; gc()
        
        scale_down_factor <- eval(parse(text = input$Entered_scale_down_factor))
        info <- image_info(after_crop);  universal_matched_info(list(cx.32multiple = info$width / 2,
                                                                     cy.32multiple = info$height / 2,
                                                                     cx.32multiple.downscale = info$width / 2 * scale_down_factor,
                                                                     cy.32multiple.downscale = info$height / 2 * scale_down_factor))
        
        ref_reduced <- if(scale_down_factor < 1){
          image_resize(after_crop, paste0(info$width*scale_down_factor, "x", info$height*scale_down_factor)) %>%
            magick2cimg()
        }else{after_crop %>%
            magick2cimg()}; 
        saveRDS(ref_reduced[,,1,1], file = temp_file$R_reduced_matrix); rm(ref_reduced); gc()
        
        image_write(after_crop, path = temp_file$R_processed)
        
        
        # Create a white image of the same size
        img_white <- image_blank(info$width, info$height, color = "white"); gc()
        
        # Use CopyRedCompositeOp to replace the Red channel in the target image
        after_crop <- after_crop %>%
          image_convert(colorspace = "sRGB")
        
        after_crop <- image_composite(after_crop, img_white, operator = "CopyBlue"); rm(img_white); gc()
        # Converting white to transparency should be done before resizing to avoid reduced image with black spots
        image_write(after_crop  %>%
                      image_scale(geometry = "25%"), path = temp_file$R_blue_reduced); gc()
        # after_crop <- after_crop %>%
        #   image_transparent("white"); gc()
        image_write(after_crop, path = temp_file$R_blue);
        rm(after_crop); gc()
        
        return(list(img = temp_file$R_processed,
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
      
      initial_thr <- round(autothresholdr::auto_thresh(as.integer(img*255), method = "Otsu")[[1]]/255, 2)
      
      lattice::levelplot(value ~ x*y ,
                         data=img %>% mirror('y') %>% as.data.frame(),
                         col.regions = gray(0:255/256),
                         scales=list(x = list(draw = FALSE),
                                     y = list(draw = FALSE)),
                         aspect = dim(img)[2]/dim(img)[1],
                         main = paste("Initial threshold suggestion: ", initial_thr, sep = ""))
    }
  })
  
  
  
  #######################################################################
  
  
  ########### To plot FILE2 being edited in a reduced scale #############
  loaded2_processed_plot <- reactive({
    req(cached_img2())
    before_crop <- magick::image_read(cached_img2()) %>% #loaded2() %>%
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
            rm(lower.thresholding1);rm(lower.thresholding2);gc()
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
            rm(lower.thresholding1);rm(lower.thresholding2);gc()
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
          image_flop(); rm(r); gc()
        
        
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
            rm(lower.thresholding1);rm(lower.thresholding2);gc()
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
            rm(lower.thresholding1);rm(lower.thresholding2);gc()
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
          image_flop(); rm(r); gc()
        
        
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
    req(cached_img2()); req(cached_info2())
    
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    
    progress$set(message = "Processing Questioned print..", value = 0.3)
    s_time <- cached_time() #Sys.time()
    
    before_crop <- magick::image_read(cached_img2()) # loaded2() 
    img_info <- cached_info2()
    
    pre_computed_info <- match_dim()
    dx <- pre_computed_info$dx; dy <- pre_computed_info$dy;
    mod1 <- pre_computed_info$mod1; mod2 <- pre_computed_info$mod2;
    temp_file <- pre_computed_info$temp_file
    
    
    
    questioned_org <- before_crop
    if(dx>0){
      questioned_org <- image_extent(questioned_org, 
                                     paste0(img_info$width + dx + mod1, "x", img_info$height), 
                                     gravity = "west", color = "white")}else{
                                       if(mod1 > 0){
                                         questioned_org <- image_extent(questioned_org, 
                                                                        paste0(img_info$width + mod1, "x", img_info$height), 
                                                                        gravity = "west", color = "white")}}; gc()
    new.width <- image_info(questioned_org)$width - img_info$width
    if(dy>0){
      questioned_org <- image_extent(questioned_org, 
                                     paste0(img_info$width + new.width, "x", img_info$height + dy + mod2), 
                                     gravity = "north", color = "white")}else{
                                       if(mod2 > 0){
                                         questioned_org <- image_extent(questioned_org, 
                                                                        paste0(img_info$width + new.width, "x", img_info$height + mod2), 
                                                                        gravity = "north", color = "white")}}; gc()
    
    image_write(questioned_org, path = temp_file$Q_original)
    rm(questioned_org); gc()
    
    if(is.null(input$file2_supp)){
      if(img_info$colorspace != "Gray"){
        before_crop <- before_crop %>%
          image_convert(colorspace = "gray")
      }
      
      if(input$preinvert2 == TRUE){ # faster computation
        before_crop <- before_crop %>%
          image_negate()
      }
      
      
      if(min(input$Threshold2)!=0 | max(input$Threshold2) != 1){
        
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
          rm(lower.thresholding1);rm(lower.thresholding2);gc()
        }
      }
      
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
          image_flop(); rm(r); gc()
        
        
        after_crop <- image_composite(before_crop, image_transparent(mask1, "black"), "over") %>%
          image_convert(colorspace = "gray"); rm(before_crop); gc()
        
        
        image_write(mask1, path = temp_file$Q_mask)
        rm(mask1); gc()
        
        
        
        if(dx>0){
          after_crop <- image_extent(after_crop, 
                                     paste0(img_info$width + dx + mod1, "x", img_info$height), 
                                     gravity = "west", color = "white")}else{
                                       if(mod1 > 0){
                                         after_crop <- image_extent(after_crop, 
                                                                    paste0(img_info$width + mod1, "x", img_info$height), 
                                                                    gravity = "west", color = "white")}}; gc()
        new.width <- image_info(after_crop)$width - img_info$width
        if(dy>0){
          after_crop <- image_extent(after_crop, 
                                     paste0(img_info$width + new.width, "x", img_info$height + dy + mod2), 
                                     gravity = "north", color = "white")}else{
                                       if(mod2 > 0){
                                         after_crop <- image_extent(after_crop, 
                                                                    paste0(img_info$width + new.width, "x", img_info$height + mod2), 
                                                                    gravity = "north", color = "white")}}; gc()
        
        
        scale_down_factor <- eval(parse(text = input$Entered_scale_down_factor))
        info <- image_info(after_crop)
        
        questioned_reduced <- if(scale_down_factor < 1){
          image_resize(after_crop, paste0(info$width*scale_down_factor, "x", info$height*scale_down_factor)) %>%
            magick2cimg()
        }else{after_crop %>%
            magick2cimg()}; 
        saveRDS(questioned_reduced, file = temp_file$Q_reduced_img); rm(questioned_reduced); gc()
        
        image_write(after_crop, path = temp_file$Q_processed)
        rm(after_crop); gc()
        
        
        
        return(list(img = temp_file$Q_processed,
                    mask = temp_file$Q_mask,
                    mask_xy = hole))
      }else{
        
        
        after_crop <- before_crop; rm(before_crop); gc()
        
        if(dx>0){
          after_crop <- image_extent(after_crop, 
                                     paste0(img_info$width + dx + mod1, "x", img_info$height), 
                                     gravity = "west", color = "white")}else{
                                       if(mod1 > 0){
                                         after_crop <- image_extent(after_crop, 
                                                                    paste0(img_info$width + mod1, "x", img_info$height), 
                                                                    gravity = "west", color = "white")}}; gc()
        new.width <- image_info(after_crop)$width - img_info$width
        if(dy>0){
          after_crop <- image_extent(after_crop, 
                                     paste0(img_info$width + new.width, "x", img_info$height + dy + mod2), 
                                     gravity = "north", color = "white")}else{
                                       if(mod2 > 0){
                                         after_crop <- image_extent(after_crop, 
                                                                    paste0(img_info$width + new.width, "x", img_info$height + mod2), 
                                                                    gravity = "north", color = "white")}}; gc()
        
        
        scale_down_factor <- eval(parse(text = input$Entered_scale_down_factor))
        info <- image_info(after_crop)
        
        
        
        questioned_reduced <- if(scale_down_factor < 1){
          image_resize(after_crop, paste0(info$width*scale_down_factor, "x", info$height*scale_down_factor)) %>%
            magick2cimg()
        }else{after_crop %>%
            magick2cimg()}; 
        
        saveRDS(questioned_reduced, file = temp_file$Q_reduced_img); rm(questioned_reduced); gc()
        
        image_write(after_crop, path = temp_file$Q_processed)
        rm(after_crop); gc()
        
        
        
        return(list(img = temp_file$Q_processed,
                    mask = NULL,
                    mask_xy = NULL))
      }
    }else{
      cond <- loaded2_process_info()
      
      if(img_info$colorspace != "Gray"){
        before_crop <- before_crop %>%
          image_convert(colorspace = "gray")
      }
      
      if(unique(cond$invert) == TRUE){ # faster computation
        
        before_crop <- before_crop %>%
          image_negate()
        
      }
      
      
      if(unique(cond$threshold_lower)!=0 | unique(cond$threshold_upper) != 1){
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
          rm(lower.thresholding1);rm(lower.thresholding2);gc()
        }
        
      }
      
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
          image_flop(); rm(r); gc()
        
        
        # after_crop <- image_composite(before_crop, image_transparent(mask1, "black"), "over") %>%
        #   magick2cimg() %>%
        #   rm.alpha() %>% 
        #   grayscale(); rm(before_crop); gc()
        
        
        after_crop <- image_composite(before_crop, image_transparent(mask1, "black"), "over") %>%
          image_convert(colorspace = "gray"); 
        
        
        image_write(mask1, path = temp_file$Q_mask)
        rm(mask1); gc()
        
        if(dx>0){
          after_crop <- image_extent(after_crop, 
                                     paste0(img_info$width + dx + mod1, "x", img_info$height), 
                                     gravity = "west", color = "white")}else{
                                       if(mod1 > 0){
                                         after_crop <- image_extent(after_crop, 
                                                                    paste0(img_info$width + mod1, "x", img_info$height), 
                                                                    gravity = "west", color = "white")}}; gc()
        new.width <- image_info(after_crop)$width - img_info$width
        if(dy>0){
          after_crop <- image_extent(after_crop, 
                                     paste0(img_info$width + new.width, "x", img_info$height + dy + mod2), 
                                     gravity = "north", color = "white")}else{
                                       if(mod2 > 0){
                                         after_crop <- image_extent(after_crop, 
                                                                    paste0(img_info$width + new.width, "x", img_info$height + mod2), 
                                                                    gravity = "north", color = "white")}}; gc()
        
        
        scale_down_factor <- eval(parse(text = input$Entered_scale_down_factor))
        info <- image_info(after_crop)
        
        questioned_reduced <- if(scale_down_factor < 1){
          image_resize(after_crop, paste0(info$width*scale_down_factor, "x", info$height*scale_down_factor)) %>%
            magick2cimg()
        }else{after_crop %>%
            magick2cimg()}; 
        
        saveRDS(questioned_reduced, file = temp_file$Q_reduced_img); rm(questioned_reduced); gc()
        
        image_write(after_crop, path = temp_file$Q_processed)
        rm(after_crop); gc()
        
        
        
        return(list(img = temp_file$Q_processed,
                    mask = temp_file$Q_mask,
                    mask_xy = hole))
      }else{
        
        after_crop <- before_crop; rm(before_crop); gc()
        
        if(dx>0){
          after_crop <- image_extent(after_crop, 
                                     paste0(img_info$width + dx + mod1, "x", img_info$height), 
                                     gravity = "west", color = "white")}else{
                                       if(mod1 > 0){
                                         after_crop <- image_extent(after_crop, 
                                                                    paste0(img_info$width + mod1, "x", img_info$height), 
                                                                    gravity = "west", color = "white")}}; gc()
        new.width <- image_info(after_crop)$width - img_info$width
        if(dy>0){
          after_crop <- image_extent(after_crop, 
                                     paste0(img_info$width + new.width, "x", img_info$height + dy + mod2), 
                                     gravity = "north", color = "white")}else{
                                       if(mod2 > 0){
                                         after_crop <- image_extent(after_crop, 
                                                                    paste0(img_info$width + new.width, "x", img_info$height + mod2), 
                                                                    gravity = "north", color = "white")}}; gc()
        
        
        scale_down_factor <- eval(parse(text = input$Entered_scale_down_factor))
        info <- image_info(after_crop)
        
        questioned_reduced <- if(scale_down_factor < 1){
          image_resize(after_crop, paste0(info$width*scale_down_factor, "x", info$height*scale_down_factor)) %>%
            magick2cimg()
        }else{after_crop %>%
            magick2cimg()}; 
        
        saveRDS(questioned_reduced, file = temp_file$Q_reduced_img); rm(questioned_reduced); gc()
        
        
        image_write(after_crop, path = temp_file$Q_processed)
        rm(after_crop); gc()
        
        
        return(list(img = temp_file$Q_processed,
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
      
      initial_thr <- round(autothresholdr::auto_thresh(as.integer(img*255), method = "Otsu")[[1]]/255, 2)
      
      lattice::levelplot(value ~ x*y ,
                         data=img %>% mirror('y') %>% as.data.frame(),
                         col.regions = gray(0:255/256),
                         scales=list(x = list(draw = FALSE),
                                     y = list(draw = FALSE)),
                         aspect = dim(img)[2]/dim(img)[1],
                         main = paste("Initial threshold suggestion: ", initial_thr, sep = ""))
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
  cached_aligned <- reactiveVal(); cached_time <- reactiveVal()
  aligned_res <- observeEvent(input$submit, {
    cached_time(Sys.time())
    req(cached_img1())
    req(cached_img2())
    req(cached_info1())
    req(cached_info2())
    req(match_dim())
    req(loaded1_processed())
    req(loaded2_processed())
    req(universal_matched_info())
    
    time_at_alignment <- format(Sys.time(),'tmp_%Y%m%d_%H%M%S')
    
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    # s_time <- Sys.time()
    s_time <- cached_time()
    
    
    temp_file <- match_dim()$temp_file
    
    
    progress$set(message = "Alignment in progress", detail = "processing prints in original scale and matching size..", value = 0.1)
    
    
    scale_down_factor <- eval(parse(text = input$Entered_scale_down_factor))
    set.angles <- seq(min(input$Angle_range), max(input$Angle_range), by = 0.5)
    
    
    center_info <- universal_matched_info()
    cx.32multiple <- center_info$cx.32multiple
    cy.32multiple <- center_info$cy.32multiple
    cx.32multiple.downscale <- center_info$cx.32multiple.downscale
    cy.32multiple.downscale <- center_info$cy.32multiple.downscale
    
    ref_reduced <- readRDS(temp_file$R_reduced_matrix); questioned_reduced <- readRDS(temp_file$Q_reduced_img)
    # To compute the best alignment
    precomputed_ref_sd_fft <- list(sd = sd(as.vector(ref_reduced)),
                                   fft = Conj(fft(ref_reduced - mean(ref_reduced)))); rm(ref_reduced); gc()
    res <- list()
    tempfile.list <- tempfile(paste("res_", 1:length(set.angles), sep = ""), tmpdir = temp_directory, fileext = ".rds")
    
    for(j in 1:length(set.angles)){
      
      rotated_img <- rotate.shift.over.white.bg.f(img = questioned_reduced, 
                                                  theta = set.angles[j], 
                                                  cx.set = cx.32multiple.downscale, cy.set = cy.32multiple.downscale,
                                                  shift.x = 0,
                                                  shift.y = 0)
      
      
      auto.cor.mat <- xcorr3d_optimized(img2 = rotated_img[,,1,1], precomputed_img1_sd_fft = precomputed_ref_sd_fft)
      saveRDS(auto.cor.mat,
              tempfile.list[[j]])
      
      res[[j]] <- auto.cor.mat[1:2]
      
      
      d1 <- Sys.time()-s_time; d1.t <- round(d1, 2); d1.u <- units(d1)
      progress$inc(0.4/length(set.angles), detail = paste("computing the best alignment..(", round(100*j/length(set.angles)), "%; ",  d1.t, " ", d1.u, ").", sep = ""))
    }
    rm(rotated_img, precomputed_ref_sd_fft); gc()
    
    each.max.idx <- res %>%
      map(~.x$max.corr) %>%
      unlist()
    each.max.idx <- which.max(each.max.idx)
    each.angle <- set.angles[each.max.idx]
    each.shift <- as.vector(res[[each.max.idx]]$max.shifts)
    
    
    ## Returned outputs   
    transformed_info <- list(Angle = each.angle,
                             X = -each.shift[1]*(1/scale_down_factor),
                             Y = -each.shift[2]*(1/scale_down_factor),
                             cx = cx.32multiple,
                             cy = cy.32multiple)
    
    
    d1 <- Sys.time()-s_time; d1.t <- round(d1, 2); d1.u <- units(d1)
    progress$inc(0.1, detail = paste("applying the best alignment..(", d1.t, " ", d1.u, ").", sep = ""))
    # 
    # questioned_input <- load.image(temp_file$Q_processed) 
    # trans_img <- rotate.shift.over.white.bg.f(img = questioned_input, 
    #                                           theta = transformed_info$Angle, 
    #                                           cx.set = transformed_info$cx, cy.set = transformed_info$cy,
    #                                           shift.x = transformed_info$X,
    #                                           shift.y = transformed_info$Y)
    # rm(questioned_input);gc()
    
    
    questioned_input <- image_read(temp_file$Q_processed) 
    trans_img <- rotate_shift_image(img = questioned_input, 
                                    theta = transformed_info$Angle, 
                                    dx = transformed_info$X,
                                    dy = transformed_info$Y)
    rm(questioned_input);gc()
    image_write(trans_img, path = temp_file$Q_processed_aligned);
    image_write(trans_img, path = "qa.png");
    trans_img <- trans_img %>% magick2cimg(); gc()
    
    d1 <- Sys.time()-s_time; d1.t <- round(d1, 2); d1.u <- units(d1)
    progress$inc(0.1, detail = paste("computing similarity..(", d1.t, " ", d1.u, ").", sep = ""))
    
    
    ref_input <- load.image(temp_file$R_processed) 
    aligned_cor <- cor(crop.bbox(ref_input, trans_img < 1),
                       crop.bbox(trans_img, trans_img < 1)); rm(ref_input); gc()
    # 
    # save.image(trans_img, file = temp_file$Q_processed_aligned);
    
    
    
    ## Colorizing the aligned images
    d1 <- Sys.time()-s_time; d1.t <- round(d1, 2); d1.u <- units(d1)
    progress$inc(0.1, detail = paste("Colorizing images..(", d1.t, " ", d1.u, ").", sep = ""))
    
    questioned_orange <- image_read(temp_file$Q_processed_aligned) %>%
      image_convert(colorspace = "sRGB"); gc()
    
    width <- image_info(questioned_orange)$width
    height <- image_info(questioned_orange)$height
    
    # Create a white image of the same size
    img_white <- image_blank(width, height, color = "white"); gc()
    
    
    questioned_orange <- image_composite(questioned_orange, img_white, operator = "CopyRed"); rm(img_white); gc()
    # questioned_orange <- questioned_orange #%>%
    #   image_transparent("white"); gc()
    
    image_write(questioned_orange, path = temp_file$Q_orange_aligned); gc()
    image_write(questioned_orange  %>%
                  image_scale(geometry = "25%"), path = temp_file$Q_orange_aligned_reduced); gc()
    
    d1 <- Sys.time()-s_time; d1.t <- round(d1, 2); d1.u <- units(d1)
    progress$inc(0.1, detail = paste("Overlapping the colorized..(", d1.t, " ", d1.u, ").", sep = ""))
    reference_blue <-  image_read(temp_file$R_blue)
    # blue_orange <- image_composite(reference_blue, questioned_orange, operator = "add"); rm(reference_blue, questioned_orange); gc()
    rq <- c(reference_blue, questioned_orange); rm(reference_blue, questioned_orange); gc()
    blue_orange <- image_average(rq); rm(rq); gc()
    image_write(blue_orange %>%
                  image_scale(geometry = "25%"), path = temp_file$overlay_processed_reduced); gc()
    image_write(blue_orange, path = temp_file$overlay_processed); rm(blue_orange); gc()
    
    # blue_orange <- load.image(temp_file$R_blue) %>% add.color() + trans_img; gc()
    # save.image(blue_orange, file = temp_file$overlay_processed)
    
    
    
    
    d1 <- Sys.time()-s_time; d1.t <- round(d1, 2); d1.u <- units(d1)
    progress$inc(0.1, detail = paste("Aligning original images..(", d1.t, " ", d1.u, ").", sep = ""))
    # questioned_org <- load.image(temp_file$Q_original)
    # trans_org <- rotate.shift.over.white.bg.f(img = questioned_org,
    #                                           theta = transformed_info$Angle,
    #                                           cx.set = transformed_info$cx, cy.set = transformed_info$cy,
    #                                           shift.x = transformed_info$X,
    #                                           shift.y = transformed_info$Y)
    # rm(questioned_org); gc()
    # save.image(trans_org, file = temp_file$Q_original_aligned); rm(trans_org); gc()
    questioned_org <- image_read(temp_file$Q_original)
    trans_org <- rotate_shift_image(img = questioned_org,
                                              theta = transformed_info$Angle,
                                    dx = transformed_info$X,
                                              dy = transformed_info$Y)
    rm(questioned_org); gc()
    image_write(trans_org, path = temp_file$Q_original_aligned); rm(trans_org); gc()
    #   questioned_org <- image_read(temp_file$Q_original)
    # image_write(questioned_org, path = temp_file$Q_original_aligned)
    
    ref_org <- image_read(temp_file$R_original); gc()
    # ref_org <- ref_org %>%
    #   image_modulate(brightness = 50); gc()
    
    questioned_org <- image_read(temp_file$Q_original_aligned); gc()
    # questioned_org <- questioned_org %>%
    #   image_modulate(brightness = 50); gc();
    
    
    # overlayed_org <- image_composite(ref_org, questioned_org, operator = "plus"); rm(ref_org, questioned_org); gc()
    rq <- c(ref_org, questioned_org); rm(ref_org, questioned_org); gc()
    overlayed_org <- image_average(rq); rm(rq); gc()
    image_write(overlayed_org, path = temp_file$overlay_original)
    overlayed_org_reduced <- overlayed_org %>% 
      image_resize(geometry = "25%"); rm (overlayed_org); gc()
    image_write(overlayed_org_reduced, path = temp_file$overlay_original_reduced)
    rm(overlayed_org_reduced); gc()
    
    
    
    # img_dims <- dim(ref_input)
    # R_channel <- ref_input # Red stays the same
    # G_channel <- ref_input # Green stays the same
    # B_channel <- array(1, dim = img_dims)  # Always 1 for blue
    # 
    # # Combine channels into an RGB image efficiently
    # ref_blue <- abind::abind(R_channel, G_channel, along = 4); rm(R_channel, G_channel); gc()
    # ref_blue <- abind::abind(ref_blue, B_channel, along = 4) %>%
    #   as.cimg(); rm(B_channel); gc()
    # save.image(ref_blue, file = temp_file$R_blue)
    # rm(ref_blue); gc()
    # 
    # img_dims <- dim(trans_img)
    # G_channel <- trans_img  # Green stays the same
    # B_channel <- trans_img # Always 1 for blue
    # R_channel <- array(1, dim = img_dims) # Red stays the same
    # # Combine channels into an RGB image efficiently
    # questioned_orange <- abind::abind(R_channel, G_channel, along = 4); rm(R_channel, G_channel); gc()
    # questioned_orange <- abind::abind(questioned_orange, B_channel, along = 4) %>%
    #   as.cimg(); rm(B_channel); gc()
    # 
    # save.image(questioned_orange, file = temp_file$Q_orange_aligned)
    # rm(questioned_orange); gc()
    # 
    # R_channel <- ref_input + 1; gc()
    # G_channel <- ref_input + trans_img; rm(ref_input); gc()
    # B_channel <- trans_img + 1; rm(trans_img); gc()
    # blue_orange <- abind::abind(R_channel, G_channel, along = 4); rm(R_channel, G_channel); gc()
    # blue_orange <- abind::abind(blue_orange, B_channel, along = 4) %>%
    #   as.cimg(); rm(B_channel); gc()
    # 
    # save.image(blue_orange, file = temp_file$overlay_processed)
    # rm(blue_orange); gc()
    # 
    #### For original overlay
    # 
    # questioned_org <- load.image(file = temp_file$Q_original)
    # trans_org <- rotate.shift.over.white.bg.f(img = questioned_org,
    #                                           theta = transformed_info$Angle, 
    #                                           cx.set = transformed_info$cx, cy.set = transformed_info$cy,
    #                                           shift.x = transformed_info$X,
    #                                           shift.y = transformed_info$Y)
    # rm(questioned_org); gc()
    # save.image(trans_org, file = temp_file$Q_original_aligned)
    # 
    # 
    # ref_org <- load.image(file = temp_file$R_original)
    # xx.d <- dim(ref_org)[4]
    # yy.d <- dim(trans_org)[4]
    # if(xx.d==yy.d){
    #   overlayed_org <- ref_org + trans_org; rm(ref_org, trans_org); gc()
    # }else{
    #   if(xx.d==1 & yy.d!=1){
    #     overlayed_org <- add.color.f(ref_org); rm(ref_org); gc()
    #     overlayed_org <- overlayed_org + trans_org; rm(trans_org); gc()
    #   }else{
    #     if(xx.d!=1 & yy.d==1){
    #       overlayed_org <- add.color.f(trans_org); rm(trans_org); gc()
    #       overlayed_org <- overlayed_org + ref_org; rm(ref_org); gc()
    #     }}}
    # save.image(overlayed_org, file = temp_file$overlay_original)
    # rm(overlayed_org); gc()
    
    
    ## cross-correlation for alignment location info
    
    d1 <- Sys.time()-s_time; d1.t <- round(d1, 2); d1.u <- units(d1)
    progress$inc(0.1, detail = paste("computing diagnosis..(", d1.t, " ", d1.u, ").", sep = ""))
    
    # indices plusminus 2 around the index of max corr
    # display.idx <- seq(max(1, each.max.idx-2), min(each.max.idx+2, length(set.angles)), 1)
    
    # Peaks of max_corr
    curve_data <- res %>%
      map(~.x[[2]]) %>%
      unlist(use.names = FALSE)
    peaks <- findpeaks(curve_data, sortstr = TRUE)
    
    # indices of three largest max corr in descending order
    display.idx <- peaks[1:3, 2]
    
    selected.res <- display.idx %>%
      map(~readRDS(tempfile.list[[.x]]))
    
    yy <- list()
    for(i in 1:length(display.idx)){
      xx <- selected.res[[i]]$corr.mat
      xx[xx<=quantile(xx, 0.9)] <- NA
      yy[[i]] <- as.cimg(xx) %>% 
        mirror('x') 
    }
    yy.df <- yy %>%
      map2(display.idx,
           ~cbind(as.data.frame(.x), rotation_angle = as.factor(paste(set.angles[.y], 
                                                                      "\u00B0", sep = "")))) %>% 
      map(~.x %>%
            mutate(x = x - round(median(x)),
                   y = -(y - round(median(y))))) %>%
      map_dfr(~.x)
    
    align_info_p <- list(max.cross.corr_angle = data.frame(angle = set.angles,
                                                           max_corr = res %>%
                                                             map(~.x$max.corr) %>%
                                                             unlist()),
                         cross.corr_map_angle = lattice::levelplot(value ~ x*y | rotation_angle, data=yy.df, 
                                                                   xlab = 'shift_x', ylab = 'shift_y',
                                                                   ylim = c(max(yy.df$y), min(yy.df$y)),
                                                                   layout = c(length(display.idx), 1),
                                                                   main = paste("Top 10% of cross-correlation by angles; scale = ", 
                                                                                scale_down_factor, ".", sep = "")))
    rm(res);gc()
    
    d1 <- Sys.time()-s_time; d1.t <- round(d1, 2); d1.u <- units(d1)
    progress$inc(0.1, detail = paste("Almost done..(", d1.t, " ", d1.u, ").", sep = ""))
    res <- list(transformed_info = transformed_info,
                similarity_cor = aligned_cor,
                align_info_p = align_info_p,
                time_at_alignment = time_at_alignment,
                original_f_Vals = temp_file)
    cached_aligned(list(processed_res = res,
                        temporary_dir = temp_directory))
    
    
    
  }) 
  
  
  # The transformation information
  
  output$transform_information <- renderText({
    req(cached_aligned())
    transformed <- cached_aligned()$processed_res$transformed_info
    print(paste("By the automated alignment, the questioned image was rotated by ",
                transformed$Angle, " degrees and shifted with horizontal shift = ", transformed$X, " and ",
                "vertical shift = ", -transformed$Y, ".", sep = "")) # -Y because the origin (0,0) is located at top-left corner, but in common sense, origin is at bottom-left corner.
    
  })
  
  # The similarity score between the aligned images
  similarity_information <- function(){
    similarity_score <- cached_aligned()$processed_res$similarity_cor
    return(paste("The Pearson correlation coefficient between two pre-processed images is ",
                 round(similarity_score, 4), ".", sep = ""))
  }
  output$similarity_information <- renderText({
    req(cached_aligned())
    similarity_score <- cached_aligned()$processed_res$similarity_cor
    print(paste("The Pearson correlation coefficient between two pre-processed images is ",
                round(similarity_score, 4), ".", sep = ""))
  })
  
  #######################################################################################
  
  # output$plot4 <- renderPlot({
  #   
  #   par(mar = c(0,0,0,0))
  #   if(is.numeric(cached_aligned()$processed_res$similarity_cor)){
  #     progress <- shiny::Progress$new()
  #     # Make sure it closes when we exit this reactive, even if there's an error
  #     on.exit(progress$close())
  #     
  #     progress$set(message = "Plotting in progress..", value = 0)
  #     
  #     plot(load.image(cached_aligned()$processed_res$original_f_Vals$R_blue) %>% #1234
  #            determine.display.scale.f(), axes = FALSE); gc()
  #     progress$inc(0.25)
  #     
  #   }
  #   
  # })
  output$plot4 <- renderImage({
    req(cached_aligned()$processed_res$original_f_Vals$R_blue_reduced)
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Plotting in progress..", value = 0)
    progress$inc(0.25)
    list(
      src = cached_aligned()$processed_res$original_f_Vals$R_blue_reduced,  # Path to the saved image
      width = "90%",  # Dynamically fit the width
      height = "auto",  # Maintain aspect ratio
      alt = "Q_processed_reduced"
    )
  }, deleteFile = FALSE)
  
  output$plot5 <- renderImage({
    req(cached_aligned()$processed_res$original_f_Vals$Q_orange_aligned_reduced)
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Plotting in progress..", value = .25)
    progress$inc(0.25)
    list(
      src = cached_aligned()$processed_res$original_f_Vals$Q_orange_aligned_reduced,  # Path to the saved image
      width = "90%",  # Dynamically fit the width
      height = "auto",  # Maintain aspect ratio
      alt = "Q_processed_reduced"
    )
  }, deleteFile = FALSE)
  
  output$plot6 <- renderImage({
    req(cached_aligned()$processed_res$original_f_Vals$overlay_processed_reduced)
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Plotting in progress..", value = .5)
    progress$inc(0.25)
    list(
      src = cached_aligned()$processed_res$original_f_Vals$overlay_processed_reduced,  # Path to the saved image
      width = "90%",  # Dynamically fit the width
      height = "auto",  # Maintain aspect ratio
      alt = "overlay_processed_reduced"
    )
  }, deleteFile = FALSE)
  #######################################################################################
  
  
  # ########### To plot the alignment results from the original FILE1 and FILE2 #############
  
  output$plot7 <- renderImage({
    req(cached_view1())
    list(
      src = cached_view1(),  # Path to the saved image
      width = "90%",  # Dynamically fit the width
      height = "auto",  # Maintain aspect ratio
      alt = "original_reference_image"
    )
  }, deleteFile = FALSE)
  
  
  output$plot8 <- renderImage({
    req(cached_view2())
    list(
      src = cached_view2(),  # Path to the saved image
      width = "90%",  # Dynamically fit the width
      height = "auto",  # Maintain aspect ratio
      alt = "original_questioned_image"
    )
  }, deleteFile = FALSE)
  
  output$plot9 <- renderImage({
    req(cached_aligned()$processed_res$original_f_Vals$overlay_original_reduced)
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Plotting almost done..", value = .75)
    progress$inc(0.25)
    list(
      src = cached_aligned()$processed_res$original_f_Vals$overlay_original_reduced,  # Path to the saved image
      width = "90%",  # Dynamically fit the width
      height = "auto",  # Maintain aspect ratio
      alt = "original_overlay_reduced"
    )
  }, deleteFile = FALSE)
  
  #########################################################################################
  
  
  
  ########### To download the edited FILE1 and FILE2, and alignment results #############
  output$download_btn <- downloadHandler(
    filename = function(){
      paste("ShoeprintAnalyzr", format(Sys.time(),'_%Y%m%d_%H%M%S'), ".zip", sep = "")
    },
    content = function(file){
      
      
      time_now <- cached_aligned()$processed_res$time_at_alignment 
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
      
      progress$set(message = "Exporting images..", value = 0.1)
      
      
      temp_file.list <- cached_aligned()$processed_res$original_f_Vals
      
      names.img <- names(temp_file.list)
      
      if(!is.null(temp_file.list)){
        for(i in 1:length(temp_file.list)){
          if(names.img[i] %in% c("R_original", "Q_original",
                                 "Q_original_aligned",
                                 "overlay_original")){
            temp_file <- temp_file.list[[i]]
            final_file <- file.path(original_f, basename(temp_file))
            file.rename(temp_file, final_file)
          }else{
            if(names.img[i] %in% c("R_processed", "Q_processed", "Q_processed_aligned",
                                   "R_blue", "Q_orange_aligned",
                                   "overlay_processed")){
              temp_file <- temp_file.list[[i]]
              final_file <- file.path(processed_f, basename(temp_file))
              file.rename(temp_file, final_file)
            }else{
              if(names.img[i] %in% c("R_mask", "Q_mask")){
                temp_file <- temp_file.list[[i]]
                final_file <- file.path(mask_f, basename(temp_file))
                file.rename(temp_file, final_file)
              }
            }}
        }}
      
      progress$inc(0.1, message = "Exporting diagnostic plots..", detail = NULL)
      
      pdf(file.path(temp_directory_time, "Diagnose_reduced_scale.pdf"))
      plot(cached_aligned()$processed_res$align_info_p$max.cross.corr_angle,
           xlab = "rotation angle",
           ylab = "maximum cross-correlation in a reduced scale")
      axis(1, at = seq(min(cached_aligned()$processed_res$align_info_p$max.cross.corr_angle$angle),
                       max(cached_aligned()$processed_res$align_info_p$max.cross.corr_angle$angle),
                       by = 5), las=1)
      
      curve_data <- cached_aligned()$processed_res$align_info_p$max.cross.corr_angle
      peaks <- findpeaks(curve_data[,2], sortstr = TRUE)
      
      plot(curve_data, type = "l", main = "Find Peaks Example")
      points(curve_data[,1][peaks[1:3, 2]], peaks[1:3, 1], col = "red", pch = 19)  # Highlight peaks
      
      print(cached_aligned()$processed_res$align_info_p$cross.corr_map_angle)
      
      dev.off()
      
      transformed <- cached_aligned()$processed_res
      
      
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
      
      progress$inc(0.1, message = "Exporting alignment information2..", detail = NULL)
      
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
      
      
      progress$inc(0.1, message = "Exporting alignment information3..", detail = NULL)
      
      # Define the output file path
      file_path <- file.path(temp_directory_time, "alignment_metadata.xlsx")
      
      progress$inc(0.1, message = "Exporting alignment information4..", detail = NULL)
      
      # Ensure the workbook is valid
      if (file.exists(file_path)) {
        wb <- tryCatch({
          loadWorkbook(file_path)  # Load the workbook
        }, error = function(e) {
          message("Error loading workbook: ", e)
          createWorkbook()  # Create a new workbook if loading fails
        })
      } else {
        wb <- createWorkbook()  # Create a new workbook
      }
      
      # Safely retrieve sheet names
      sheet_names <- wb$sheet_names  # Access sheet names directly
      if (is.null(sheet_names)) {
        sheet_names <- character(0)  # Handle empty workbooks
      }
      
      # Add "pair_info" sheet if necessary
      if (!"pair_info" %in% sheet_names) {
        addWorksheet(wb, "pair_info")
      }
      
      # Write data to "pair_info" sheet
      writeData(wb, "pair_info", out_report, colNames = TRUE, rowNames = FALSE)
      
      
      progress$inc(0.1, message = "Exporting alignment information5..", detail = NULL)
      
      # Process and write data for loaded1_processed()
      if (!is.null(loaded1_processed()$mask_xy)) {
        # Add "crop_info_R" sheet if necessary
        if (!"crop_info_R" %in% sheet_names) {
          addWorksheet(wb, "crop_info_R")
        }
        
        # Write mask_xy to "crop_info_R" sheet
        writeData(wb, "crop_info_R",
                  loaded1_processed()$mask_xy %>%
                    as.data.frame() %>%
                    rename(x = V1, y = V2),
                  colNames = TRUE, rowNames = FALSE)
        
        # Write mask_xy to a text file
        write.table(
          loaded1_processed()$mask_xy %>%
            as.data.frame() %>%
            rename(x = V1, y = V2) %>%
            mutate(grayscale = gr,
                   invert = ir,
                   threshold_lower = tlr,
                   threshold_upper = tur),
          file = file.path(temp_directory_time, paste("p_info_", rm.extension(input$file1$name), ".txt", sep = "")),
          row.names = FALSE,
          col.names = TRUE
        )
      }
      
      # Process and write data for loaded2_processed()
      if (!is.null(loaded2_processed()$mask_xy)) {
        # Add "crop_info_Q" sheet if necessary
        if (!"crop_info_Q" %in% sheet_names) {
          addWorksheet(wb, "crop_info_Q")
        }
        
        # Write mask_xy to "crop_info_Q" sheet
        writeData(wb, "crop_info_Q",
                  loaded2_processed()$mask_xy %>%
                    as.data.frame() %>%
                    rename(x = V1, y = V2),
                  colNames = TRUE, rowNames = FALSE)
        
        # Write mask_xy to a text file
        write.table(
          loaded2_processed()$mask_xy %>%
            as.data.frame() %>%
            rename(x = V1, y = V2) %>%
            mutate(grayscale = gq,
                   invert = iq,
                   threshold_lower = tlq,
                   threshold_upper = tuq),
          file = file.path(temp_directory_time, paste("p_info_", rm.extension(input$file2$name), ".txt", sep = "")),
          row.names = FALSE,
          col.names = TRUE
        )
      }
      
      # Save the workbook
      saveWorkbook(wb, file_path, overwrite = TRUE)
      
      
      progress$inc(0.1, message = "Zipping everything..")
      
      
      zip::zip(
        zipfile = file,
        files = dir(temp_directory_time),
        root = temp_directory_time
      )
      
      
      
      unlink(temp_directory, recursive = TRUE) # Remove the files saved temporarily.
      
      
      progress$inc(0.05, message = "Complete!", detail = NULL)
    },
    contentType = "application/zip"
    
  )
  #######################################################################################
}


# Run the application 
shinyApp(ui, server)



