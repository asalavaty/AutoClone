library(shiny)
library(Cairo)   # For nicer ggplot2 output when deployed on Linux ## Remember to install the followings on the server before installing the Cairo
# apt-get install libcairo2-dev
# apt-get install libxt-dev
library(DT)
library(shinyWidgets)
library(shinyFeedback)
library(shinyjs)
library(shinycssloaders)
library(colordistance)
library(imagefx) 
library(magick)
library(imager)
library(grid)
library(scales)
library(ggplot2)
library(ggpubr)
library(fishualize)
library(waiter)
library(argonR)
library(bs4Dash)
library(ICSNP)
library(magrittr)
library(plyr)
library(dplyr)
library(tidyselect)
library(tidyr)
library(emayili)

Email_Addresses <- suppressWarnings(read.csv(file = "www/Email_database/Email_Addresses.csv", header = TRUE, sep = ","))

adrian_url <- "https://www.abbassalavaty.com/"
pete_url <- "https://www.armi.org.au/about/our-people/peter-currie/"

# options(bitmapType='cairo')

options(shiny.usecairo = TRUE)
options(shiny.maxRequestSize = Inf)
options(warn=-1)

####**********************************************####

# Creating required functions for making screen shot of the images 
# Based on the Snapper package: https://github.com/yonicd/snapper

####**************####

build_call <- function(type,arg,opts,ui){
  
  if(nzchar(opts)){
    opts <- sprintf(',%s',opts)
  }
  
  sprintf(canvas_template, ui, opts, call_contents(type,arg))
  
}

call_contents <- function(type = c('preview','download'),arg){
  
  switch(type,
         'preview' = {
           sprintf('var img = document.createElement("img");
                    img.src = canvas.toDataURL("png");
                    img.width = parseInt(canvas.style.width);
                    img.height = parseInt(canvas.style.height);
                    $("#%s").empty();
                    $("#%s").append(img);',
                   arg,arg)
         },
         'download' = {
           sprintf('saveAs(canvas.toDataURL("png"), "%s");',arg)
         })
  
}

canvas_template <- '(function () {
    html2canvas($("%s")[0]%s).then(canvas=>{
      %s
    });
    })();'

####**************####

load_snapper <- function(
  html2canvas = 'html2canvas.min.js',
  jquery = 'jquery-3.5.0.min.js'){
  
  shiny::tagList(
    htmltools::htmlDependency("jquery", "3.5.0",
                              src = c(href = dirname(jquery)),
                              script = basename(jquery)),
    htmltools::htmlDependency("html2canvas", "1.0.0",
                              src = c(href = dirname(html2canvas)),
                              script = basename(html2canvas)),
    shiny::tags$script('
      var saveAs = function(uri, filename) {
            var link = document.createElement("a");
            if (typeof link.download === "string") {
              link.href = uri;
              link.download = filename;
              //Firefox requires the link to be in the body
              document.body.appendChild(link);
              //simulate click
              link.click();
              //remove the link when done
              document.body.removeChild(link);
            } else {
              window.open(uri);
            }
          };')
  )
}

####**************####

default_class <- list(
  'allowTaint' = 'logical',
  'backgroundColor' = 'character',
  'canvas' = c('character','NULL'),
  'foreignObjectRendering' = 'logical',
  'imageTimeout' = 'numeric',
  'ignoreElements' = 'character',
  'logging' = 'logical',
  'onclone' = c('character','NULL'),
  'proxy' = c('character','NULL'),
  'removeContainer' = 'logical',
  'useCORS' = 'logical',
  'scale' = c('character','numeric'),
  'width' = c('character','numeric'),
  'height' = c('character','numeric'),
  'x' = c('character','numeric'),
  'y' = c('character','numeric'),
  'scrollX' = c('character','numeric'),
  'scrollY' = c('character','numeric'),
  'windowWidth' = c('character','numeric'),
  'windowHeight' = c('character','numeric')
)

####**************####

find_args <- function (...) {
  env <- parent.frame()
  args <- names(formals(sys.function(sys.parent(1))))
  vals <- mget(args, envir = env)
  vals <- vals[!vapply(vals, is_missing_arg, logical(1))]
  utils::modifyList(vals, list(..., ... = NULL))
}

is_missing_arg <- function (x) identical(x, quote(expr = ))

check_setting <- function(e,elements){
  e_val <- elements[[e]]
  if(!inherits(e_val,unlist(default_class[e],use.names = FALSE)))
    stop(sprintf('%s must be of class %s',e,default_class[e]))
}

####**************####

config <- function (allowTaint, backgroundColor, canvas, foreignObjectRendering, 
                    imageTimeout, ignoreElements, logging, onclone, proxy, removeContainer, 
                    useCORS, scale, width, height, x, y, scrollX, scrollY, windowWidth, 
                    windowHeight, ...) 
{
  setting <- find_args(...)
  bad_name <- setdiff(names(setting), names(default_class))
  if (length(bad_name) > 0) 
    stop(sprintf("%s not a valid element", paste0(bad_name, 
                                                  collapse = ", ")))
  invisible(lapply(names(setting), check_setting, setting))
  canvas_opts <- ""
  if (length(setting) > 0) {
    canvas_opts <- jsonlite::toJSON(setting, auto_unbox = TRUE, 
                                    null = "null")
    canvas_opts <- gsub("[\"]", "", canvas_opts)
  }
  canvas_opts
}

####**************####

downloadScreenShotBtn <- function(inputId = 'btn-Convert-Html2Image',
                                  label = 'Download',
                                  ui = "#html-content-holder",
                                  filename = 'canvas.png',
                                  opts = config()){
  
  shiny::actionButton(
    inputId = inputId,
    label = label,
    icon = icon("check-circle"), 
    width = "100%", 
    class = "btn-xs btn-success",
    onclick = build_call(
      type = 'download',
      arg = filename,
      opts = opts,
      ui = ui
    )
  )
  
  
}

####**********************************************####

# Define the function for circle plotting of the section palette (based on https://www.r-bloggers.com/2019/01/extracting-colours-from-your-images-with-image-quantization/)

get_colorPal <- function(im, n=10, cs="RGB") {
  #print(cs) 
  tmp <-im %>% magick::image_resize("100") %>% 
    image_quantize(max=n, colorspace=cs) %>%  ## reducing colours!
    imager::magick2cimg() %>%  ## We are converting, because we want to use as.data.frame function in imager package.
    imager::RGBtoHSV() %>% ## we like sorting colour by hue rather than RGB (red green blue)
    as.data.frame(wide="c") %>%  #3 making it wide makes it easier to output hex colour
    dplyr::mutate(hex=hsv(rescale(c.1, from=c(0,360)),c.2,c.3),
                  hue = c.1,
                  sat = c.2,
                  value = c.3) %>%
    dplyr::count(hex, hue, sat, value, sort=T) %>% 
    dplyr::mutate(colorspace = cs)
  
  return(tmp %>% dplyr::select(colorspace,hex,hue,sat,value,n)) ## We want data frame as a result.
  
}

####**********************************************####

# Define a function for conversion of RGB to HSL (according to the plotwidgets package)

rgb2hsl <- function (rgb) {
  if (nrow(rgb) == 4) {
    alpha <- rgb[4, , drop = F]
    rgb <- rgb[-4, , drop = F]
  }
  else {
    alpha <- NULL
  }
  rgb <- rgb/255
  mins <- apply(rgb, 2, min)
  maxs <- apply(rgb, 2, max)
  d <- maxs - mins
  L <- (maxs + mins)/2
  S <- d/(1 - abs(2 * L - 1))
  sel <- d == 0
  S[sel] <- 0
  wmax <- apply(rgb, 2, which.max)
  H <- L
  HR <- (rgb[2, ] - rgb[3, ])/(maxs - mins)
  HG <- 2 + (rgb[3, ] - rgb[1, ])/(maxs - mins)
  HB <- 4 + (rgb[1, ] - rgb[2, ])/(maxs - mins)
  sel <- wmax == 1
  H[sel] <- HR[sel]
  sel <- wmax == 2
  H[sel] <- HG[sel]
  sel <- wmax == 3
  H[sel] <- HB[sel]
  H <- (H * 60)%%360
  H[mins == maxs] <- 0
  ret <- rbind(H = H, S = S, L = L, alpha = alpha)
  return(ret)
}

####**********************************************####

# Define a function for conversion of HSL to RGB (according to the plotwidgets package)

hsl2rgb <- function(hsl) {
  if (nrow(hsl) == 4) {
    alpha <- hsl[4, , drop = F]
    hsl <- hsl[-4, , drop = F]
  }
  else {
    alpha <- NULL
  }
  H <- hsl[1, ]
  S <- hsl[2, ]
  L <- hsl[3, ]
  C <- (1 - abs(2 * L - 1)) * S
  X <- C * (1 - abs(((H/60)%%2) - 1))
  m <- L - C/2
  rgb <- matrix(0, ncol = ncol(hsl), nrow = 3)
  rownames(rgb) <- c("R", "G", "B")
  iX <- c(2, 1, 3, 2, 1, 3)
  iC <- c(1, 2, 2, 3, 3, 1)
  for (i in 1:6) {
    sel <- 60 * (i - 1) <= H & H < 60 * i
    kX <- iX[i]
    kC <- iC[i]
    rgb[kX, sel] <- X[sel]
    rgb[kC, sel] <- C[sel]
  }
  rgb <- rgb + rep(m, each = 3)
  rgb <- round(rgb * 255)
  if (!is.null(alpha)) 
    rgb <- rbind(rgb, alpha = alpha)
  rgb
}

####**********************************************####

# Define a function for conversion of HSV to HEX color (according to the ColorPalette package)

hsv2col <- function(h, s, v) {
  h <- h/60
  chroma <- v * s
  X <- chroma * (1 - abs((h%%2) - 1))
  i <- floor(h)
  rgbVector <- switch(as.character(i), `0` = c(chroma, X, 0), 
                      `1` = c(X, chroma, 0), `2` = c(0, chroma, X), `3` = c(0, 
                                                                            X, chroma), `4` = c(X, 0, chroma), `5` = c(chroma, 
                                                                                                                       0, X))
  m <- v - chroma
  rgbVector <- rgbVector + m
  return(rgb(rgbVector[1], rgbVector[2], rgbVector[3]))
}

####**********************************************####

# Define a function for calculating X and Y coordinates based on Hue and Saturation

coordinate_calc <- function(HSL) {
  
  # Multiply Saturation (Hypotenuse) by 100 to bring it to a range of 0:100
  HSL$S <- HSL$S*100
  
  # Multiply Lightness by 100 to bring it to a range of 0:100
  HSL$L <- HSL$L*100
  
  ####************####
  
  # Calculate the Theta (angle) in Radians so that we can input them later in cos and sin functions
  HSL$theta <- apply(X = as.data.frame(HSL$H), MARGIN = 1, 
                     FUN = function(i) {
                       if(i <= 90) {
                         i*(pi/180)
                       } else if(i <= 180) {
                         (180 - i)*(pi/180)
                       } else if(i <= 270) {
                         (i - 180)*(pi/180)
                       } else if(i <= 360) {
                         (360 - i)*(pi/180)
                       }
                     }
  )
  
  ####************####
  
  # Calculate the Adjacent (X-side)
  HSL$adjacent <- cos(HSL$theta) * HSL$S
  
  ####************####
  
  # Calculate the Opposite (Y-side)
  HSL$opposite <- sin(HSL$theta) * HSL$S
  
  ####************####
  
  # Calculate the X coordinate
  HSL$X <- HSL$adjacent
  HSL$X[which(HSL$H > 90 & HSL$H < 270)] <- HSL$X[which(HSL$H > 90 & HSL$H < 270)] * -1
  
  # Calculate the Y coordinate
  HSL$Y <- HSL$opposite
  HSL$Y[which(HSL$H > 180 & HSL$H < 360)] <- HSL$Y[which(HSL$H > 180 & HSL$H < 360)] * -1
  
  ####************####
  
  # Prepare the results table
  Results <- data.frame(X = HSL$X,
                        Y = HSL$Y,
                        Z = HSL$L)
  
  return(Results)
  
}

####**********************************************####

# Define a function for calculating distance and mean distances

dist_calculate <- function(dataset) {
  
  ## Remove the lightness (L) from the table
  points_matrix <- dataset[,c(1,2)]
  
  points_matrix <- (ICSNP::pair.diff(as.matrix(points_matrix)))^2
  
  points_matrix <- sqrt(c(points_matrix[,1] + points_matrix[,2]))
  
  ####************####
  
  final.results <- list(Table = points_matrix,
                        Mean = round(mean(points_matrix, na.rm = TRUE), digits = 2),
                        SD = round(sd(points_matrix, na.rm = TRUE), digits = 2))
  
  return(final.results)
  
}

####**********************************************####

# Define a function for calculating distance and mean distances in 3D space

dist_calculate3D <- function(dataset) {
  
  points_matrix <- (ICSNP::pair.diff(as.matrix(dataset)))^2
  
  points_matrix <- sqrt(c(rowSums(points_matrix)))
  
  ####************####
  
  final.results <- list(Table = points_matrix,
                        Mean = round(mean(points_matrix, na.rm = TRUE), digits = 2),
                        SD = round(sd(points_matrix, na.rm = TRUE), digits = 2))
  
  return(final.results)
  
}

####**********************************************####

# Define a function for visualing as RainCload plot

"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

geom_flat_violin <- function(mapping = NULL, data = NULL, stat = "ydensity",
                             position = "dodge", trim = TRUE, scale = "area",
                             show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomFlatViolin,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      trim = trim,
      scale = scale,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomFlatViolin <-
  ggproto("GeomFlatViolin", Geom,
          setup_data = function(data, params) {
            data$width <- data$width %||%
              params$width %||% (resolution(data$x, FALSE) * 0.9)
            
            # ymin, ymax, xmin, and xmax define the bounding rectangle for each group
            data %>%
              group_by(group) %>%
              mutate(ymin = min(y),
                     ymax = max(y),
                     xmin = x,
                     xmax = x + width / 2)
            
          },
          
          draw_group = function(data, panel_scales, coord) {
            # Find the points for the line to go all the way around
            data <- transform(data, xminv = x,
                              xmaxv = x + violinwidth * (xmax - x))
            
            # Make sure it's sorted properly to draw the outline
            newdata <- rbind(plyr::arrange(transform(data, x = xminv), y),
                             plyr::arrange(transform(data, x = xmaxv), -y))
            
            # Close the polygon: set first and last point the same
            # Needed for coord_polar and such
            newdata <- rbind(newdata, newdata[1,])
            
            ggplot2:::ggname("geom_flat_violin", GeomPolygon$draw_panel(newdata, panel_scales, coord))
          },
          
          draw_key = draw_key_polygon,
          
          default_aes = aes(weight = 1, colour = "grey20", fill = "white", size = 0.5,
                            alpha = NA, linetype = "solid"),
          
          required_aes = c("x", "y")
  )

####**********************************************####

# Define the Toast options (shinywidgest and shinyfeedback)

myToastOptions <- list(
  positionClass = "toast-bottom-left",
  progressBar = TRUE,
  timeOut = 3000,
  closeButton = TRUE,
  
  # same as defaults
  newestOnTop = TRUE,
  preventDuplicates = FALSE,
  showDuration = 300,
  hideDuration = 1000,
  extendedTimeOut = 1000,
  showEasing = "linear",
  hideEasing = "linear",
  showMethod = "fadeIn",
  hideMethod = "fadeOut"
)

####**********************************************####

# JS script for maximizing the plot size based on the height and width of browser

maxPlotHeight <- "
shinyjs.init = function() {
  $(window).resize(shinyjs.calcHeight);
}
shinyjs.calcHeight = function() { 
  Shiny.onInputChange('plotHeight', $(window).height());
}
"

maxPlotWidth <- "
shinyjs.init = function() {
  $(window).resize(shinyjs.calcWidth);
}
shinyjs.calcWidth = function() { 
  Shiny.onInputChange('plotWidth', $(window).width());
}
"

####**********************************************####

# Fix the fileInput names
fixUploadedFilesNames <- function(x) {
    if (is.null(x)) {
        return()
    }
    
    oldNames = x$datapath
    newNames = file.path(dirname(x$datapath),
                         x$name)
    file.rename(from = oldNames, to = newNames)
    x$datapath <- newNames
    x
}

####***********************************************************####

# The App

# Create the UI
ui = bs4DashPage(
    
    title = "AutoClone",
    freshTheme = NULL,
    
    # Define a Auto-waiter
    autoWaiter(fadeout = TRUE, html = spin_pixel()),
    # Define the waiter preloader
    preloader = list(html = tagList(spin_pixel(), "Loading ..."), color = "#343a40"),

    # Define the header
    header = dashboardHeader(title = dashboardBrand(
        title = tags$i("Adrian Salavaty", style = "font-size:18px;"),
        color = "gray-dark",
        href = "https://www.abbassalavaty.com/",
        image = "Adrian (Abbas) Salavaty-Logo.jpg"
    ),
                             titleWidth = NULL,
                             disable = FALSE,
                             .list = NULL,
                             skin = "light",
                             status = "white",
                             border = TRUE,
                             compact = TRUE,
                             sidebarIcon = shiny::icon("bars"),
                             controlbarIcon = shiny::icon("th"),
                             fixed = FALSE,
    
                             leftUi = NULL,
    
    # Use Shinyjs
    useShinyjs(),
    
    # Use shinyFeedback
    useShinyFeedback(),
    
    # Set the style of the Screenshot (approve section) button
    tags$head(
        tags$style(HTML('#approveMajorSection{background-color:#28b78d; color:white;}
                         #approveMajorSection:hover {background-color:white; color:#28b78d;}'))
    ),
    
    # Add the favicon
    tags$head(tags$link(rel="shortcut icon", href="AutoClone_favicon.ico")),
    
    # Subscription
        tags$head(
            tags$script("Shiny.addCustomMessageHandler('close_subscription', function(x){
                  $('html').click();
                });")
        ),
                    shinyWidgets::dropdown(
                        inputId = "subscribeMenu",
                        style = "default", icon = NULL,
                        label = " Subscribe",
                        up = FALSE,tooltip = FALSE,
                        size = "xs",
                        status = "info",
                        animate = animateOptions(
                            enter = animations$fading_entrances$fadeInLeftBig,
                            exit = animations$fading_exits$fadeOutRightBig
                        ),
                        
                        textInput(inputId = "emailAddress_forSub", label = "Email Address", 
                                  placeholder = "Email Address"),
                        
                        textInput(inputId = "userName", label = "First Name", 
                                  placeholder = "Name"),
                        
                        actionBttn(
                            inputId = "submitSubscription",
                            size = "sm",
                            block = FALSE,
                            label = "Submit", 
                            style = "stretch",
                            color = "primary"
                        )
                    ),
    
                             rightUi = NULL
    ),
    
    # Define the Sidebar
    sidebar = dashboardSidebar(disable = FALSE,
                               width = NULL,
                               skin = "dark",
                               status = "lightblue",
                               elevation = 4,
                               collapsed = FALSE,
                               minified = TRUE,
                               expandOnHover = TRUE,
                               fixed = TRUE,
                               id = "main_sidebar",
                               customArea = NULL,
                               
                               # Sidebar Header
                               # sidebarHeader(title = "AutoClone Dashboard"),
                               
                               # sidebarUserPanel
                               sidebarUserPanel(
                                   image = "AutoClone-Logo.png",
                                   name = tags$a(tags$b("AutoClone Dashboard"), 
                                                 href = "https://github.com/asalavaty/AutoClone")
                               ),
                               
                               # Sidebar menu Introduction
                               
                               sidebarMenu(
                                   id = "main_intro",
                                   flat = TRUE,
                                   compact = TRUE,
                                   childIndent = TRUE,
                                   legacy = FALSE,

                                   ## Sidebar menu item 1
                                   menuItem(
                                       text = "Introduction",
                                       badgeLabel = NULL,
                                       badgeColor = "success",
                                       href = NULL,
                                       newTab = TRUE,
                                       selected = NULL,
                                       startExpanded = FALSE,
                                       condition = NULL,
                                       tabName = "introduction",
                                       icon = icon("file-alt")
                                   )),
                               br(),
                               
                               #############################
                               
                               # Sidebar menu Automatic Analysis
                               sidebarMenu(
                                   id = "automatic_analysis",
                                   flat = TRUE,
                                   compact = TRUE,
                                   childIndent = TRUE,
                                   legacy = FALSE,
                                   sidebarHeader("Automatic Analysis"),
                                   
                                   ## Sidebar menu item 1
                                   menuItem(
                                       text = "Brush-based",
                                       badgeLabel = NULL,
                                       badgeColor = "success",
                                       href = NULL,
                                       newTab = TRUE,
                                       selected = NULL,
                                       startExpanded = FALSE,
                                       condition = NULL,
                                       tabName = "brush_based",
                                       icon = icon("brush")
                                   ),
                                   
                                   ## Sidebar menu item 2
                                   menuItem(
                                       text = "Pre-cropped-based",
                                       badgeLabel = NULL,
                                       badgeColor = "success",
                                       href = NULL,
                                       newTab = TRUE,
                                       selected = NULL,
                                       startExpanded = FALSE,
                                       condition = NULL,
                                       tabName = "pre_cropped_based",
                                       icon = icon("crop-alt")
                                   )
                               ),
                               br(),
                               
                               #############################
                               
                               # Sidebar menu Semi-automatic Analysis
                               sidebarMenu(
                                   id = "semiautomatic_analysis",
                                   flat = TRUE,
                                   compact = TRUE,
                                   childIndent = TRUE,
                                   legacy = FALSE,
                                   sidebarHeader("Semi-automatic Analysis"),
                                   
                                   ## Sidebar menu item 1
                               menuItem(
                                   tabName = "color_coord_based",
                                   text = "Color/Coord-based",
                                   icon = icon("palette"),
                                   startExpanded = FALSE
                                   
                                   # menuSubItem(
                                   #     text = "Item 3",
                                   #     tabName = "tab3",
                                   #     icon = icon("circle-thin"),
                                   #     selected = NULL
                                   # )
                               ),
                               br(),
                               menuItem(tabName = "citation",
                                        text = "Citation",
                                        icon = icon("scroll"),
                                        startExpanded = FALSE),
                               menuItem(tabName = "about",
                                        text = "About",
                                        icon = icon("address-card"),
                                        startExpanded = FALSE),
                               menuItem(tabName = "contact",
                                        text = "Contact",
                                        icon = icon("envelope"),
                                        startExpanded = FALSE)
                               
                               )
                               ),
    
    # Define the Controlbar (right sidebar)
    controlbar = dashboardControlbar(id = "right_sidebar",
                                     disable = FALSE,
                                     width = 250,
                                     collapsed = TRUE,
                                     overlay = FALSE,
                                     skin = "light",
                                     pinned = TRUE,
                                     
                                     div(
                                         class = "p-3",
                                         # any content
                                     ),
                                     
                                     # Right sidebar menu
                                     controlbarMenu(
                                         id = "right_sidebar_menu",
                                         selected = NULL,
                                         type = "tabs",
                                         position = NULL,
                                         vertical = FALSE,
                                         side = "left",
                                         
                                         controlbarItem(title = "Skin", 
                                                        div(class = "p-3", skinSelector()),
                                                        value = NULL, icon = icon("palette"))
                                     )
    ),
    
    ##################################################################################
    
    # Define the Body
    body = dashboardBody(
        tabItems(
          
          #########################################################
          #########################################################
          
          # Define the body of automatic introduction analysis
          tabItem(
            tabName = "introduction",
            
            fluidRow(
              #######################
              # What is Clonality Assessment
              box(
                title = tags$b("What is the Clonality Assessment?"),
                id = NULL,
                status = "olive",
                solidHeader = FALSE,
                background = NULL,
                gradient = FALSE,
                width = 12,
                collapsible = TRUE,
                collapsed = FALSE,
                closable = FALSE,
                maximizable = FALSE,
                icon = NULL,
                boxToolSize = "xs",
                elevation = 2,
                headerBorder = TRUE,
                label = boxLabel("Intro", status = "info", 
                                 tooltip = "Intro: What is the Clonality Assessment?"),
                dropdownMenu = NULL,
                sidebar = NULL,
                footer = argonColumn(width = 12,
                                     tags$b("References"), br(),br(),
                                     tags$ol(style = "padding-left: 14px; text-align: justify; font-size: 16px;",
                                       tags$li("Weissman, Tamily A, and Y Albert Pan. “Brainbow: new resources and emerging biological applications for multicolor genetic labeling and analysis.” Genetics vol. 199,2 (2015): 293-306."), 
                                       tags$li("Tabansky, Inna et al. “Developmental bias in cleavage-stage mouse blastomeres.” Current biology : CB vol. 23,1 (2013): 21-31."), 
                                       tags$li("Hadjieconomou, Dafni et al. “Flybow: genetic multicolor cell labeling for neural circuit analysis in Drosophila melanogaster.” Nature methods vol. 8,3 (2011): 260-6."),
                                       tags$li("García-Moreno, Fernando et al. “CLoNe is a new method to target single progenitors and study their progeny in mouse and chick.” Development (Cambridge, England) vol. 141,7 (2014): 1589-98."),
                                       tags$li("Pan, Y Albert et al. “Zebrabow: multispectral cell labeling for cell tracing and lineage analysis in zebrafish.” Development (Cambridge, England) vol. 140,13 (2013): 2835-46."),
                                       tags$li("Livet, Jean et al. “Transgenic strategies for combinatorial expression of fluorescent proteins in the nervous system.” Nature vol. 450,7166 (2007): 56-62."),
                                       tags$li("Wu, J., Turcotte, R., Alt, C. et al. Defining Clonal Color in Fluorescent Multi-Clonal Tracking. Sci Rep 6, 24303 (2016)."),
                                       tags$li("Nguyen PD, Currie PD. Guidelines and best practices in successfully using Zebrabow for lineage tracing multiple cells within tissues. Methods. 2018;150:63-67."),
                                       tags$li("By SharkD - Own work, CC BY-SA 4.0, https://commons.wikimedia.org/w/index.php?curid=3260879")
                                     )
                                     ),
                argonRow(
                argonColumn(width = 7, style = "padding-right: 50px;",
                            p(style = "text-align: justify; font-size: 16px;",
                            "The clonality assessment is a technique used to evaluate the similarity of cells/fibers based on their color profiles. 
                            It is taking advantage of multi-color systems generally used for fate mapping and lineage tracing strategies upon biological 
                            processes such as development and regeneration. Among those systems, the “Brainbow” mouse consists of the stochastic multi-color 
                            labelling of neuronal cells [1]. This technique relies on the Cre-Lox recombination system that 
                            allows the combination of multiple fluorophores within the cell of interest. Subsequently, this technique 
                            has been adapted to several other tissues and model organisms such as the Rainbow (for a variety of mice/mammalian tissues) [2], 
                            Flybow (for neural circuit analysis in Drosophila melanogaster) [3], CLoNe (for different tissues and species of 
                            mouse and chick) [4], and Zebrabow (for different zebrafish organs and tissues) [5]. Importantly, the Zebrabow 
                            system has been key in uncovering muscle stem cell dynamics upon growth and regeneration, through the concept of 
                            clonality. Tissue-resident stem cells have the capacity to self-renew and give rise to progenies committed to terminal 
                            differentiation. Upon their expansion, these cells will generate clones that are believed to share some genetic features, 
                            such as the initial color they have been attributed. In order to identify cells coming from a common progenitor, one has 
                            to be able to distinguish them from the randomly distributed ones."), 
                            p(style = "text-align: justify; font-size: 16px;",
                              "One of the important aspects of multi-colored lineage tracing systems is the quantification and 
                              statistical evaluation of the results (i.e. converting visual colors to interpretable statistics). 
                              In this regard, several methods have been proposed so far for color quantification and clonality 
                              assessment [6, 7]. Nguyen and Currie in a Methods paper [8] proposed a novel approach for quantifying 
                              the clonality and assessing multiple color profiles of clones, which enables the simultaneous comparison of 
                              the clonality of several clusters of cells/fibers. This approach works based on the translation of color values, 
                              such as Hue and Saturation, to trigonometric points with X and Y coordinates. Accordingly, to statistically evaluate 
                              the color similarity between individual clones, distances between two points (equivalent to cell or fiber) are calculated. 
                              While a high distance value between two points indicates a stochastic profile, a low mean distance corresponds to a clonal 
                              profile. In this approach, the three-channel (HSL) profile of the clones is converted to a 2D space color wheel. However, it 
                              is possible to maintain all three channels and convert the HSL to a 3D space color profile (i.e. converting 
                              Hue, Saturation, and Lightness values, to obtain X, Y and Z coordinates). The following figures [9] illustrate 
                              how colors could be transformed into coordinates in a 2D or 3D space. The AutoClone web app implements both 
                              of these approaches, namely 2D and 3D color space-based coordinate measuremant, to perform clonality assessment 
                              in three different modes which are illustrated in the right-hand side figure and are explained in the following three boxes."
                            )
                            ),
                argonColumn(width = 5, center = TRUE,
                            tags$img(src = "AutoClone Workflow.png", width = "100%", height = "67%")
                            )
                ),
                argonRow(center = TRUE,
                         argonColumn(width = 7, center = TRUE,
                                     argonRow(center = TRUE,
                         argonColumn(width = 6, center = TRUE,
                                     tags$b("2D Color Space"), br(),
                                     tags$img(src = "HSLCircle.png", width = "60%", height = "60%")
                         ),
                         argonColumn(width = 6, center = TRUE, 
                                     tags$b("3D Color Space"),br(),
                                     tags$img(src = "HSLSphere.png", width = "60%", height = "60%")
                         )
                                     )
                         ),
                         argonColumn(width = 5, center = TRUE)
                )
              )
            ),
            
            fluidRow(
                     #######################
                     # Brush-based Mode
                     box(
                       title = tags$b("Brush-based Mode"),
                       id = NULL,
                       status = "olive",
                       solidHeader = FALSE,
                       background = NULL,
                       gradient = FALSE,
                       width = 4,
                       collapsible = TRUE,
                       collapsed = FALSE,
                       closable = FALSE,
                       maximizable = FALSE,
                       icon = NULL,
                       boxToolSize = "xs",
                       elevation = 2,
                       headerBorder = TRUE,
                       label = boxLabel("Mode 1", status = "info", 
                                        tooltip = "Mode 1: Brush-based autoclonization"),
                       dropdownMenu = NULL,
                       sidebar = NULL,
                       footer = NULL,
                       argonColumn(width = 12, 
                                   p(style = "text-align: justify; font-size: 16px;",
                                   "In the Brush-based mode of the app you can directly analyse your imaging files (in 
                                     TIFF, PNG, or JPG format) whithout any preprocessing. You may just upload your imaging files 
                                     and define your desired sections by brushing on the images using the toolset provided. A 
                                     detailed workflow on how to perfoerm AutoClonization through the Brush-based mode is provided 
                                     in the ", tags$em(tags$b("Brush-based tab.")), "Click on the following button to go to the 
                                   Brush-based tab."),
                                   
                                   actionBttn(
                                     inputId = "brush_basedMode_tabBtn",
                                     block = TRUE, size = "sm",
                                     label = "Jump to Brush-based Tab", 
                                     style = "fill",
                                     icon = icon("arrow-circle-up"),
                                     color = "primary"
                                   )
                                   )
                     ),
                     
                     #######################
                     # Pre-cropped-based Mode
                     box(
                       title = tags$b("Pre-cropped-based Mode"),
                       id = NULL,
                       status = "olive",
                       solidHeader = FALSE,
                       background = NULL,
                       gradient = FALSE,
                       width = 4,
                       collapsible = TRUE,
                       collapsed = FALSE,
                       closable = FALSE,
                       maximizable = FALSE,
                       icon = NULL,
                       boxToolSize = "xs",
                       elevation = 2,
                       headerBorder = TRUE,
                       label = boxLabel("Mode 2", status = "info", 
                                        tooltip = "Mode 2: Pre-cropped-based autoclonization"),
                       dropdownMenu = NULL,
                       sidebar = NULL,
                       footer = NULL,
                       argonColumn(width = 12, 
                                   p(style = "text-align: justify; font-size: 16px;",
                                     "In the Pre-cropped-based mode of the app you can analyse your imaging section files (in 
                                     TIFF, PNG, or JPG format) that you have previously generated. You may just upload your pre-cropped section files 
                                     and performing the AutoClonization. A 
                                     detailed workflow on how to perfoerm AutoClonization through the Pre-cropped-based mode is provided 
                                     in the ", tags$em(tags$b("Pre-cropped-based tab.")), "Click on the following button to go to the 
                                   Pre-cropped-based tab."),
                                   
                                   actionBttn(
                                     inputId = "Pre_cropped_basedMode_tabBtn",
                                     block = TRUE, size = "sm",
                                     label = "Jump to Pre-cropped-based Tab", 
                                     style = "fill",
                                     icon = icon("arrow-circle-up"),
                                     color = "primary"
                                   )
                       )
                     ),
                     
                     #######################
                     # Color/Coord-based Mode
                     box(
                       title = tags$b("Color/Coord-based Mode"),
                       id = NULL,
                       status = "olive",
                       solidHeader = FALSE,
                       background = NULL,
                       gradient = FALSE,
                       width = 4,
                       collapsible = TRUE,
                       collapsed = FALSE,
                       closable = FALSE,
                       maximizable = FALSE,
                       icon = NULL,
                       boxToolSize = "xs",
                       elevation = 2,
                       headerBorder = TRUE,
                       label = boxLabel("Mode 3", status = "info", 
                                        tooltip = "Mode 3: Color/Coord-based autoclonization"),
                       dropdownMenu = NULL,
                       sidebar = NULL,
                       footer = NULL,
                       argonColumn(width = 12, 
                                   p(style = "text-align: justify; font-size: 16px;",
                                     "In the Color/Coord-based mode of the app you can perform the AutoClonization based on the
                                     colors or coordinates you have previously generated for your desired sections. A 
                                     detailed workflow on how to perfoerm AutoClonization through the this mode is provided 
                                     in the ", tags$em(tags$b("Color/Coord-based tab.")), "Click on the following button to go to the 
                                   Color/Coord-based tab. Also, template file types are shown in the box below."),
                                   
                                   actionBttn(
                                     inputId = "Color_Coord_basedMode_tabBtn",
                                     block = TRUE, size = "sm",
                                     label = "Jump to Color/Coord-based Tab", 
                                     style = "fill",
                                     icon = icon("arrow-circle-up"),
                                     color = "primary"
                                   )
                       )
                     )
            ),
            
            fluidRow(
              #######################
              # Color/Coord file types
              box(
                title = tags$b("Color/Coordinate File Templates"),
                id = NULL,
                status = "olive",
                solidHeader = FALSE,
                background = NULL,
                gradient = FALSE,
                width = 12,
                collapsible = TRUE,
                collapsed = FALSE,
                closable = FALSE,
                maximizable = FALSE,
                icon = NULL,
                boxToolSize = "xs",
                elevation = 2,
                headerBorder = TRUE,
                label = boxLabel("Templates", status = "info", 
                                 tooltip = "Color/Coordinate File Templates"),
                dropdownMenu = NULL,
                sidebar = NULL,
                footer = NULL,
                
                # Color/Coordinate File Templates
                bs4TabCard(width = 12, solidHeader = TRUE, 
                           type = "tabs",
                           maximizable = FALSE,
                           background = "white",
                           status = "purple",
                           icon = NULL,
                           
                           tabPanel(title = p("HEX"),
                                    argonRow(
                                    argonColumn(width = 4,
                                                p("The columns Image and Group are optional.")
                                                ),
                                    argonColumn(width = 4,center = TRUE,
                                                tableOutput("HEX_sample_table")
                                    ),
                                    argonColumn(width = 4,
                                    )
                                    )
                           ),
                           tabPanel(title = p("RGB"),
                                    argonRow(
                                    argonColumn(width = 4,
                                                p("The columns Image and Group are optional.")
                                    ),
                                    argonColumn(width = 4,center = TRUE,
                                                tableOutput("RGB_sample_table")
                                    ),
                                    argonColumn(width = 4,
                                    )
                                    )
                                    
                           ),
                           tabPanel(title = p("RGB from Fiji"),
                                    argonRow(
                                      argonColumn(width = 3,
                                                  p("The columns Image and Group are optional.")
                                      ),
                                      argonColumn(width = 4, center = TRUE,
                                                  tags$b("Example 1"), br(), br(),
                                                  tableOutput("RGBFiji1_sample_table")
                                      ),
                                      argonColumn(width = 4, center = TRUE, 
                                                  tags$b("Example 2"), br(), br(),
                                                  tableOutput("RGBFiji2_sample_table")
                                      )
                                    )
                                    
                           ),
                           tabPanel(title = p("HSL"),
                                    argonRow(
                                      argonColumn(width = 4,
                                                  p("The columns Image and Group are optional.")
                                      ),
                                      argonColumn(width = 4,center = TRUE,
                                                  tableOutput("HSL_sample_table")
                                      ),
                                      argonColumn(width = 4,
                                      )
                                    )
                                    
                           ),
                           tabPanel(title = p("HSV"),
                                    argonRow(
                                      argonColumn(width = 4,
                                                  p("The columns Image and Group are optional.")
                                      ),
                                      argonColumn(width = 4,center = TRUE,
                                                  tableOutput("HSV_sample_table")
                                      ),
                                      argonColumn(width = 4,
                                      )
                                    )
                                    
                           ),
                           tabPanel(title = p("X/Y coordinates"),
                                    argonRow(
                                      argonColumn(width = 4,
                                                  p("The columns Image and Group are optional.")
                                      ),
                                      argonColumn(width = 4,center = TRUE,
                                                  tableOutput("XY_sample_table")
                                      ),
                                      argonColumn(width = 4,
                                      )
                                    )
                                    
                           )
                )
                
                )
            )
          ),
            
            #########################################################
            #########################################################
            # Define the body of automatic brush_based analysis
            tabItem(
                tabName = "brush_based",
                
                # Column 0
                         #######################
                         # Workflow
                         box(
                           title = "Workflow",
                           id = NULL,
                           status = NULL,
                           solidHeader = FALSE,
                           background = "secondary",
                           gradient = TRUE,
                           width = 12,
                           collapsible = TRUE,
                           collapsed = TRUE,
                           closable = TRUE,
                           maximizable = FALSE,
                           icon = icon("road"),
                           boxToolSize = "xs",
                           elevation = 4,
                           headerBorder = TRUE,
                           label = boxLabel("Step 0", status = "info", 
                                            tooltip = "Workflow: Use the following pipeline to to perform the analyses."),
                           dropdownMenu = NULL,
                           sidebar = NULL,
                           footer = NULL,
                                    tags$b("Follow the workflow explained below to perform the Brush-based AutoClonization.",
                                      style = "text-align: justify; font-size: 16px;"),
                           br(),br(),
                           argonRow(center = TRUE,
                           argonColumn(width = 6,
                                       argonColumn(width = 11, offset = 0.5,
                               p(tags$b("I. ", style = "font-size: 18px;"),
                                 "Select your desired image by clicking on its corresponding button in ",
                                 tags$b("Step 1"), ".",
                                 style = "font-size: 14px; border-bottom:1px dotted grey;"),

                               p(tags$b("II. ", style = "font-size: 18px;"),
                                 "Define a new section by clicking on the button ", tags$b("New Section"), " in ",
                                 tags$b("Step 2"), ".",
                                 style = "font-size: 14px; border-bottom:1px dotted grey;"),
                               
                               p(tags$b("III. ", style = "font-size: 18px;"),
                                 "Select your desired clone of cells/fibers by dragging/brushing on the image in ",
                                 tags$b("Step 2"), ".",
                                 style = "font-size: 14px; border-bottom:1px dotted grey;"),
                               
                               p(tags$b("IV. ", style = "font-size: 18px;"),
                                 "After you are happy with your selection, click on the button ", tags$b("Approve Section"), " in ",
                                 tags$b("Step 2"), ".",
                                 style = "font-size: 14px; border-bottom:1px dotted grey;"),
                               
                               p(tags$b("V. ", style = "font-size: 18px;"),
                                 "Download the color scheme plots of your defind section in .",
                                 tags$b("Step 3"), ".",
                                 style = "font-size: 14px; border-bottom:1px dotted grey;")
                           )),
                           argonColumn(width = 6,
                                       argonColumn(width = 11, offset = 0.5,
                                       p(tags$b("VI. ", style = "font-size: 18px;"),
                                         "Select your desired color space mode and perform autoclonization by clicking on the button ", tags$b("Autoclonize"), " in ",
                                         tags$b("Step 4"), ".",
                                         style = "font-size: 14px; border-bottom:1px dotted grey;"),
                                       
                                       p(tags$b("VII. ", style = "font-size: 18px;"),
                                         "Download the Mean Distance plot and the table of Autoclonization results in ",
                                         tags$b("Step 4"), ".",
                                         style = "font-size: 14px; border-bottom:1px dotted grey;"),
                                       
                                       p(tags$b("VIII. ", style = "font-size: 18px;"),
                                         "Download the RanCload statistics plot of sections distances in ",
                                         tags$b("Step 5"), ".",
                                         style = "font-size: 14px; border-bottom:1px dotted grey;"),
                                       
                                       p(tags$b("IX. ", style = "font-size: 18px;"),
                                         "Download the section hierarchical clustering plot in ",
                                         tags$b("Step 6"), ".",
                                         style = "font-size: 14px; border-bottom:1px dotted grey;"),
                                       
                                       p(tags$b("X. ", style = "font-size: 18px;"),
                                         "Download the table of ANOVA results in .",
                                         tags$b("Step 6"), ".",
                                         style = "font-size: 14px; border-bottom:1px dotted grey;")
                                       ))
                           )
                         ),
                
                fluidRow(
                  
                  # Column 1 and 4
                  column(width = 4,
          
                    #######################
                    # Input Major Image
                    box(
                        title = "Input Image(s)",
                        id = "input_major_image",
                        status = "navy",
                        solidHeader = TRUE,
                        background = NULL,
                        gradient = FALSE,
                        width = 12,
                        collapsible = TRUE,
                        collapsed = FALSE,
                        closable = FALSE,
                        maximizable = FALSE,
                        icon = icon("file-image"),
                        boxToolSize = "xs",
                        elevation = 2,
                        headerBorder = TRUE,
                        label = boxLabel("Step 1", status = "info", 
                                         tooltip = "Step 1: Upload your image(s)"),
                        dropdownMenu = NULL,
                        sidebar = NULL,
                        footer = NULL,
                        argonRow(center = TRUE,
                                 p(tags$b("NOTE:"), "If your images belong to several conditions (", tags$i("e.g."),
                                   "test (mutant) versus control (wild type) samples), define the required 
                                   groups by clicking on the following button.",
                                   style = "text-align: justify; font-size: 16px;"),
                                 
                                 column(12,
                                        style='margin-bottom:15px; padding: 5px;',
                                        
                                 # Define group button
                                 actionBttn(
                                     inputId = "add_group",
                                     icon = icon("folder-plus"),
                                     size = "xs", block = TRUE, no_outline = TRUE,
                                     label = " Add a New Group",
                                     style = "fill", 
                                     color = "primary"
                                 ))
                                 ),
                                 
                                 argonColumn(width = 12, center = FALSE,
                                             style='border-top:1px solid; padding: 10px',
                                 fileInput("major_image", label = p(icon("upload"), " Upload your original imaging file(s) (TIFF/PNG/JPG)", 
                                                                    style = "padding:0px; margin:0; text-align: justify; font-size: 14px"),
                                           accept = c(".png", ".tif", ".tiff", ".jpg", ".jpeg"),
                                           width = "100%",multiple = TRUE,
                                           buttonLabel = "Browse")
                        ),
                        argonR::argonRow(center = TRUE, 
                                        
                                         hidden(
                                         argonColumn(width = 6, center = TRUE, id = "majorImage_header",
                                                     tags$b("Image")                                 )
                                         ),
                                 
                                         hidden(
                                 argonColumn(width = 6, center = TRUE, id = "majorGroup_header",
                                             tags$b("Group")
                                             )
                                         )
                            ),
                        
                        argonR::argonRow(center = TRUE,
                                         argonColumn(width = 6, center = TRUE,
                                                     uiOutput(outputId = "image_buttons")
                                         ),
                                         hidden(
                                         argonColumn(width = 6, center = TRUE, id = 'groupPicker_column',
                                                     style='border-left:1px dotted grey;',
                                                     uiOutput(outputId = "image_groups")
                                         )
                                         )
                                         ),
                        hidden(
                        argonR::argonRow(center = TRUE, id = "clearGrpsRow",
                                         style='border-top:1px solid grey; padding-top:10px',
                                         actionBttn(
                                             inputId = "clearGrpsBtn",
                                             label = "Clean Groups",
                                             icon = icon("trash-alt"),
                                             style = "fill",
                                             color = "danger",
                                             size = "xs",
                                             block = TRUE,
                                             no_outline = TRUE
                                         )
                        )
                    )
                    ),
                    
                    #########################################
                    
                    # Autoclonize
                    box(
                      title = "Autoclonize",
                      id = "autoclonize_box",
                      status = "navy",
                      solidHeader = TRUE,
                      background = NULL,
                      gradient = FALSE,
                      width = 12,
                      collapsible = TRUE,
                      collapsed = FALSE,
                      closable = FALSE,
                      maximizable = FALSE,
                      icon = icon("robot"),
                      boxToolSize = "xs",
                      elevation = 2,
                      headerBorder = TRUE,
                      label = boxLabel("Step 4", status = "info", 
                                       tooltip = "Step 4: Calculate the clonality of selected sections."),
                      dropdownMenu = NULL,
                      sidebar = NULL,
                      footer = NULL,
                      bs4Dash::tooltip(title = "Refer to the Introduction tab for more info", placement = "right",
                      prettyRadioButtons(
                        inputId = "majorImg_colorSpaceMode",
                        label = "Select a color space mode:", 
                        choiceNames = c("3D Color Space (Recommended)", "2D Color Space"),
                        choiceValues =  c(3,2),
                        icon = icon("check"), 
                        bigger = TRUE,
                        status = "primary",
                        animation = "jelly"
                      )
                      ),
                      fluidRow(center = TRUE,
                               p(icon("hand-pointer"), "Click on the following button to calculate the clonality of all of the sections you defined in the previous step (", tags$b("Step 2"), ").",
                                 style = "text-align: justify; font-size: 16px;"),
                               
                               # Autoclonize Button
                               actionBttn(
                                 inputId = "autoclonizeBtn",
                                 block = TRUE, size = "xs",
                                 label = "Autoclonize", 
                                 style = "fill",
                                 icon = icon("calculator"),
                                 color = "royal"
                               )
                      ),
                      
                      hidden(
                      argonR::argonRow(center = TRUE,
                                       id = "autoclonizeResRow",
                                       style='margin-top:20px;',
                                       
                                       # Clonality tabs
                                       bs4TabCard(width = 12, solidHeader = TRUE, 
                                                  type = "tabs",
                                                  maximizable = TRUE,
                                                  background = "white",
                                                  status = "danger",
                                                  icon = icon("eye-dropper"),
                                                  
                                                  tabPanel(title = p("Clonality Plot", style = "color:#302E34"),
                                                           
                                                           # Show Plot title
                                                           tags$b("Show title?"),
                                                           prettyToggle(
                                                             inputId = "secDistsPlotTitleOption",
                                                             label_on = "Yes!", 
                                                             icon_on = icon("check"),
                                                             status_on = "success",
                                                             status_off = "warning", 
                                                             label_off = "No",
                                                             icon_off = icon("times"),
                                                             value = FALSE
                                                           ),
                                                           hidden(
                                                           textInput(inputId = "secDistsPlotTitle", 
                                                                     label = NULL, 
                                                                     value = "Mean Distance Plot", 
                                                                     width = "100%", 
                                                                     placeholder = "Type in your desired plot title")
                                                           ),
                                                           
                                                           argonRow(
                                                           
                                                           # Show group names
                                                           argonColumn(width = 6, center = FALSE,
                                                                       tags$b("Add group names?"),
                                                           prettyToggle(
                                                             inputId = "secDistsPlotGroupNames",
                                                             label_on = "Yes!", 
                                                             icon_on = icon("check"),
                                                             status_on = "success",
                                                             status_off = "warning", 
                                                             label_off = "No",
                                                             icon_off = icon("times"),
                                                             value = TRUE
                                                           )
                                                           ),
                                                           
                                                           # Show image names
                                                           argonColumn(width = 6, center = FALSE,
                                                                       tags$b("Add image names?"),
                                                           prettyToggle(
                                                             inputId = "secDistsPlotImageNames",
                                                             label_on = "Yes!", 
                                                             icon_on = icon("check"),
                                                             status_on = "success",
                                                             status_off = "warning", 
                                                             label_off = "No",
                                                             icon_off = icon("times"),
                                                             value = FALSE
                                                           )
                                                           )
                                                           ),
                                                           
                                                           # Show error bars
                                                                       tags$b("Add error bars?"),
                                                           prettyToggle(
                                                             inputId = "secDistsPlotErrorBars",
                                                             label_on = "Yes!", 
                                                             icon_on = icon("check"),
                                                             status_on = "success",
                                                             status_off = "warning", 
                                                             label_off = "No",
                                                             icon_off = icon("times"),
                                                             value = TRUE
                                                           ),
                                                          
                                                           # Select color palette
                                                           pickerInput(
                                                             inputId = "secDistsPlotPalette",
                                                             width = "100%",
                                                             label = "Color Palette", 
                                                             choices = fishualize::fish_palettes(),
                                                             selected = "Centropyge_loricula",
                                                             options = list(
                                                               size = 5,
                                                               `live-search` = TRUE)
                                                           ),
                                                           
                                                           plotOutput(outputId = "majorSecClonalityPlot") %>% shinycssloaders::withSpinner(type = 4),
                                                           
                                                           br(),
                                                           
                                                           argonR::argonRow(
                                                                            column(6,
                                                                                   numericInput(inputId = "majorSecClonality.figure.width", label = "Figure width (in)", value = 8,
                                                                                                min = 1, max = Inf, step = 1, width = "100%")
                                                                            ),
                                                                            column(6,
                                                                                   numericInput(inputId = "majorSecClonality.figure.height", label = "Figure height (in)", value = 8,
                                                                                                min = 1, max = Inf, step = 1, width = "100%")
                                                                            )
                                                           ),
                                                           
                                                           argonR::argonRow(center = TRUE,
                                                                            column(12,
                                                                                   numericInput(inputId = "majorSecClonality.PNG.resolution", label = "PNG file resolution (dpi)", value = 300,
                                                                                                min = 72, max = Inf, step = 1, width = "100%"))
                                                           ),
                                                           
                                                           argonR::argonRow(
                                                                            column(6,
                                                                                   downloadButton("download_majorSecClonality_PDF", "Download PDF file", icon = icon("download"),
                                                                                                  class = "btn-sm btn-block")),
                                                                            column(6,
                                                                                   downloadButton("download_majorSecClonality_PNG", "Download PNG file", icon = icon("download"),
                                                                                                  class = "btn-sm btn-block")
                                                                            )
                                                           )
                                                  ),
                                                  
                                                  tabPanel(title = p("Clonality Table", style = "color:#302E34"),
                                                           
                                                           DT::dataTableOutput("majorSecClonalityTable") %>% shinycssloaders::withSpinner(type = 4)
                                                           
                                                  )
                                       )
                      )
                      )
                    )
                    
                    ),
                    
                    #######################
                  
                  # Column 2 and 5
                  column(width = 4,  
                  
                    # Define Sections
                    box(
                        title = "Define Sections",
                        id = "major_image_visualization",
                        status = "navy",
                        solidHeader = TRUE,
                        background = NULL,
                        gradient = FALSE,
                        width = 12,
                        collapsible = TRUE,
                        collapsed = FALSE,
                        closable = FALSE,
                        maximizable = TRUE,
                        icon = icon("image"),
                        boxToolSize = "xs",
                        elevation = 2,
                        headerBorder = TRUE,
                        label = boxLabel("Step 2", status = "info", 
                                         tooltip = "Step 2: Define and approve your desired sections(s)"),
                        dropdownMenu = NULL,
                        # boxDropdown(
                        # boxDropdownItem(actionBttn(inputId = "remove_major_image_bttn", 
                        # label = "Remove image", style = "minimal", 
                        # color = "danger", size = "xs", 
                        # icon = icon("trash-alt"), block = TRUE, no_outline = TRUE), 
                        #                 id = "remove_major_image", 
                        #                 icon = NULL))
                        sidebar = NULL,
                        footer = NULL,
                        argonRow(center = TRUE, 
                                 p(icon("hand-pointer"), "Click on your desired image button generated in", tags$b("Step 1"), "to illustrate it below.",
                                   style = "text-align: justify; font-size: 16px;"),
                                 
                                 p(tags$b("NOTE:"), "You can click on the maximize button on the top right corner of the box 
                                   to have a look at the image at a larger scale before clone selection.",
                                   style = "text-align: justify; font-size: 16px; padding-bottom:1px; margin-bottom:1px")
                                 ),
                        
                        argonR::argonRow(center = TRUE, id = "defineMajorSectionRow",
                                         style='padding-bottom:10px; padding-top:1px; margin-top:1px;',
                                         
                                         argonColumn(width = 6, center = TRUE,
                                         actionBttn(
                                             inputId = "defineMajorSection",
                                             label = "New Section",
                                             icon = icon("plus-circle"),
                                             style = "fill",
                                             color = "primary",
                                             size = "xs",
                                             block = TRUE,
                                             no_outline = TRUE
                                         )
                                         ),
                                         argonColumn(width = 6, center = TRUE, 
                                                     # load snapper into the app for taking screen shot of the section
                                                     load_snapper(),
                                                     
                                                     # add a download link for the section by id
                                                     uiOutput(outputId = "majorImgScreenshotBtnUI")
                                         )
                        ),
                        
                        hidden(
                        argonR::argonRow(center = TRUE, id = "majorPlotRow",
                                 
                                 # Use the maxPlotHeight JS script
                                 shinyjs::extendShinyjs(text = maxPlotHeight, functions = c("init", "calcHeight")),
                                 
                                 # Use the maxPlotWidth JS script
                                 shinyjs::extendShinyjs(text = maxPlotWidth, functions = c("init", "calcWidth")),
                                 
                                 argonR::argonColumn(center = TRUE, width = 12, id = "majorPlotColumn",
                                 
                                 plotOutput("uploaded_major_image",
                                            brush = brushOpts(
                                                id = "major_image_zoom_section",
                                                fill = NA,
                                                stroke = "white",
                                                opacity = 1,
                                                resetOnNew = TRUE
                                            )
                                 ) %>% shinycssloaders::withSpinner(type = 4)
                                 )
                        )
                        )
                    ),
                    
                    #####################
                    # Distance statistics
                    box(
                      title = "Distance Statistics",
                      id = "majorImage_DistanceStatistics",
                      status = "navy",
                      solidHeader = TRUE,
                      background = NULL,
                      gradient = FALSE,
                      width = 12,
                      collapsible = TRUE,
                      collapsed = TRUE,
                      closable = FALSE,
                      maximizable = TRUE,
                      icon = icon("chart-bar"),
                      boxToolSize = "xs",
                      elevation = 2,
                      headerBorder = TRUE,
                      label = boxLabel("Step 5", status = "info", 
                                       tooltip = "Step 5: Visualize Distance Statistics"),
                      dropdownMenu = NULL,
                      sidebar = NULL,
                      footer = NULL,
                      
                      hidden(
                      argonRow(id = "majorImageDistStatsRow", 
                               argonColumn(width = 12, 
                      
                      # Facet by
                      argonRow(
                      radioGroupButtons(
                        inputId = "majorImgSecsFacet",
                        label = "Categorize top facet by:",
                        choices = c("Do not categorize" = 1, 
                                    "Image" = "ImgName", 
                                    "Group" = "GroupName"),
                        individual = TRUE,
                        checkIcon = list(
                          yes = tags$i(class = "fa fa-circle", 
                                       style = "color: steelblue"),
                          no = tags$i(class = "fa fa-circle-o", 
                                      style = "color: steelblue"))
                      )
                      ),
                      
                      # Show Plot title
                      argonColumn(width = 12,
                      argonRow(
                      tags$b("Show title?")
                      ),
                      argonRow(
                      prettyToggle(
                        inputId = "secDistsStatsPlotTitleOption",
                        label_on = "Yes!", 
                        icon_on = icon("check"),
                        status_on = "success",
                        status_off = "warning", 
                        label_off = "No",
                        icon_off = icon("times"),
                        value = FALSE
                      ),
                      hidden(
                      textInput(inputId = "secDistsStatsPlotTitle", 
                                label = NULL, 
                                value = "Rain-Cloud Statistics Plot of Distances", 
                                width = "100%", 
                                placeholder = "Type in your desired plot title")
                      )
                      )
                      ),
                      
                      argonRow(
                               # Show group names
                               argonColumn(width = 6, center = FALSE,
                                           tags$b("Add group names?"),
                                           prettyToggle(
                                             inputId = "secDistsStatsPlotGroupNames",
                                             label_on = "Yes!", 
                                             icon_on = icon("check"),
                                             status_on = "success",
                                             status_off = "warning", 
                                             label_off = "No",
                                             icon_off = icon("times"),
                                             value = TRUE
                                           )
                               ),
                               
                               # Show image names
                               argonColumn(width = 6, center = FALSE,
                                           tags$b("Add image names?"),
                                           prettyToggle(
                                             inputId = "secDistsStatsPlotImageNames",
                                             label_on = "Yes!", 
                                             icon_on = icon("check"),
                                             status_on = "success",
                                             status_off = "warning", 
                                             label_off = "No",
                                             icon_off = icon("times"),
                                             value = FALSE
                                           )
                               )
                      ),
                    
                      
                      # Select color palette
                      pickerInput(
                        inputId = "secDistsStatsPlotPalette",
                        width = "100%",
                        label = "Color Palette", 
                        choices = fishualize::fish_palettes(),
                        selected = "Centropyge_loricula",
                        options = list(
                          size = 5,
                          `live-search` = TRUE)
                      ),
                      
                      plotOutput(outputId = "majorSecClonalityStatsPlot") %>% shinycssloaders::withSpinner(type = 4),
                      
                      br(),
                      
                      argonR::argonRow(
                                       column(6,
                                              numericInput(inputId = "majorSecStatsClonality.figure.width", label = "Figure width (in)", value = 8,
                                                           min = 1, max = Inf, step = 1, width = "100%")
                                       ),
                                       column(6,
                                              numericInput(inputId = "majorSecStatsClonality.figure.height", label = "Figure height (in)", value = 8,
                                                           min = 1, max = Inf, step = 1, width = "100%")
                                       )
                      ),
                      
                      argonR::argonRow(center = TRUE,
                                       column(12,
                                              numericInput(inputId = "majorSecStatsClonality.PNG.resolution", label = "PNG file resolution (dpi)", value = 300,
                                                           min = 72, max = Inf, step = 1, width = "100%"))
                      ),
                      
                      argonR::argonRow(
                                       column(6,
                                              downloadButton("download_majorSecStatsClonality_PDF", "Download PDF file", icon = icon("download"),
                                                             class = "btn-sm btn-block")),
                                       column(6,
                                              downloadButton("download_majorSecStatsClonality_PNG", "Download PNG file", icon = icon("download"),
                                                             class = "btn-sm btn-block")
                                       )
                      )
                      
                    )
                      )
                      )
                    )
                    ),
                    
                    #######################
                  
                  # Column 3 and 6
                  column(width = 4,  
                  
                    # Define clones
                    box(
                        title = "Visualize Section Palette",
                        id = "define_colonies",
                        status = "navy",
                        solidHeader = TRUE,
                        background = NULL,
                        gradient = FALSE,
                        width = 12,
                        collapsible = TRUE,
                        collapsed = FALSE,
                        closable = FALSE,
                        maximizable = FALSE,
                        icon = icon("object-group"),
                        boxToolSize = "xs",
                        elevation = 2,
                        headerBorder = TRUE,
                        label = boxLabel("Step 3", status = "info", 
                                         tooltip = "Step 3: Visualize the color palette of the approved section."),
                        dropdownMenu = NULL,
                        sidebar = NULL,
                        
                        # Explain workflow in the footer
                        footer = NULL, 
                        hidden(
                        argonR::argonRow(center = TRUE, id = "majorSecPlotsRow",

                                         bs4TabCard(width = 12, solidHeader = TRUE, 
                                                    type = "tabs",
                                                    background = "white",
                                                    status = "danger",
                                                    icon = icon("eye-dropper"),

                                           tabPanel(title = p("Circle Palette", style = "color:#302E34"),
                                                    tags$b("Show color legend"),
                                                    switchInput(
                                                      inputId = "majorCirclePalLegend",
                                                      label = "<i class=\"fa fa-thumbs-up\"></i>",
                                                      value = FALSE,
                                                      size = "mini"
                                                    ),
                                                    
                                                    sliderInput(
                                                      inputId = "majorCirclePalN",
                                                      label = "Max number of distinct colors",
                                                      min = 1,
                                                      max = 100,
                                                      value = 5,
                                                      step = 1,
                                                      round = TRUE,
                                                      ticks = FALSE,
                                                      width = "100%",
                                                      sep = ","
                                                    ),
                                                    
                                                    plotOutput(outputId = "majorSecCirclePlot") %>% shinycssloaders::withSpinner(type = 4),
                                                    
                                                    argonR::argonRow(center = TRUE,
                                                                     column(6,
                                                                            numericInput(inputId = "majorSecCircle.figure.width", label = "Figure width (in)", value = 8,
                                                                                         min = 1, max = Inf, step = 1, width = "100%")
                                                                     ),
                                                                     column(6,
                                                                            numericInput(inputId = "majorSecCircle.figure.height", label = "Figure height (in)", value = 8,
                                                                                         min = 1, max = Inf, step = 1, width = "100%")
                                                                     )
                                                                     ),
                                                    
                                                    argonR::argonRow(center = TRUE,
                                                                     column(12,
                                                                            numericInput(inputId = "majorSecCircle.PNG.resolution", label = "PNG file resolution (dpi)", value = 300,
                                                                                         min = 72, max = Inf, step = 1, width = "100%"))
                                                    ),
                                                    
                                                    argonR::argonRow(center = TRUE,
                                                    column(6,
                                                           downloadButton("download_majorSecCircle_PDF", "Download PDF file", icon = icon("download"),
                                                                          class = "btn-sm btn-block")),
                                                    column(6,
                                                           downloadButton("download_majorSecCircle_PNG", "Download PNG file", icon = icon("download"),
                                                                          class = "btn-sm btn-block")
                                                    )
                                                    )
                                                    ),
                                           
                                           tabPanel(title = p("Pixel Plot", style = "color:#302E34"),
                                                    sliderInput(
                                                      inputId = "majorPixelPlotN",
                                                      label = "Number of pixels",
                                                      min = 500,
                                                      max = 10000,
                                                      value = 3000,
                                                      step = 100,
                                                      round = TRUE,
                                                      ticks = FALSE,
                                                      width = "100%",
                                                      sep = ","
                                                    ),
                                                    
                                                    argonR::argonColumn(center = TRUE, width = 12,
                                                      plotOutput(outputId = "majorSecPixelPlot") %>% shinycssloaders::withSpinner(type = 4),
                                                      
                                                      
                                                    
                                                    shiny::uiOutput(
                                                      outputId = "download_majorSecPixel")
                                                    )
                                                    
                                                      ),
                                           
                                           tabPanel(title = p("Histogram", style = "color:#302E34"),
                                                    
                                                    argonR::argonColumn(center = TRUE, width = 12,
                                                    
                                                    plotOutput(outputId = "majorSecDistHist") %>% shinycssloaders::withSpinner(type = 4),
                                                    
                                                    shiny::uiOutput(
                                                      outputId = "download_majorSecDistHist")
                                                    )
                                                    
                                           )
                                         )
                        )
                        )
                    ),
                    
                    #####################################
                    
                    # Clustering and ANOVA
                    tabBox(
                      title = NULL,
                      id = "majorImage_ClustANOVA_box",
                      status = "navy",
                      solidHeader = TRUE,
                      background = NULL,
                      gradient = FALSE,
                      width = 12,
                      collapsible = TRUE,
                      collapsed = FALSE,
                      closable = FALSE,
                      maximizable = TRUE,
                      icon = NULL,
                      boxToolSize = "xs",
                      elevation = 2,
                      headerBorder = TRUE,
                      label = boxLabel("Step 6", status = "info", 
                                       tooltip = "Step 6: Clustering and Variance Analysis of Sections"),
                      dropdownMenu = NULL,
                      sidebar = NULL,
                      footer = NULL,
                      
                      ## Clustering tab
                      tabPanel(title = p("Clustering", style = "color:white"),
                               
                               hidden(
                               argonRow(center = TRUE, id = "majorImage_ClustNotifRow",
                                        tags$h6(br(),
                                                tags$b("The number of defined sections are less than 3 and the clustering cannot be done!"),
                                                br(),
                                                br(),
                                                style = "background-color: lightgrey !important; text-align: center;
                                              padding:12px; margin:48px; border: 1px solid black; border-radius: 12px;"))
                               ),
                               
                               hidden(
                               argonRow(id = "majorImage_ClustRow",
                                        
                               # Show Plot title
                               argonColumn(width = 12,
                                           argonRow(
                                             tags$b("Show title?")
                                           ),
                                           argonRow(
                                             prettyToggle(
                                               inputId = "majorSecsDistsClusterTitleOption",
                                               label_on = "Yes!", 
                                               icon_on = icon("check"),
                                               status_on = "success",
                                               status_off = "warning", 
                                               label_off = "No",
                                               icon_off = icon("times"),
                                               value = FALSE
                                             ),
                                             hidden(
                                             textInput(inputId = "majorSecsDistsClusterTitle", 
                                                       label = NULL, 
                                                       value = NULL, 
                                                       width = "100%", 
                                                       placeholder = "Type in your desired plot title")
                                             )
                                           )
                               ),
                               
                               # Select color palette
                               pickerInput(
                                 inputId = "majorSecsDistsClusterPalette",
                                 width = "100%",
                                 label = "Color Palette", 
                                 choices = fishualize::fish_palettes(),
                                 selected = "Centropyge_loricula",
                                 options = list(
                                   size = 5,
                                   `live-search` = TRUE)
                               ),
                               
                               argonR::argonColumn(center = TRUE, width = 12,
                                                   plotOutput(outputId = "majorSecsDistsClusterPlot") %>% shinycssloaders::withSpinner(type = 4),
                                                   
                                                   downloadScreenShotBtn(inputId = "download_majorSecsDistsCluster",
                                                                         ui = '#majorSecsDistsClusterPlot',
                                                                         label = 'Download Plot', 
                                                                         filename = paste0("Hierarchical Clustering of Sections (", Sys.Date(), ")", ".png"))
                               )
                               )
                               ) 
                      ),
                      
                      ## ANOVA
                      tabPanel(title = p("ANOVA", style = "color:white"),
                               
                               hidden(
                                 argonRow(center = TRUE, id = "majorImage_ANOVANotifRow",
                                          tags$h6(br(),
                                                  tags$b("Only a single section has been defined and the ANOVA cannot be done!"),
                                                  br(),
                                                  br(),
                                                  style = "background-color: lightgrey !important; text-align: center;
                                              padding:12px; margin:48px; border: 1px solid black; border-radius: 12px;"))
                               ),
                               
                               hidden(
                                 argonRow(id = "majorImage_ANOVARow",
                                          
                                          # Define the variable for ANOVA
                                          radioGroupButtons(
                                            inputId = "majorImgDistsANOVA_variable",
                                            label = "Define the variable for performing ANOVA:",
                                            choices = c("Section" = "Group_Img_SectionName", 
                                                        "Image" = "ImgName", 
                                                        "Group" = "GroupName"),
                                            individual = TRUE,
                                            checkIcon = list(
                                              yes = tags$i(class = "fa fa-circle", 
                                                           style = "color: steelblue"),
                                              no = tags$i(class = "fa fa-circle-o", 
                                                          style = "color: steelblue"))
                                          ),
                                          
                                          br(),
                                         
                                          # Generate ANOVA table 
                               DT::dataTableOutput("majorSecsDistsANOVATable") %>% shinycssloaders::withSpinner(type = 4)
                                 )
                               )
                               
                      )
                    )
                  )
                )
            ),
            
            #########################################################
            #########################################################
            
            # Define the body of automatic pre_cropped_based analysis
            tabItem(
                tabName = "pre_cropped_based",
                
                # Column 0
                #######################
                # Workflow
                box(
                  title = "Workflow",
                  id = NULL,
                  status = NULL,
                  solidHeader = FALSE,
                  background = "secondary",
                  gradient = TRUE,
                  width = 12,
                  collapsible = TRUE,
                  collapsed = TRUE,
                  closable = TRUE,
                  maximizable = FALSE,
                  icon = icon("road"),
                  boxToolSize = "xs",
                  elevation = 4,
                  headerBorder = TRUE,
                  label = boxLabel("Step 0", status = "info", 
                                   tooltip = "Workflow: Use the following pipeline to to perform the analyses."),
                  dropdownMenu = NULL,
                  sidebar = NULL,
                  footer = NULL,
                  tags$b("Follow the workflow explained below to perform the pre-cropped-based AutoClonization.",
                         style = "text-align: justify; font-size: 16px;"),
                  br(),br(),
                  argonRow(center = TRUE,
                           argonColumn(width = 6,
                                       argonColumn(width = 11, offset = 0.5,
                                                   p(tags$b("I. ", style = "font-size: 18px;"),
                                                     "Select your desired section image by clicking on its corresponding button in ",
                                                     tags$b("Step 1"), ".",
                                                     style = "font-size: 14px; border-bottom:1px dotted grey;"),
                                                   
                                                   p(tags$b("II. ", style = "font-size: 18px;"),
                                                     "Click on the button ", tags$b("Approve Section"), " in ",
                                                     tags$b("Step 2"), ".",
                                                     style = "font-size: 14px; border-bottom:1px dotted grey;"),
                                                   
                                                   p(tags$b("III. ", style = "font-size: 18px;"),
                                                     "Download the color scheme plots of your approved section in ",
                                                     tags$b("Step 3"), ".",
                                                     style = "font-size: 14px; border-bottom:1px dotted grey;"),
                                                   
                                                   p(tags$b("IV. ", style = "font-size: 18px;"),
                                                     "Select your desired color space mode and perform autoclonization by clicking on the button ", tags$b("Autoclonize"), " in ",
                                                     tags$b("Step 4"), ".",
                                                     style = "font-size: 14px; border-bottom:1px dotted grey;")
                                       )),
                           argonColumn(width = 6,
                                       argonColumn(width = 11, offset = 0.5,
                                                   
                                                   p(tags$b("V. ", style = "font-size: 18px;"),
                                                     "Download the Mean Distance plot in ",
                                                     tags$b("Step 4"), ".",
                                                     style = "font-size: 14px; border-bottom:1px dotted grey;"),
                                                   
                                                   p(tags$b("VI. ", style = "font-size: 18px;"),
                                                     "Download the table of Autoclonization results in ",
                                                     tags$b("Step 5"), ".",
                                                     style = "font-size: 14px; border-bottom:1px dotted grey;"),
                                                   
                                                   p(tags$b("VII. ", style = "font-size: 18px;"),
                                                     "Download the section hierarchical clustering plot in ",
                                                     tags$b("Step 6"), ".",
                                                     style = "font-size: 14px; border-bottom:1px dotted grey;")
                                       ))
                  )
                ),
                
                fluidRow(
                  
                  # Column 1 and 4
                  column(width = 4,
                         
                         #######################
                         # Input Minor Image
                         box(
                           title = "Input Section Image(s)",
                           id = "input_minor_image",
                           status = "navy",
                           solidHeader = TRUE,
                           background = NULL,
                           gradient = FALSE,
                           width = 12,
                           collapsible = TRUE,
                           collapsed = FALSE,
                           closable = FALSE,
                           maximizable = FALSE,
                           icon = icon("file-image"),
                           boxToolSize = "xs",
                           elevation = 2,
                           headerBorder = TRUE,
                           label = boxLabel("Step 1", status = "info", 
                                            tooltip = "Step 1: Upload your section image(s)"),
                           dropdownMenu = NULL,
                           sidebar = NULL,
                           footer = NULL,
                           argonRow(center = TRUE,
                                    p(tags$b("NOTE:"), "If your section images belong to several conditions (", tags$i("e.g."),
                                      "test (mutant) versus control (wild type) samples), define the required 
                                   groups by clicking on the following button.",
                                      style = "text-align: justify; font-size: 16px;"),
                                    
                                    column(12,
                                           style='margin-bottom:15px; padding: 5px;',
                                           
                                           # Define group button
                                           actionBttn(
                                             inputId = "minor_add_group",
                                             icon = icon("folder-plus"),
                                             size = "xs", block = TRUE, no_outline = TRUE,
                                             label = " Add a New Group",
                                             style = "fill", 
                                             color = "primary"
                                           ))
                           ),
                           
                           argonColumn(width = 12, center = FALSE,
                                       style='border-top:1px solid; padding: 10px',
                                       fileInput("minor_image", label = p(icon("upload"), " Upload your image section(s) (TIFF/PNG/JPG)", 
                                                                          style = "padding:0px; margin:0; text-align: justify; font-size: 14px"),
                                                 accept = c(".png", ".tif", ".tiff", ".jpg", ".jpeg"),
                                                 width = "100%",multiple = TRUE,
                                                 buttonLabel = "Browse")
                           ),
                           argonR::argonRow(center = TRUE, 
                                            
                                            hidden(
                                              argonColumn(width = 6, center = TRUE, id = "minorImage_header",
                                                          tags$b("Image")                                 )
                                            ),
                                            
                                            hidden(
                                              argonColumn(width = 6, center = TRUE, id = "minorGroup_header",
                                                          tags$b("Group")
                                              )
                                            )
                           ),
                           
                           argonR::argonRow(center = TRUE,
                                            argonColumn(width = 6, center = TRUE,
                                                        uiOutput(outputId = "minor_image_buttons")
                                            ),
                                            hidden(
                                              argonColumn(width = 6, center = TRUE, id = 'minor_groupPicker_column',
                                                          style='border-left:1px dotted grey;',
                                                          uiOutput(outputId = "minor_image_groups")
                                              )
                                            )
                           ),
                           hidden(
                             argonR::argonRow(center = TRUE, id = "minor_clearGrpsRow",
                                              style='border-top:1px solid grey; padding-top:10px',
                                              actionBttn(
                                                inputId = "minor_clearGrpsBtn",
                                                label = "Clean Groups",
                                                icon = icon("trash-alt"),
                                                style = "fill",
                                                color = "danger",
                                                size = "xs",
                                                block = TRUE,
                                                no_outline = TRUE
                                              )
                             )
                           )
                         ),
                         
                         #########################################
                         
                         # Autoclonize
                         box(
                           title = "Autoclonize",
                           id = "minor_autoclonize_box",
                           status = "navy",
                           solidHeader = TRUE,
                           background = NULL,
                           gradient = FALSE,
                           width = 12,
                           collapsible = TRUE,
                           collapsed = FALSE,
                           closable = FALSE,
                           maximizable = FALSE,
                           icon = icon("robot"),
                           boxToolSize = "xs",
                           elevation = 2,
                           headerBorder = TRUE,
                           label = boxLabel("Step 4", status = "info", 
                                            tooltip = "Step 4: Calculate the clonality of selected sections."),
                           dropdownMenu = NULL,
                           sidebar = NULL,
                           footer = NULL,
                           bs4Dash::tooltip(title = "Refer to the Introduction tab for more info", placement = "right",
                           prettyRadioButtons(
                             inputId = "minorImg_colorSpaceMode",
                             label = "Select a color space mode:", 
                             choiceNames = c("3D Color Space (Recommended)", "2D Color Space"),
                             choiceValues =  c(3,2),
                             icon = icon("check"), 
                             bigger = TRUE,
                             status = "primary",
                             animation = "jelly"
                           )
                           ),
                           fluidRow(center = TRUE,
                                    p(icon("hand-pointer"), "Click on the following button to calculate the clonality of all of the sections you defined in the previous step (", tags$b("Step 2"), ").",
                                      style = "text-align: justify; font-size: 16px;"),
                                    
                                    # Autoclonize Button
                                    actionBttn(
                                      inputId = "minor_autoclonizeBtn",
                                      block = TRUE, size = "xs",
                                      label = "Autoclonize", 
                                      style = "fill",
                                      icon = icon("calculator"),
                                      color = "royal"
                                    )
                           ),
                           
                           hidden(
                             argonR::argonRow(center = TRUE,
                                              id = "minor_autoclonizeResRow",
                                              style='margin-top:20px;',
                                              
                                              # Clonality tabs
                                              bs4Dash::box(width = 12, solidHeader = TRUE, 
                                                           maximizable = TRUE,
                                                           background = "white",
                                                           status = "danger",
                                                           icon = icon("eye-dropper"),
                                                           
                                                           # Show Plot title
                                                           tags$b("Show title?"),
                                                           prettyToggle(
                                                             inputId = "minor_secDistsPlotTitleOption",
                                                             label_on = "Yes!", 
                                                             icon_on = icon("check"),
                                                             status_on = "success",
                                                             status_off = "warning", 
                                                             label_off = "No",
                                                             icon_off = icon("times"),
                                                             value = FALSE
                                                           ),
                                                           hidden(
                                                             textInput(inputId = "minor_secDistsPlotTitle", 
                                                                       label = NULL, 
                                                                       value = "Mean Distance Plot", 
                                                                       width = "100%", 
                                                                       placeholder = "Type in your desired plot title")
                                                           ),
                                                           
                                                           argonRow(
                                                             
                                                             # Show group names
                                                             argonColumn(width = 6, center = FALSE,
                                                                         tags$b("Add group names?"),
                                                                         prettyToggle(
                                                                           inputId = "minor_secDistsPlotGroupNames",
                                                                           label_on = "Yes!", 
                                                                           icon_on = icon("check"),
                                                                           status_on = "success",
                                                                           status_off = "warning", 
                                                                           label_off = "No",
                                                                           icon_off = icon("times"),
                                                                           value = TRUE
                                                                         )
                                                             ),
                                                             
                                                             argonColumn(width = 6, center = FALSE,
                                                                         tags$b("Add error bars?"),
                                                                         prettyToggle(
                                                                           inputId = "minor_secDistsPlotErrorBars",
                                                                           label_on = "Yes!", 
                                                                           icon_on = icon("check"),
                                                                           status_on = "success",
                                                                           status_off = "warning", 
                                                                           label_off = "No",
                                                                           icon_off = icon("times"),
                                                                           value = TRUE
                                                                         )
                                                             )
                                                           ),
                                                           
                                                           # Select color palette
                                                           pickerInput(
                                                             inputId = "minor_secDistsPlotPalette",
                                                             width = "100%",
                                                             label = "Color Palette", 
                                                             choices = fishualize::fish_palettes(),
                                                             selected = "Centropyge_loricula",
                                                             options = list(
                                                               size = 5,
                                                               `live-search` = TRUE)
                                                           ),
                                                           
                                                           plotOutput(outputId = "minorSecClonalityPlot") %>% shinycssloaders::withSpinner(type = 4),
                                                           
                                                           br(),
                                                           
                                                           argonR::argonRow(
                                                             column(6,
                                                                    numericInput(inputId = "minorSecClonality.figure.width", label = "Figure width (in)", value = 8,
                                                                                 min = 1, max = Inf, step = 1, width = "100%")
                                                             ),
                                                             column(6,
                                                                    numericInput(inputId = "minorSecClonality.figure.height", label = "Figure height (in)", value = 8,
                                                                                 min = 1, max = Inf, step = 1, width = "100%")
                                                             )
                                                           ),
                                                           
                                                           argonR::argonRow(center = TRUE,
                                                                            column(12,
                                                                                   numericInput(inputId = "minorSecClonality.PNG.resolution", label = "PNG file resolution (dpi)", value = 300,
                                                                                                min = 72, max = Inf, step = 1, width = "100%"))
                                                           ),
                                                           
                                                           argonR::argonRow(
                                                             column(6,
                                                                    downloadButton("download_minorSecClonality_PDF", "Download PDF file", icon = icon("download"),
                                                                                   class = "btn-sm btn-block")),
                                                             column(6,
                                                                    downloadButton("download_minorSecClonality_PNG", "Download PNG file", icon = icon("download"),
                                                                                   class = "btn-sm btn-block")
                                                             )
                                                           )
                                              )
                             )
                           )
                         )
                         
                  ),
                  
                  #######################
                  
                  # Column 2 and 5
                  column(width = 4,  
                         
                         # Approve Sections
                         box(
                           title = "Approve Sections",
                           id = "minor_image_visualization",
                           status = "navy",
                           solidHeader = TRUE,
                           background = NULL,
                           gradient = FALSE,
                           width = 12,
                           collapsible = TRUE,
                           collapsed = FALSE,
                           closable = FALSE,
                           maximizable = FALSE,
                           icon = icon("image"),
                           boxToolSize = "xs",
                           elevation = 2,
                           headerBorder = TRUE,
                           label = boxLabel("Step 2", status = "info", 
                                            tooltip = "Step 2: Approve your desired sections(s)"),
                           dropdownMenu = NULL,
                           sidebar = NULL,
                           footer = NULL,
                           argonRow(center = TRUE, style = "padding-bottom:1px; margin-bottom:-5px;",
                                    p(icon("hand-pointer"), "Click on your desired section image button generated in", tags$b("Step 1"), "to illustrate it below.",
                                      style = "text-align: justify; font-size: 16px; padding-bottom:1px; margin-bottom:-5px;")
                           ),
                           
                           argonR::argonRow(center = TRUE, id = "defineMinorSectionRow",
                                            style='padding-bottom:10px; padding-top:1px; margin-top:1px;',
                                            
                                            argonColumn(width = 12, center = TRUE, 
                                                        # load snapper into the app for taking screen shot of the section
                                                        load_snapper(),
                                                        
                                                        # add approve button
                                                        actionBttn(
                                                          inputId = "approveMinorSection",
                                                          icon = icon("check-circle"),
                                                          size = "xs", block = TRUE, no_outline = TRUE,
                                                          label = " Approve Section",
                                                          style = "fill", 
                                                          color = "success"
                                                        )
                                                        
                                            )
                           ),
                           
                           hidden(
                             argonR::argonRow(center = TRUE, id = "minorPlotRow",
                                              
                                              # Use the maxPlotHeight JS script
                                              shinyjs::extendShinyjs(text = maxPlotHeight, functions = c("init", "calcHeight")),
                                              
                                              # Use the maxPlotWidth JS script
                                              shinyjs::extendShinyjs(text = maxPlotWidth, functions = c("init", "calcWidth")),
                                              
                                              argonR::argonColumn(center = TRUE, width = 12, id = "minorPlotColumn",
                                                                  
                                                                  plotOutput("uploaded_minor_image"
                                                                  ) %>% shinycssloaders::withSpinner(type = 4)
                                              )
                             )
                           )
                         ),
                         
                         #####################
                         # Distance statistics
                         box(
                           title = "Clonality Table",
                           id = "minorImage_ClonalityTableBox",
                           status = "navy",
                           solidHeader = TRUE,
                           background = NULL,
                           gradient = FALSE,
                           width = 12,
                           collapsible = TRUE,
                           collapsed = TRUE,
                           closable = FALSE,
                           maximizable = TRUE,
                           icon = icon("chart-bar"),
                           boxToolSize = "xs",
                           elevation = 2,
                           headerBorder = TRUE,
                           label = boxLabel("Step 5", status = "info", 
                                            tooltip = "Step 5: Download the clonality table"),
                           dropdownMenu = NULL,
                           sidebar = NULL,
                           footer = NULL,
                           
                           hidden(
                             argonRow(id = "minorImageDist_ClonalityTableRow", 
                                      argonColumn(width = 12, 
                                                  
                                                  DT::dataTableOutput("minorSecClonalityTable") %>% shinycssloaders::withSpinner(type = 4)
                                                  
                                      )
                             )
                           )
                         )
                  ),
                  
                  #######################
                  
                  # Column 3 and 6
                  column(width = 4,  
                         
                         # Define sections
                         box(
                           title = "Visualize Section Palette",
                           id = "minor_define_colonies",
                           status = "navy",
                           solidHeader = TRUE,
                           background = NULL,
                           gradient = FALSE,
                           width = 12,
                           collapsible = TRUE,
                           collapsed = FALSE,
                           closable = FALSE,
                           maximizable = FALSE,
                           icon = icon("object-group"),
                           boxToolSize = "xs",
                           elevation = 2,
                           headerBorder = TRUE,
                           label = boxLabel("Step 3", status = "info", 
                                            tooltip = "Step 3: Visualize the color palette of the approved section."),
                           dropdownMenu = NULL,
                           sidebar = NULL,
                           
                           # Explain workflow in the footer
                           footer = NULL, 
                           hidden(
                             argonR::argonRow(center = TRUE, id = "minorSecPlotsRow",
                                              
                                              bs4TabCard(width = 12, solidHeader = TRUE, 
                                                         type = "tabs",
                                                         background = "white",
                                                         status = "danger",
                                                         icon = icon("eye-dropper"),
                                                         
                                                         tabPanel(title = p("Circle Palette", style = "color:#302E34"),
                                                                  tags$b("Show color legend"),
                                                                  switchInput(
                                                                    inputId = "minorCirclePalLegend",
                                                                    label = "<i class=\"fa fa-thumbs-up\"></i>",
                                                                    value = FALSE,
                                                                    size = "mini"
                                                                  ),
                                                                  
                                                                  sliderInput(
                                                                    inputId = "minorCirclePalN",
                                                                    label = "Max number of distinct colors",
                                                                    min = 1,
                                                                    max = 100,
                                                                    value = 5,
                                                                    step = 1,
                                                                    round = TRUE,
                                                                    ticks = FALSE,
                                                                    width = "100%",
                                                                    sep = ","
                                                                  ),
                                                                  
                                                                  plotOutput(outputId = "minorSecCirclePlot") %>% shinycssloaders::withSpinner(type = 4),
                                                                  
                                                                  argonR::argonRow(center = TRUE,
                                                                                   column(6,
                                                                                          numericInput(inputId = "minorSecCircle.figure.width", label = "Figure width (in)", value = 8,
                                                                                                       min = 1, max = Inf, step = 1, width = "100%")
                                                                                   ),
                                                                                   column(6,
                                                                                          numericInput(inputId = "minorSecCircle.figure.height", label = "Figure height (in)", value = 8,
                                                                                                       min = 1, max = Inf, step = 1, width = "100%")
                                                                                   )
                                                                  ),
                                                                  
                                                                  argonR::argonRow(center = TRUE,
                                                                                   column(12,
                                                                                          numericInput(inputId = "minorSecCircle.PNG.resolution", label = "PNG file resolution (dpi)", value = 300,
                                                                                                       min = 72, max = Inf, step = 1, width = "100%"))
                                                                  ),
                                                                  
                                                                  argonR::argonRow(center = TRUE,
                                                                                   column(6,
                                                                                          downloadButton("download_minorSecCircle_PDF", "Download PDF file", icon = icon("download"),
                                                                                                         class = "btn-sm btn-block")),
                                                                                   column(6,
                                                                                          downloadButton("download_minorSecCircle_PNG", "Download PNG file", icon = icon("download"),
                                                                                                         class = "btn-sm btn-block")
                                                                                   )
                                                                  )
                                                         ),
                                                         
                                                         tabPanel(title = p("Pixel Plot", style = "color:#302E34"),
                                                                  sliderInput(
                                                                    inputId = "minorPixelPlotN",
                                                                    label = "Number of pixels",
                                                                    min = 500,
                                                                    max = 10000,
                                                                    value = 3000,
                                                                    step = 100,
                                                                    round = TRUE,
                                                                    ticks = FALSE,
                                                                    width = "100%",
                                                                    sep = ","
                                                                  ),
                                                                  
                                                                  argonR::argonColumn(center = TRUE, width = 12,
                                                                                      plotOutput(outputId = "minorSecPixelPlot") %>% shinycssloaders::withSpinner(type = 4),
                                                                                      
                                                                                      
                                                                                      
                                                                                      shiny::uiOutput(
                                                                                        outputId = "download_minorSecPixel")
                                                                  )
                                                                  
                                                         ),
                                                         
                                                         tabPanel(title = p("Histogram", style = "color:#302E34"),
                                                                  
                                                                  argonR::argonColumn(center = TRUE, width = 12,
                                                                                      
                                                                                      plotOutput(outputId = "minorSecDistHist") %>% shinycssloaders::withSpinner(type = 4),
                                                                                      
                                                                                      shiny::uiOutput(
                                                                                        outputId = "download_minorSecDistHist")
                                                                  )
                                                                  
                                                         )
                                              )
                             )
                           )
                         ),
                         
                         #####################################
                         
                         # Clustering
                         bs4Dash::box(
                           title = "Color-based Clustering",
                           id = "minorImage_ClustANOVA_box",
                           status = "navy",
                           solidHeader = TRUE,
                           background = NULL,
                           gradient = FALSE,
                           width = 12,
                           collapsible = TRUE,
                           collapsed = FALSE,
                           closable = FALSE,
                           maximizable = TRUE,
                           icon = NULL,
                           boxToolSize = "xs",
                           elevation = 2,
                           headerBorder = TRUE,
                           label = boxLabel("Step 6", status = "info", 
                                            tooltip = "Step 6: Clustering of Sections"),
                           dropdownMenu = NULL,
                           sidebar = NULL,
                           footer = NULL,
                           
                           hidden(
                             argonRow(center = TRUE, id = "minorImage_ClustNotifRow",
                                      tags$h6(br(),
                                              tags$b("The number of defined sections are less than 3 and the clustering cannot be done!"),
                                              br(),
                                              br(),
                                              style = "background-color: lightgrey !important; text-align: center;
                                              padding:12px; margin:48px; border: 1px solid black; border-radius: 12px;"))
                           ),
                           
                           hidden(
                             argonRow(id = "minorImage_ClustRow",
                                      
                                      # Show Plot title
                                      argonColumn(width = 12,
                                                  argonRow(
                                                    tags$b("Show title?")
                                                  ),
                                                  argonRow(
                                                    prettyToggle(
                                                      inputId = "minorSecsDistsClusterTitleOption",
                                                      label_on = "Yes!", 
                                                      icon_on = icon("check"),
                                                      status_on = "success",
                                                      status_off = "warning", 
                                                      label_off = "No",
                                                      icon_off = icon("times"),
                                                      value = FALSE
                                                    ),
                                                    hidden(
                                                      textInput(inputId = "minorSecsDistsClusterTitle", 
                                                                label = NULL, 
                                                                value = NULL, 
                                                                width = "100%", 
                                                                placeholder = "Type in your desired plot title")
                                                    )
                                                  )
                                      ),
                                      
                                      # Select color palette
                                      pickerInput(
                                        inputId = "minorSecsDistsClusterPalette",
                                        width = "100%",
                                        label = "Color Palette", 
                                        choices = fishualize::fish_palettes(),
                                        selected = "Centropyge_loricula",
                                        options = list(
                                          size = 5,
                                          `live-search` = TRUE)
                                      ),
                                      
                                      argonR::argonColumn(center = TRUE, width = 12,
                                                          plotOutput(outputId = "minorSecsDistsClusterPlot") %>% shinycssloaders::withSpinner(type = 4),
                                                          
                                                          downloadScreenShotBtn(inputId = "download_minorSecsDistsCluster",
                                                                                ui = '#minorSecsDistsClusterPlot',
                                                                                label = 'Download Plot', 
                                                                                filename = paste0("Hierarchical Clustering of Sections (", Sys.Date(), ")", ".png"))
                                      )
                             )
                           )
                         )
                  )
                )
            ),
            
            #########################################################
            #########################################################
            
            # Define the body of automatic color_coord_based analysis
            tabItem(
              tabName = "color_coord_based",
              
              # Column 0
              #######################
              # Workflow
              box(
                title = "Workflow",
                id = NULL,
                status = NULL,
                solidHeader = FALSE,
                background = "secondary",
                gradient = TRUE,
                width = 12,
                collapsible = TRUE,
                collapsed = TRUE,
                closable = TRUE,
                maximizable = FALSE,
                icon = icon("road"),
                boxToolSize = "xs",
                elevation = 4,
                headerBorder = TRUE,
                label = boxLabel("Step 0", status = "info", 
                                 tooltip = "Workflow: Use the following pipeline to to perform the analyses."),
                dropdownMenu = NULL,
                sidebar = NULL,
                footer = NULL,
                tags$b("Follow the workflow explained below to perform the Brush-based AutoClonization.",
                       style = "text-align: justify; font-size: 16px;"),
                br(),br(),
                argonRow(center = TRUE,
                         argonColumn(width = 6,
                                     argonColumn(width = 11, offset = 0.5,
                                                 p(tags$b("I. ", style = "font-size: 18px;"),
                                                   "Determine the type your input data, perform data reformatting (if required), and define the columns corresponding to the color/coord data in ",
                                                   tags$b("Step 1"), ".",
                                                   style = "font-size: 14px; border-bottom:1px dotted grey;"),
                                                 
                                                 p(tags$b("II. ", style = "font-size: 18px;"),
                                                   "Define the Section column and optionally Group and Image columns, and, finally, click on the button ", tags$b("Preprocess the Data"), " in ",
                                                   tags$b("Step 2"), ".",
                                                   style = "font-size: 14px; border-bottom:1px dotted grey;"),

                                                 p(tags$b("III. ", style = "font-size: 18px;"),
                                                   "Selecet your desired sections and download their color scheme plots in .",
                                                   tags$b("Step 3"), ".",
                                                   style = "font-size: 14px; border-bottom:1px dotted grey;")
                                                 
                                     )),
                         argonColumn(width = 6,
                                     argonColumn(width = 11, offset = 0.5,
                                                 
                                                 
                                                 p(tags$b("IV. ", style = "font-size: 18px;"),
                                                   "Select your desired color space mode and perform autoclonization by clicking on the button ", tags$b("Autoclonize"), " in ",
                                                   tags$b("Step 4"), ".",
                                                   style = "font-size: 14px; border-bottom:1px dotted grey;"),
                                                 
                                                 p(tags$b("V. ", style = "font-size: 18px;"),
                                                   "Download the Mean Distance plot and the table of Autoclonization results in ",
                                                   tags$b("Step 4"), ".",
                                                   style = "font-size: 14px; border-bottom:1px dotted grey;"),
                                                 
                                                 p(tags$b("VI. ", style = "font-size: 18px;"),
                                                   "Download the RanCload statistics plot of sections distances in ",
                                                   tags$b("Step 5"), ".",
                                                   style = "font-size: 14px; border-bottom:1px dotted grey;"),
                                                
                                                 p(tags$b("VII. ", style = "font-size: 18px;"),
                                                   "Download the table of ANOVA results in .",
                                                   tags$b("Step 6"), ".",
                                                   style = "font-size: 14px; border-bottom:1px dotted grey;")
                                     ))
                )
              ),
              
              fluidRow(
                
                # Column 1 and 4
                column(width = 4,
                       
                       #######################
                       # Input Color/Coord file
                       box(
                         title = "Input Color/Coord File",
                         id = "input_color_coord_box",
                         status = "navy",
                         solidHeader = TRUE,
                         background = NULL,
                         gradient = FALSE,
                         width = 12,
                         collapsible = TRUE,
                         collapsed = FALSE,
                         closable = FALSE,
                         maximizable = FALSE,
                         icon = icon("file-import"),
                         boxToolSize = "xs",
                         elevation = 2,
                         headerBorder = TRUE,
                         label = boxLabel("Step 1", status = "info", 
                                          tooltip = "Step 1: Upload color/coord file"),
                         dropdownMenu = NULL,
                         sidebar = NULL,
                         footer = NULL,
                         argonRow(center = FALSE,
                         argonColumn(width = 12, center = FALSE,
                                     fileInput("color_coord_file", label = p(icon("upload"), " Upload your color/coord file (CSV/TSV/TXT)", 
                                                                        style = "padding:0px; margin:0; text-align: justify; font-size: 14px"),
                                               accept = c(".csv", ".tsv", ".txt"),
                                               width = "100%",multiple = FALSE,
                                               buttonLabel = "Browse")
                         )
                         ),
                         # Data type
                         argonRow(center = TRUE, 
                                  argonColumn(width = 12, center = TRUE, 
                           tags$head(tags$style('.btn-group{ margin-bottom: 6px; margin-right: 2px; margin-left: 2px}')),  # add the spacing
                           radioGroupButtons(
                             inputId = "color_coord_type", 
                             label = "Determine the type of your input data:",
                             choices = c("HEX color codes" = "hexCode", 
                                         "RGB values" = "RGBval",
                                         "RGB values (from Fiji)" = "RGBval_Fiji",
                                         "HSL values" = "HSLval",
                                         "HSV/HSB values" = "HSVval",
                                         "X and Y coordinates" = "XYcoord"),
                             selected = FALSE,
                             size = "sm",
                             individual = TRUE,
                             justified = TRUE,
                             disabled = TRUE,
                             checkIcon = list(
                               yes = tags$i(class = "fa fa-circle", 
                                            style = "color: steelblue"),
                               no = tags$i(class = "fa fa-circle-o", 
                                           style = "color: steelblue"))
                           )
                                  )
                         ),
                         
                         # Columns of the data
                              argonRow(center = TRUE, 
                                  conditionalPanel(
                                    condition = "input.color_coord_type == 'hexCode'",
                                    uiOutput('hexCode_column')
                                  ),
                                  
                                  #########
                                  column(width = 4,
                                  conditionalPanel(
                                    condition = "input.color_coord_type == 'RGBval'",
                                    uiOutput('RGBval_Rcolumn')
                                  )
                                  ),
                                  column(width = 4,
                                         conditionalPanel(
                                           condition = "input.color_coord_type == 'RGBval'",
                                           uiOutput('RGBval_Gcolumn')
                                         )
                                  ),
                                  column(width = 4,
                                         conditionalPanel(
                                           condition = "input.color_coord_type == 'RGBval'",
                                           uiOutput('RGBval_Bcolumn')
                                         )
                                  ),
                                  
                                  #########
                                  column(width = 6,
                                         conditionalPanel(
                                           condition = "input.color_coord_type == 'RGBval_Fiji'",
                                           uiOutput('RGBval_Unifiedcolumn')
                                         )
                                  ),
                                  column(width = 6,
                                         conditionalPanel(
                                           condition = "input.color_coord_type == 'RGBval_Fiji'",
                                           uiOutput('RGBval_UnifiedValuecolumn')
                                         )
                                  ),
                                  column(width = 12,
                                           hidden(
                                             actionBttn(
                                               inputId = "RGBval_Unified_reformatBtn",
                                               label = "Reformat the Data", 
                                               style = "fill",
                                               size = "sm",
                                               block = TRUE,
                                               no_outline = TRUE,
                                               color = "primary"
                                             )
                                           ),
                                         conditionalPanel(
                                           condition = "input.color_coord_type == 'RGBval_Fiji'",
                                           br()
                                         )
                                  ),
                                  column(width = 4,
                                         conditionalPanel(
                                           condition = "input.color_coord_type == 'RGBval_Fiji'",
                                           uiOutput('RGBvalunif_Rcolumn')
                                         )
                                  ),
                                  column(width = 4,
                                         conditionalPanel(
                                           condition = "input.color_coord_type == 'RGBval_Fiji'",
                                           uiOutput('RGBvalunif_Gcolumn')
                                         )
                                  ),
                                  column(width = 4,
                                         conditionalPanel(
                                           condition = "input.color_coord_type == 'RGBval_Fiji'",
                                           uiOutput('RGBvalunif_Bcolumn')
                                         )
                                  ),
                                  
                                  #########
                                  column(width = 4,
                                         conditionalPanel(
                                           condition = "input.color_coord_type == 'HSLval'",
                                           uiOutput('HSLval_Hcolumn')
                                         )
                                  ),
                                  column(width = 4,
                                         conditionalPanel(
                                           condition = "input.color_coord_type == 'HSLval'",
                                           uiOutput('HSLval_Scolumn')
                                         )
                                  ),
                                  column(width = 4,
                                         conditionalPanel(
                                           condition = "input.color_coord_type == 'HSLval'",
                                           uiOutput('HSLval_Lcolumn')
                                         )
                                  ),
                                  
                                  #########
                                  column(width = 4,
                                         conditionalPanel(
                                           condition = "input.color_coord_type == 'HSVval'",
                                           uiOutput('HSVval_Hcolumn')
                                         )
                                  ),
                                  column(width = 4,
                                         conditionalPanel(
                                           condition = "input.color_coord_type == 'HSVval'",
                                           uiOutput('HSVval_Scolumn')
                                         )
                                  ),
                                  column(width = 4,
                                         conditionalPanel(
                                           condition = "input.color_coord_type == 'HSVval'",
                                           uiOutput('HSVval_Vcolumn')
                                         )
                                  ),
                                  
                                  #########
                                  column(width = 6,
                                         conditionalPanel(
                                           condition = "input.color_coord_type == 'XYcoord'",
                                           uiOutput('XYcoord_Xcolumn')
                                         )
                                  ),
                                  column(width = 6,
                                         conditionalPanel(
                                           condition = "input.color_coord_type == 'XYcoord'",
                                           uiOutput('XYcoord_Ycolumn')
                                         )
                                  )
                                  )
                       ),
                       
                       #########################################
                       
                       # Autoclonize
                       box(
                         title = "Autoclonize",
                         id = "color_coord_autoclonize_box",
                         status = "navy",
                         solidHeader = TRUE,
                         background = NULL,
                         gradient = FALSE,
                         width = 12,
                         collapsible = TRUE,
                         collapsed = FALSE,
                         closable = FALSE,
                         maximizable = FALSE,
                         icon = icon("robot"),
                         boxToolSize = "xs",
                         elevation = 2,
                         headerBorder = TRUE,
                         label = boxLabel("Step 4", status = "info", 
                                          tooltip = "Step 4: Calculate the clonality of sections."),
                         dropdownMenu = NULL,
                         sidebar = NULL,
                         footer = NULL,
                         bs4Dash::tooltip(title = "Refer to the Introduction tab for more info", placement = "right",
                         prettyRadioButtons(
                           inputId = "color_coord_colorSpaceMode",
                           label = "Select a color space mode:", 
                           choiceNames = c("3D Color Space (Recommended)", "2D Color Space"),
                           choiceValues =  c(3,2),
                           icon = icon("check"), 
                           bigger = TRUE,
                           status = "primary",
                           animation = "jelly"
                         )
                         ),
                         fluidRow(center = TRUE,
                                  p(icon("hand-pointer"), "Click on the following button to calculate the clonality of the sections",
                                    style = "text-align: justify; font-size: 16px;"),
                                  
                                  # Autoclonize Button
                                  actionBttn(
                                    inputId = "color_coord_autoclonizeBtn",
                                    block = TRUE, size = "xs",
                                    label = "Autoclonize", 
                                    style = "fill",
                                    icon = icon("calculator"),
                                    color = "royal"
                                  )
                         ),
                         
                           argonR::argonRow(center = TRUE,
                                            style='margin-top:20px;',
                                            
                                            # Clonality tabs
                                            hidden(
                                            argonR::argonRow(center = TRUE,
                                                             id = "color_coord_autoclonizeResRow",
                                            bs4TabCard(width = 12, solidHeader = TRUE, 
                                                       type = "tabs",
                                                       maximizable = TRUE,
                                                       background = "white",
                                                       status = "danger",
                                                       icon = icon("eye-dropper"),
                                                       
                                                       tabPanel(title = p("Clonality Plot", style = "color:#302E34"),
                                                                
                                                                # Show Plot title
                                                                tags$b("Show title?"),
                                                                prettyToggle(
                                                                  inputId = "color_coord_secDistsPlotTitleOption",
                                                                  label_on = "Yes!", 
                                                                  icon_on = icon("check"),
                                                                  status_on = "success",
                                                                  status_off = "warning", 
                                                                  label_off = "No",
                                                                  icon_off = icon("times"),
                                                                  value = FALSE
                                                                ),
                                                                hidden(
                                                                  textInput(inputId = "color_coord_secDistsPlotTitle", 
                                                                            label = NULL, 
                                                                            value = "Mean Distance Plot", 
                                                                            width = "100%", 
                                                                            placeholder = "Type in your desired plot title")
                                                                ),
                                                                
                                                                argonRow(
                                                                  
                                                                  # Show group names
                                                                  argonColumn(width = 6, center = FALSE,
                                                                              tags$b("Add group names?"),
                                                                              prettyToggle(
                                                                                inputId = "color_coord_secDistsPlotGroupNames",
                                                                                label_on = "Yes!", 
                                                                                icon_on = icon("check"),
                                                                                status_on = "success",
                                                                                status_off = "warning", 
                                                                                label_off = "No",
                                                                                icon_off = icon("times"),
                                                                                value = FALSE
                                                                              )
                                                                  ),
                                                                  
                                                                  # Show image names
                                                                  argonColumn(width = 6, center = FALSE,
                                                                              tags$b("Add image names?"),
                                                                              prettyToggle(
                                                                                inputId = "color_coord_secDistsPlotImageNames",
                                                                                label_on = "Yes!", 
                                                                                icon_on = icon("check"),
                                                                                status_on = "success",
                                                                                status_off = "warning", 
                                                                                label_off = "No",
                                                                                icon_off = icon("times"),
                                                                                value = FALSE
                                                                              )
                                                                  )
                                                                ),
                                                                
                                                                # Show error bars
                                                                tags$b("Add error bars?"),
                                                                prettyToggle(
                                                                  inputId = "color_coord_secDistsPlotErrorBars",
                                                                  label_on = "Yes!", 
                                                                  icon_on = icon("check"),
                                                                  status_on = "success",
                                                                  status_off = "warning", 
                                                                  label_off = "No",
                                                                  icon_off = icon("times"),
                                                                  value = TRUE
                                                                ),
                                                                
                                                                # Select color palette
                                                                pickerInput(
                                                                  inputId = "color_coord_secDistsPlotPalette",
                                                                  width = "100%",
                                                                  label = "Color Palette", 
                                                                  choices = fishualize::fish_palettes(),
                                                                  selected = "Centropyge_loricula",
                                                                  options = list(
                                                                    size = 5,
                                                                    `live-search` = TRUE)
                                                                ),
                                                                
                                                                plotOutput(outputId = "color_coord_majorSecClonalityPlot") %>% shinycssloaders::withSpinner(type = 4),
                                                                
                                                                br(),
                                                                
                                                                argonR::argonRow(
                                                                  column(6,
                                                                         numericInput(inputId = "color_coord_majorSecClonality.figure.width", label = "Figure width (in)", value = 8,
                                                                                      min = 1, max = Inf, step = 1, width = "100%")
                                                                  ),
                                                                  column(6,
                                                                         numericInput(inputId = "color_coord_majorSecClonality.figure.height", label = "Figure height (in)", value = 8,
                                                                                      min = 1, max = Inf, step = 1, width = "100%")
                                                                  )
                                                                ),
                                                                
                                                                argonR::argonRow(center = TRUE,
                                                                                 column(12,
                                                                                        numericInput(inputId = "color_coord_majorSecClonality.PNG.resolution", label = "PNG file resolution (dpi)", value = 300,
                                                                                                     min = 72, max = Inf, step = 1, width = "100%"))
                                                                ),
                                                                
                                                                argonR::argonRow(
                                                                  column(6,
                                                                         downloadButton("color_coord_download_majorSecClonality_PDF", "Download PDF file", icon = icon("download"),
                                                                                        class = "btn-sm btn-block")),
                                                                  column(6,
                                                                         downloadButton("color_coord_download_majorSecClonality_PNG", "Download PNG file", icon = icon("download"),
                                                                                        class = "btn-sm btn-block")
                                                                  )
                                                                )
                                                       ),
                                                       
                                                       tabPanel(title = p("Clonality Table", style = "color:#302E34"),
                                                                
                                                                DT::dataTableOutput("color_coord_majorSecClonalityTable") %>% shinycssloaders::withSpinner(type = 4)
                                                                
                                                       )
                                            )
                           )
                                            )
                           )
                       )
                ),
                
                # Column 2 and 5
                column(width = 4,
                       
                       #######################
                       # preprocessing
                       box(
                         title = "Preprocessing",
                         id = "color_coord_preprocessing_box",
                         status = "navy",
                         solidHeader = TRUE,
                         background = NULL,
                         gradient = FALSE,
                         width = 12,
                         collapsible = TRUE,
                         collapsed = FALSE,
                         closable = FALSE,
                         maximizable = FALSE,
                         icon = icon("table"),
                         boxToolSize = "xs",
                         elevation = 2,
                         headerBorder = TRUE,
                         label = boxLabel("Step 2", status = "info", 
                                          tooltip = "Step 2: Preprocessing the input data"),
                         dropdownMenu = NULL,
                         sidebar = NULL,
                         footer = NULL,
                         argonRow(center = TRUE,
                                  argonColumn(width = 12,
                                  p(tags$b("NOTE:"), "If your data includes separate columns for image names and/or group names 
                                    (", tags$i("e.g."), "test (mutant) versus control (wild type) samples) corresponding to each 
                                    section, define them below by clicking on the respective switch buttons.",
                                    style = "text-align: justify; font-size: 16px;")
                                  )
                         ),
                         
                         ###########
                         ## Section name
                         argonColumn(width = 12,
                                     uiOutput(outputId = "section_column")
                         ),
                         
                         argonColumn(width = 12, center = FALSE,
                                     prettySwitch(
                                       inputId = "columnImageOption",
                                       label = "Determine Image Names",
                                       value = FALSE,
                                       status = "success",
                                       fill = TRUE
                                     ),
                                     conditionalPanel(
                                       condition = "input.columnImageOption",
                                       uiOutput(outputId = "columnImage")
                                     )
                         ),
                         
                         argonColumn(width = 12, center = FALSE,
                                     prettySwitch(
                                       inputId = "columnGroupOption",
                                       label = "Determine Group Names",
                                       value = FALSE,
                                       status = "success",
                                       fill = TRUE
                                     ),
                                     conditionalPanel(
                                       condition = "input.columnGroupOption",
                                       uiOutput(outputId = "columnGroup")
                                     )
                         ),
                         
                         argonRow(center = TRUE,
                                  argonColumn(width = 12, center = TRUE,
                                              disabled(
                                                actionBttn(
                                                  inputId = "color_coord_preprocess",
                                                  label = "Preprocess the Data", 
                                                  style = "fill",
                                                  size = "sm",
                                                  block = TRUE,
                                                  no_outline = TRUE,
                                                  color = "primary"
                                                )
                                              )
                                  )
                         )
                       ),
                       
                       #####################
                       # Distance statistics
                       box(
                         title = "Distance Statistics",
                         id = "color_coord_DistanceStatistics_box",
                         status = "navy",
                         solidHeader = TRUE,
                         background = NULL,
                         gradient = FALSE,
                         width = 12,
                         collapsible = TRUE,
                         collapsed = TRUE,
                         closable = FALSE,
                         maximizable = TRUE,
                         icon = icon("chart-bar"),
                         boxToolSize = "xs",
                         elevation = 2,
                         headerBorder = TRUE,
                         label = boxLabel("Step 5", status = "info", 
                                          tooltip = "Step 5: Visualize Distance Statistics"),
                         dropdownMenu = NULL,
                         sidebar = NULL,
                         footer = NULL,
                         
                         hidden(
                           argonRow(id = "color_coord_DistStatsRow", 
                                    argonColumn(width = 12, 
                                                
                                                # Facet by
                                                argonRow(id = "color_coord_SecsFacetRow",
                                                  radioGroupButtons(
                                                    inputId = "color_coord_SecsFacet",
                                                    label = "Categorize top facet by:",
                                                    choices = c("Do not categorize" = 1, 
                                                                "Image" = "ImgName", 
                                                                "Group" = "GroupName"),
                                                    individual = TRUE,
                                                    checkIcon = list(
                                                      yes = tags$i(class = "fa fa-circle", 
                                                                   style = "color: steelblue"),
                                                      no = tags$i(class = "fa fa-circle-o", 
                                                                  style = "color: steelblue"))
                                                )
                                                ),
                                                
                                                # Show Plot title
                                                argonColumn(width = 12,
                                                            argonRow(
                                                              tags$b("Show title?")
                                                            ),
                                                            argonRow(
                                                              prettyToggle(
                                                                inputId = "color_coord_secDistsStatsPlotTitleOption",
                                                                label_on = "Yes!", 
                                                                icon_on = icon("check"),
                                                                status_on = "success",
                                                                status_off = "warning", 
                                                                label_off = "No",
                                                                icon_off = icon("times"),
                                                                value = FALSE
                                                              ),
                                                              hidden(
                                                                textInput(inputId = "color_coord_secDistsStatsPlotTitle", 
                                                                          label = NULL, 
                                                                          value = "Rain-Cloud Statistics Plot of Distances", 
                                                                          width = "100%", 
                                                                          placeholder = "Type in your desired plot title")
                                                              )
                                                            )
                                                ),
                                                
                                                argonRow(
                                                  # Show group names
                                                  argonColumn(width = 6, center = FALSE,
                                                              tags$b("Add group names?"),
                                                              prettyToggle(
                                                                inputId = "color_coord_secDistsStatsPlotGroupNames",
                                                                label_on = "Yes!", 
                                                                icon_on = icon("check"),
                                                                status_on = "success",
                                                                status_off = "warning", 
                                                                label_off = "No",
                                                                icon_off = icon("times"),
                                                                value = FALSE
                                                              )
                                                  ),
                                                  
                                                  # Show image names
                                                  argonColumn(width = 6, center = FALSE,
                                                              tags$b("Add image names?"),
                                                              prettyToggle(
                                                                inputId = "color_coord_secDistsStatsPlotImageNames",
                                                                label_on = "Yes!", 
                                                                icon_on = icon("check"),
                                                                status_on = "success",
                                                                status_off = "warning", 
                                                                label_off = "No",
                                                                icon_off = icon("times"),
                                                                value = FALSE
                                                              )
                                                  )
                                                ),
                                                
                                                
                                                # Select color palette
                                                pickerInput(
                                                  inputId = "color_coord_secDistsStatsPlotPalette",
                                                  width = "100%",
                                                  label = "Color Palette", 
                                                  choices = fishualize::fish_palettes(),
                                                  selected = "Centropyge_loricula",
                                                  options = list(
                                                    size = 5,
                                                    `live-search` = TRUE)
                                                ),
                                                
                                                plotOutput(outputId = "color_coord_SecClonalityStatsPlot") %>% shinycssloaders::withSpinner(type = 4),
                                                
                                                br(),
                                                
                                                argonR::argonRow(
                                                  column(6,
                                                         numericInput(inputId = "color_coord_SecStatsClonality.figure.width", label = "Figure width (in)", value = 8,
                                                                      min = 1, max = Inf, step = 1, width = "100%")
                                                  ),
                                                  column(6,
                                                         numericInput(inputId = "color_coord_SecStatsClonality.figure.height", label = "Figure height (in)", value = 8,
                                                                      min = 1, max = Inf, step = 1, width = "100%")
                                                  )
                                                ),
                                                
                                                argonR::argonRow(center = TRUE,
                                                                 column(12,
                                                                        numericInput(inputId = "color_coord_SecStatsClonality.PNG.resolution", label = "PNG file resolution (dpi)", value = 300,
                                                                                     min = 72, max = Inf, step = 1, width = "100%"))
                                                ),
                                                
                                                argonR::argonRow(
                                                  column(6,
                                                         downloadButton("download_color_coord_SecStatsClonality_PDF", "Download PDF file", icon = icon("download"),
                                                                        class = "btn-sm btn-block")),
                                                  column(6,
                                                         downloadButton("download_color_coord_SecStatsClonality_PNG", "Download PNG file", icon = icon("download"),
                                                                        class = "btn-sm btn-block")
                                                  )
                                                )
                                                
                                    )
                           )
                         )
                       )
                ),
                
                # Column 3 and 6
                column(width = 4,
                       
                       #######################
                       # Section color palette
                       box(
                         title = "Section Palette",
                         id = "color_coord_palette_box",
                         status = "navy",
                         solidHeader = TRUE,
                         background = NULL,
                         gradient = FALSE,
                         width = 12,
                         collapsible = TRUE,
                         collapsed = FALSE,
                         closable = FALSE,
                         maximizable = FALSE,
                         icon = icon("palette"),
                         boxToolSize = "xs",
                         elevation = 2,
                         headerBorder = TRUE,
                         label = boxLabel("Step 3", status = "info", 
                                          tooltip = "Step 3: Visualize and download the color palette of sections"),
                         dropdownMenu = NULL,
                         sidebar = NULL,
                         footer = NULL,
                         
                         argonRow(center = TRUE,
                                  argonColumn(width = 12,
                                              p("Select your desired section below to visualize its pie color palette.",
                                                style = "text-align: justify; font-size: 16px;")
                                  ),
                                  argonColumn(width = 12,
                                              p(tags$b("NOTE:"), "The color palette visualization is not applicable to",
                                                tags$em("X and Y coordinates"), "input file type.",
                                                style = "text-align: justify; font-size: 16px;")
                                  )
                         ),
                         
                         # define the section to visualize
                         argonRow(center = TRUE,
                                  column(width = 6,
                                         pickerInput(
                                           inputId = "color_coord_palette_groupby",
                                           width = "100%",
                                           label = "Group colors by:", 
                                           choices = c("Section" = "section",
                                                       "Section + Img" = "section_image",
                                                       "Section + Grp" = "section_group",
                                                       "Section + Img + Grp" = "section_image_group"),
                                           multiple = FALSE
                                         )
                                  ),
                                  column(width = 6,
                                           uiOutput('color_coord_pie_section')
                                  )
                                  ),
                         
                         argonColumn(width = 12,
                                     # Show Plot title
                                     tags$b("Show title?"),
                                     prettyToggle(
                                       inputId = "color_coord_pieTitleOption",
                                       label_on = "Yes!", 
                                       icon_on = icon("check"),
                                       status_on = "success",
                                       status_off = "warning", 
                                       label_off = "No",
                                       icon_off = icon("times"),
                                       value = FALSE
                                     ),
                                     hidden(
                                       uiOutput(outputId = "color_coord_pieTitleUI")
                                     ),
                                     ),
                         
                         argonColumn(width = 12,
                                  tags$b("Show color legend"),
                                  switchInput(
                                    inputId = "color_coordPalLegend",
                                    label = "<i class=\"fa fa-thumbs-up\"></i>",
                                    value = FALSE,
                                    size = "mini"
                                  ),
                                  
                                  hidden(
                                  plotOutput(outputId = "color_coord_SecPiePlot") 
                                  ),
                                  
                                  argonR::argonRow(center = TRUE,
                                                   column(6,
                                                          numericInput(inputId = "color_coord_SecPie.figure.width", label = "Figure width (in)", value = 8,
                                                                       min = 1, max = Inf, step = 1, width = "100%")
                                                   ),
                                                   column(6,
                                                          numericInput(inputId = "color_coord_SecPie.figure.height", label = "Figure height (in)", value = 8,
                                                                       min = 1, max = Inf, step = 1, width = "100%")
                                                   )
                                  ),
                                  
                                  argonR::argonRow(center = TRUE,
                                                   column(12,
                                                          numericInput(inputId = "color_coord_SecPie.PNG.resolution", label = "PNG file resolution (dpi)", value = 300,
                                                                       min = 72, max = Inf, step = 1, width = "100%"))
                                  ),
                                  
                                  argonR::argonRow(center = TRUE,
                                                   column(6,
                                                          disabled(downloadButton("download_color_coord_SecPie_PDF", "Download PDF file", icon = icon("download"),
                                                                         class = "btn-sm btn-block"))
                                                          ),
                                                   column(6,
                                                          disabled(downloadButton("download_color_coord_SecPie_PNG", "Download PNG file", icon = icon("download"),
                                                                         class = "btn-sm btn-block"))
                                                   )
                                  )
                         )
                       ),
                       
                       #####################################
                       
                       # ANOVA
                       box(
                         title = "ANOVA",
                         id = "color_coord_ANOVA_box",
                         status = "navy",
                         solidHeader = TRUE,
                         background = NULL,
                         gradient = FALSE,
                         width = 12,
                         collapsible = TRUE,
                         collapsed = FALSE,
                         closable = FALSE,
                         maximizable = TRUE,
                         icon = icon("not-equal"),
                         boxToolSize = "xs",
                         elevation = 2,
                         headerBorder = TRUE,
                         label = boxLabel("Step 6", status = "info", 
                                          tooltip = "Step 6: Variance Analysis of Sections"),
                         dropdownMenu = NULL,
                         sidebar = NULL,
                         footer = NULL,
                         
                         ## ANOVA
                         hidden(
                           argonRow(center = TRUE, id = "color_coord_ANOVANotifRow",
                                    tags$h6(br(),
                                            tags$b("Your input file includes only a single section and the ANOVA cannot be done!"),
                                            br(),
                                            br(),
                                            style = "background-color: lightgrey !important; text-align: center;
                                              padding:12px; margin:48px; border: 1px solid black; border-radius: 12px;"))
                         ),
                         
                         hidden(
                           argonRow(id = "color_coord_ANOVARow",
                                    
                                    # Define the variable for ANOVA
                                    radioGroupButtons(
                                      inputId = "color_coord_DistsANOVA_variable",
                                      label = "Define the variable for performing ANOVA:",
                                      choices = c("Section" = "Group_Img_SectionName", 
                                                  "Image" = "ImgName", 
                                                  "Group" = "GroupName"),
                                      individual = TRUE,
                                      checkIcon = list(
                                        yes = tags$i(class = "fa fa-circle", 
                                                     style = "color: steelblue"),
                                        no = tags$i(class = "fa fa-circle-o", 
                                                    style = "color: steelblue"))
                                    ),
                                    
                                    br(),
                                    
                                    # Generate ANOVA table 
                                    DT::dataTableOutput("color_coord_SecsDistsANOVATable") %>% shinycssloaders::withSpinner(type = 4)
                           )
                         )
                       )
                )
              )
            ),
            
            #########################################################
            #########################################################
            
            # Define the body of citation tab
            tabItem(
              tabName = "citation",
              
              argonSection(
                size = "sm",
                status = "default",
                gradient = TRUE,
                separator = FALSE,
                cascade = TRUE,
                separator_color = "white",
                shape = TRUE,
                         argonColumn(width = 12, 
                                     argonRow(center = TRUE,
                tags$b("Please use the following Zenodo DOI to cite the AutoClone App", style="font-size:20px;"), br(), br(), br(), 
                
                bs4InfoBox(
                  title = p("Abbas Salavaty and Peter D. Currie. ", tags$em("AutoClone: automatic analysis of genetic labelling data and clonality assessment."), " 2021."),
                  value = "DOI:10.5281/zenodo.5553555",
                  subtitle = tags$a(href = "https://zenodo.org/record/5553555", "AutoClone App Zenodo Web Page"),
                  icon = shiny::icon("scroll"),
                  color = "olive",
                  width = 12,
                  href = NULL,
                  fill = FALSE,
                  gradient = FALSE,
                  elevation = 2,
                  iconElevation = 2,
                  tabName = NULL
                )
                                     )
                )
              )
            ),
            
            #########################################################
            #########################################################
            
            # Define the body of automatic about analysis
            tabItem(
              tabName = "about",
              
              ## WHAT IS IT?
              argonSection(
                size = "sm",
                status = "default",
                gradient = TRUE,
                separator = FALSE,
                cascade = TRUE,
                separator_color = "white",
                shape = TRUE,
                argonRow(
                  argonColumn(
                    width = 12,
                    tags$h2("What is AutoClone?") %>% argonTextColor(color = "secondary"),
                  )
                )
              ),
              
              argonSection(
                size = "sm",
                status = "default",
                gradient = TRUE,
                separator = FALSE,
                cascade = TRUE,
                separator_color = "white",
                shape = TRUE,
                argonRow(center = FALSE,
                         argonColumn(width = 12, center = TRUE,
                         p(style = "text-align: justify; font-size: 16px; font-family:'sans-serif';",
                           "AutoClone is a shiny app for the automatic assessment of clonality based on the 
                         data generated through genetic cell labelling and lineage tracing assays. The AutoClone app includes 
                         three major modes relevant to the input data. To get more detailed information regarding the 
                         clonality assessment, different modes of the app, and template input data refer to the ", 
                           tags$em("Introduction"), " tab by clinking on the following button."),
                         
                         actionBttn(
                           inputId = "jump2Intro_tabBtn",
                           block = TRUE, size = "sm",
                           label = "Jump to Introduction Tab", 
                           style = "fill",
                           icon = icon("arrow-circle-up"),
                           color = "default"
                         ), br()
                )
                ), br(),
              ),
              
              ## Updates
              argonSection(
                size = "sm",
                status = "default",
                gradient = TRUE,
                separator = FALSE,
                cascade = TRUE,
                separator_color = "white",
                shape = TRUE,
              box(
                title = tags$b("Updates"),
                id = NULL,
                status = "navy",
                solidHeader = FALSE,
                background = NULL,
                gradient = FALSE,
                width = 12,
                collapsible = TRUE,
                collapsed = FALSE,
                closable = FALSE,
                maximizable = FALSE,
                icon = NULL,
                boxToolSize = "xs",
                elevation = 2,
                headerBorder = TRUE,
                label = NULL,
                dropdownMenu = NULL,
                sidebar = NULL,
                footer = NULL,
                argonRow(center = TRUE,
                         argonColumn(width = 8,
                                     p("Change Log - version and update history"), br(),
                                     
                                     p(tags$b("Version 1.0")),
                                     tags$ul(
                                       tags$li("Initial deployment")),
                                     p("Update: October 21, 2021.")
                                     ),
                         argonColumn(width = 4,
                                     bs4InfoBox(
                                       title = "Lastest Version",
                                       value = "V. 1.0",
                                       subtitle = "October 21, 2021",
                                       icon = shiny::icon("laptop-code"),
                                       color = "maroon",
                                       width = 12,
                                       href = NULL,
                                       fill = FALSE,
                                       gradient = FALSE,
                                       elevation = 2,
                                       iconElevation = 2,
                                       tabName = NULL
                                     )
                         )
                         )
                )
              ),
              
              ## Intro about the team
              argonSection(
                size = "sm",
                status = "default",
                gradient = TRUE,
                separator = FALSE,
                cascade = TRUE,
                separator_color = "white",
                shape = TRUE,
                argonRow(
                  argonColumn(
                    width = 12,
                    tags$h2("AutoClone App Team") %>% argonTextColor(color = "secondary")
                  )
                )
              ) %>% argonMargin(orientation = "t", value = -200),

              argonSection(
                size = "lg",
                status = "default",
                gradient = TRUE,
                separator = FALSE,
                cascade = FALSE,
                separator_color = "white",
                shape = TRUE,
                argonRow(
                  argonCard(
                    width = 50,
                    hover_lift = TRUE,
                    shadow = TRUE,
                    shadow_size = 5,
                    hover_shadow = TRUE,
                    icon = NULL,
                    btn_text = "More ...", 
                    status = "secondary", 
                    gradient = TRUE,
                    src = adrian_url,
                    title = argonRow(
                                     center = TRUE,
                                     list(
                                       argonColumn(width = 12, center = TRUE, 
                                       argonImage(
                                         src = "Abbas (Adrian) Salavaty.png",
                                         url = NULL,
                                         floating = FALSE,
                                         card_mode = FALSE,
                                         hover_lift = FALSE,
                                         width = 200
                                       )), 
                                       argonColumn(width = 12, center = TRUE, style = "padding-top: 15px;;",
                                       tags$h3("Adrian Salavaty", style = "text-align: center;")
                                       ),
                                       argonColumn(width = 12, center = TRUE, style = "text-align: center;",
                                              argonR::argonSocialButton(src = "https://twitter.com/mania_abbas", 
                                                                        status = "info",
                                                                        icon = icon("twitter")),
                                              argonR::argonSocialButton(src = "https://www.linkedin.com/in/abbassalavaty/", 
                                                                        status = "primary",
                                                                        icon = icon("linkedin-in"))
                                       )
                                     )
                    ),
                    argonRow(width = 12,
                             center = TRUE,
                             icon("user-secret"),
                             argonBadge(text = "Researcher & Developer", status = "primary")
                    ),
                    br(),
                    p("Abbas (Adrian) obtained his B.Sc. and M.Sc. degrees in Molecular Genetics and 
                                           Biology-Biochemistry, respectively. His Bachelor's project as well as his
                                           Master's thesis was purely computational. In this regard, he used both bioinformatics and 
                                           systems biology approaches to acomplish these projects. He is now a bioinformatician and 
                                           systems biologist pursuing his Ph.D. at the Australian Regenerative Medicine 
                                           Institute (ARMI), Monash University.", style = "text-align: justify; font-family:'sans-serif';")
                  ),
                  argonCard(
                    width = 50,
                    hover_lift = TRUE,
                    shadow = TRUE,
                    shadow_size = 5,
                    hover_shadow = TRUE,
                    icon = NULL,
                    btn_text = "More ...", 
                    status = "secondary", 
                    gradient = TRUE,
                    src = pete_url,
                    title = argonRow(
                                     center = TRUE,
                                     list(
                                       argonColumn(width = 12, center = TRUE, 
                                       argonImage(
                                         src = "Peter Currie.png",
                                         url = NULL,
                                         floating = FALSE,
                                         card_mode = FALSE,
                                         hover_lift = FALSE,
                                         width = 200
                                       )),
                                       argonColumn(width = 12, center = TRUE, style = "padding-top: 15px;;",
                                       tags$h3("Peter Currie", style = "text-align: center;")
                                       ),
                                       argonColumn(width = 12, center = TRUE, style = "text-align: center;",
                                              argonR::argonSocialButton(src = "https://twitter.com/petecurr", 
                                                                        status = "info",
                                                                        icon = icon("twitter")),
                                              argonR::argonSocialButton(src = "https://www.linkedin.com/in/peter-currie-076bb514/", 
                                                                        status = "primary",
                                                                        icon = icon("linkedin-in"))
                                       )
                                     )
                    ),
                    argonRow(width = 12,
                             center = TRUE,
                             icon("chalkboard-teacher"),
                             argonBadge(text = "Supervisor", status = "primary")
                    ),
                    br(),
                    p("Peter D. Currie received his PhD in Drosophila genetics from Syracuse University. 
                               He undertook postdoctoral training in zebrafish development at the Imperial Cancer Research Fund (now Cancer Research UK) in London, UK.. 
                               He has worked as a laboratory head at MRC Human Genetics Unit in Edinburgh and Victor Chang 
                               Cardiac Research Institute in Sydney. 
                               In 2016 he was appointed Director of Research of the ARMI at Monash University, Melbourne.", 
                      style = "text-align: justify; font-family:'sans-serif';")
                  )
                )
              ) %>% argonMargin(orientation = "t", value = -200), br(), 
              
              # acknowledgments
              argonSection(
                size = "sm",
                status = "default",
                gradient = TRUE,
                separator = FALSE,
                cascade = TRUE,
                separator_color = "white",
                shape = TRUE,
                argonRow(
                  argonColumn(
                    width = 12,
                    tags$h2("Acknowledgments") %>% argonTextColor(color = "secondary"), br(),
                  )
                )
              ),
              
              argonSection(
                size = "sm",
                status = "default",
                gradient = TRUE,
                separator = FALSE,
                cascade = TRUE,
                separator_color = "white",
                shape = TRUE,
                argonColumn(width = 12,
                            argonTabSet(
                              id = "acknowledgments",
                              card_wrapper = TRUE,
                              horizontal = TRUE,
                              circle = FALSE,
                              size = "lg",
                              width = 12,
                              
                              argonTab(
                                tabName = "Margo Montandon",
                                active = TRUE,
                                argonRow(
                                  argonColumn(width = 4, center = TRUE,
                                              argonImage(
                                                src = "Margo Montandon.png",
                                                url = NULL,
                                                floating = TRUE,
                                                card_mode = FALSE,
                                                hover_lift = FALSE,
                                                width = 100
                                              )
                                  ),
                                  argonColumn(width = 8, center = TRUE,
                                              "Margo Montandon, PhD, for her consultations regarding use cases of the app and 
                                              the input data to which the app should be compatible with."
                                  )
                                )
                              ),
                              
                              argonTab(
                                tabName = "Avnika Ruparelia",
                                active = FALSE,
                                argonRow(
                                  argonColumn(width = 4, center = TRUE,
                                              argonImage(
                                                src = "Avnika Ruparelia.png",
                                                url = NULL,
                                                floating = TRUE,
                                                card_mode = FALSE,
                                                hover_lift = FALSE,
                                                width = 100
                                              )
                                  ),
                                  argonColumn(width = 8, center = TRUE,
                                              "Avnika Ruparelia, PhD, for for her consultations regarding use cases of the app and 
                                              the input data to which the app should be compatible with."
                                  )
                                )
                              )
                            )
                )
              )
            ),
            
            #########################################################
            #########################################################
            
            # Define the body of Contact tab
            tabItem(
                tabName = "contact",
                argonRow(center = TRUE, tags$b(tags$h2(strong("GET IN TOUCH")), style = "font-family:'sans-serif';")),
                argonRow(center = TRUE, tags$b(icon("user-ninja"), style = "font-size:100px;")), br(),
                argonRow(center = TRUE, tags$b(p("We appreciate your interest in the AutoClone web app. Use the form below to drop us an email.", 
                                                 style = "font-size:120%; font-family:'sans-serif';"))), br(),
                argonCard(
                    width = 50,
                    hover_lift = FALSE,
                    shadow = TRUE,
                    shadow_size = 7,
                    hover_shadow = TRUE,
                    icon = NULL,
                    floating = TRUE, 
                    btn_text = NULL, 
                    status = "primary", 
                    gradient = TRUE,
                    argonRow(center = TRUE, textInput("userContactName", "Name:", placeholder = "Your name", width = "600px")),
                    argonRow(center = TRUE, textInput("fromUser", "Email Address:", placeholder = "Your email address", width = "600px")),
                    argonRow(center = TRUE, textInput("subjectUser", "Subject:", placeholder = "Subject", width = "600px")),
                    argonRow(center = TRUE, textAreaInput(inputId = "messageUser", label= "Email Content:", width = "600px", 
                                                          height = "200px", resize = "vertical", placeholder = "Enter your message here")),
                    argonRow(center = TRUE, tags$b("How is your math?"), br()),
                    argonRow(center = TRUE, textOutput("mathQuestion")), 
                    argonRow(center = TRUE, numericInput(inputId = "mathAnswer", label = NULL,
                                                         value = NULL, step = 1)),
                    argonRow(center = TRUE, 
                             actionBttn(inputId = "sendContactEmail", label = "Submit", size = "sm",
                                        icon("paper-plane"), style="minimal", block = FALSE,
                                        color = "primary", no_outline = TRUE)),
                    br()
                )
            )
        )
    ),
    
    ##################################################################################
    
    # Define the Footer
    footer = dashboardFooter( 
        left = argonFooter(
            has_card = FALSE,
            argonContainer(
                size = NULL,
                # Copyrights
                argonRow(
                    center = TRUE,
                    argonColumn(
                        center = FALSE,
                        width = 5,
                        div(
                            class = "copyright",
                            argonLead("©2021 The AutoClone Dashboard Team", br(),"Monash University, Australia") 
                        )
                    ),
                    argonColumn(
                        width = 7,
                        center = TRUE,
                        argonMargin(
                            orientation = "x", 
                            value = 2,
                            argonImage(
                                src = "ARMI.png",
                                url = "http://www.armi.org.au/",
                                width = "15%"
                            ) 
                        ),
                        argonMargin(
                            orientation = "x", 
                            value = 2,
                            argonImage(
                                src = "Monash_University.png",
                                url = "https://www.monash.edu/",
                                width = "17%"
                            ) 
                        ),
                        argonMargin(
                            orientation = "x", 
                            value = 2,
                            argonImage(
                                src = "Victoria_State_Government.png",
                                url = "https://www.vic.gov.au/",
                                width = "11%"
                            ) 
                        ),
                        argonMargin(
                            orientation = "x", 
                            value = 2,
                            argonImage(
                                src = "Australian-government.png",
                                url = "https://www.australia.gov.au/",
                                width = "14%"
                            ) 
                        )
                    )
                )
            )
        ),
    right = tabPanel(
        #######################
        # Keep shiny alive
        tags$head(
            HTML(
                "
          <script>
          var socket_timeout_interval
          var n = 0
          $(document).on('shiny:connected', function(event) {
          socket_timeout_interval = setInterval(function(){
          Shiny.onInputChange('count', n++)
          }, 1500000)
          });
          $(document).on('shiny:disconnected', function(event) {
          clearInterval(socket_timeout_interval)
          });
          </script>
          "
            )
        ),
        textOutput("keepAlive"),
        #######################
    )
    ),

    options = NULL,
    fullscreen = TRUE,
    help = TRUE,
    dark = FALSE,
    scrollToTop = TRUE
    
)

##***************************************************************##
##***************************************************************##

server <- function(input, output, session) {
    
    #######################
    # Keep shiny alive
    
    output$keepAlive <- renderText({
        req(input$count)
        paste("keep alive ", input$count)
    })
    
    #######################
    
    ###*********************###
    
    # Take care of sending contact form
    test_a <- round(runif(1, min=0, max=21))
    test_b <- round(runif(1, min=0, max=10))
    
    output$mathQuestion <- renderText({ 
        paste0(test_a, "+", test_b, "=")
    })
    
    isValidEmail <- function(x) {
        grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>", as.character(x), ignore.case=TRUE)
    }
    
    # check name
    observeEvent(input$sendContactEmail, {
        if(input$userContactName == "") {
            showNotification("The Name field is empty! Please type in your name in the Name box.", type = "warning")
        }
    })
    
    # check email address
    observeEvent(input$sendContactEmail, {
        if(!isValidEmail(input$fromUser)) {
            showNotification("Invalid email address; Please type in a valid email address!", type = "warning")
        }
    })
    
    # check subject
    observeEvent(input$sendContactEmail, {
        if(input$subjectUser == "") {
            showNotification("The Subject field is empty! Please type in a subject for your email in the Subject box.", type = "warning")
        }
    })
    
    # check message
    observeEvent(input$sendContactEmail, {
        if(input$messageUser == "") {
            showNotification("The Email Content field is empty! Please type in your message in the Email Content box.", type = "warning")
        }
    })
    
    # Check math answer
    observeEvent(input$sendContactEmail, {
        if(is.na(input$mathAnswer)|| input$mathAnswer != test_a+test_b) {
            showNotification("The math question is not answered correctly! Please type in the correct answer in the corresponding box.", type = "warning")
        }
    })
    
    observeEvent(input$sendContactEmail, {
        
        if(is.null(input$sendContactEmail) || input$sendContactEmail==0 || is.na(input$mathAnswer) || 
           input$mathAnswer != test_a+test_b || !isValidEmail(input$fromUser) || input$fromUser == "" ||
           input$userContactName == "" || input$subjectUser == "" || input$messageUser == "") {
            return(NULL)
        } else {
            
            contactEmail = emayili::envelope() %>%
                from(input$fromUser) %>%
                to("influential.shiny.app@gmail.com") %>%
                subject(input$subjectUser) %>%
                text(paste(paste0("This email is sent from: ", input$userContactName, "\n\n", "Regarding the AutoClone App", "\n\n", 
                                  "User email address: ", input$fromUser, "\n"), "\n", input$messageUser))
            
            smtp4contactEmail = emayili::server(host = "smtp.gmail.com" # need to make sure it knows it's the emayili::server function, not the shiny::server function
                                                , port = 465
                                                , username = "influential.shiny.app@gmail.com"
                                                , password = "absa9531!")
            
            withProgress(message = 'Sending the email is in progress ...',
                         detail = 'This may take a while...', value = 60, max = 100,  {
                             smtp4contactEmail(contactEmail, verbose = FALSE)
                             for (i in 61:100) {
                                 setProgress(i)
                             }
                         })
            
            
            
            sendSweetAlert(
                session = session,
                title = "Thanks for getting in touch!",
                text = tags$p("Your email is successfully sent and we will respond to that shortly."),
                type = "info"
            )
            
        }
        
    })
    
    ###*********************###
    
    # Take care of email subscription
    
    # Close on clicking the submit button
    observeEvent(input$submitSubscription, {
        session$sendCustomMessage("close_subscription", "")
    })
    
    # Add a function for checking the validity of email
    isValidEmail <- function(x) {
        grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>", as.character(x), ignore.case=TRUE)
    }
    
    observeEvent(input$submitSubscription, {
        if(isValidEmail(input$emailAddress_forSub)) {
            email_addresses_table <- rbind(Email_Addresses,
                                           c(input$userName, input$emailAddress_forSub))
            
            colnames(email_addresses_table) <- c("Name", "Email")
            
            write.csv(x = email_addresses_table,
                      file = paste0("./www/Email_database/", session$token, "-Email_Address ", "(", Sys.Date(), ")", ".csv"),
                      row.names = FALSE)
        } else {
            showNotification("Invalid email address; Please type in a valid email address!", type = "warning")
        }
        
    })
    
    ###*********************###
    
    # Jump to different tabs
    
    observeEvent(input$jump2Intro_tabBtn, {
      
      bs4Dash::updateTabItems(
        session = session,
        inputId = "main_intro",
        selected = "introduction"
      )
      
    }, ignoreInit = TRUE)
    
    observeEvent(input$brush_basedMode_tabBtn, {
      
      bs4Dash::updateTabItems(
        session = session,
        inputId = "automatic_analysis",
        selected = "brush_based"
      )
      
    }, ignoreInit = TRUE)
    
    ###########
    
    observeEvent(input$Pre_cropped_basedMode_tabBtn, {
      
      bs4Dash::updateTabItems(
        session = session,
        inputId = "automatic_analysis",
        selected = "pre_cropped_based"
      )
      
    }, ignoreInit = TRUE)
    
    ###########
    
    observeEvent(input$Color_Coord_basedMode_tabBtn, {
      
      bs4Dash::updateTabItems(
        session = session,
        inputId = "semiautomatic_analysis",
        selected = "color_coord_based"
      )
      
    }, ignoreInit = TRUE)
    
    ###*********************###
    
    # Sample tables
    
    # Generate the sample HEX table
    output$HEX_sample_table <- renderTable({
      
      data <- read.csv("www/sample_HEX_data.csv")
      
      data
      
    }, rownames = FALSE, hover = TRUE, digits = 2)
    
    ##########
    
    # Generate the sample RGB table
    output$RGB_sample_table <- renderTable({
      
      data <- read.csv("www/sample_RGB_data.csv")
      
      data
      
    }, rownames = FALSE, hover = TRUE, digits = 2)
    
    ##########
    
    # Generate the sample RGBFiji1 table
    output$RGBFiji1_sample_table <- renderTable({
      
      data <- read.csv("www/sample_RGBFiji1_data.csv")
      
      data
      
    }, rownames = FALSE, hover = TRUE, digits = 2)
    
    ##########
    
    # Generate the sample RGBFiji2 table
    output$RGBFiji2_sample_table <- renderTable({
      
      data <- read.csv("www/sample_RGBFiji2_data.csv")
      
      data
      
    }, rownames = FALSE, hover = TRUE, digits = 2)
    
    ##########
    
    # Generate the sample HSL table
    output$HSL_sample_table <- renderTable({
      
      data <- read.csv("www/sample_HSL_data.csv")
      
      data
      
    }, rownames = FALSE, hover = TRUE, digits = 2)
    
    ##########
    
    # Generate the sample HSV table
    output$HSV_sample_table <- renderTable({
      
      data <- read.csv("www/sample_HSV_data.csv")
      
      data
      
    }, rownames = FALSE, hover = TRUE, digits = 2)
    
    ##########
    
    # Generate the sample XY table
    output$XY_sample_table <- renderTable({
      
      data <- read.csv("www/sample_XY_coordinates.csv")
      
      data
      
    }, rownames = FALSE, hover = TRUE, digits = 2)
    
    ###*********************###
    
    # Create placeholders for new groups
    
    # observeEvent(input$add_group, {
    #     insertUI(
    #         selector = "#add_group",
    #         where = "afterEnd",
    #         ui = argonRow(center = TRUE,
    #                       argonColumn(width = 3, center = TRUE,
    #                     tags$b("Group ", input$add_group, ":")
    #                     ),
    #                     argonColumn(width = 9, center = TRUE,
    #                                 style='margin-top:11px; margin-buttom:0px;  padding-top: 0px;',
    #             textInput(inputId = paste0("group", input$add_group),
    #                        label = NULL, value = NULL, 
    #                        width = "100%", placeholder = "Type in your desired group name")
    #                     )
    #         )
    #     )
    # })
    # 
    
    # Instead we will use an inputSweetalert
    observeEvent(input$add_group, {
        inputSweetAlert(
            session = session,
            inputId = "majorGroupName",
            title = "Type in your desired group name",
            text = "You can assign each of the images you will upload/have uploaded to this group.",
            type = "info",
            input = "text",
            inputOptions = NULL,
            inputPlaceholder = "Group name",
            btn_labels = "OK",
            btn_colors = NULL,
            reset_input = TRUE
        )
    })
    
    ###*********************###
    
    # Generate buttons for imported images
    
    observeEvent(input$major_image,{
      shinyjs::show(id = "majorImage_header")
      
      # Reset the results
      majorImgSections$df <- data.frame(ImgName = character(), ImgPath = character(),
                                        GroupName = character(), SectionName = character(),
                                        Group_SectionName = character(), 
                                        Group_Img_SectionName = character(), 
                                        BtnID = character(), GroupID = character(), 
                                        ID = integer(),
                                        Mean = numeric(),
                                        SD = numeric())
      autoClonizeRes$result <- list()
      majorImgSecCroppedList$section <- list()
      majorImgSecsHistList$Hist <- list()
    })
    
    observeEvent(input$major_image, {
        output$image_buttons <- renderUI({
            image_filenames <- input$major_image[['name']]
            image_filepaths <- input$major_image[['datapath']]
            
            lapply(
                1:length(image_filenames), 
                function(i) {
                    shinyFeedback::loadingButton(inputId = paste0("majorImageBtn", i), 
                                                 label = gsub(pattern = paste0(".", tools::file_ext(image_filepaths[[i]])), 
                                                              replacement = "", x = image_filenames[[i]]),
                                                 loadingLabel = gsub(pattern = paste0(".", tools::file_ext(image_filepaths[[i]])), 
                                                                     replacement = "", x = image_filenames[[i]]),
                                                 class = "btn-sm btn-info",
                                                 style = "width: 100%; margin-top:6px; margin-bottom:6px; padding:3px")
                }
            )
        })
    })
    
    ###*********************###

    # Generate pickerinputs for imported groups
    
    # Clear Groups
    observeEvent(input$clearGrpsBtn, {
        majorGroupNames$groups <- NULL
    })
    
    observe({
        if(!is.null(majorGroupNames$groups) & !is.null(input$major_image)) {
        shinyjs::show(id = "majorGroup_header")
            shinyjs::show(id = "clearGrpsRow")
        }
    })
    
    observeEvent(input$add_group,{
        shinyjs::show(id = "groupPicker_column")
    })
    

    # Define major group names as a reactive value
    majorGroupNames <- reactiveValues(groups = NULL)
    observeEvent(input$majorGroupName, {
        majorGroupNames$groups <- c(majorGroupNames$groups, input$majorGroupName)
    })

    
    observeEvent(input$major_image, {
        output$image_groups <- renderUI({
            image_filenames <- input$major_image[['name']]
            
            lapply(
                1:length(image_filenames), 
                function(i) {
                    pickerInput(
                        inputId = paste0("groupPicker", i),
                        label = NULL, width = "fit",
                        choices = majorGroupNames$groups,
                        options = list(
                            title = "Assign to a group",
                            style = "info btn-xs",
                            width = 'fit')
                    )
                }
            )
        })
    })
    
    #####****************************************************************#####
    
    # Visualize the image
    
    #****************#
    # First determine which image button was clicked
    
    lastMajorBtnClicked <- reactiveValues(lastBtn = character(),
                                          lastMajorImg = character(),
                                          lastMajorImgPath = character())
    
    observe({
        if(!is.null(input$major_image))
    lapply(
        X = 1:length(input$major_image[['name']]),
        FUN = function(i){
            observeEvent(input[[paste0("majorImageBtn", i)]], {
                if (input[[paste0("majorImageBtn", i)]] > 0) {
                  
                    lastMajorBtnClicked$lastBtn = paste0("majorImageBtn", i)
                    
                    lastMajorBtnClicked$lastMajorImg = gsub(pattern = paste0(".", tools::file_ext(input$major_image[['datapath']][[i]])), 
                                                            replacement = "", x = input$major_image[['name']][[i]])
                    
                    lastMajorBtnClicked$lastMajorImgPath = input$major_image[['datapath']][i]
                    
                }
              
              # Show the plot row
              shinyjs::show(id = "majorPlotRow")
              
              # Reset all feedback buttons on selecting another
              lapply(X = c(1:length(input$major_image[['name']]))[-i],
                     FUN = function(x) {
                       shinyFeedback::resetLoadingButton(inputId = paste0("majorImageBtn", x))
                       
                     }
                     )
              
              # Hide the section plot row when clicking on a new image
              shinyjs::hide(id = "majorSecPlotsRow")
              
            })
        }
    )
    })
    
    #****************#
    
    # Read in the image
    major_image_file <- reactive({
      
      ext <- tools::file_ext(lastMajorBtnClicked$lastMajorImgPath)
      df = switch(ext,
                  png = colordistance::loadImage(path = lastMajorBtnClicked$lastMajorImgPath,
                                                 lower = c(0, 0, 0), upper = c(0.1, 0.1, 0.1),
                                                 hsv = TRUE, alpha.channel = FALSE),
                  jpg = colordistance::loadImage(path = lastMajorBtnClicked$lastMajorImgPath,
                                                 lower = c(0, 0, 0), upper = c(0.1, 0.1, 0.1),
                                                 hsv = TRUE, alpha.channel = FALSE),
                  jpeg = colordistance::loadImage(path = lastMajorBtnClicked$lastMajorImgPath,
                                                  lower = c(0, 0, 0), upper = c(0.1, 0.1, 0.1),
                                                  hsv = TRUE, alpha.channel = FALSE),
                  tif =  tiff::readTIFF(source = lastMajorBtnClicked$lastMajorImgPath),
                  tiff = tiff::readTIFF(source = lastMajorBtnClicked$lastMajorImgPath),
                  validate("Invalid file; Please upload a .tif, .tiff, .png, .jpg, or .jpeg file")
      )
      
      if(ext == "tif" | ext == "tiff") {
        ## Create a user specific directory
        dir.create(session$token)
        
        ## Saving section to the directory
        png::writePNG(image = df,
                      target = paste0(session$token, "/MajorImg.png"), dpi = 600)
        
        df <- colordistance::loadImage(path = paste0(session$token, "/MajorImg.png"),
                                       lower = c(0, 0, 0), upper = c(0.1, 0.1, 0.1),
                                       hsv = TRUE, alpha.channel = FALSE)
        
        ## Remove the image from directory
        unlink(session$token, recursive = TRUE)
      }
      
      df

    })
    
    #****************#
    
    # Save image RGB into a reactive obj
    major_image_rgb <- reactive({
      
      major_image_file()$original.rgb
      
    })
    
    #****************#
    
    # Generate the plot file
    major_image <- reactiveValues(plotFile = NULL)
    observe({
        if(length(lastMajorBtnClicked$lastBtn)> 0) {
            
            major_image$plotFile <- 
                ggplot() +
                annotation_custom(rasterGrob(major_image_rgb(),
                                             width = unit(1,"npc"),
                                             height = unit(1,"npc")),
                                  -Inf, Inf, -Inf, Inf) +
                theme_void()
        }
    })
    
    #****************#
    
    # Use the maxPlotHeight and maxPlotWidth JS scripts for maximizing image height and width
    
    plotHeight <- reactive({ 
        ifelse(is.null(input$plotHeight), 0, input$plotHeight)
    })
    
    js$calcHeight()
    
    #*****#
    
    plotWidth <- reactive({ 
        ifelse(is.null(input$plotWidth), 0, input$plotWidth)
    })
    
    js$calcWidth()
    
    #****************#
    
    # Define the max height and width based on either the box is maximized or not
    
    majorPlotHeight <- reactive({
        ifelse(input$major_image_visualization$maximized, plotHeight(), "auto")
    })
    
    majorPlotWidth <- reactive({
        ifelse(input$major_image_visualization$maximized, plotWidth(), "auto")
    })
    
    #****************#
    
    # render the plot
    output$uploaded_major_image <- renderPlot({
        
        major_image$plotFile
        
    }, height = majorPlotHeight, width = majorPlotWidth)
    
    #****************#
    
    # Set the name of the section
    
    # Determine the group name associated with the section
    SecGroupName <- reactiveValues(name = NULL, name4title = NULL)
    
    observeEvent(input$defineMajorSection, {
      
      # Send the alert for defining group name
        inputSweetAlert(
            session = session,
            inputId = "majorSectionName",
            title = "Type in your desired section name",
            text = "Choose a name for this section that would remind you the features of this section and 
            the image and group it belongs to.",
            type = "info",
            input = "text",
            inputOptions = NULL,
            inputPlaceholder = "Section name",
            btn_labels = "OK",
            btn_colors = NULL,
            reset_input = TRUE
        )
      
      ## Determine the group name associated with the section
        if(is.null(input[[paste0("groupPicker", substr(x = lastMajorBtnClicked$lastBtn, start = nchar(lastMajorBtnClicked$lastBtn), stop = nchar(lastMajorBtnClicked$lastBtn)))]]) ||
           input[[paste0("groupPicker", substr(x = lastMajorBtnClicked$lastBtn, start = nchar(lastMajorBtnClicked$lastBtn), stop = nchar(lastMajorBtnClicked$lastBtn)))]] == "") {
          SecGroupName$name = NA
          SecGroupName$name4title = ""
        } else {
          SecGroupName$name = input[[paste0("groupPicker", substr(x = lastMajorBtnClicked$lastBtn, start = nchar(lastMajorBtnClicked$lastBtn), stop = nchar(lastMajorBtnClicked$lastBtn)))]]
          SecGroupName$name4title = paste0(input[[paste0("groupPicker", substr(x = lastMajorBtnClicked$lastBtn, start = nchar(lastMajorBtnClicked$lastBtn), stop = nchar(lastMajorBtnClicked$lastBtn)))]])
        }
      
      ## Hide the section plot row when clicking on the load image or add section
      shinyjs::hide(id = "majorSecPlotsRow")
        
    })

    #****************#
    
    # Check if new section is created before clicking on the approve section 
    
    approveBtnClicked <- reactiveValues(number = 0)
    
    observeEvent(input$defineMajorSection, {
        approveBtnClicked$number = 1
    })
    
    observeEvent(input$approveMajorSection, {
        approveBtnClicked$number = approveBtnClicked$number + 1
    })
    
    #****************#
    
    # Enable the Define/Approve section buttons
    
    observe({
        if(input$major_image_visualization$maximized) {
            shinyjs::hide("defineMajorSectionRow")
            
        } else {
            shinyjs::show("defineMajorSectionRow")
        }
    })
    
    shinyjs::disable("defineMajorSection")
    
    observe({
        if(!is.null(major_image$plotFile)) {
            shinyjs::enable("defineMajorSection")
        }
    })
    
    observe({
        if(!is.null(input$major_image_zoom_section) & approveBtnClicked$number == 1 &
           input$major_image_visualization$maximized == FALSE) {
            shinyjs::enable(id = "approveMajorSection")
        } else {
            shinyjs::disable(id = "approveMajorSection")
        }
    })
    
    #****************#
    
    # Take screenshot of the selected section
    
    output$majorImgScreenshotBtnUI <- renderUI({
      disabled(
        downloadScreenShotBtn(inputId = "approveMajorSection",
                              ui = '#uploaded_major_image',
                              label = 'Approve Section', 
                              filename = paste0(lastMajorBtnClicked$lastMajorImg,
                                                "-",
                                                ifelse(is.na(SecGroupName$name), "", paste0(SecGroupName$name4title, "-")),
                                                input$majorSectionName, " (", Sys.Date(), ")", ".png"))
      )
    })
    
    observeEvent(input$approveMajorSection, {
      
      showToast(type = "success", 
                message =  "Save the approved section image for your reference.", 
                title = "The section is now approved!", 
                .options = myToastOptions
      )
    })
    
    
    #****************************************************************#
    
    # Zoom into the major uploaded image (define coordinates to be used for cropping)
    zoom_ranges <- reactiveValues(x = NULL, y = NULL)
    
    # When a double-click happens, check if there's a brush on the plot.
    # If so, zoom to the brush bounds; if not, reset the zoom.
    observe({
        brush <- input$major_image_zoom_section
        if (!is.null(brush)) {
            zoom_ranges$x <- c(brush$xmin, brush$xmax)
            zoom_ranges$y <- c(brush$ymin, brush$ymax)

        } else {
            zoom_ranges$x <- NULL
            zoom_ranges$y <- NULL
        }
        
        # Get the X and Y coords
        # xmin <- event_data("major_image_zoom_section")$x[1]
        # xmax <- event_data("major_image_zoom_section")$x[2]
        # ymin <- event_data("major_image_zoom_section")$y[1]
        # ymax <- event_data("major_image_zoom_section")$y[2]
        
    })
    
    #****************************************************************#
    
    # Get the dim of the Major input image
    majorImage_dim <- reactive({
        dim(major_image_rgb())[c(1,2)]
    })

    #****************************************************************#
    
    # Define the xy coordinates of the selection for cropping the image
    
    majorImageSection <- reactiveValues(xmin = NULL, xmax = NULL,
                                        ymin = NULL, ymax = NULL)
    
    # reduce xmin and xmax
    
    observe({
        if(length(lastMajorBtnClicked$lastBtn)> 0 & !is.null(input$major_image_zoom_section)) {
            
            majorImageSection$xmin <- (((1.05-zoom_ranges$y[2]) * majorImage_dim()[1])/1.075) + 1
            
            majorImageSection$xmax <- (((1.05-zoom_ranges$y[1]) * majorImage_dim()[1])/1.1)
            
            majorImageSection$ymin <- (((zoom_ranges$x[1]+0.04) * majorImage_dim()[2])/1.05) + 1
            
            majorImageSection$ymax <- (((zoom_ranges$x[2]+0.03) * majorImage_dim()[2])/1.05)
            
        }
    })
    
    observeEvent(input$approveMajorSection,  {

      if(majorImageSection$xmin < 1) {majorImageSection$xmin <- 1}
      if(majorImageSection$xmax < 1) {majorImageSection$xmax <- 1}
      if(majorImageSection$ymin < 1) {majorImageSection$ymin <- 1}
      if(majorImageSection$ymax < 1) {majorImageSection$ymax <- 1}

      if(majorImageSection$xmin > majorImage_dim()[1]) {majorImageSection$xmin <- majorImage_dim()[1]}
      if(majorImageSection$xmax > majorImage_dim()[1]) {majorImageSection$xmax <- majorImage_dim()[1]}
      if(majorImageSection$ymin > majorImage_dim()[2]) {majorImageSection$ymin <- majorImage_dim()[2]}
      if(majorImageSection$ymax > majorImage_dim()[2]) {majorImageSection$ymax <- majorImage_dim()[2]}

    }, ignoreInit = TRUE)
    
    #****************#

    # Crop the section
    majorImgSecCropped <- reactiveValues(plot = NULL)
    majorImgSecCroppedList <- reactiveValues(section = list())
    majorImgSecPixelPlot <- reactiveValues(plot = NULL)
    majorImgSecCirclePlot <- reactiveValues(plot = NULL)
    majorImgSecDistHist <- reactiveValues(plot = NULL)
    majorImgSecsHistList <- reactiveValues(Hist = list())
    
    observeEvent(input$approveMajorSection, {
      
      majorImgSec <- imagefx::crop.image(img = major_image_rgb(),
                                         xleft = majorImageSection$xmin,
                                         xright = majorImageSection$xmax,
                                         ybottom = majorImageSection$ymin,
                                         ytop = majorImageSection$ymax,
                                         pick = FALSE)

      ## Create a user specific directory
      dir.create(session$token)

      ## Saving section to the directory
      png::writePNG(image = majorImgSec$img.crop,
                    target = paste0(session$token, "/section.png"), dpi = 600)

      majorImgSecCropped$plot <- colordistance::loadImage(path = paste0(session$token, "/section.png"),
                                                          lower = c(0, 0, 0), upper = c(0.1, 0.1, 0.1),
                                                          hsv = TRUE, alpha.channel = FALSE)

      ## Remove the image from directory
      unlink(session$token, recursive = TRUE)
      
      
      # Generate the Section histogram plot
      majorImgSecDistHist$plot <- colordistance::getImageHist(image = majorImgSecCropped$plot,
                                                              lower = c(0, 0, 0),
                                                              upper = c(0.1, 0.1, 0.1), hsv = TRUE, 
                                                              title = paste0(lastMajorBtnClicked$lastMajorImg,
                                                                             "-",
                                                                             ifelse(is.na(SecGroupName$name), "", paste0(SecGroupName$name4title, "-")),
                                                                             input$majorSectionName,
                                                                             "\n",
                                                                             "Color distribution histogram"))
      
      # Add the section histogram to the list for later color distance assessment
      majorImgSecsHistList$Hist[[paste0(lastMajorBtnClicked$lastMajorImg, "-", SecGroupName$name4title, 
                                        ifelse(is.na(SecGroupName$name), "","-"), input$majorSectionName)]] <- majorImgSecDistHist$plot
      
    })
    
    #****************************************************************#
    
    # Create color scheme plots for the section
    
    ## Hide the section plot row when clicking on the load image
    observeEvent(input$major_image, {
      shinyjs::hide(id = "majorSecPlotsRow")
    })
    
    #****************#
    
    observeEvent(input$approveMajorSection, {
      
      shinyjs::show(id = "majorSecPlotsRow")
      
      ## Pixel plot
    
    output$majorSecPixelPlot <- renderPlot({
      
      majorImgSecPixelPlot$plot <- colordistance::plotPixels(img = majorImgSecCropped$plot, n = input$majorPixelPlotN,
                                                             lower = c(0, 0, 0),
                                                             upper = c(0.1, 0.1, 0.1), 
                                                             main = paste0(lastMajorBtnClicked$lastMajorImg,
                                                                           "-",
                                                                           ifelse(is.na(SecGroupName$name), "", paste0(SecGroupName$name4title, "-")),
                                                                           input$majorSectionName,
                                                                           "\n",
                                                                           "Pixel Plot: ", input$majorPixelPlotN," Points"))
        
        majorImgSecPixelPlot$plot
        
      })
    
    #********#
    
    ## Color distribution histogram
    
    output$majorSecDistHist <- renderPlot({
      
      # Generate the Section histogram plot
      majorImgSecDistHist$plot <- colordistance::getImageHist(image = majorImgSecCropped$plot,
                                                              lower = c(0, 0, 0),
                                                              upper = c(0.1, 0.1, 0.1), hsv = TRUE, 
                                                              title = paste0(lastMajorBtnClicked$lastMajorImg,
                                                                             "-",
                                                                             ifelse(is.na(SecGroupName$name), "", paste0(SecGroupName$name4title, "-")),
                                                                             input$majorSectionName,
                                                                             "\n",
                                                                             "Color distribution histogram"))
      
      majorImgSecDistHist$plot
      
    })
    
    #********#
    
    ## Circle plot
    
    output$majorSecCirclePlot <- renderPlot({
      
      im <- magick::image_read(majorImgSecCropped$plot$original.rgb)
      
      im_colorPal <- get_colorPal(im, n = input$majorCirclePalN)
      
      im_colorPalCircle <-im_colorPal %>%  
        mutate(ypos=row_number(hue)) %>%  ## alter stacking order
        ggplot(aes(x=colorspace, y=ypos, fill=hex)) +
        geom_tile() +
        scale_fill_identity() +
        scale_y_continuous(breaks=NULL) +
        ggtitle(paste0(lastMajorBtnClicked$lastMajorImg,
                       "-",
                       ifelse(is.na(SecGroupName$name), "", paste0(SecGroupName$name4title, "-")),
                       input$majorSectionName,
                       "\n",
                       "Circle Color Palette: ", nrow(im_colorPal), " Distinct Colors")) +
        theme_void() +
        theme(plot.title = element_text(face = "bold", hjust = 0.5))+
        coord_polar() +
        expand_limits(y=-1)
      
      im_colorPalette <-im_colorPal %>%  
        mutate(ypos=row_number(hue)) %>%  ## alter stacking order
        ggplot(aes(x=colorspace, y=ypos, fill=hex)) +
        geom_tile() +
        geom_text(aes(label=hex, y=ypos, fontface = "bold"), color="#ffffffbe", 
                  size=4)+
        scale_fill_identity() +
        theme_void() +
        scale_y_continuous(breaks=NULL)
      
      if(input$majorCirclePalLegend) {
        
        majorImgSecCirclePlot$plot <- ggarrange(im_colorPalCircle, im_colorPalette,
                                                labels = NULL,
                                                ncol = 2, nrow = 1, widths = c(9,2))
        
      } else {
        majorImgSecCirclePlot$plot <- im_colorPalCircle
      }
      
      majorImgSecCirclePlot$plot
      
    })
    
    })
    
    #****************************************************************#
    
    # Take care of downloading the color scheme of the selected section
    
    ## Circle plot
    
    output$download_majorSecCircle_PDF <- downloadHandler(
      filename = paste0(lastMajorBtnClicked$lastMajorImg,
                        "-",
                        ifelse(is.na(SecGroupName$name), "", paste0(SecGroupName$name4title, "-")),
                        input$majorSectionName, " Circle plot (", Sys.Date(), ").pdf"),
      content = function(file) {
        majorImgSecCirclePlot$plot %>%
          ggsave(filename = file, device = "pdf",
                 width = input$majorSecCircle.figure.width, 
                 height = input$majorSecCircle.figure.height, units = "in")
      }
    )
    
    output$download_majorSecCircle_PNG <- downloadHandler(
      filename = paste0(lastMajorBtnClicked$lastMajorImg,
                        "-",
                        ifelse(is.na(SecGroupName$name), "", paste0(SecGroupName$name4title, "-")),
                        input$majorSectionName, " Circle plot (", Sys.Date(), ").png"),
      content = function(file) {
        majorImgSecCirclePlot$plot %>%
          ggsave(filename = file, device = "png",
                 width = input$majorSecCircle.figure.width, 
                 height = input$majorSecCircle.figure.height, units = "in", dpi = input$majorSecCircle.PNG.resolution)
      }
    )
    
    ####************####
    
    ## Pixel plot
    
    output$download_majorSecPixel <- renderUI({
      
      downloadScreenShotBtn(ui = '#majorSecPixelPlot',
                            label = 'Download Pixel Plot', 
                            filename = paste0(lastMajorBtnClicked$lastMajorImg,
                                              "-",
                                              ifelse(is.na(SecGroupName$name), "", paste0(SecGroupName$name4title, "-")),
                                              input$majorSectionName, " Pixel plot (", Sys.Date(), ")", ".png"))
    })
    
    ####************####
    
    ## Color distribution histogram
    
    output$download_majorSecDistHist <- renderUI({
      
      downloadScreenShotBtn(ui = '#majorSecDistHist',
                            label = 'Download Color Distribution Histogram', 
                            filename = paste0(lastMajorBtnClicked$lastMajorImg,
                                              "-",
                                              ifelse(is.na(SecGroupName$name), "", paste0(SecGroupName$name4title, "-")),
                                              input$majorSectionName, " Color distribution histogram (", Sys.Date(), ")", ".png"))
    })

    #****************************************************************#
    
    # Prepare the dataframe of sections
    majorImgSections <- reactiveValues(df = data.frame(ImgName = character(), ImgPath = character(),
                                                       GroupName = character(), SectionName = character(),
                                                       Group_SectionName = character(), 
                                                       Group_Img_SectionName = character(), 
                                                       BtnID = character(), GroupID = character(), 
                                                       ID = integer(),
                                                       Mean = numeric(),
                                                       SD = numeric()))
    
    
    observeEvent(input$approveMajorSection, {
      
      tmpDF <- data.frame(
        ImgName = lastMajorBtnClicked$lastMajorImg,
        
        ImgPath = lastMajorBtnClicked$lastMajorImgPath,
        
        GroupName = SecGroupName$name,
        
        SectionName = input$majorSectionName,
        
        Group_SectionName = paste0(SecGroupName$name4title, ifelse(is.na(SecGroupName$name), "","-"), input$majorSectionName),
        
        Group_Img_SectionName = paste0(SecGroupName$name4title, ifelse(is.na(SecGroupName$name), "","-"), lastMajorBtnClicked$lastMajorImg, "-", input$majorSectionName),
        
        BtnID = paste0("majorImageBtn", substr(x = lastMajorBtnClicked$lastBtn, start = nchar(lastMajorBtnClicked$lastBtn), stop = nchar(lastMajorBtnClicked$lastBtn))),
        
        GroupID = paste0("groupPicker", substr(x = lastMajorBtnClicked$lastBtn, start = nchar(lastMajorBtnClicked$lastBtn), stop = nchar(lastMajorBtnClicked$lastBtn))),
        
        ID = (nrow(majorImgSections$df) + 1),
        
        Mean = 0,
        
        SD = 0
      )
      
      majorImgSections$df <- rbind(majorImgSections$df, tmpDF)
      
      # Add the section plot to the list of sections
      majorImgSecCroppedList$section[[nrow(majorImgSections$df)]] <- majorImgSecCropped$plot
      
    })
    
    ####************####
    
    # Calculate the clonality of sections
    
    autoClonizeRes <- reactiveValues(result = list())
    
    ## disable the button by default
    shinyjs::disable(id = "autoclonizeBtn")
    
    ## enable the button after approving the first section
    observeEvent(input$approveMajorSection, {
      
      shinyjs::enable(id = "autoclonizeBtn")
    })
    
    # Ask if the user is done with defining the sections
    observeEvent(input$autoclonizeBtn, {
      
      shinyjs::hide(id = "majorSecPlotsRow")
      
      ask_confirmation(
        inputId = "AutoClonizeConfirmation",
        title = "Have you defined all of your desired sections?",
        text = p(
          icon("calculator"),
          " Do you want to proceed to Autoclonization?",
          style = "color: black;"),
        btn_labels = c("No", "Sure!"),
        btn_colors = c("#FE2E2E", "#00BFFF"),
        html = TRUE
      )
    })
    
    # Perform the Autoclonization
    
    ## Define a reactive value to check if the autoclonization is done
    autoClonized <- reactiveValues(job = 0, BtnClicked = 0)
    
    observeEvent(input$AutoClonizeConfirmation, {

      if(input$AutoClonizeConfirmation == TRUE) {
        
        if(autoClonized$BtnClicked == 0) {
          
          ## Toggle the first three steps
          bs4Dash::updateBox(id = "input_major_image", action = "toggle")
          bs4Dash::updateBox(id = "major_image_visualization", action = "toggle")
          bs4Dash::updateBox(id = "define_colonies", action = "toggle")
          
        }
        
        progressSweetAlert(
          session = session, id = "majorImg_autoClonizeRes_progress",
          title = "Autoclonization is in progress ...",
          striped = TRUE, 
          display_pct = TRUE, value = 20
        )

        lapply(X = 1:nrow(majorImgSections$df),
               FUN = function(i) {

                 # Get the RGB values of the section
                 section.rgb <- majorImgSecCroppedList$section[[i]]$filtered.rgb.2d * 255

                 # Convert RGB values to HSL
                 section.hsl <- as.data.frame(t(rgb2hsl(t(section.rgb))))

                 # Convert HSL values to X,Y coordinates
                 section.coordinates <- coordinate_calc(HSL = section.hsl)
                 
                 # Update the progress bar
                     
                 if(input$majorImg_colorSpaceMode == 3) {
                   
                   autoClonizeRes$result[[i]] <- dist_calculate3D(section.coordinates)
                 } else {
                   
                   autoClonizeRes$result[[i]] <- dist_calculate(section.coordinates)
                 }

                     ## Add Mean and SD of the section to dataframe
                     majorImgSections$df[i,"Mean"] <- as.numeric(autoClonizeRes$result[[i]]$Mean)
                     majorImgSections$df[i,"SD"] <- as.numeric(autoClonizeRes$result[[i]]$SD)
                     
                     autoClonized$job <- i + 1
                     
                     
                     # Update the progress bar
                     updateProgressBar(
                       session = session,
                       title = paste0("Section ", i, " is now processed ..."),
                       id = "majorImg_autoClonizeRes_progress",
                       value = i*(100/nrow(majorImgSections$df))
                     )
                     
                   })
                   
        autoClonized$BtnClicked <- autoClonized$BtnClicked + 1
        
        closeSweetAlert(session = session)
        sendSweetAlert(
          session = session,
          title =" Autoclonization completed!",
          type = "success"
        )

      }

    })
    
    ####************####
    
    # Plot the autoclonized results
    
    ## Hide the plot title text box by default and show if requested
    observe({
      if(input$secDistsPlotTitleOption == TRUE) {
        shinyjs::show("secDistsPlotTitle")
      } else {
        shinyjs::hide("secDistsPlotTitle")
      }
    })
    
    ## Open other boxes of results
    observeEvent(input$autoclonizeBtn, {
      
      if(autoClonized$BtnClicked == 0) {
      bs4Dash::updateBox(id = "majorImage_DistanceStatistics", action = "toggle")
      }
    })
    
    observe({
      
      if(autoClonized$job == nrow(majorImgSections$df) + 1) {

        ## Define the X axis
        if(input$secDistsPlotImageNames == TRUE &
           input$secDistsPlotGroupNames == TRUE) {
          secDistsPlotXaxis <- paste0(majorImgSections$df$ImgName, "-", as.character(majorImgSections$df$Group_SectionName))
          
        } else if(input$secDistsPlotImageNames == TRUE &
                  input$secDistsPlotGroupNames == FALSE) {
          secDistsPlotXaxis <- paste0(majorImgSections$df$ImgName, "-", as.character(majorImgSections$df$SectionName))
          
        } else if(input$secDistsPlotImageNames == FALSE &
                  input$secDistsPlotGroupNames == TRUE) {
          secDistsPlotXaxis <- as.character(majorImgSections$df$Group_SectionName)
          
        } else {secDistsPlotXaxis <- as.character(majorImgSections$df$SectionName)}
        
        ###################
        # Generate plot
        
        secDistsPlot <-  
          majorImgSections$df %>% 
          ggplot(
            aes(x = secDistsPlotXaxis, y = Mean, fill = secDistsPlotXaxis)) + 
          ylab("Mean Distance") + xlab(NULL) +
          scale_fill_manual(name = NULL,
                            values = fish(n = length(unique(secDistsPlotXaxis)), 
                                          option = input$secDistsPlotPalette)) +
          theme_bw() + theme_classic() +
          theme(legend.box.background = element_rect(color="black", size=1))
        
        if(input$secDistsPlotTitleOption == TRUE) {
          secDistsPlot <- secDistsPlot + ggtitle(input$secDistsPlotTitle)
        }
        
        if(input$secDistsPlotErrorBars == TRUE) {
          secDistsPlot <- secDistsPlot + 
            geom_errorbar(aes(ymin=Mean - 1, ymax=Mean+abs(SD)), width=.2, color = "black",
                          position=position_dodge(.9))
        }
        
        secDistsPlot <- secDistsPlot +
          geom_bar(stat = "identity", width = 0.5, show.legend = FALSE) + 
          coord_flip()
        
        ###################
        ## Render the plot
        output$majorSecClonalityPlot <- renderPlot({
          
          secDistsPlot
          
        })
        
        ###################
        
        shinyjs::show(id = "autoclonizeResRow")
        
        # Taking care of downloading autoclonization plot
        
        output$download_majorSecClonality_PDF <- downloadHandler(
          filename = paste0("AutoClone-Distance Plot (", Sys.Date(), ").pdf"),
          content = function(file) {
            secDistsPlot %>%
              ggsave(filename = file, device = "pdf",
                     width = input$majorSecClonality.figure.width, 
                     height = input$majorSecClonality.figure.height, units = "in")
          }
        )
        
        output$download_majorSecClonality_PNG <- downloadHandler(
          filename = paste0("AutoClone-Distance Plot (", Sys.Date(), ").png"),
          content = function(file) {
            secDistsPlot %>%
              ggsave(filename = file, device = "png",
                     width = input$majorSecClonality.figure.width, 
                     height = input$majorSecClonality.figure.height, units = "in", 
                     dpi = input$majorSecClonality.PNG.resolution)
          }
        )
        
        ###################
        
        # Generate table of autoclonization
        
        majorImgSections4Table <- majorImgSections$df[,c(1,5, 10,11)]
        colnames(majorImgSections4Table) <- c("Image", "Section-Group", "Mean_Distance", "SD")
        
        brks_drivers <- quantile(majorImgSections4Table$Mean_Distance, probs = seq(.05, .95, .05), na.rm = TRUE)
        clrs_drivers <- rev(round(seq(200, 40, length.out = length(brks_drivers) + 1), 0) %>%
                              {paste0("rgb(205,", ., ",", ., ")")})

        output$majorSecClonalityTable <- DT::renderDataTable(server = FALSE,

              DT::datatable(
                { majorImgSections4Table },
                
                extensions = c('Buttons', 'SearchPanes', 'FixedHeader'),
                
                options = list(
                  columnDefs=list(list(targets=1:4, class="dt-left")),
                  paging = TRUE,
                  searching = TRUE,
                  pageLength = 5,
                  fixedColumns = TRUE,
                  scrollX = TRUE,
                  autoWidth = TRUE,
                  ordering = TRUE,
                  dom = 'Bfrtip',
                  buttons = list(list(extend = 'csv', filename= paste0("AutoClone-Distance Table ", "(", Sys.Date(), ")")),
                                 list(extend = 'excel', filename= paste0("AutoClone-Distance Table ", "(", Sys.Date(), ")")),
                                 list(extend = 'print', filename= paste0("AutoClone-Distance Table ", "(", Sys.Date(), ")"))
                  )
                ),
                
                class = "display"
              ) %>%
                formatStyle('Mean_Distance', fontWeight = styleInterval(1, c('bold', 'bold'))) %>%
                formatStyle('Mean_Distance', backgroundColor = styleInterval(brks_drivers, clrs_drivers))
          )
        
      }
      
    })
    
    #****************************************************************#
    
    # Visualize the Distribution statistics as a RainCloud plot
    
    ## Hide the plot title text box by default and show if requested
    observe({
      if(input$secDistsStatsPlotTitleOption == TRUE) {
        shinyjs::show("secDistsStatsPlotTitle")
      } else {
        shinyjs::hide("secDistsStatsPlotTitle")
      }
    })
    
    # Define a reactiveValue for storing the autoclonization results
    majorSecStatsDF4Plotting <- reactiveValues(df = NULL)
    
    observe({
      
      if(autoClonized$job == nrow(majorImgSections$df) + 1) {
        
        ###################
        # Generate plot
        
        ### Prepare the table for plotting
        majorSecStatsDF4Plotting$df <- do.call(rbind, 
                                            lapply(X = 1:nrow(majorImgSections$df), 
                                                   FUN = function(i) {
                                                     data.frame(Distance = autoClonizeRes$result[[i]]$Table,
                                                                ImgName = majorImgSections$df[i,"ImgName"],
                                                                GroupName = majorImgSections$df[i,"GroupName"],
                                                                SectionName = majorImgSections$df[i,"SectionName"],
                                                                Group_SectionName = majorImgSections$df[i,"Group_SectionName"],
                                                                Group_Img_SectionName = majorImgSections$df[i,"Group_Img_SectionName"])
                                                   }))
        
        ## Define the X axis
        if(input$secDistsStatsPlotImageNames == TRUE &
           input$secDistsStatsPlotGroupNames == TRUE) {
          secDistsStatsPlotXaxis <- paste0(majorSecStatsDF4Plotting$df$ImgName, "-", as.character(majorSecStatsDF4Plotting$df$Group_SectionName))
          
        } else if(input$secDistsStatsPlotImageNames == TRUE &
                  input$secDistsStatsPlotGroupNames == FALSE) {
          secDistsStatsPlotXaxis <- paste0(majorSecStatsDF4Plotting$df$ImgName, "-", as.character(majorSecStatsDF4Plotting$df$SectionName))
          
        } else if(input$secDistsStatsPlotImageNames == FALSE &
                  input$secDistsStatsPlotGroupNames == TRUE) {
          secDistsStatsPlotXaxis <- as.character(majorSecStatsDF4Plotting$df$Group_SectionName)
          
        } else {secDistsStatsPlotXaxis <- as.character(majorSecStatsDF4Plotting$df$SectionName)}
        
        ###################
        # Generate the plot
        majorSecStatsPlot <- 
          
          ggplot(data = majorSecStatsDF4Plotting$df, 
                            aes(y = Distance, 
                                x = secDistsStatsPlotXaxis, 
                                fill = secDistsStatsPlotXaxis)) +
          
          geom_flat_violin(position = position_nudge(x = .2, y = 0)) +
          
          geom_boxplot(width = .1, guides = FALSE, outlier.shape = NA) +
          
          coord_flip() +
          
          guides(fill = FALSE) +
          
          
          scale_fill_manual(values = fish(n = length(unique(secDistsStatsPlotXaxis)), 
                                          option = input$secDistsStatsPlotPalette)) +
          
          theme_bw() + theme_classic() +
          
          xlab(NULL) +
          ylab("Distance")
        
        if(input$majorImgSecsFacet != 1) {
          majorSecStatsPlot <- majorSecStatsPlot + 
            facet_grid( ~ get(input$majorImgSecsFacet))
        } 
        
        if(input$secDistsStatsPlotTitleOption == TRUE) {
          majorSecStatsPlot <- majorSecStatsPlot + ggtitle(input$secDistsStatsPlotTitle)
        }
        
        ###################
        ## Render the plot
        output$majorSecClonalityStatsPlot <- renderPlot({
          
          majorSecStatsPlot
          
        })
        
        ###################
      
        shinyjs::show(id = "majorImageDistStatsRow")
        
        # Taking care of downloading sec stats plot
        
        output$download_majorSecStatsClonality_PDF <- downloadHandler(
          filename = paste0("AutoClone-Distance RainCloud Plot (", Sys.Date(), ").pdf"),
          content = function(file) {
            majorSecStatsPlot %>%
              ggsave(filename = file, device = "pdf",
                     width = input$majorSecStatsClonality.figure.width, 
                     height = input$majorSecStatsClonality.figure.height, units = "in")
          }
        )
        
        output$download_majorSecStatsClonality_PNG <- downloadHandler(
          filename = paste0("AutoClone-Distance RainCloud Plot (", Sys.Date(), ").png"),
          content = function(file) {
            majorSecStatsPlot %>%
              ggsave(filename = file, device = "png",
                     width = input$majorSecStatsClonality.figure.width, 
                     height = input$majorSecStatsClonality.figure.height, units = "in", 
                     dpi = input$majorSecStatsClonality.PNG.resolution)
          }
        )
        
      }
        
      })
    
    #****************************************************************#
    
    # Visualize the clustering of ColorDistanceMatrices (CMDs)
    
    ## Hide the plot title text box by default and show if requested
    observe({
      if(input$majorSecsDistsClusterTitleOption == TRUE) {
        shinyjs::show("majorSecsDistsClusterTitle")
      } else {
        shinyjs::hide("majorSecsDistsClusterTitle")
        shinyjs::reset("majorSecsDistsClusterTitle")
      }
    })
    
    observe({
      
      if(autoClonized$job == nrow(majorImgSections$df) + 1 & length(majorImgSecsHistList$Hist) < 3) {
        
        ## Show the clustering notif row
        shinyjs::show(id = "majorImage_ClustNotifRow", anim = TRUE, animType = "slide")
        shinyjs::hide(id = "majorImage_ClustRow", anim = TRUE, animType = "slide")
        
      } else if(autoClonized$job == nrow(majorImgSections$df) + 1 & length(majorImgSecsHistList$Hist) > 2) {
        
        ## Show the clustering row
        shinyjs::show(id = "majorImage_ClustRow", anim = TRUE, animType = "slide")
        shinyjs::hide(id = "majorImage_ClustNotifRow", anim = TRUE, animType = "slide")

        ## Prepare the table
        majorImgSecs_CDM <- colordistance::getColorDistanceMatrix(majorImgSecsHistList$Hist,
                                                     method="emd", 
                                                     plotting=FALSE)
        
        ## Visualize the plot
        output$majorSecsDistsClusterPlot <- renderPlot({
          
          majorSecsDistsHierClustering <- 
          colordistance::heatmapColorDistance(majorImgSecs_CDM, 
                                              main = input$majorSecsDistsClusterTitle, 
                                              dendrogram="column",
                                              col = colorRampPalette(fish(n = 3, option = input$majorSecsDistsClusterPalette))(299),
                                              cexRow = 0.8,
                                              cexCol = 0.8,
                                              key.par=list(mgp=c(1.5, 0.5, 0),
                                                           mar=c(3, 1, 2, 0)),
                                              # labRow = NA,
                                              margins = c(7, 7))
          
          majorSecsDistsHierClustering
          
        })

      }

    })
    
    #****************************************************************#
    
    # ANOVA for major sections
    
    majorSecDistsDF4ANOVA <- reactiveValues(df = NULL)

    observe({
      
      if(autoClonized$job == nrow(majorImgSections$df) + 1 & length(majorImgSecsHistList$Hist) == 1) {
        
        ## Show the ANOVA notif row
        shinyjs::show(id = "majorImage_ANOVANotifRow", anim = TRUE, animType = "slide")
        
      } else if(autoClonized$job == nrow(majorImgSections$df) + 1 & length(majorImgSecsHistList$Hist) > 1) {
        
        ### Prepare the table for ANOVA
        majorSecDistsDF4ANOVA$df <- do.call(rbind, 
                                               lapply(X = 1:nrow(majorImgSections$df), 
                                                      FUN = function(i) {
                                                        data.frame(Distance = autoClonizeRes$result[[i]]$Table,
                                                                   ImgName = majorImgSections$df[i,"ImgName"],
                                                                   GroupName = majorImgSections$df[i,"GroupName"],
                                                                   SectionName = majorImgSections$df[i,"SectionName"],
                                                                   Group_SectionName = majorImgSections$df[i,"Group_SectionName"],
                                                                   Group_Img_SectionName = majorImgSections$df[i,"Group_Img_SectionName"])
                                                      }))
        
        ## Show the ANOVA row
        shinyjs::show(id = "majorImage_ANOVARow", anim = TRUE, animType = "slide")

        ################

        ## Perform the ANOVA
        majorSecsDistsANOVA <- aov(Distance ~ get(input$majorImgDistsANOVA_variable), 
                                   data = majorSecDistsDF4ANOVA$df)

        ### Perform pairwise comparison for adjusting p-values
        majorSecsDistsANOVA <- as.data.frame(TukeyHSD(majorSecsDistsANOVA)[[1]])
        
        colnames(majorSecsDistsANOVA) <- c("Difference of Means", "Lower 95% CI",
                                               "Upper 95% CI", "Padj")
        
        ################
        
        brks_drivers <- quantile(majorSecsDistsANOVA$Padj, probs = seq(.05, .95, .05), na.rm = TRUE)
        clrs_drivers <- rev(round(seq(200, 40, length.out = length(brks_drivers) + 1), 0) %>%
                              {paste0("rgb(205,", ., ",", ., ")")})

        output$majorSecsDistsANOVATable <- DT::renderDataTable(server = FALSE,

          DT::datatable(
            { majorSecsDistsANOVA },

            extensions = c('Buttons', 'SearchPanes', 'FixedHeader'),

            options = list(
              columnDefs=list(list(targets=1:4, class="dt-left")),
              paging = TRUE,
              searching = TRUE,
              pageLength = 5,
              fixedColumns = TRUE,
              scrollX = TRUE,
              autoWidth = TRUE,
              ordering = TRUE,
              dom = 'Bfrtip',
              buttons = list(list(extend = 'csv', filename= paste0("Clonality ANOVA Table ", "(", Sys.Date(), ")")),
                             list(extend = 'excel', filename= paste0("Clonality ANOVA Table ", "(", Sys.Date(), ")")),
                             list(extend = 'print', filename= paste0("Clonality ANOVA Table ", "(", Sys.Date(), ")"))
              )
            ),

            class = "display"
          ) %>%
            formatStyle('Padj', fontWeight = styleInterval(1, c('bold', 'bold'))) %>%
            formatStyle('Padj', backgroundColor = styleInterval(brks_drivers, clrs_drivers)) %>%
            formatRound(columns=c(1:4), digits=2)
        )

      }
      
    })
    
    #########################################################
    #########################################################
    #########################################################
    #########################################################
    
    # Server side of the pre-cropped-based tab
    
    # Create placeholders for new groups
    
    # Instead we will use an inputSweetalert
    observeEvent(input$minor_add_group, {
      inputSweetAlert(
        session = session,
        inputId = "minorGroupName",
        title = "Type in your desired group name",
        text = "You can assign each of the images you will upload/have uploaded to this group.",
        type = "info",
        input = "text",
        inputOptions = NULL,
        inputPlaceholder = "Group name",
        btn_labels = "OK",
        btn_colors = NULL,
        reset_input = TRUE
      )
    })
    
    ###*********************###
    
    # Generate buttons for imported images
    
    observeEvent(input$minor_image,{
      shinyjs::show(id = "minorImage_header")
      
      # reset the results
      minorImgSections$df <- data.frame(ImgName = character(), 
                                        ImgPath = character(),
                                        GroupName = character(), 
                                        SectionName = character(),
                                        Group_SectionName = character(), 
                                        Group_Img_SectionName = character(), 
                                        BtnID = character(), 
                                        GroupID = character(), 
                                        ID = integer(),
                                        Mean = numeric(),
                                        SD = numeric())
      
      minor_autoClonizeRes$result <- list()
      minorImgSecCroppedList$section <- list()
      minorImgSecsHistList$Hist <- list()
    })
    
    observeEvent(input$minor_image, {
      output$minor_image_buttons <- renderUI({
        image_filenames <- input$minor_image[['name']]
        image_filepaths <- input$minor_image[['datapath']]
        
        lapply(
          1:length(image_filenames), 
          function(i) {
            shinyFeedback::loadingButton(inputId = paste0("minorImageBtn", i), 
                                         label = gsub(pattern = paste0(".", tools::file_ext(image_filepaths[[i]])), 
                                                      replacement = "", x = image_filenames[[i]]),
                                         loadingLabel = gsub(pattern = paste0(".", tools::file_ext(image_filepaths[[i]])), 
                                                             replacement = "", x = image_filenames[[i]]),
                                         class = "btn-sm btn-info",
                                         style = "width: 100%; margin-top:6px; margin-bottom:6px; padding:3px")
          }
        )
      })
    })
    
    ###*********************###
    
    # Generate pickerinputs for imported groups
    
    # Clear Groups
    observeEvent(input$minor_clearGrpsBtn, {
      minorGroupNames$groups <- NULL
    })
    
    observe({
      if(!is.null(minorGroupNames$groups) & !is.null(input$minor_image)) {
        shinyjs::show(id = "minorGroup_header")
        shinyjs::show(id = "minor_clearGrpsRow")
      }
    })
    
    observeEvent(input$minor_add_group,{
      shinyjs::show(id = "minor_groupPicker_column")
    })
    
    
    # Define minor group names as a reactive value
    minorGroupNames <- reactiveValues(groups = NULL)
    observeEvent(input$minorGroupName, {
      minorGroupNames$groups <- c(minorGroupNames$groups, input$minorGroupName)
    })
    
    
    observeEvent(input$minor_image, {
      output$minor_image_groups <- renderUI({
        image_filenames <- input$minor_image[['name']]
        
        lapply(
          1:length(image_filenames), 
          function(i) {
            pickerInput(
              inputId = paste0("minor_groupPicker", i),
              label = NULL, width = "fit",
              choices = minorGroupNames$groups,
              options = list(
                title = "Assign to a group",
                style = "info btn-xs",
                width = 'fit')
            )
          }
        )
      })
    })
    
    #####****************************************************************#####
    
    # Visualize the image
    
    #****************#
    # First determine which image button was clicked
    
    lastMinorBtnClicked <- reactiveValues(lastBtn = character(),
                                          lastMinorImg = character(),
                                          lastMinorImgPath = character())
    
    observe({
      if(!is.null(input$minor_image))
        lapply(
          X = 1:length(input$minor_image[['name']]),
          FUN = function(i){
            observeEvent(input[[paste0("minorImageBtn", i)]], {
              
              if (input[[paste0("minorImageBtn", i)]] > 0) {
                
                lastMinorBtnClicked$lastBtn = paste0("minorImageBtn", i)
                
                lastMinorBtnClicked$lastMinorImg = gsub(pattern = paste0(".", tools::file_ext(input$minor_image[['datapath']][[i]])), 
                                                        replacement = "", x = input$minor_image[['name']][[i]])
                
                lastMinorBtnClicked$lastMinorImgPath = input$minor_image[['datapath']][i]
                
              }
              
              # Show the plot row
              shinyjs::show(id = "minorPlotRow")
              
              # Reset all feedback buttons on selecting another
              lapply(X = c(1:length(input$minor_image[['name']]))[-i],
                     FUN = function(x) {
                       shinyFeedback::resetLoadingButton(inputId = paste0("minorImageBtn", x))
                       
                     }
              )
              
              # Hide the section plot row when clicking on a new image
              shinyjs::hide(id = "minorSecPlotsRow")
              
            })
          }
        )
    })
    
    #****************#
    
    # Read in the image
    minor_image_file <- reactive({
      
      ext <- tools::file_ext(lastMinorBtnClicked$lastMinorImgPath)
      df = switch(ext,
                  png = colordistance::loadImage(path = lastMinorBtnClicked$lastMinorImgPath,
                                                 lower = c(0, 0, 0), upper = c(0.1, 0.1, 0.1),
                                                 hsv = TRUE, alpha.channel = FALSE),
                  jpg = colordistance::loadImage(path = lastMinorBtnClicked$lastMinorImgPath,
                                                 lower = c(0, 0, 0), upper = c(0.1, 0.1, 0.1),
                                                 hsv = TRUE, alpha.channel = FALSE),
                  jpeg = colordistance::loadImage(path = lastMinorBtnClicked$lastMinorImgPath,
                                                  lower = c(0, 0, 0), upper = c(0.1, 0.1, 0.1),
                                                  hsv = TRUE, alpha.channel = FALSE),
                  tif =  tiff::readTIFF(source = lastMinorBtnClicked$lastMinorImgPath),
                  tiff = tiff::readTIFF(source = lastMinorBtnClicked$lastMinorImgPath),
                  validate("Invalid file; Please upload a .tif, .tiff, .png, .jpg, or .jpeg file")
      )
      
      if(ext == "tif" | ext == "tiff") {
        ## Create a user specific directory
        dir.create(session$token)
        
        ## Saving section to the directory
        png::writePNG(image = df,
                      target = paste0(session$token, "/MinorImg.png"), dpi = 600)
        
        df <- colordistance::loadImage(path = paste0(session$token, "/MinorImg.png"),
                                       lower = c(0, 0, 0), upper = c(0.1, 0.1, 0.1),
                                       hsv = TRUE, alpha.channel = FALSE)
        
        ## Remove the image from directory
        unlink(session$token, recursive = TRUE)
      }
      
      df
      
    })
    
    #****************#
    
    # Save image RGB into a reactive obj
    minor_image_rgb <- reactive({
      
      minor_image_file()$original.rgb
      
    })
    
    #****************#
    
    # Generate the plot file
    minor_image <- reactiveValues(plotFile = NULL)
    observe({
      if(length(lastMinorBtnClicked$lastBtn)> 0) {
        
        minor_image$plotFile <- 
          ggplot() +
          annotation_custom(rasterGrob(minor_image_rgb(),
                                       width = unit(1,"npc"),
                                       height = unit(1,"npc")),
                            -Inf, Inf, -Inf, Inf) +
          theme_void()
      }
    })
    
    #****************#
    
    # render the plot
    output$uploaded_minor_image <- renderPlot({
      
      minor_image$plotFile
      
    })
    
    #****************#
    
    # Set the name of the section
    
    # Determine the group name associated with the section
    minor_SecGroupName <- reactiveValues(name = NULL, name4title = NULL)
    
    observeEvent(input$approveMinorSection, {
      
      ## Determine the group name associated with the section
      if(is.null(input[[paste0("minor_groupPicker", substr(x = lastMinorBtnClicked$lastBtn, start = nchar(lastMinorBtnClicked$lastBtn), stop = nchar(lastMinorBtnClicked$lastBtn)))]]) ||
         input[[paste0("minor_groupPicker", substr(x = lastMinorBtnClicked$lastBtn, start = nchar(lastMinorBtnClicked$lastBtn), stop = nchar(lastMinorBtnClicked$lastBtn)))]] == "") {
        minor_SecGroupName$name = NA
        minor_SecGroupName$name4title = ""
      } else {
        minor_SecGroupName$name = input[[paste0("minor_groupPicker", substr(x = lastMinorBtnClicked$lastBtn, start = nchar(lastMinorBtnClicked$lastBtn), stop = nchar(lastMinorBtnClicked$lastBtn)))]]
        minor_SecGroupName$name4title = paste0(input[[paste0("minor_groupPicker", substr(x = lastMinorBtnClicked$lastBtn, start = nchar(lastMinorBtnClicked$lastBtn), stop = nchar(lastMinorBtnClicked$lastBtn)))]])
      }
      
    })
    
    #****************#
    
    # Check if new section is created before clicking on the approve section 
    
    approveBtnClicked <- reactiveValues(number = 0)
    
    observeEvent(input$approveMinorSection, {
      approveBtnClicked$number = approveBtnClicked$number + 1
    })
    
    # Set the approveBtnClicked$number to make the approve button only available one time for each image
    observe({
      if(!is.null(input$minor_image))
        lapply(
          X = 1:length(input$minor_image[['name']]),
          FUN = function(i){
            observeEvent(input[[paste0("minorImageBtn", i)]], {
              
              approveBtnClicked$number <<- input[[paste0("minorImageBtn", i)]]
              
            })
          })
    })
    
    #****************#
    
    # Enable the Approve section buttons
    
    observe({
      if(approveBtnClicked$number == 1) {
        shinyjs::enable(id = "approveMinorSection")
      } else {
        shinyjs::disable(id = "approveMinorSection")
      }
    })
    
    #****************#
    
    # Approve section notif
    observeEvent(input$approveMinorSection, {
      
      showToast(type = "success", message = NULL,
                title = "The section is now approved!", 
                .options = myToastOptions
      )
    })
    
    #****************************************************************#
    
    # Crop the section
    minorImgSecCropped <- reactiveValues(plot = NULL)
    minorImgSecCroppedList <- reactiveValues(section = list())
    minorImgSecPixelPlot <- reactiveValues(plot = NULL)
    minorImgSecCirclePlot <- reactiveValues(plot = NULL)
    minorImgSecDistHist <- reactiveValues(plot = NULL)
    minorImgSecsHistList <- reactiveValues(Hist = list())
    
    observeEvent(input$approveMinorSection, {
      
      minorImgSecCropped$plot <- minor_image_file()
      
      # Generate the Section histogram plot
      minorImgSecDistHist$plot <- colordistance::getImageHist(image = minorImgSecCropped$plot,
                                                              lower = c(0, 0, 0),
                                                              upper = c(0.1, 0.1, 0.1), hsv = TRUE, 
                                                              title = paste0(lastMinorBtnClicked$lastMinorImg,
                                                                             ifelse(is.na(minor_SecGroupName$name), "", paste0("-", minor_SecGroupName$name4title)),
                                                                             "\n",
                                                                             "Color distribution histogram"))
      
      # Add the section histogram to the list for later color distance assessment
      minorImgSecsHistList$Hist[[paste0(lastMinorBtnClicked$lastMinorImg, 
                                        ifelse(is.na(minor_SecGroupName$name), "", paste0("-", minor_SecGroupName$name4title)))]] <- minorImgSecDistHist$plot
      
    })
    
    #****************************************************************#
    
    # Create color scheme plots for the section
    
    ## Hide the section plot row when clicking on the load image
    observeEvent(input$minor_image, {
      shinyjs::hide(id = "minorSecPlotsRow")
    })
    
    #****************#
    
    observeEvent(input$approveMinorSection, {
      
      shinyjs::show(id = "minorSecPlotsRow")
      
      ## Pixel plot
      
      output$minorSecPixelPlot <- renderPlot({
        
        minorImgSecPixelPlot$plot <- colordistance::plotPixels(img = minorImgSecCropped$plot, n = input$minorPixelPlotN,
                                                               lower = c(0, 0, 0),
                                                               upper = c(0.1, 0.1, 0.1), 
                                                               main = paste0(lastMinorBtnClicked$lastMinorImg,
                                                                             ifelse(is.na(minor_SecGroupName$name), "", paste0("-", minor_SecGroupName$name4title)),
                                                                             "\n",
                                                                             "Pixel Plot: ", input$minorPixelPlotN," Points"))
        
        minorImgSecPixelPlot$plot
        
      })
      
      #********#
      
      ## Color distribution histogram
      
      output$minorSecDistHist <- renderPlot({
        
        # Generate the Section histogram plot
        minorImgSecDistHist$plot <- colordistance::getImageHist(image = minorImgSecCropped$plot,
                                                                lower = c(0, 0, 0),
                                                                upper = c(0.1, 0.1, 0.1), hsv = TRUE, 
                                                                title = paste0(lastMinorBtnClicked$lastMinorImg,
                                                                               ifelse(is.na(minor_SecGroupName$name), "", paste0("-", minor_SecGroupName$name4title)),
                                                                               "\n",
                                                                               "Color distribution histogram"))
        
        minorImgSecDistHist$plot
        
      })
      
      #********#
      
      ## Circle plot
      
      output$minorSecCirclePlot <- renderPlot({
        
        im <- magick::image_read(minorImgSecCropped$plot$original.rgb)
        
        im_colorPal <- get_colorPal(im, n = input$minorCirclePalN)
        
        im_colorPalCircle <-im_colorPal %>%  
          mutate(ypos=row_number(hue)) %>%  ## alter stacking order
          ggplot(aes(x=colorspace, y=ypos, fill=hex)) +
          geom_tile() +
          scale_fill_identity() +
          scale_y_continuous(breaks=NULL) +
          ggtitle(paste0(lastMinorBtnClicked$lastMinorImg,
                         ifelse(is.na(minor_SecGroupName$name), "", paste0("-", minor_SecGroupName$name4title)),
                         "\n",
                         "Circle Color Palette: ", nrow(im_colorPal), " Distinct Colors")) +
          theme_void() +
          theme(plot.title = element_text(face = "bold", hjust = 0.5))+
          coord_polar() +
          expand_limits(y=-1)
        
        im_colorPalette <-im_colorPal %>%  
          mutate(ypos=row_number(hue)) %>%  ## alter stacking order
          ggplot(aes(x=colorspace, y=ypos, fill=hex)) +
          geom_tile() +
          geom_text(aes(label=hex, y=ypos, fontface = "bold"), color="#ffffffbe", 
                    size=4)+
          scale_fill_identity() +
          theme_void() +
          scale_y_continuous(breaks=NULL)
        
        if(input$minorCirclePalLegend) {
          
          minorImgSecCirclePlot$plot <- ggarrange(im_colorPalCircle, im_colorPalette,
                                                  labels = NULL,
                                                  ncol = 2, nrow = 1, widths = c(9,2))
          
        } else {
          minorImgSecCirclePlot$plot <- im_colorPalCircle
        }
        
        minorImgSecCirclePlot$plot
        
      })
      
    })
    
    #****************************************************************#
    
    # Take care of downloading the color scheme of the selected section
    
    ## Circle plot
    
    output$download_minorSecCircle_PDF <- downloadHandler(
      filename = paste0(lastMinorBtnClicked$lastMinorImg,
                        ifelse(is.na(minor_SecGroupName$name), "", paste0("-", minor_SecGroupName$name4title)),
                        " Circle plot (", Sys.Date(), ").pdf"),
      content = function(file) {
        minorImgSecCirclePlot$plot %>%
          ggsave(filename = file, device = "pdf",
                 width = input$minorSecCircle.figure.width, 
                 height = input$minorSecCircle.figure.height, units = "in")
      }
    )
    
    output$download_minorSecCircle_PNG <- downloadHandler(
      filename = paste0(lastMinorBtnClicked$lastMinorImg,
                        ifelse(is.na(minor_SecGroupName$name), "", paste0("-", minor_SecGroupName$name4title)),
                        " Circle plot (", Sys.Date(), ").png"),
      content = function(file) {
        minorImgSecCirclePlot$plot %>%
          ggsave(filename = file, device = "png",
                 width = input$minorSecCircle.figure.width, 
                 height = input$minorSecCircle.figure.height, units = "in", dpi = input$minorSecCircle.PNG.resolution)
      }
    )
    
    ####************####
    
    ## Pixel plot
    
    output$download_minorSecPixel <- renderUI({
      
      downloadScreenShotBtn(ui = '#minorSecPixelPlot',
                            label = 'Download Pixel Plot', 
                            filename = paste0(lastMinorBtnClicked$lastMinorImg,
                                              ifelse(is.na(minor_SecGroupName$name), "", paste0("-", minor_SecGroupName$name4title)),
                                              " Pixel plot (", Sys.Date(), ")", ".png"))
    })
    
    ####************####
    
    ## Color distribution histogram
    
    output$download_minorSecDistHist <- renderUI({
      
      downloadScreenShotBtn(ui = '#minorSecDistHist',
                            label = 'Download Color Distribution Histogram', 
                            filename = paste0(lastMinorBtnClicked$lastMinorImg,
                                              ifelse(is.na(minor_SecGroupName$name), "", paste0("-", minor_SecGroupName$name4title)),
                                              " Color distribution histogram (", Sys.Date(), ")", ".png"))
    })
    
    #****************************************************************#
    
    # Prepare the dataframe of sections
    minorImgSections <- reactiveValues(df = data.frame(ImgName = character(), 
                                                       ImgPath = character(),
                                                       GroupName = character(), 
                                                       SectionName = character(),
                                                       Group_SectionName = character(), 
                                                       Group_Img_SectionName = character(), 
                                                       BtnID = character(), 
                                                       GroupID = character(), 
                                                       ID = integer(),
                                                       Mean = numeric(),
                                                       SD = numeric()))
    
    
    observeEvent(input$approveMinorSection, {
      
      tmpDF <- data.frame(
        ImgName = lastMinorBtnClicked$lastMinorImg,
        
        ImgPath = lastMinorBtnClicked$lastMinorImgPath,
        
        GroupName = minor_SecGroupName$name,
        
        SectionName = lastMinorBtnClicked$lastMinorImg,
        
        Group_SectionName = paste0(minor_SecGroupName$name4title, ifelse(is.na(minor_SecGroupName$name), "","-"), lastMinorBtnClicked$lastMinorImg),
        
        Group_Img_SectionName = paste0(minor_SecGroupName$name4title, ifelse(is.na(minor_SecGroupName$name), "","-"), lastMinorBtnClicked$lastMinorImg),
        
        BtnID = paste0("minorImageBtn", substr(x = lastMinorBtnClicked$lastBtn, start = nchar(lastMinorBtnClicked$lastBtn), stop = nchar(lastMinorBtnClicked$lastBtn))),
        
        GroupID = paste0("minor_groupPicker", substr(x = lastMinorBtnClicked$lastBtn, start = nchar(lastMinorBtnClicked$lastBtn), stop = nchar(lastMinorBtnClicked$lastBtn))),
        
        ID = (nrow(minorImgSections$df) + 1),
        
        Mean = 0,
        
        SD = 0
      )
      
      minorImgSections$df <- rbind(minorImgSections$df, tmpDF)
      
      # Add the section plot to the list of sections
      minorImgSecCroppedList$section[[nrow(minorImgSections$df)]] <- minorImgSecCropped$plot
      
    })
    
    ####************####
    
    # Calculate the clonality of sections
    
    minor_autoClonizeRes <- reactiveValues(result = list())
    
    ## disable the button by default
    shinyjs::disable(id = "minor_autoclonizeBtn")
    
    ## enable the button after approving the first section
    observeEvent(input$approveMinorSection, {
      
      shinyjs::enable(id = "minor_autoclonizeBtn")
    })
    
    # Ask if the user is done with defining the sections
    observeEvent(input$minor_autoclonizeBtn, {
      
      shinyjs::hide(id = "minorSecPlotsRow")
      
      ask_confirmation(
        inputId = "Minor_AutoClonizeConfirmation",
        title = "Have you approved all of your desired sections?",
        text = p(
          icon("calculator"),
          " Do you want to proceed to Autoclonization?",
          style = "color: black;"),
        btn_labels = c("No", "Sure!"),
        btn_colors = c("#FE2E2E", "#00BFFF"),
        html = TRUE
      )
    })
    
    # Perform the Autoclonization
    
    ## Define a reactive value to check if the autoclonization is done
    minor_autoClonized <- reactiveValues(job = 0, BtnClicked = 0)
    
    observeEvent(input$Minor_AutoClonizeConfirmation, {
      
      if(input$Minor_AutoClonizeConfirmation == TRUE) {
        
        if(minor_autoClonized$BtnClicked == 0) {
          
          ## Toggle the first three steps
          bs4Dash::updateBox(id = "input_minor_image", action = "toggle")
          bs4Dash::updateBox(id = "minor_image_visualization", action = "toggle")
          bs4Dash::updateBox(id = "minor_define_colonies", action = "toggle")
          
        }
        
        progressSweetAlert(
          session = session, id = "minorImg_autoClonizeRes_progress",
          striped = TRUE, 
          title = "Autoclonization is in progress ...",
          display_pct = TRUE, value = 20
        )
          
          lapply(X = 1:nrow(minorImgSections$df),
                 FUN = function(i) {
                   
                   # Get the RGB values of the section
                   section.rgb <- minorImgSecCroppedList$section[[i]]$filtered.rgb.2d * 255
                   
                   # Convert RGB values to HSL
                   section.hsl <- as.data.frame(t(rgb2hsl(t(section.rgb))))
                   
                   # Convert HSL values to X,Y coordinates
                   section.coordinates <- coordinate_calc(HSL = section.hsl)
                   
                   # Update the progress bar
                   
                   if(input$minorImg_colorSpaceMode == 3) {
                     
                     minor_autoClonizeRes$result[[i]] <- dist_calculate3D(section.coordinates)[c(2,3)]
                   } else {
                     
                     minor_autoClonizeRes$result[[i]] <- dist_calculate(section.coordinates)[c(2,3)]
                   }
                   
                   ## Add Mean and SD of the section to dataframe
                   minorImgSections$df[i,"Mean"] <- as.numeric(minor_autoClonizeRes$result[[i]]$Mean)
                   minorImgSections$df[i,"SD"] <- as.numeric(minor_autoClonizeRes$result[[i]]$SD)
                   
                   minor_autoClonized$job <- i + 1
                   
                   # Update the progress bar
                   updateProgressBar(
                     session = session,
                     title = paste0("Section ", i, " is now processed ..."),
                     id = "minorImg_autoClonizeRes_progress",
                     value = i*(100/nrow(minorImgSections$df))
                   )
                   
                 })
        
        minor_autoClonized$BtnClicked <- minor_autoClonized$BtnClicked + 1
        
        closeSweetAlert(session = session)
        sendSweetAlert(
          session = session,
          title =" Autoclonization completed!",
          type = "success"
        )
        
      }
      
    })
    
    ####************####
    
    # Plot the minor_autoClonized results and show table
    
    ## Hide the plot title text box by default and show if requested
    observe({
      if(input$minor_secDistsPlotTitleOption == TRUE) {
        shinyjs::show("minor_secDistsPlotTitle")
      } else {
        shinyjs::hide("minor_secDistsPlotTitle")
      }
    })
    
    ## Open other boxes of results
    observeEvent(input$minor_autoclonizeBtn, {
      
      if(minor_autoClonized$BtnClicked == 0) {
      bs4Dash::updateBox(id = "minorImage_ClonalityTableBox", action = "toggle")
      }
      
    })
    
    observe({
      
      if(minor_autoClonized$job == nrow(minorImgSections$df) + 1) {
        
        shinyjs::show(id = "minorImageDist_ClonalityTableRow")
        
        ## Define the X axis
        if(input$minor_secDistsPlotGroupNames == TRUE) {
          minor_secDistsPlotXaxis <- as.character(minorImgSections$df$Group_SectionName)
          
        } else {minor_secDistsPlotXaxis <- as.character(minorImgSections$df$SectionName)}
        
        ###################
        # Generate plot
        
        minor_secDistsPlot <- 
          minorImgSections$df %>% 
          ggplot(
            aes(x = minor_secDistsPlotXaxis, y = Mean, fill = minor_secDistsPlotXaxis)) + 
          ylab("Mean Distance") + xlab(NULL) +
          scale_fill_manual(name = NULL,
                            values = fish(n = length(unique(minor_secDistsPlotXaxis)), 
                                          option = input$minor_secDistsPlotPalette)) +
          theme_bw() + theme_classic() +
          theme(legend.box.background = element_rect(color="black", size=1))
        
        if(input$minor_secDistsPlotTitleOption == TRUE) {
          minor_secDistsPlot <- minor_secDistsPlot + ggtitle(input$minor_secDistsPlotTitle)
        }
        
        if(input$minor_secDistsPlotErrorBars == TRUE) {
          minor_secDistsPlot <- minor_secDistsPlot + 
            geom_errorbar(aes(ymin=Mean - 1, ymax=Mean+abs(SD)), width=.2, color = "black",
                          position=position_dodge(.9))
        }
        
        minor_secDistsPlot <- minor_secDistsPlot +
          geom_bar(stat = "identity", width = 0.5, show.legend = FALSE) + 
          coord_flip()
        
        ###################
        ## Render the plot
        output$minorSecClonalityPlot <- renderPlot({
          
          minor_secDistsPlot
          
        })
        
        ###################
        
        shinyjs::show(id = "minor_autoclonizeResRow")
        
        # Taking care of downloading autoclonization plot
        
        output$download_minorSecClonality_PDF <- downloadHandler(
          filename = paste0("AutoClone-Distance Plot (", Sys.Date(), ").pdf"),
          content = function(file) {
            minor_secDistsPlot %>%
              ggsave(filename = file, device = "pdf",
                     width = input$minorSecClonality.figure.width, 
                     height = input$minorSecClonality.figure.height, units = "in")
          }
        )
        
        output$download_minorSecClonality_PNG <- downloadHandler(
          filename = paste0("AutoClone-Distance Plot (", Sys.Date(), ").png"),
          content = function(file) {
            minor_secDistsPlot %>%
              ggsave(filename = file, device = "png",
                     width = input$minorSecClonality.figure.width, 
                     height = input$minorSecClonality.figure.height, units = "in", 
                     dpi = input$minorSecClonality.PNG.resolution)
          }
        )
        
        ###################
        
        # Generate table of autoclonization
        
        minorImgSections4Table <- minorImgSections$df[,c(1,5, 10,11)]
        colnames(minorImgSections4Table) <- c("Image", "Section-Group", "Mean_Distance", "SD")
        
        brks_drivers <- quantile(minorImgSections4Table$Mean_Distance, probs = seq(.05, .95, .05), na.rm = TRUE)
        clrs_drivers <- rev(round(seq(200, 40, length.out = length(brks_drivers) + 1), 0) %>%
                              {paste0("rgb(205,", ., ",", ., ")")})
        
        output$minorSecClonalityTable <- DT::renderDataTable(server = FALSE,
          
          DT::datatable(
            { minorImgSections4Table },
            
            extensions = c('Buttons', 'SearchPanes', 'FixedHeader'),
            
            options = list(
              columnDefs=list(list(targets=1:4, class="dt-left")),
              paging = TRUE,
              searching = TRUE,
              pageLength = 5,
              fixedColumns = TRUE,
              scrollX = TRUE,
              autoWidth = TRUE,
              ordering = TRUE,
              dom = 'Bfrtip',
              buttons = list(list(extend = 'csv', filename= paste0("AutoClone-Distance Table ", "(", Sys.Date(), ")")),
                             list(extend = 'excel', filename= paste0("AutoClone-Distance Table ", "(", Sys.Date(), ")")),
                             list(extend = 'print', filename= paste0("AutoClone-Distance Table ", "(", Sys.Date(), ")"))
              )
            ),
            
            class = "display"
          ) %>%
            formatStyle('Mean_Distance', fontWeight = styleInterval(1, c('bold', 'bold'))) %>%
            formatStyle('Mean_Distance', backgroundColor = styleInterval(brks_drivers, clrs_drivers))
        )
        
      }
      
    })
    
    #****************************************************************#
    
    # Visualize the clustering of ColorDistanceMatrices (CMDs)
    
    ## Hide the plot title text box by default and show if requested
    observe({
      if(input$minorSecsDistsClusterTitleOption == TRUE) {
        shinyjs::show("minorSecsDistsClusterTitle")
      } else {
        shinyjs::hide("minorSecsDistsClusterTitle")
        shinyjs::reset("minorSecsDistsClusterTitle")
      }
    })
    
    observe({
      
      if(minor_autoClonized$job == nrow(minorImgSections$df) + 1 & length(minorImgSecsHistList$Hist) < 3) {
        
        ## Show the clustering notif row
        shinyjs::show(id = "minorImage_ClustNotifRow", anim = TRUE, animType = "slide")
        shinyjs::hide(id = "minorImage_ClustRow", anim = TRUE, animType = "slide")
        
      } else if(minor_autoClonized$job == nrow(minorImgSections$df) + 1 & length(minorImgSecsHistList$Hist) > 2) {
        
        ## Show the clustering row
        shinyjs::show(id = "minorImage_ClustRow", anim = TRUE, animType = "slide")
        shinyjs::hide(id = "minorImage_ClustNotifRow", anim = TRUE, animType = "slide")
        
        ## Prepare the table
        minorImgSecs_CDM <- colordistance::getColorDistanceMatrix(minorImgSecsHistList$Hist,
                                                                  method="emd",
                                                                  plotting=FALSE)
        
        ## Visualize the plot
        output$minorSecsDistsClusterPlot <- renderPlot({
          
          minorSecsDistsHierClustering <-
            colordistance::heatmapColorDistance(minorImgSecs_CDM,
                                                main = input$minorSecsDistsClusterTitle,
                                                dendrogram="column",
                                                col = colorRampPalette(fish(n = 3, option = input$minorSecsDistsClusterPalette))(299),
                                                cexRow = 0.8,
                                                cexCol = 0.8,
                                                key.par=list(mgp=c(1.5, 0.5, 0),
                                                             mar=c(3, 1, 2, 0)),
                                                # labRow = NA,
                                                margins = c(7, 7))
          
          minorSecsDistsHierClustering
          
        })
        
      }
      
    })
    
    #########################################################
    #########################################################
    #########################################################
    #########################################################
    
    # Server side of the color/coord-based tab
    
    # Update the type on uploading a new file
    observeEvent(input$color_coord_file, {
      shinyWidgets::updateRadioGroupButtons(session = session, 
                                            inputId = "color_coord_type",
                                            disabled = FALSE,
                                            selected = FALSE)
      shinyjs::reset(id = "color_coord_pie_sectionName")
      shinyjs::reset(id = "columnImageOption")
      shinyjs::reset(id = "columnGroupOption")
      color_coord_processed$data <- NULL
      shinyjs::hide(id = "RGBvalunif_RcolumnName")
      shinyjs::hide(id = "RGBvalunif_GcolumnName")
      shinyjs::hide(id = "RGBvalunif_BcolumnName")
    })
    
    ## read in the input data
    color_coord_data <- eventReactive(input$color_coord_file, {

      if (is.null(input$color_coord_file))
        return(NULL)
      
      ext <- tools::file_ext(input$color_coord_file$name)
      tmp_data <- 
      switch(ext,
             csv = read.csv(input$color_coord_file$datapath, sep = ","),
             tsv = read.delim(input$color_coord_file$datapath, sep = "\t"),
             txt = read.delim(input$color_coord_file$datapath, sep = "\t"),
             validate("Invalid file; Please upload a .csv, .tsv, or .txt file")
      )
      
      tmp_data[tmp_data == ""] <- NA

      return(tmp_data)
      
    })
    
    ## Reformat the data
    color_coord_data4Fiji <- reactiveValues(data = NULL)
    
    observeEvent(input$RGBval_Unified_reformatBtn, {
      
      color_coord_data4Fiji$data <- color_coord_data()[complete.cases(color_coord_data()[,c(input$RGBval_UnifiedcolumnName,
                                                                                    input$RGBval_UnifiedValuecolumnName)]),]
      
      if(as.integer(nrow(color_coord_data4Fiji$data)/3) != nrow(color_coord_data4Fiji$data)/3) {
        
        sendSweetAlert(
          session = session,
          title = "Some data are missing!",
          text = tags$p("Please check the missing information before re-uploading the dataset. 
                        Perhaps some RGB values are not complete (i.e. one of R, G, or B values are missing). 
                        The number of rows of the dataset should be divisible by 3!"),
          type = "warning"
        )
        
        color_coord_data4Fiji$data <- NULL
      } else {
      
      color_coord_data4Fiji$data$Index <- rep(1:(nrow(color_coord_data4Fiji$data)/3), each = 3)
      
      color_coord_data4Fiji$data <- color_coord_data4Fiji$data %>% 
        tidyr::fill(colnames(color_coord_data4Fiji$data)) %>% 
        tidyr::spread(key = input$RGBval_UnifiedcolumnName, 
                      value = input$RGBval_UnifiedValuecolumnName)
      
      color_coord_data4Fiji$data$Index <- NULL
      
      }
      
      if(any(is.na(color_coord_data4Fiji$data))) {
        
        sendSweetAlert(
          session = session,
          title = "Some data are missing or the data is not properly structured!",
          text = tags$p("Please check the formatting of your data and complete any missing information before re-uploading the dataset. 
                        Perhaps some RGB values are not complete (i.e. one of R, G, or B values are missing!)"),
          type = "warning"
        )
        
        color_coord_data4Fiji$data <- NULL
      }
      
    })
    
    observe({
      if(!is.null(input$color_coord_file)) {
        shinyWidgets::updateRadioGroupButtons(session = session, 
                                              inputId = "color_coord_type", 
                                              disabled = FALSE)
      }
    })
    
    ####************####
    
    ## Set the required columns of the data
    
    ### HEX code
    output$hexCode_column <-renderUI({
      pickerInput(
        inputId = "hexCode_columnName",
        width = "100%",
        label = "Select the column including HEX codes", 
        choices = colnames(color_coord_data()),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(maxOptions = 1, height = 10,
                                noneSelectedText = "Select",
                                size = 5,
                                `live-search` = TRUE)
      )
    })
    
    ########
    
    ### RGB values Unified column
    output$RGBval_Unifiedcolumn <-renderUI({
      pickerInput(
        inputId = "RGBval_UnifiedcolumnName",
        width = "100%",
        label = "Column RGB Names", 
        choices = colnames(color_coord_data()),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(maxOptions = 1,
                                noneSelectedText = "Select",
                                size = 5,
                                `live-search` = TRUE)
      )
    })
    
    output$RGBval_UnifiedValuecolumn <-renderUI({
      pickerInput(
        inputId = "RGBval_UnifiedValuecolumnName",
        width = "100%",
        label = "Column RGB Values", 
        choices = colnames(color_coord_data()),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(maxOptions = 1,
                                noneSelectedText = "Select",
                                size = 5,
                                `live-search` = TRUE)
      )
    })
    
    observe({
      req(input$color_coord_file)
      req(input$color_coord_type)
      
      if(input$color_coord_type == 'RGBval_Fiji') {
        
        if(!is.null(input$RGBval_UnifiedValuecolumnName) & !is.null(input$RGBval_UnifiedcolumnName)) {
          shinyjs::show(id = "RGBval_Unified_reformatBtn")
        } else {
          shinyjs::hide(id = "RGBval_Unified_reformatBtn")
        }
      } else {
        shinyjs::reset(id = "RGBval_UnifiedcolumnName")
        shinyjs::reset(id = "RGBval_UnifiedValuecolumnName")
        shinyjs::hide(id = "RGBval_Unified_reformatBtn")
        removeUI(selector = "div:has(> #RGBvalunif_RcolumnName)")
        removeUI(selector = "div:has(> #RGBvalunif_GcolumnName)")
        removeUI(selector = "div:has(> #RGBvalunif_BcolumnName)")
      }
    })
    
    observeEvent(input$RGBval_Unified_reformatBtn, {
      
      output$RGBvalunif_Rcolumn <-renderUI({
        pickerInput(
          inputId = "RGBvalunif_RcolumnName",
          width = "100%",
          label = "Column R", 
          choices = colnames(color_coord_data4Fiji$data),
          selected = NULL,
          multiple = TRUE,
          options = pickerOptions(maxOptions = 1,
                                  noneSelectedText = "Select",
                                  size = 5,
                                  `live-search` = TRUE)
        )
      })
      
      output$RGBvalunif_Gcolumn <-renderUI({
        pickerInput(
          inputId = "RGBvalunif_GcolumnName",
          width = "100%",
          label = "Column G", 
          choices = colnames(color_coord_data4Fiji$data),
          selected = NULL,
          multiple = TRUE,
          options = pickerOptions(maxOptions = 1,
                                  noneSelectedText = "Select",
                                  size = 5,
                                  `live-search` = TRUE)
        )
      })
      
      output$RGBvalunif_Bcolumn <-renderUI({
        pickerInput(
          inputId = "RGBvalunif_BcolumnName",
          width = "100%",
          label = "Column B", 
          choices = colnames(color_coord_data4Fiji$data),
          selected = NULL,
          multiple = TRUE,
          options = pickerOptions(maxOptions = 1,
                                  noneSelectedText = "Select",
                                  size = 5,
                                  `live-search` = TRUE)
        )
      })
      
    })
    
    ### RGB values
    output$RGBval_Rcolumn <-renderUI({
      pickerInput(
        inputId = "RGBval_RcolumnName",
        width = "100%",
        label = "Column R", 
        choices = colnames(color_coord_data()),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(maxOptions = 1,
                                noneSelectedText = "Select",
                                size = 5,
                                `live-search` = TRUE)
      )
    })
    
    output$RGBval_Gcolumn <-renderUI({
      pickerInput(
        inputId = "RGBval_GcolumnName",
        width = "100%",
        label = "Column G", 
        choices = colnames(color_coord_data()),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(maxOptions = 1,
                                noneSelectedText = "Select",
                                size = 5,
                                `live-search` = TRUE)
      )
    })
    
    output$RGBval_Bcolumn <-renderUI({
      pickerInput(
        inputId = "RGBval_BcolumnName",
        width = "100%",
        label = "Column B", 
        choices = colnames(color_coord_data()),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(maxOptions = 1,
                                noneSelectedText = "Select",
                                size = 5,
                                `live-search` = TRUE)
      )
    })
    
    ########
    
    ### HSL values
    output$HSLval_Hcolumn <-renderUI({
      pickerInput(
        inputId = "HSLval_HcolumnName",
        width = "100%",
        label = "Column H", 
        choices = colnames(color_coord_data()),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(maxOptions = 1,
                                noneSelectedText = "Select",
                                size = 5,
                                `live-search` = TRUE)
      )
    })
    
    output$HSLval_Scolumn <-renderUI({
      pickerInput(
        inputId = "HSLval_ScolumnName",
        width = "100%",
        label = "Column S", 
        choices = colnames(color_coord_data()),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(maxOptions = 1,
                                noneSelectedText = "Select",
                                size = 5,
                                `live-search` = TRUE)
      )
    })
    
    output$HSLval_Lcolumn <-renderUI({
      pickerInput(
        inputId = "HSLval_LcolumnName",
        width = "100%",
        label = "Column L", 
        choices = colnames(color_coord_data()),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(maxOptions = 1,
                                noneSelectedText = "Select",
                                size = 5,
                                `live-search` = TRUE)
      )
    })
    
    ########
    
    ### HSV values
    output$HSVval_Hcolumn <-renderUI({
      pickerInput(
        inputId = "HSVval_HcolumnName",
        width = "100%",
        label = "Column H", 
        choices = colnames(color_coord_data()),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(maxOptions = 1,
                                noneSelectedText = "Select",
                                size = 5,
                                `live-search` = TRUE)
      )
    })
    
    output$HSVval_Scolumn <-renderUI({
      pickerInput(
        inputId = "HSVval_ScolumnName",
        width = "100%",
        label = "Column S", 
        choices = colnames(color_coord_data()),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(maxOptions = 1,
                                noneSelectedText = "Select",
                                size = 5,
                                `live-search` = TRUE)
      )
    })
    
    output$HSVval_Vcolumn <-renderUI({
      pickerInput(
        inputId = "HSVval_VcolumnName",
        width = "100%",
        label = "Column V/B", 
        choices = colnames(color_coord_data()),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(maxOptions = 1,
                                noneSelectedText = "Select",
                                size = 5,
                                `live-search` = TRUE)
      )
    })
    
    ########
    
    ### XY coords
    output$XYcoord_Xcolumn <-renderUI({
      pickerInput(
        inputId = "XYcoord_XcolumnName",
        width = "100%",
        label = "Column X", 
        choices = colnames(color_coord_data()),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(maxOptions = 1,
                                noneSelectedText = "Select",
                                size = 5,
                                `live-search` = TRUE)
      )
    })
    
    output$XYcoord_Ycolumn <-renderUI({
      pickerInput(
        inputId = "XYcoord_YcolumnName",
        width = "100%",
        label = "Column Y", 
        choices = colnames(color_coord_data()),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(maxOptions = 1,
                                noneSelectedText = "Select",
                                size = 5,
                                `live-search` = TRUE)
      )
    })
    
    ####************####
    
    ## define the name of the section column
    
    output$section_column <- renderUI({
      pickerInput(
        inputId = "section_columnName",
        width = "100%",
        label = "Select the column including section names", 
        choices = colnames(color_coord_data()),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(maxOptions = 1,
                                noneSelectedText = "Select",
                                size = 5,
                                `live-search` = TRUE)
      )
    })
    
    ####************####
    
    ## Define the Image and Group names
    output$columnImage <- renderUI({
      pickerInput(
        inputId = "columnImageName",
        width = "100%",
        label = NULL, 
        choices = colnames(color_coord_data()),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(maxOptions = 1,
                                noneSelectedText = "Select",
                                size = 5,
                                `live-search` = TRUE)
      )
    })
    
    output$columnGroup <- renderUI({
      pickerInput(
        inputId = "columnGroupName",
        width = "100%",
        label = NULL, 
        choices = colnames(color_coord_data()),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(maxOptions = 1,
                                noneSelectedText = "Select",
                                size = 5,
                                `live-search` = TRUE)
      )
    })

    ####************####
    
    ## Preprocess the data
    
    observe({
      if(!is.null(input$section_columnName)) {
        shinyjs::enable(id = "color_coord_preprocess")
      } else {
        shinyjs::disable(id = "color_coord_preprocess")
      }
    })
    
    color_coord_processed <- reactiveValues(data = NULL)
    
    observeEvent(input$color_coord_preprocess, {
      
      ## disable selecting another file type
        shinyWidgets::updateRadioGroupButtons(session = session, 
                                              inputId = "color_coord_type",
                                              disabled = TRUE)
      
      if(input$color_coord_type == "hexCode") {
        color_coord_processed$data <- color_coord_data()
        colnames(color_coord_processed$data)[which(colnames(color_coord_processed$data) %in% input$hexCode_columnName)] <- 
          "HEX_code"
        
      } else if(input$color_coord_type == "XYcoord") {
        color_coord_processed$data <- color_coord_data()
        
      } else if(input$color_coord_type == "RGBval") {
        color_coord_processed$data <- cbind(HEX_code = apply(color_coord_data()[,c(input$RGBval_RcolumnName,
                                                                                        input$RGBval_GcolumnName,
                                                                                        input$RGBval_BcolumnName)], 1, function(x)
                                                                  rgb(x[1], x[2], x[3], maxColorValue=255)),
                                            color_coord_data()
                                                 )
        
      } else if(input$color_coord_type == "RGBval_Fiji") {
        color_coord_processed$data <- cbind(HEX_code = apply(color_coord_data4Fiji$data[,c(input$RGBvalunif_RcolumnName,
                                                                                           input$RGBvalunif_GcolumnName,
                                                                                           input$RGBvalunif_BcolumnName)], 1, function(x)
                                                                                     rgb(x[1], x[2], x[3], maxColorValue=255)),
                                            color_coord_data4Fiji$data
        )
        
      } else if(input$color_coord_type == "HSLval") {
        tmp_RGBValues <- as.data.frame(t(hsl2rgb(t(color_coord_data()[,c(input$HSLval_HcolumnName,
                                                                         input$HSLval_ScolumnName,
                                                                         input$HSLval_LcolumnName)]))))
        
        color_coord_processed$data <- cbind(HEX_code = apply(tmp_RGBValues, 1, function(x) rgb(x[1], x[2], x[3], maxColorValue=255)),
                                            color_coord_data()
        )
        
      } else if(input$color_coord_type == "HSVval") {
        color_coord_processed$data <- cbind(HEX_code = apply(color_coord_data()[,c(input$HSVval_HcolumnName,
                                                                                   input$HSVval_ScolumnName,
                                                                                   input$HSVval_VcolumnName)], 1, function(x)
                                                                                     hsv2col(x[1], x[2], x[3])),
                                            color_coord_data()
        )
      }
      
      ### Add section, group and image names
      
      color_coord_processed$data$section <- as.character(color_coord_processed$data[,input$section_columnName])
      
      if(!is.null(input$columnImageName)) {
        color_coord_processed$data$image <- as.character(color_coord_processed$data[,input$columnImageName])
      }
      
      if(!is.null(input$columnGroupName)) {
        color_coord_processed$data$group <- as.character(color_coord_processed$data[,input$columnGroupName])
      }
      
      color_coord_processed$data$section_group <- 
        if(!is.null(input$columnGroupName)) {
          paste(color_coord_processed$data[,"section"],
                as.character(color_coord_processed$data[,"group"]),
                sep = "-")
        } else {
          color_coord_processed$data[,"section"]
        }
      
      color_coord_processed$data$section_image <- 
        if(!is.null(input$columnImageName)) {
          paste(color_coord_processed$data[,"section"],
                as.character(color_coord_processed$data[,"image"]),
                sep = "-")
        } else {
          color_coord_processed$data[,"section"]
        }
      
      color_coord_processed$data$section_image_group <- 
        if(!is.null(input$columnImageName) & !is.null(input$columnGroupName)) {
          paste(color_coord_processed$data[,"section"],
                as.character(color_coord_processed$data[,"image"]),
                as.character(color_coord_processed$data[,"group"]),
                sep = "-")
        } else if(!is.null(input$columnImageName) & is.null(input$columnGroupName)) {
          paste(color_coord_processed$data[,"section"],
                as.character(color_coord_processed$data[,"image"]),
                sep = "-")
        } else if(is.null(input$columnImageName) & !is.null(input$columnGroupName)) {
          paste(color_coord_processed$data[,"section"],
                as.character(color_coord_processed$data[,"group"]),
                sep = "-")
        } else {
          color_coord_processed$data[,"section"]
        }

      showToast(type = "success", 
                message =  NULL, 
                title = "The data is now preprocessed!", 
                .options = myToastOptions
      )
    })
    
    ####************####
    
    ## Visualize the section palette
    
    color_coord_pie_choices <- reactive({
      if(input$color_coord_palette_groupby == "section") {
        unique(color_coord_processed$data[,"section"])
        
      } else if(input$color_coord_palette_groupby == "section_image") {
        unique(color_coord_processed$data[,"section_image"])
        
      } else if(input$color_coord_palette_groupby == "section_group") {
        unique(color_coord_processed$data[,"section_group"])
        
      } else if(input$color_coord_palette_groupby == "section_image_group") {
        unique(color_coord_processed$data[,"section_image_group"])
      }
    })
    
    output$color_coord_pie_section <-renderUI({
      pickerInput(
        inputId = "color_coord_pie_sectionName",
        width = "100%",
        label = "Section", 
        choices = color_coord_pie_choices(),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(maxOptions = 1,
                                noneSelectedText = "Select",
                                size = 5,
                                `live-search` = TRUE)
      )
    })
    
    ### Prepare the data for plotting
    color_coord_secData4Palette <- reactive({
      req(input$color_coord_type)
      
      if(!is.null(input$color_coord_pie_sectionName) & input$color_coord_type == "XYcoord") {
        
        sendSweetAlert(
          session = session,
          title = "Not applicable to X and Y coordinates!",
          text = tags$p("The color palette visualization is not applicable to ", tags$em("X and Y coordinates"),  " input file type.", 
                        br(), br(), 
                        tags$b("However, you may input a different file type",  
                               tags$em("(e.g. a HEX color codes or an RGB containg file)"), " to visualize the color palette of each section.")),
          type = "info"
        )
        
        return(NULL)
        
      } else if(!is.null(input$color_coord_pie_sectionName) & input$color_coord_type != "XYcoord") {
        
        if(any(color_coord_processed$data[,input$color_coord_palette_groupby] %in% 
               input$color_coord_pie_sectionName)) {
        
        data <- color_coord_processed$data[which(color_coord_processed$data[,input$color_coord_palette_groupby] %in% 
                                                   input$color_coord_pie_sectionName),]

        data <- as.data.frame(table(data$HEX_code))
      
        # Compute the position of labels
        data <- data %>% 
          dplyr::arrange(dplyr::desc(Var1)) %>%
          dplyr::mutate(prop = Freq / sum(data$Freq) *100) %>%
          dplyr::mutate(ypos = cumsum(prop) - 0.5*prop )
        
        }
        
        return(data)
      }
      
      NULL
      
    })
    
    ## Prepare the plot
    
    ### Hide the plot title text box by default and show if requested
    observe({
      if(input$color_coord_pieTitleOption == TRUE) {
        shinyjs::show("color_coord_pieTitleUI")
      } else {
        shinyjs::hide("color_coord_pieTitleUI")
      }
    })
    
    output$color_coord_pieTitleUI <- renderUI({
      
      textInput(inputId = "color_coord_pieTitle", 
                label = NULL, 
                value = paste0("Section Pie Palette-", input$color_coord_pie_sectionName), 
                width = "100%", 
                placeholder = "Type in your desired plot title")
      
    })
    
    color_coord_secPiePalette <- reactiveValues(plot = NULL)

    observe({

      if(!is.null(color_coord_secData4Palette())) {
        
        if(any(color_coord_processed$data[,input$color_coord_palette_groupby] %in% 
               input$color_coord_pie_sectionName)) {

          ### main plot
          color_coord_secPiePalette_tmp <- 
          ggplot(color_coord_secData4Palette(), aes(x="", y=prop)) +
          geom_bar(stat="identity", width=1, color="white", fill= color_coord_secData4Palette()$Var1) +
          coord_polar("y", start=0) +
          theme_void() +
          theme(legend.position="none",
                plot.title = element_text(face = "bold", hjust = 0.5),
                plot.title.position = "plot")
          
          if(input$color_coord_pieTitleOption) {
            color_coord_secPiePalette_tmp <- color_coord_secPiePalette_tmp + labs(title = input$color_coord_pieTitle)
          }
          
          ### legend
          color_coord_secPiePalette_legend <-color_coord_secData4Palette() %>%  
            mutate(ypos2=row_number(Var1)) %>%  ## alter stacking order
            ggplot(aes(x="", y=ypos2, fill=Var1)) +
            geom_tile() +
            geom_text(aes(label=Var1, y=ypos2, fontface = "bold"), color="#ffffffbe", 
                      size=4)+
            scale_fill_identity() +
            theme_void() +
            scale_y_continuous(breaks=NULL)
          
        
        if(input$color_coordPalLegend) {
          
          color_coord_secPiePalette$plot <- ggarrange(color_coord_secPiePalette_tmp, color_coord_secPiePalette_legend,
                                                  labels = NULL,
                                                  ncol = 2, nrow = 1, widths = c(9,2))
          
        } else {
          color_coord_secPiePalette$plot <- color_coord_secPiePalette_tmp
        }
        
        }
      } else {color_coord_secPiePalette$plot <-NULL}
    })
    
    # render the plot
    observe({
      if(!is.null(color_coord_secPiePalette$plot)) {
        shinyjs::show(id = "color_coord_SecPiePlot")
        shinyjs::enable(id = "download_color_coord_SecPie_PDF")
        shinyjs::enable(id = "download_color_coord_SecPie_PNG")

        output$color_coord_SecPiePlot <- renderPlot({
          color_coord_secPiePalette$plot
        })
      } else {
        shinyjs::hide(id = "color_coord_SecPiePlot")
        shinyjs::disable(id = "download_color_coord_SecPie_PDF")
        shinyjs::disable(id = "download_color_coord_SecPie_PNG")
        }
    })
    
    ### Taking care of downloading the plot
    
    output$download_color_coord_SecPie_PDF <- downloadHandler(
      filename = paste0("Pie Palette (", Sys.Date(), ").pdf"),
      content = function(file) {
        color_coord_secPiePalette$plot %>%
          ggsave(filename = file, device = "pdf",
                 width = input$color_coord_SecPie.figure.width, 
                 height = input$color_coord_SecPie.figure.height, units = "in")
      }
    )
    
    output$download_color_coord_SecPie_PNG <- downloadHandler(
      filename = paste0("Pie Palette (", Sys.Date(), ").png"),
      content = function(file) {
        color_coord_secPiePalette$plot %>%
          ggsave(filename = file, device = "png",
                 width = input$color_coord_SecPie.figure.width, 
                 height = input$color_coord_SecPie.figure.height, units = "in", 
                 dpi = input$color_coord_SecPie.PNG.resolution)
      }
    )
    
    ####************************************************####
    
    # Calculate the clonality of sections
    
    color_coord_autoClonizeRes <- reactiveValues(result = list())
    
    ## disable the button by default
    shinyjs::disable(id = "color_coord_autoclonizeBtn")
    
    ## enable the button after approving the first section
    observeEvent(input$color_coord_preprocess, {
      shinyjs::enable(id = "color_coord_autoclonizeBtn")
    })
    
    # Prepare the dataframe of sections
    color_coord_ImgSectionsTbl <- reactiveValues(df = data.frame(SectionName = character(),
                                                                 Image_SectionName = character(), 
                                                                 Group_SectionName = character(), 
                                                                 Group_Img_SectionName = character(), 
                                                                 Mean = numeric(),
                                                                 SD = numeric(),
                                                                 ImgName = character(), 
                                                                 GroupName = character()))
    
    # Perform the Autoclonization
    
    ## Define a reactive value to check if the autoclonization is done
    color_coord_autoClonized <- reactiveValues(job = 0, BtnClicked = 0)
    
    ### Reset the color_coord_autoClonized whenever an input argument changed
    color_coord_autoClonized2Listen <- reactive({
      list(input$color_coord_file,
           input$color_coord_type,
           input$color_coord_preprocess,
           input$section_columnName,
           input$columnImageName,
           input$columnGroupName)
    })
    
    observeEvent(color_coord_autoClonized2Listen(), {
      color_coord_autoClonized$job <- 0
    })
    
    observeEvent(input$color_coord_autoclonizeBtn, {
      
      if(any(table(color_coord_processed$data$section_image_group) == 1)) {
        
        snglRecordSecs <- which(table(color_coord_processed$data$section_image_group) == 1)

        sendSweetAlert(
          session = session,
          title = "Some sections have only a single record!",
          text = tags$p("Each section of the input data should include at least two records (", tags$em("e.g. two X and Y
                                              coordinates or two HEX codes depending on the input file type"),  " ).",
                        br(), br(),
                        tags$b("However, the following section(s) have only a single color information record.",  br(),
                               tags$em(gsub(pattern = ", ", replacement = "", 
                                            x = toString(paste0(seq(1:length(snglRecordSecs)), ". ", names(snglRecordSecs), "\n")))))),
          type = "warning"
        )

        return(NULL)
      }

      if(color_coord_autoClonized$BtnClicked == 0) {
        ## Toggle the first three steps
        bs4Dash::updateBox(id = "input_color_coord_box", action = "toggle")
        bs4Dash::updateBox(id = "color_coord_preprocessing_box", action = "toggle")
        bs4Dash::updateBox(id = "color_coord_palette_box", action = "toggle")
      }


      progressSweetAlert(
        session = session, id = "color_coord_autoClonizeRes_progress",
        title = "Autoclonization is in progress ...",
        striped = TRUE, 
        display_pct = TRUE, value = 20
      )

      all_sections <- unique(color_coord_processed$data$section_image_group)

      all_sectionsDataFrame <- dplyr::distinct(color_coord_processed$data, section_image_group, .keep_all = TRUE) %>%
        dplyr::mutate(Mean = 1,
                      SD = 1) %>% 
        dplyr::select(SectionName = section,
                      Image_SectionName = section_image,
                      Group_SectionName = section_group,
                      Group_Img_SectionName = section_image_group,
                      Mean = Mean,
                      SD = SD,
                      ImgName = any_of("image"),
                      GroupName = any_of("group"))


      ## correcting the order of rows of the data frame
      all_sectionsDataFrame <- all_sectionsDataFrame[match(all_sections,
                                                           all_sectionsDataFrame$Group_Img_SectionName),]

      color_coord_ImgSectionsTbl$df <- all_sectionsDataFrame

      lapply(X = 1:length(all_sections),
             FUN = function(i) {

               this_section.Data <- color_coord_processed$data[which(color_coord_processed$data$section_image_group %in% all_sections[i]),]

               if(input$color_coord_type != "XYcoord") {

                 section.hexCodes <- this_section.Data$HEX_code

                 # Get the RGB values of the section
                 section.rgb <- t(col2rgb(section.hexCodes))

                 # Convert RGB values to HSL
                 section.hsl <- as.data.frame(t(rgb2hsl(t(section.rgb))))

                 # Convert HSL values to X,Y coordinates
                 section.coordinates <- coordinate_calc(HSL = section.hsl)

               } else {
                 section.coordinates <- as.data.frame(this_section.Data[, c(input$XYcoord_XcolumnName, input$XYcoord_YcolumnName)])
               }
               
               if(input$color_coord_colorSpaceMode == 3) {
                 
                 color_coord_autoClonizeRes$result[[i]] <- dist_calculate3D(section.coordinates)
               } else {
                 
                 color_coord_autoClonizeRes$result[[i]] <- dist_calculate(section.coordinates)
               }
               
               

               ## Add Mean and SD of the section to dataframe
               color_coord_ImgSectionsTbl$df[i,"Mean"] <- as.numeric(color_coord_autoClonizeRes$result[[i]]$Mean)
               color_coord_ImgSectionsTbl$df[i,"SD"] <- as.numeric(color_coord_autoClonizeRes$result[[i]]$SD)

               color_coord_autoClonized$job <- i + 1

               # Update the progress bar
               updateProgressBar(
                 session = session,
                 title = paste0("Section ", i, " is now processed ..."),
                 id = "color_coord_autoClonizeRes_progress",
                 value = i*(100/nrow(color_coord_ImgSectionsTbl$df))
               )
             })
      
      color_coord_autoClonized$BtnClicked <- color_coord_autoClonized$BtnClicked + 1

      closeSweetAlert(session = session)
      sendSweetAlert(
        session = session,
        title =" Autoclonization completed!",
        type = "success"
      )
      
    })
    
    ####************####
    
    # Plot the color_coord_autoClonized results
    
    ## Hide the plot title text box by default and show if requested
    observe({
      if(input$color_coord_secDistsPlotTitleOption == TRUE) {
        shinyjs::show("color_coord_secDistsPlotTitle")
      } else {
        shinyjs::hide("color_coord_secDistsPlotTitle")
      }
    })
    
    ## Open other boxes of results
    observeEvent(input$color_coord_autoclonizeBtn, {
      
      if(color_coord_autoClonized$BtnClicked == 1) {
      bs4Dash::updateBox(id = "color_coord_DistanceStatistics_box", action = "toggle")
      }
    })
    
    observe({
      
      if(color_coord_autoClonized$job == nrow(color_coord_ImgSectionsTbl$df) + 1) {
        
        ## Define the X axis
        if(input$color_coord_secDistsPlotImageNames == TRUE &
           input$color_coord_secDistsPlotGroupNames == TRUE) {
          color_coord_secDistsPlotXaxis <- as.character(color_coord_ImgSectionsTbl$df$Group_Img_SectionName)
          
        } else if(input$color_coord_secDistsPlotImageNames == TRUE &
                  input$color_coord_secDistsPlotGroupNames == FALSE) {
          color_coord_secDistsPlotXaxis <- as.character(color_coord_ImgSectionsTbl$df$Image_SectionName)
          
        } else if(input$color_coord_secDistsPlotImageNames == FALSE &
                  input$color_coord_secDistsPlotGroupNames == TRUE) {
          color_coord_secDistsPlotXaxis <- as.character(color_coord_ImgSectionsTbl$df$Group_SectionName)
          
        } else {color_coord_secDistsPlotXaxis <- as.character(color_coord_ImgSectionsTbl$df$SectionName)}
        
        ###################
        # Generate plot
        
        color_coord_secDistsPlot <- 
          color_coord_ImgSectionsTbl$df %>% 
          ggplot(
            aes(x = color_coord_secDistsPlotXaxis, y = Mean, fill = color_coord_secDistsPlotXaxis)) + 
          ylab("Mean Distance") + xlab(NULL) +
          scale_fill_manual(name = NULL,
                            values = fish(n = length(unique(color_coord_secDistsPlotXaxis)), 
                                          option = input$color_coord_secDistsPlotPalette)) +
          theme_bw() + theme_classic() +
          theme(legend.box.background = element_rect(color="black", size=1))
        
        if(input$color_coord_secDistsPlotTitleOption == TRUE) {
          color_coord_secDistsPlot <- color_coord_secDistsPlot + ggtitle(input$color_coord_secDistsPlotTitle)
        }
        
        if(input$color_coord_secDistsPlotErrorBars == TRUE) {
          color_coord_secDistsPlot <- color_coord_secDistsPlot + 
            geom_errorbar(aes(ymin=Mean - 1, ymax=Mean+abs(SD)), width=.2, color = "black",
                          position=position_dodge(width = .9))
        }
        
        color_coord_secDistsPlot <- color_coord_secDistsPlot +
          geom_bar(stat = "identity", width = 0.5, show.legend = FALSE) + 
          coord_flip()
        
        ###################
        # Render the plot
        output$color_coord_majorSecClonalityPlot <- renderPlot({
          
          color_coord_secDistsPlot
          
        })
        
        ###################
        
        shinyjs::show(id = "color_coord_autoclonizeResRow")
        

          # Taking care of downloading autoclonization plot

          output$color_coord_download_majorSecClonality_PDF <- downloadHandler(
            filename = paste0("AutoClone-Distance Plot (", Sys.Date(), ").pdf"),
            content = function(file) {
              color_coord_secDistsPlot %>%
                ggsave(filename = file, device = "pdf",
                       width = input$color_coord_majorSecClonality.figure.width,
                       height = input$color_coord_majorSecClonality.figure.height, units = "in")
            }
          )

          output$color_coord_download_majorSecClonality_PNG <- downloadHandler(
            filename = paste0("AutoClone-Distance Plot (", Sys.Date(), ").png"),
            content = function(file) {
              color_coord_secDistsPlot %>%
                ggsave(filename = file, device = "png",
                       width = input$color_coord_majorSecClonality.figure.width,
                       height = input$color_coord_majorSecClonality.figure.height, units = "in",
                       dpi = input$color_coord_majorSecClonality.PNG.resolution)
            }
          )

          ###################

          # Generate table of autoclonization
          
          if(color_coord_ImgSectionsTbl$df$SectionName == gsub(pattern = "-", replacement = "", 
                                                               x = color_coord_ImgSectionsTbl$df$Group_Img_SectionName)) {
            color_coord_ImgSectionsTbl4Table <- color_coord_ImgSectionsTbl$df[,c(1, 5, 6)]
            
          } else if(color_coord_ImgSectionsTbl$df$SectionName == gsub(pattern = "-", replacement = "", 
                                                                      x = color_coord_ImgSectionsTbl$df$Group_SectionName)) {
            color_coord_ImgSectionsTbl4Table <- color_coord_ImgSectionsTbl$df[,c(2, 5, 6)]
          } else if(color_coord_ImgSectionsTbl$df$SectionName == gsub(pattern = "-", replacement = "", 
                                                                      x = color_coord_ImgSectionsTbl$df$Image_SectionName)) {
            color_coord_ImgSectionsTbl4Table <- color_coord_ImgSectionsTbl$df[,c(3, 5, 6)]
          } else {
            color_coord_ImgSectionsTbl4Table <- color_coord_ImgSectionsTbl$df[,c(4, 5, 6)]
          }

          colnames(color_coord_ImgSectionsTbl4Table) <- c("Section-Image-Group", "Mean_Distance", "SD")

          brks_drivers <- quantile(color_coord_ImgSectionsTbl4Table$Mean_Distance, probs = seq(.05, .95, .05), na.rm = TRUE)
          clrs_drivers <- rev(round(seq(200, 40, length.out = length(brks_drivers) + 1), 0) %>%
                                {paste0("rgb(205,", ., ",", ., ")")})

          output$color_coord_majorSecClonalityTable <- DT::renderDataTable(server = FALSE,

            DT::datatable(
              { color_coord_ImgSectionsTbl4Table },
              extensions = c('Buttons', 'SearchPanes', 'FixedHeader'),

              options = list(
                columnDefs=list(list(targets=1:3, class="dt-left")),
                paging = TRUE,
                searching = TRUE,
                pageLength = 5,
                fixedColumns = TRUE,
                scrollX = TRUE,
                autoWidth = TRUE,
                ordering = TRUE,
                dom = 'Bfrtip',
                buttons = list(list(extend = 'csv', filename= paste0("AutoClone-Distance Table ", "(", Sys.Date(), ")")),
                               list(extend = 'excel', filename= paste0("AutoClone-Distance Table ", "(", Sys.Date(), ")")),
                               list(extend = 'print', filename= paste0("AutoClone-Distance Table ", "(", Sys.Date(), ")"))
                )
              ),

              class = "display"
            ) %>%
              formatStyle('Mean_Distance', fontWeight = styleInterval(1, c('bold', 'bold'))) %>%
              formatStyle('Mean_Distance', backgroundColor = styleInterval(brks_drivers, clrs_drivers))
          )

      }
      
    })
    
    #****************************************************************#
    
    # Visualize the Distribution statistics as a RainCloud plot
    
    ### reset the group and image names on changing options
    observeEvent(input$columnGroupOption, {
      shinyjs::reset(id = "columnGroupName")
      shinyjs::reset(id = "columnGroup")
    })
    
    observeEvent(input$columnImageOption, {
      shinyjs::reset(id = "columnImageName")
      shinyjs::reset(id = "columnImage")
    })
    

    ## Hide the plot title text box by default and show if requested
    observe({
      if(input$color_coord_secDistsStatsPlotTitleOption == TRUE) {
        shinyjs::show("color_coord_secDistsStatsPlotTitle")
      } else {
        shinyjs::hide("color_coord_secDistsStatsPlotTitle")
      }
    })

    # Define a reactiveValue for storing the autoclonization results
    color_coord_SecStatsDF4Plotting <- reactiveValues(df = NULL)
    
    observe({

      if(color_coord_autoClonized$job == nrow(color_coord_ImgSectionsTbl$df) + 1) {

        ###################
        # Generate plot

    # Prepare the table for plotting
        if(!is.null(input$columnImageName) & is.null(input$columnGroupName)) {
          color_coord_SecStatsDF4Plotting$df <- do.call(rbind,
                                                        lapply(X = 1:nrow(color_coord_ImgSectionsTbl$df),
                                                               FUN = function(i) {
                                                                 data.frame(Distance = color_coord_autoClonizeRes$result[[i]]$Table,
                                                                            ImgName = color_coord_ImgSectionsTbl$df[i,"ImgName"],
                                                                            SectionName = color_coord_ImgSectionsTbl$df[i,"SectionName"],
                                                                            Group_SectionName = color_coord_ImgSectionsTbl$df[i,"Group_SectionName"],
                                                                            Image_SectionName = color_coord_ImgSectionsTbl$df[i,"Image_SectionName"],
                                                                            Group_Img_SectionName = color_coord_ImgSectionsTbl$df[i,"Group_Img_SectionName"])
                                                               }))
        } else if(is.null(input$columnImageName) & !is.null(input$columnGroupName)) {
          color_coord_SecStatsDF4Plotting$df <- do.call(rbind,
                                                        lapply(X = 1:nrow(color_coord_ImgSectionsTbl$df),
                                                               FUN = function(i) {
                                                                 data.frame(Distance = color_coord_autoClonizeRes$result[[i]]$Table,
                                                                            GroupName = color_coord_ImgSectionsTbl$df[i,"GroupName"],
                                                                            SectionName = color_coord_ImgSectionsTbl$df[i,"SectionName"],
                                                                            Group_SectionName = color_coord_ImgSectionsTbl$df[i,"Group_SectionName"],
                                                                            Image_SectionName = color_coord_ImgSectionsTbl$df[i,"Image_SectionName"],
                                                                            Group_Img_SectionName = color_coord_ImgSectionsTbl$df[i,"Group_Img_SectionName"])
                                                               }))
        } else if(!is.null(input$columnImageName) & !is.null(input$columnGroupName)) {
          color_coord_SecStatsDF4Plotting$df <- do.call(rbind,
                                                        lapply(X = 1:nrow(color_coord_ImgSectionsTbl$df),
                                                               FUN = function(i) {
                                                                 data.frame(Distance = color_coord_autoClonizeRes$result[[i]]$Table,
                                                                            ImgName = color_coord_ImgSectionsTbl$df[i,"ImgName"],
                                                                            GroupName = color_coord_ImgSectionsTbl$df[i,"GroupName"],
                                                                            SectionName = color_coord_ImgSectionsTbl$df[i,"SectionName"],
                                                                            Group_SectionName = color_coord_ImgSectionsTbl$df[i,"Group_SectionName"],
                                                                            Image_SectionName = color_coord_ImgSectionsTbl$df[i,"Image_SectionName"],
                                                                            Group_Img_SectionName = color_coord_ImgSectionsTbl$df[i,"Group_Img_SectionName"])
                                                               }))
        } else {
          color_coord_SecStatsDF4Plotting$df <- do.call(rbind,
                                                        lapply(X = 1:nrow(color_coord_ImgSectionsTbl$df),
                                                               FUN = function(i) {
                                                                 data.frame(Distance = color_coord_autoClonizeRes$result[[i]]$Table,
                                                                            SectionName = color_coord_ImgSectionsTbl$df[i,"SectionName"],
                                                                            Group_SectionName = color_coord_ImgSectionsTbl$df[i,"Group_SectionName"],
                                                                            Image_SectionName = color_coord_ImgSectionsTbl$df[i,"Image_SectionName"],
                                                                            Group_Img_SectionName = color_coord_ImgSectionsTbl$df[i,"Group_Img_SectionName"])
                                                               }))
        }

        ## Define the X axis
        if(input$color_coord_secDistsStatsPlotImageNames == TRUE &
           input$color_coord_secDistsStatsPlotGroupNames == TRUE) {
          color_coord_secDistsStatsPlotXaxis <- as.character(color_coord_SecStatsDF4Plotting$df$Group_Img_SectionName)

        } else if(input$color_coord_secDistsStatsPlotImageNames == TRUE &
                  input$color_coord_secDistsStatsPlotGroupNames == FALSE) {
          color_coord_secDistsStatsPlotXaxis <- as.character(color_coord_SecStatsDF4Plotting$df$Image_SectionName)

        } else if(input$color_coord_secDistsStatsPlotImageNames == FALSE &
                  input$color_coord_secDistsStatsPlotGroupNames == TRUE) {
          color_coord_secDistsStatsPlotXaxis <- as.character(color_coord_SecStatsDF4Plotting$df$Group_SectionName)

        } else {color_coord_secDistsStatsPlotXaxis <- as.character(color_coord_SecStatsDF4Plotting$df$SectionName)}

        ###################
        # Generate the plot
        color_coord_SecStatsPlot <-

          ggplot(data = color_coord_SecStatsDF4Plotting$df,
                 aes(y = Distance,
                     x = color_coord_secDistsStatsPlotXaxis,
                     fill = color_coord_secDistsStatsPlotXaxis)) +

          geom_flat_violin(position = position_nudge(x = .2, y = 0)) +
          
          geom_point(aes(y = Distance, color = "lightgrey"), show.legend = F,
                     position = position_jitter(width = .15), size = 1.2, alpha = 0.7) +

          geom_boxplot(width = .1, guides = FALSE, outlier.shape = NA) +

          coord_flip() +

          guides(fill = FALSE) +
          
          scale_color_manual(values = "lightgrey") +

          scale_fill_manual(values = fish(n = length(unique(color_coord_secDistsStatsPlotXaxis)),
                                          option = input$color_coord_secDistsStatsPlotPalette)) +

          theme_bw() + theme_classic() +

          xlab(NULL) +
          ylab("Distance")
          
        if(input$color_coord_SecsFacet != 1 & any(input$color_coord_SecsFacet %in% colnames(color_coord_SecStatsDF4Plotting$df))) {
          color_coord_SecStatsPlot <- color_coord_SecStatsPlot +
            facet_grid( ~ get(input$color_coord_SecsFacet))
        }

        if(input$color_coord_secDistsStatsPlotTitleOption == TRUE) {
          color_coord_SecStatsPlot <- color_coord_SecStatsPlot + ggtitle(input$color_coord_secDistsStatsPlotTitle)
        }

        ###################
        ## Render the plot
        output$color_coord_SecClonalityStatsPlot <- renderPlot({

          color_coord_SecStatsPlot

        })

        ###################

        shinyjs::show(id = "color_coord_DistStatsRow")

        # Taking care of downloading sec stats plot

        output$download_color_coord_SecStatsClonality_PDF <- downloadHandler(
          filename = paste0("AutoClone-Distance RainCloud Plot (", Sys.Date(), ").pdf"),
          content = function(file) {
            color_coord_SecStatsPlot %>%
              ggsave(filename = file, device = "pdf",
                     width = input$color_coord_SecStatsClonality.figure.width,
                     height = input$color_coord_SecStatsClonality.figure.height, units = "in")
          }
        )

        output$download_color_coord_SecStatsClonality_PNG <- downloadHandler(
          filename = paste0("AutoClone-Distance RainCloud Plot (", Sys.Date(), ").png"),
          content = function(file) {
            color_coord_SecStatsPlot %>%
              ggsave(filename = file, device = "png",
                     width = input$color_coord_SecStatsClonality.figure.width,
                     height = input$color_coord_SecStatsClonality.figure.height, units = "in",
                     dpi = input$color_coord_SecStatsClonality.PNG.resolution)
          }
        )

      }

    })
    
    #****************************************************************#
    
    # ANOVA for major sections
    
    color_coord_SecDistsDF4ANOVA <- reactiveValues(df = NULL)
    
    observe({
      
      if(color_coord_autoClonized$job == nrow(color_coord_ImgSectionsTbl$df) + 1 & nrow(color_coord_ImgSectionsTbl$df) == 1) {
        
        ## Show the ANOVA notif row
        shinyjs::show(id = "color_coord_ANOVANotifRow", anim = TRUE, animType = "slide")
        
      } else if(color_coord_autoClonized$job == nrow(color_coord_ImgSectionsTbl$df) + 1 & nrow(color_coord_ImgSectionsTbl$df) > 1) {
        
        ### Prepare the table for ANOVA
        if(!is.null(input$columnImageName) & is.null(input$columnGroupName)) {
          color_coord_SecDistsDF4ANOVA$df <- do.call(rbind,
                                                     lapply(X = 1:nrow(color_coord_ImgSectionsTbl$df),
                                                            FUN = function(i) {
                                                              data.frame(Distance = color_coord_autoClonizeRes$result[[i]]$Table,
                                                                         ImgName = color_coord_ImgSectionsTbl$df[i,"ImgName"],
                                                                         SectionName = color_coord_ImgSectionsTbl$df[i,"SectionName"],
                                                                         Group_SectionName = color_coord_ImgSectionsTbl$df[i,"Group_SectionName"],
                                                                         Image_SectionName = color_coord_ImgSectionsTbl$df[i,"Image_SectionName"],
                                                                         Group_Img_SectionName = color_coord_ImgSectionsTbl$df[i,"Group_Img_SectionName"])
                                                            }))
        } else if(is.null(input$columnImageName) & !is.null(input$columnGroupName)) {
          color_coord_SecDistsDF4ANOVA$df <- do.call(rbind,
                                                     lapply(X = 1:nrow(color_coord_ImgSectionsTbl$df),
                                                            FUN = function(i) {
                                                              data.frame(Distance = color_coord_autoClonizeRes$result[[i]]$Table,
                                                                         GroupName = color_coord_ImgSectionsTbl$df[i,"GroupName"],
                                                                         SectionName = color_coord_ImgSectionsTbl$df[i,"SectionName"],
                                                                         Group_SectionName = color_coord_ImgSectionsTbl$df[i,"Group_SectionName"],
                                                                         Image_SectionName = color_coord_ImgSectionsTbl$df[i,"Image_SectionName"],
                                                                         Group_Img_SectionName = color_coord_ImgSectionsTbl$df[i,"Group_Img_SectionName"])
                                                            }))
        } else if(!is.null(input$columnImageName) & !is.null(input$columnGroupName)) {
          color_coord_SecDistsDF4ANOVA$df <- do.call(rbind,
                                                     lapply(X = 1:nrow(color_coord_ImgSectionsTbl$df),
                                                            FUN = function(i) {
                                                              data.frame(Distance = color_coord_autoClonizeRes$result[[i]]$Table,
                                                                         ImgName = color_coord_ImgSectionsTbl$df[i,"ImgName"],
                                                                         GroupName = color_coord_ImgSectionsTbl$df[i,"GroupName"],
                                                                         SectionName = color_coord_ImgSectionsTbl$df[i,"SectionName"],
                                                                         Group_SectionName = color_coord_ImgSectionsTbl$df[i,"Group_SectionName"],
                                                                         Image_SectionName = color_coord_ImgSectionsTbl$df[i,"Image_SectionName"],
                                                                         Group_Img_SectionName = color_coord_ImgSectionsTbl$df[i,"Group_Img_SectionName"])
                                                            }))
        } else {
          color_coord_SecDistsDF4ANOVA$df <- do.call(rbind,
                                                     lapply(X = 1:nrow(color_coord_ImgSectionsTbl$df),
                                                            FUN = function(i) {
                                                              data.frame(Distance = color_coord_autoClonizeRes$result[[i]]$Table,
                                                                         SectionName = color_coord_ImgSectionsTbl$df[i,"SectionName"],
                                                                         Group_SectionName = color_coord_ImgSectionsTbl$df[i,"Group_SectionName"],
                                                                         Image_SectionName = color_coord_ImgSectionsTbl$df[i,"Image_SectionName"],
                                                                         Group_Img_SectionName = color_coord_ImgSectionsTbl$df[i,"Group_Img_SectionName"])
                                                            }))
        }
        
        ## Show the ANOVA row
        shinyjs::show(id = "color_coord_ANOVARow", anim = TRUE, animType = "slide")
        
        ################
        
        ## Perform the ANOVA
        if(any(input$color_coord_DistsANOVA_variable %in% colnames(color_coord_SecDistsDF4ANOVA$df))) {
          
          color_coord_SecsDistsANOVA <- aov(Distance ~ get(input$color_coord_DistsANOVA_variable),
                                            data = color_coord_SecDistsDF4ANOVA$df)
        } else {
          color_coord_SecsDistsANOVA <- aov(Distance ~ Group_Img_SectionName,
                                            data = color_coord_SecDistsDF4ANOVA$df)
        }
        
        ### Perform pairwise comparison for adjusting p-values
        color_coord_SecsDistsANOVA <- as.data.frame(TukeyHSD(color_coord_SecsDistsANOVA)[[1]])
        
        colnames(color_coord_SecsDistsANOVA) <- c("Difference of Means", "Lower 95% CI",
                                                  "Upper 95% CI", "Padj")
        
        ################
        
        brks_drivers <- quantile(color_coord_SecsDistsANOVA$Padj, probs = seq(.05, .95, .05), na.rm = TRUE)
        clrs_drivers <- rev(round(seq(200, 40, length.out = length(brks_drivers) + 1), 0) %>%
                              {paste0("rgb(205,", ., ",", ., ")")})
        
        output$color_coord_SecsDistsANOVATable <- DT::renderDataTable(server = FALSE,
          
          DT::datatable(
            { color_coord_SecsDistsANOVA },
            
            extensions = c('Buttons', 'SearchPanes', 'FixedHeader'),
            
            options = list(
              columnDefs=list(list(targets=1:4, class="dt-left")),
              paging = TRUE,
              searching = TRUE,
              pageLength = 5,
              fixedColumns = TRUE,
              scrollX = TRUE,
              autoWidth = TRUE,
              ordering = TRUE,
              dom = 'Bfrtip',
              buttons = list(list(extend = 'csv', filename= paste0("Clonality ANOVA Table ", "(", Sys.Date(), ")")),
                             list(extend = 'excel', filename= paste0("Clonality ANOVA Table ", "(", Sys.Date(), ")")),
                             list(extend = 'print', filename= paste0("Clonality ANOVA Table ", "(", Sys.Date(), ")"))
              )
            ),
            
            class = "display"
          ) %>%
            formatStyle('Padj', fontWeight = styleInterval(1, c('bold', 'bold'))) %>%
            formatStyle('Padj', backgroundColor = styleInterval(brks_drivers, clrs_drivers)) %>%
            formatRound(columns=c(1:4), digits=2)
        )
        
      }
      
    })
    
    ####************####
    
    # Stop the app after closing the tab
    if (!interactive()) {
        session$onSessionEnded(function() {
          
          ## Remove all session files
          unlink(session$token, recursive = TRUE)
          
            stopApp()
        })
    }
}

shinyApp(ui = ui, server = server)

