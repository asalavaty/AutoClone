library(ggplot2)
library(fishualize)

# Define a function for conversion of RGB to HSL

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

##****************************************************##

# Define a function for calculating X and Y coordinates based on Hue and Saturation

coordinate_calc <- function(HSL) {
  
  # Multiply Saturation (Hypotenuse) by 100 to bring it to a range of 0:100
  HSL$S <- HSL$S*100
  
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
                        Y = HSL$Y)
  
  return(Results)
  
}

##****************************************************##

# Define a function for calculating distance and mean distances

dist_calculate <- function(dataset) {
  
  points_matrix <- (ICSNP::pair.diff(as.matrix(dataset)))^2
  
  points_matrix <- sqrt(c(points_matrix[,1] + points_matrix[,2]))
  
  ####************####
  
  final.results <- list(Table = points_matrix,
                        Mean = round(mean(points_matrix, na.rm = TRUE), digits = 2),
                        SD = round(sd(points_matrix, na.rm = TRUE), digits = 2))
  
  return(final.results)
  
}

##****************************************************##

# Read in the input file
inputFile <- read.csv("www/sample_coordinates.csv", sep = ",")

inputFile$sample_segment <- paste(inputFile$Sample, inputFile$Segment, sep = "_")

## Merged (both sample- and segment-based) analysis
inputFile_mergedSplit <- base::split(x = inputFile, f = inputFile$sample_segment)

inputFile.merged.res <- base::lapply(X = inputFile_mergedSplit, function(i) {dist_calculate(i[,c(1,2)])})

inputFile.merged.res.summaried <- as.data.frame(matrix(data = c(1:3), nrow = 1, ncol = 3))
colnames(inputFile.merged.res.summaried) <- c("Sample_Segment", "Mean", "SD")

for (i in 1:length(inputFile.merged.res)) {
  inputFile.merged.res.summaried <- rbind(inputFile.merged.res.summaried,
                                        c(
                                          names(inputFile.merged.res)[i],
                                          inputFile.merged.res[[i]][["Mean"]],
                                          inputFile.merged.res[[i]][["SD"]])
  )
}

inputFile.merged.res.summaried <- inputFile.merged.res.summaried[-1,]
inputFile.merged.res.summaried$Mean <- as.numeric(inputFile.merged.res.summaried$Mean)
inputFile.merged.res.summaried$SD <- as.numeric(inputFile.merged.res.summaried$SD)

merged_plot <- ggplot(data = inputFile.merged.res.summaried, 
                      aes(x = Sample_Segment, y = Mean, fill = Sample_Segment)) + 
  geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD), width=.2, color = "black",
                position=position_dodge(.9)) +
  geom_bar(stat = "identity", width = 0.5, show.legend = FALSE) + coord_flip() +
  ylab("Mean Distance") + xlab(NULL) +
  scale_fill_manual(name = NULL,
                    values = fish(n = nrow(inputFile.merged.res.summaried))) +
  theme_bw() + theme_classic() +
  theme(legend.box.background = element_rect(color="black", size=1))

##****************************##

## Sample-based analysis
inputFile_sampleSplit <- base::split(x = inputFile, f = inputFile$Sample)
inputFile.sample.res <- base::lapply(X = inputFile_sampleSplit, function(i) {dist_calculate(i[,c(1,2)])})

inputFile.sample.res.summaried <- as.data.frame(matrix(data = c(1:3), nrow = 1, ncol = 3))
colnames(inputFile.sample.res.summaried) <- c("Sample_Segment", "Mean", "SD")

for (i in 1:length(inputFile.sample.res)) {
  inputFile.sample.res.summaried <- rbind(inputFile.sample.res.summaried,
                                          c(
                                            names(inputFile.sample.res)[i],
                                            inputFile.sample.res[[i]][["Mean"]],
                                            inputFile.sample.res[[i]][["SD"]])
  )
}

inputFile.sample.res.summaried <- inputFile.sample.res.summaried[-1,]
inputFile.sample.res.summaried$Mean <- as.numeric(inputFile.sample.res.summaried$Mean)
inputFile.sample.res.summaried$SD <- as.numeric(inputFile.sample.res.summaried$SD)

sample_plot <- ggplot(data = inputFile.sample.res.summaried, 
                      aes(x = Sample_Segment, y = Mean, fill = Sample_Segment)) + 
  geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD), width=.2, color = "black",
                position=position_dodge(.9)) +
  geom_bar(stat = "identity", width = 0.5, show.legend = FALSE) + coord_flip() +
  ylab("Mean Distance") + xlab(NULL) +
  scale_fill_manual(name = NULL,
                    values = fish(n = nrow(inputFile.sample.res.summaried))) +
  theme_bw() + theme_classic() +
  theme(legend.box.background = element_rect(color="black", size=1))

  #calculate anova
  sample.anova <- stats::aov( ~ Sample, data = sample.data.ddup.analyzed)
  
  # Summary of the analysis
  sum.anova <- anova(sample.anova)
  
  anova.p.value <- sum.anova$`Pr(>F)`[complete.cases(sum.anova$`Pr(>F)`)]
  
  #Tukey HSD multiple pairwise-comparisons
  multp.comp.anova <- TukeyHSD(sample.anova)

  
  
  
  
  
  ##****************************************************##

  install.packages("imager")
  install.packages("imagefx")
  BiocManager::install("EBImage")
  BiocManager::install("xRing")
  install.packages("colordistance")
  library(imager)
  library(plotly)
  library(imagefx)
  library(EBImage)
  library(jpeg)
  library(colordistance)
  
  test.image <- colordistance::loadImage(path = "Resources/download (1).jpeg", 
                                         lower = c(0, 0, 0), upper = c(0, 0, 0), 
                                         hsv = TRUE, alpha.channel = FALSE)

  test.image2 <- colordistance::loadImage(path = "Resources/test1.png", 
                                         lower = c(0, 0, 0), upper = c(0, 0, 0), 
                                         hsv = TRUE, alpha.channel = FALSE)

  test.image3 <- colordistance::loadImage(path = "Resources/test2.png", 
                                          lower = c(0, 0, 0), upper = c(0, 0, 0), 
                                          hsv = TRUE, alpha.channel = FALSE)
  
  
  test.image <- readImage(files = "/Users/asal0019/Downloads/untitled folder/F3.large.jpg")
  writeImage(x = test.image, files = "/Users/asal0019/Downloads/untitled folder/testEB.tiff", type = "tiff")
  
  colordistance::plotImage(test.image)
  
  
  img <- test.image$original.rgb

  img <- 
    ggplot() +
    annotation_custom(rasterGrob(img, 
                                 width = unit(1,"npc"),
                                 height = unit(1,"npc")), 
                      -Inf, Inf, -Inf, Inf)
  
  img
  
  dim(test.image$original.rgb)
  
  
  
  test.image.section1 <- imagefx::crop.image(img = test.image$original.rgb, 
                                             xleft = 74.78469, ybottom = 179.2653, 
                                             xright = 94.04187, ytop = 233.4366, pick = FALSE)
  png::writePNG(image = test.image.section1$img.crop, target = "Resources/test1.png", dpi = 300)
  
  
  test.image.section2 <- imagefx::crop.image(img = test.image$original.rgb, pick = TRUE)

  png::writePNG(image = test.image.section2$img.crop, target = "Resources/test2.png", dpi = 300)
  
  # test.image$original.rgb
  # test.image$filtered.hsv.2d
  
  test.image.section1 <- colordistance::loadImage(path = "Resources/test1.png", 
                                                  lower = c(0, 0, 0), upper = c(0.1, 0.1, 0.1), 
                                                  hsv = TRUE, alpha.channel = FALSE)
  colordistance::plotImage(test.image.section1)
  
  getImageHist(test.image.section1, hsv = TRUE, bins = 4)
  
  test.image.section1$filtered.rgb.2d <- test.image.section1$filtered.rgb.2d * 255
  test.image.section1.hsl <- as.data.frame(t(rgb2hsl(t(test.image.section1$filtered.rgb.2d))))
  test.image.section1.coordinates <- coordinate_calc(HSL = test.image.section1.hsl)
  test.image.section1.dist <- dist_calculate(test.image.section1.coordinates)

  test.image.section2$filtered.rgb.2d <- test.image.section2$filtered.rgb.2d * 255
  test.image.section2.hsl <- as.data.frame(t(rgb2hsl(t(test.image.section2$filtered.rgb.2d))))
  test.image.section2.coordinates <- coordinate_calc(HSL = test.image.section2.hsl)
  test.image.section2.dist <- dist_calculate(test.image.section2.coordinates)
  
  
  # test.image <- imager::load.image(file = "Resources/Nguyen-2017-Cell Stem Cell 5.png")
  # plot(test.image)

  # test.image2 <- jpeg::readJPEG("Resources/Nguyen-2017-Cell Stem Cell 5.jpg")
  # imagefx::image2(test.image2)
  
  # test.image.section2 <- imagefx::crop.image(img = test.image2, pick = TRUE)
  # 
  # 
  # 
  # imagefx::image2(test.image.section1$img.crop)
  # 
  # EBImage::displayOutput
  
  
  alaki <- imager::cannyEdges(im = test.image.section1,alpha = 0.3)
  plot(alaki)
  
  
  
  
  
