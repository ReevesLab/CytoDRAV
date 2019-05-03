
## DATA NORMALIZATION FOR PLOTTING

# Sets outliers equal to the 1st and 99th percentile
normData <- function(x){
  quantiles <- quantile( x, c(.01, .99 ) )
  x[ x < quantiles[1] ] <- quantiles[1]
  x[ x > quantiles[2] ] <- quantiles[2]
  x
}

## PLOTTING FUNCTIONS
plotTSNE <- function(dataToPlot, marker, dotsize, dotalpha, sampleColor, showDensity) {
  data <- as.data.frame(dataToPlot)
  if (marker == "Sample") {

    plot <- ggplot(data) + aes(x=data[,"tSNEX"], y=data[,"tSNEY"]) + scale_colour_manual(name="Sample",values=sampleColor) +
      geom_point(aes(color=data[,marker]), size=dotsize, alpha=dotalpha) +
      labs(x="bh-SNE1", y="bh-SNE2") + guides(colour = guide_legend(override.aes = list(size=3)))
    if (isTRUE(showDensity)) plot <- plot + geom_density_2d()

  } else {

    plot <- ggplot(data, aes(x=tSNEX, y=tSNEY)) +
      geom_point(aes(color=normData(data[, marker])), size=dotsize, alpha=dotalpha) +
      scale_color_gradientn(colors=c("blue", "cyan", "yellow", "red" )) +
      labs(x="bh-SNE1", y="bh-SNE2", color=strsplit(marker, "::")[[1]][2]) +
      guides(colour = guide_colourbar(title = waiver(), barheight=25))
    if (isTRUE(showDensity)) plot <- plot + geom_density_2d()

  }

  return(plot + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                      panel.background = element_blank(), axis.line = element_line(colour = "black")))
}


## FCS Loading
#' @import flowCore
loadFCS <- function(fcsFiles, doTransform) {
  exprsData <- data.frame()

  for (i in 1:length(fcsFiles[,1])) {
    singleFCS <- flowCore::read.FCS(fcsFiles[i, "datapath"])

    if (isTRUE(doTransform)) {
      lgcl <- flowCore::logicleTransform( w = 0.5, t= 262144, m = 4)
      singleFCS <- flowCore::transform(singleFCS,
                                       flowCore::transformList(paste(singleFCS@parameters@data$name), lgcl))

    }
    dff <- data.frame(singleFCS@exprs)
    colnames(dff) <- as.vector(paste(singleFCS@parameters@data$name, singleFCS@parameters@data$desc, sep="::"))
    name <- strsplit(fcsFiles[i, "name"], "[.]")[[1]][1]
    name <- stringr::str_replace_all(name, " ", "_")
    dff$Sample <- rep(name, nrow(dff))
    if ("Infection" %in% names(singleFCS@description)) {
      inf <- singleFCS@description$Infection
      dff$Infection <- rep(inf, nrow(dff))
    }
    exprsData <- rbind(exprsData, dff)
  }


  exprsData$Duped <- duplicated(exprsData[,!colnames(exprsData)%in%c("Sample", "Infection")])

  exprsData <- exprsData %>% filter(Duped==FALSE)
  exprsData <- exprsData[sample(1:nrow(exprsData)),]
  exprsData <- exprsData[,!colnames(exprsData)%in%c("Duped")]
  return(exprsData)
}
