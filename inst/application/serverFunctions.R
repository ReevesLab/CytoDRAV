
`%>%` <- dplyr::`%>%`
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
  plot <- ggplot2::ggplot(data) + ggplot2::aes(x=data[,"tSNEX"], y=data[,"tSNEY"])
  if (marker == "Sample") {

    plot <- plot + ggplot2::scale_colour_manual(name="Sample",values=sampleColor) +
      ggplot2::geom_point(ggplot2::aes(color=data[,marker]), size=dotsize, alpha=dotalpha) +
      ggplot2::labs(x="bh-SNE1", y="bh-SNE2") +
      ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(size=3)))

  } else {

    plot <- plot +
      ggplot2::geom_point(ggplot2::aes(color=normData(data[, marker])), size=dotsize, alpha=dotalpha) +
      ggplot2::scale_color_gradientn(colors=c("blue", "cyan", "yellow", "red" ),
                                     breaks=c(min(normData(data[, marker])), median(normData(data[, marker])), max(normData(data[, marker]))),
                                     labels = c("Low", "", "High")) +
      ggplot2::labs(x="bh-SNE1", y="bh-SNE2", color=strsplit(marker, "::")[[1]][2]) +
      ggplot2::guides(colour = ggplot2::guide_colourbar(title = ggplot2::waiver(), barheight=25))


  }
  if (isTRUE(showDensity)) plot <- plot + ggplot2::geom_density_2d()
  return(plot + ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                      panel.grid.minor = ggplot2::element_blank(),
                      legend.text = ggplot2::element_text(size=18, face="bold"),
                      legend.title = ggplot2::element_text(size=18, face="bold"),
                      panel.background = ggplot2::element_blank(),
                      axis.line = ggplot2::element_line(colour = "black"),
                      axis.title = ggplot2::element_text(size=18, face="bold") ))
}


## FCS Loading
#' @import flowCore
loadFCS <- function(fcsFiles, doTransform) {
  exprsData <- data.frame()
  withProgress(message="Reading FCS files...", value=0, min=0, max=length(fcsFiles[,1]), {
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
    incProgress(1)
  }
  })


  exprsData$Duped <- duplicated(exprsData[,!colnames(exprsData)%in%c("Sample", "Infection")])

  exprsData <- exprsData %>% dplyr::filter(Duped==FALSE)
  exprsData <- exprsData[sample(1:nrow(exprsData)),]
  exprsData <- exprsData[,!colnames(exprsData)%in%c("Duped")]
  return(exprsData)
}
