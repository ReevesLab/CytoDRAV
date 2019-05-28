## FCS Loading
loadFCS <- function(fcsFiles, doTransform) {
  fcs_dataframes <- c()
  withProgress(message="Reading FCS files...", value=0, min=0, max=length(fcsFiles[,1]), {
    for (i in 1:length(fcsFiles[,1])) {
      single_fcs_raw <- flowCore::read.FCS(fcsFiles[i, "datapath"])

      # Biexponential transformation. Uses a linear transformation for data around 0 and log transformation
      # for larger values
      if (isTRUE(doTransform)) {
        m = 4.5
        t = 262144
        w = 1.5
        lgcl <- flowCore::logicleTransform( w = w, t= t, m = m)
        single_fcs_transformed <- flowCore::transform(single_fcs_raw,
                                                      flowCore::transformList(paste(single_fcs_raw@parameters@data$name), lgcl))

        single_fcs_df <- data.frame(single_fcs_transformed@exprs)
      } else {
        single_fcs_df <- data.frame(single_fcs_raw@exprs)
      }


      # Get the $PnS from the FCS file. If $PnS is NA (FSC, SSC, Time, Event) then use $PnN
      column_names <- c()
      for (j in 1:ncol(single_fcs_df)) {
        if (is.na(single_fcs_raw@parameters@data$desc[[j]])) {
          column_names <- c(column_names, single_fcs_raw@parameters@data$name[[j]])
        } else {
          column_names <- c(column_names, single_fcs_raw@parameters@data$desc[[j]])
        }
      }
      colnames(single_fcs_df) <- column_names

      # Use input filename as sample keyword in the dataframe
      name <- strsplit(fcsFiles[i, "name"], "[.]")[[1]][1]
      name <- gsub(" ", "_", name)
      name <- gsub("\\+", ".POS", name)
      name <- gsub("\\-", ".NEG", name)
      single_fcs_df$Sample <- as.character(rep(name, nrow(single_fcs_df)))

      # Add the single FCS data to a list of dataframes
      fcs_dataframes[[i]] <- single_fcs_df
      incProgress(1)
    }
  })

  # Flow data is gated sequentially. So if you include your 'Live' population, that will also include all your
  # other populations. This increases the number of data in you analysis, which makes it slower. This portion
  # sorts the input files from smallest to larget and removes duplicates. R works in such a way that the first event
  # it finds and removes subsequent matches.

  # Sort the list of dataframes by number of rows and join them into a single dataframe
  fcs_dataframes = fcs_dataframes[order(sapply(fcs_dataframes,nrow),decreasing = F)]
  exprsData <- data.frame()
  for (i in 1:length(fcs_dataframes)) exprsData <- rbind(exprsData, fcs_dataframes[[i]])

  # Remove duplicates and shuffle dataset
  exprsData <- exprsData[!duplicated(exprsData[,!colnames(exprsData)%in%c("Sample")]),]
  exprsData <- exprsData[sample(1:nrow(exprsData)),]
  return(exprsData)
}

## PLOTTING FUNCTIONS
create_plot <- function(dataToPlot, marker="Sample", dotsize, dotalpha, sampleColor="black", show_legend,show_axis_labels, show_title) {
  data <- as.data.frame(dataToPlot)
  plot <- ggplot2::ggplot(data) + ggplot2::aes(x=data[,"tSNEX"], y=data[,"tSNEY"])

  if (marker == "Sample") {

    plot <- plot + ggplot2::scale_colour_manual(values=sampleColor) +
      ggplot2::geom_point(ggplot2::aes(color=data[,marker]), size=dotsize, alpha=dotalpha) +
      ggplot2::labs(x="bh-SNE1", y="bh-SNE2", color="") +
      ggplot2::ggtitle("Sample") +
      ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(size=3), nrow = 1))

  } else if(marker == "Density") {
    df <- data
    x <- densCols(df$tSNEX,df$tSNEY, colramp=colorRampPalette(c("black", "white")))
    df$dens <- col2rgb(x)[1,] + 1L
    cols <-  colorRampPalette(c("#000099", "#00FEFF", "#45FE4F","#FCFF00", "#FF9400", "#FF3100"))(256)
    df$col <- cols[df$dens]
    plot <- ggplot2::ggplot(df[order(df$dens),], ggplot2::aes(tSNEX, tSNEY, color = dens)) +
      ggplot2::geom_point(size=dotsize, alpha=dotalpha) +
      ggplot2::scale_color_gradientn(colors = unique(df[order(df$dens),'col']),
                                     breaks = c(min(df$dens), median(df$dens), max(df$dens)),
                                     labels = c("Low", "", "High")) +
      ggplot2::labs(x="bh-SNE1", y="bh-SNE2", color="") +
      ggplot2::ggtitle("Density") +
      ggplot2::guides(colour = ggplot2::guide_colourbar(title = ggplot2::waiver(), barwidth = 25))

  } else {

    plot <- plot +
      ggplot2::geom_point(ggplot2::aes(color=normData(data[, marker])), size=dotsize, alpha=dotalpha) +
      ggplot2::scale_color_gradientn(colors=c("blue", "cyan", "yellow", "red" ),
                                     breaks=c(min(normData(data[, marker])),
                                              median(normData(data[, marker])),
                                              max(normData(data[, marker]))),
                                     labels = c("Low", "", "High")) +
      ggplot2::labs(x="bh-SNE1", y="bh-SNE2", color="") +
      ggplot2::ggtitle(marker) +
      ggplot2::guides(colour = ggplot2::guide_colourbar(title = ggplot2::waiver(), barwidth = 25))


  }
  plot <- plot +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   legend.text = ggplot2::element_text(size=12, face="bold"),
                   legend.title = ggplot2::element_text(size=16, face="bold"),
                   panel.background = ggplot2::element_blank(),
                   axis.line = ggplot2::element_line(colour = "black"),
                   axis.title = ggplot2::element_text(size=18, face="bold"),
                   plot.title = ggplot2::element_text(size=18, face = "bold"),
                   legend.position = "bottom",
                   legend.direction = "horizontal")
  if (!show_legend) plot <- plot + ggplot2::theme(legend.position="none")
  if (!show_title) plot <- plot + ggplot2::theme(plot.title = ggplot2::element_blank())
  if (!show_axis_labels) plot <- plot + ggplot2::theme(axis.title = ggplot2::element_blank())
  return(plot)
}

# Sets outliers equal to the 1st and 99th percentile
normData <- function(x){
  quantiles <- quantile( x, c(.01, .99 ) )
  x[ x < quantiles[1] ] <- quantiles[1]
  x[ x > quantiles[2] ] <- quantiles[2]
  x
}
