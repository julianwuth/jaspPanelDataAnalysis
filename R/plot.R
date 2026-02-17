#' @import jaspBase
#' @export
.createPlmPlot <- function(jaspResults, dataset, options, ready){

  plmPlot <- createJaspPlot(title = "Line Plot",  width = 750, height = 300)

  plmPlot$dependOn(c("time", "id", "dependent", "plot"))

  jaspResults[["plmPlot"]] <- plmPlot

  if (!ready)
    return()

  .plmFillPlotDescriptives(plmPlot, dataset, options)

  return()
}

.plmFillPlotDescriptives <- function(plmPlot, dataset, options){
  dataset <- dataset

  dataset[[options$time]] <- as.numeric(as.character(dataset[[options$time]]))

  rangeTime <- range(dataset[[options$time]])

  xBreaks <- unique(c(jaspGraphs::getPrettyAxisBreaks(rangeTime), rangeTime))

  aes <- ggplot2::aes

  plot <- ggplot2::ggplot(dataset, aes(dataset[[options$time]], dataset[[options$dependent]], color = dataset[[options$id]]))+ggplot2::geom_line()

  plot <- plot +
    jaspGraphs::scale_x_continuous(options$time, breaks = xBreaks, limits = rangeTime) +
    jaspGraphs::themeJaspRaw(legend.position = 'right') +
    jaspGraphs::geom_rangeframe(sides = 'bl')+
    ggplot2::labs(x = options$time , y = options$dependent, color = options$id)

  plmPlot$plotObject <- plot

  return()
}
