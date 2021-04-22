library("ggplot2")

#' @export
HPK_plot <- function(df){
  tagpt_color <- c(NA, rgb(112,173,71, max=255), rgb(255,0,0, max=255), "black", rgb(46,117,182, max=255))

  df$parameter_value <- df$parameter_value*0.028316847 # cfs to m3/s

  df$dgtag <- as.factor(df$dgtag)

  ggplot()+geom_line(data = df, aes(x=datetime, y=parameter_value, group=1), size=0.7, show.legend = FALSE) +
    geom_point(data = df, aes(x=datetime, y=parameter_value,
                              shape=factor(dgtag, levels = c(0,1,2,3,4)),
                              fill=factor(dgtag, levels = c(0,1,2,3,4)),
                              color=factor(dgtag, levels = c(0,1,2,3,4))), na.rm = TRUE,
               size=4, show.legend = FALSE) +
    geom_point(data = df, aes(x=datetime, y=parameter_value,
                              shape=factor(dgtag, levels = c(0,1,2,3,4)),
                              fill=factor(dgtag, levels = c(0,1,2,3,4)),
                              color=factor(dgtag, levels = c(0,1,2,3,4))), na.rm = TRUE,
               size=4, show.legend = FALSE) +
    scale_shape_manual(values = c(20, 24, 16, 16, 23))+
    scale_color_manual(values = tagpt_color)+
    scale_fill_manual(values = tagpt_color)+
    scale_x_datetime(date_breaks = "3 days")+ # this allows user to plot date x axis
    labs(x = "Time", color = "Type")+
    ylab(expression(Discharge~(m^3/s))) +
    theme(panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_rect(colour = "black", fill=NA),
          axis.title.x = element_text(size = 28),
          axis.title.y = element_text(size = 26),
          axis.text.x = element_text(angle = 0, size = 24),
          axis.text.y = element_text(size = 24))
}
