# # Weak attempt to fake a mosaic plot in ggplot
# source.widths <- as.data.frame(prop.table(source.type.pub, 2))
# plot.data <- as.data.frame(prop.table(source.type.pub, 1))
# plot.data$source.width <- source.widths$Freq
# 
# p <- ggplot(plot.data, aes(x=source_type, y=Freq, fill=publication))
# p + geom_bar(aes(width=source.width), stat="identity", position="identity") +
#   coord_flip() + facet_grid(~publication) + 
#   theme_bw() + theme(legend.position="none")
