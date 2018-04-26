# create stacked barplot with % info
# based on http://t-redactyl.io/blog/2016/01/creating-plots-in-r-using-ggplot2-part-4-stacked-bar-plots.html

ggplot_stacked_barplot_percent = function(longDF, pal, .adj = 0.75){
 # takes in long data (longDF)
  #colour scale (pal)
  # and adjustment for % inside bars
  
  require(ggplot2)
  require(ggthemes)
  require(extrafont)
  require(plyr)
  require(scales)
  require(stringr)
  
  # adjust the position of the data labels

  longDF = ddply(longDF, .(type),
                 #transform, pos = cumsum(value) - (0.5 * value))
                 transform, pos = 100 - cumsum(adj*value))
  
  
  # plot

  p <- ggplot() + geom_bar(data = longDF, aes(y = value, x = type, fill = anno), 
                           stat="identity") + 
                  geom_text(data=longDF, aes(x = type, y = pos, label = paste0(floor(value),"%")), # adding % in bars
                            size=4,fontface="bold") +
                  scale_y_continuous(limits=c(0,100),
                       expand=c(0,0),   # setting limits so bars start at 0
                       labels = dollar_format(suffix = "%", prefix = "")) +   # add % to y values
                  scale_x_discrete(labels = function(type) str_wrap(type, width = 25)) +  # wrap x labels
                  scale_fill_manual(values=pal) +                 
                    theme( 
                    axis.line = element_line(size=0.5, colour = "black"),  # axis lines
                    panel.grid.major = element_blank(), panel.grid.minor = element_blank(), # no background grids
                    panel.background = element_blank(),  # white background
                  axis.text.x = element_text(size=16,face="bold",colour="black"),
                   axis.text.y =element_text(size=16,face="bold",colour ="black"),  # bold ticks on y, leave margin for inward ticks
                   axis.title.x=element_blank(), 
                   axis.title.y=element_blank(), # no title on x or y
                   legend.title = element_blank(),
                   legend.text = element_text(size=15,face="bold",colour="black"))
  
  return(p)

}
