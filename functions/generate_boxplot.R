generate_boxplot = function(df,selected,label){
  
  #low.col = rgb(0, 0, 0.5)
  #high.col = rgb(0.5, 0, 0)
  #cols = c(high.col,low.col)
  cols = c("red","blue")
  
  cols <- setNames(cols, c("High", "Low"))

  boxplot(df$gx ~ df$group, 
          main = selected, 
          notch = FALSE, 
          col = cols, 
          show.names=TRUE,
          ylab=label,
          xlab="",
          cex.lab=2.0, cex.names=2.0, cex.axis=2.0, cex.main=2.0)
  abline(v=1.5,lty=2,col='black')
}

generate_boxplot_gg = function(df, indSel, selected){
  
  gx_data <- data.frame(df$sample, df$group, df[,indSel+1])
  colnames(gx_data) <- c("sample", "group", selected)
  df.m <- melt(gx_data, id.vars=c("sample", "group"))
  
  p <- ggplot(data = df.m, aes(x=group, y=value)) 
  p <- p + scale_color_manual("", values=c("red", "blue"))
  p <- p + geom_boxplot(aes(fill = group))
  p <- p + geom_point(aes(y=value, group=group), position = position_dodge(width=0.75))
  p <- p + facet_wrap( ~ variable, scales="free")
  p <- p + xlab("") + ylab(paste0("CIBERSOFT fraction", "\n"))
  #p <- p + ggtitle(paste0(datasets[i,]," [low=",nrow(low_tmb_samples), " | high=", nrow(high_tmb_samples), "]"))
  #p <- p + guides(fill=guide_legend(title=input$checkdataset))
  p <- p + theme(panel.background = element_rect(fill="NA"))
  p <- p + theme(panel.border = element_rect(colour = "black", fill="NA"))
  p <- p + theme(panel.grid.major.y = element_line(colour="NA"))
  p <- p + theme(panel.grid.minor = element_line(colour="NA"))
  p <- p + theme(axis.text.x = element_text(hjust = 0.5, vjust = 0.5, colour = "black", face="bold", size=16.0))
  p <- p + theme(axis.text.y = element_text(hjust = 1.0, vjust = 0.5, colour = "black", face="bold", size=14.0))
  p <- p + theme(axis.title.y = element_text(colour="black", face="bold", size=16.0))
  p <- p + theme(axis.title.x = element_text(colour="black", face="bold", size=16.0))
  p <- p + theme(strip.text = element_text(colour="black", face="bold", size = 20))
  p <- p + theme(legend.position="none")
  p <- p + geom_vline(xintercept = 1.5, color="black", lty=4)
  
  print(p)
}
