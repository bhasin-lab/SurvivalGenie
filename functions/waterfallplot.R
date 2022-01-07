#'@name waterfallplot
#'@aliases waterfallplot
#'@title A plotting function for SISPA sample identifiers
#'@description Given a sample changepoint data frame, will plot all samples zscores from that data.
#'@usage waterfallplot(x)
#'@param x A data frame containing samples as rows followed by zscores and estimated sample_groups to be plotted.
#'@details This function expects the output from cptSamples function of SISPA package, and highlights the sample profile of interest in the changepoint 1 with orange filled bars.
#'@return Bar plot pdf illustrating distinct SISPA sample profiles.
#'@import ggplot2
#'@examples
#'samples <- c("s1","s2","s3","s4","s5","s6","s7","s8","s9","s10")
#'zscores <- c(3.83,2.70,2.67,2.31,1.70,1.25,-0.42,-1.01,-2.43,-3.37)
#'changepoints <- c(1,1,1,2,2,3,3,NA,NA,NA)
#'sample_groups <- c(1,1,1,0,0,0,0,0,0,0)
#'my.data = data.frame(samples,zscores,changepoints,sample_groups)
#'waterfallplot(my.data)
#'@export
waterfallplot = function(x, selected, label){
    if(missing(x)){
      stop("input data is missing!")
    }
    type <- samples <- End <- Start <- NULL # Setting the variables to NULL first
    
    colnames(x)[1] <- "samples"
    colnames(x)[ncol(x)] <- "sample_groups"
    #create a start column
    x[,ncol(x)+1] <- 0
    #define global variable
    #select and modify the data for readability
    read_data_sort <- x[ order(x[,2], decreasing=FALSE), ]
    #arrange the columns
    arrange_read_data_sort <- data.frame(read_data_sort[,1],read_data_sort[,4],read_data_sort[,2],read_data_sort[,3])
    colnames(arrange_read_data_sort)<-c("samples","Start","End","sample_groups")
    arrange_read_data_sort[,1] <- factor(arrange_read_data_sort[,1], levels=arrange_read_data_sort[,1])
    arrange_read_data_sort$id <- seq_along(arrange_read_data_sort$End)
    arrange_read_data_sort$type <- ifelse(arrange_read_data_sort$sample_groups=="High", "High", "Low")
    arrange_read_data_sort <- arrange_read_data_sort[,c(5,1,6,2,3)]
    #generate the waterfall plot
    #wf <- ggplot(arrange_read_data_sort, aes(colour=type,samples,fill=type)) + scale_fill_manual(values=c("#800000","#000080")) + scale_colour_manual(values=c("#800000","#000080")) + geom_rect(aes(x=samples,xmin=id-0.35,xmax=id+0.35,ymin=End,ymax=Start))  
    wf <- ggplot(arrange_read_data_sort, aes(colour=type,samples,fill=type)) + scale_fill_manual(values=c("red","blue")) + scale_colour_manual(values=c("red","blue")) + geom_rect(aes(x=samples,xmin=id-0.35,xmax=id+0.35,ymin=End,ymax=Start))  
    
    wf <- wf + labs(x="", y="", fill="")
    wf <- wf + theme(legend.position = "none")
    wf <- wf + theme(axis.text.y = element_text(colour="black",size=14.0))
    wf <- wf + theme(axis.text.x = element_blank())
    wf <- wf + theme(axis.title.x = element_text(colour="black",face="bold",size=16.0))
    wf <- wf + theme(axis.title.y = element_text(colour="black",face="bold",size=16.0))
    wf <- wf + theme(plot.title = element_text(colour="black", face="bold", size=18.0, hjust=0.5))
    wf <- wf + theme(panel.background = element_rect(fill="NA"))
    wf <- wf + theme(panel.border = element_rect(colour = "black", fill="NA"))
    wf <- wf + theme(panel.grid.major.y = element_line(colour="NA"))
    wf <- wf + theme(panel.grid.minor = element_line(colour = "NA"))
    wf <- wf + theme(axis.ticks.x = element_blank())
    wf <- wf + theme(axis.ticks.y=element_line(colour="black"))
    #wf <- wf + ylab(label)
    wf <- wf + xlab("Samples")
    #wf <- wf + ggtitle(selected)
    return(wf)
}



