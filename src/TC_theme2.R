TC_theme2 <- function(ax.txt.siz=NULL,ax.tit.siz=NULL,lgd.txt.siz=NULL,lgd.tit.siz=NULL,strp.txt.siz=NULL,
                     t=NULL,r=NULL,b=NULL,l=NULL, ...){
  
  if (is.null(ax.txt.siz)){ax.txt.siz <-14}
  if (is.null(ax.tit.siz)){ax.tit.siz <- 16}
  if (is.null(lgd.txt.siz)){lgd.txt.siz <- 14}
  if (is.null(lgd.tit.siz)){lgd.tit.siz <- 12}
  if (is.null(strp.txt.siz)){strp.txt.siz <- 14}
  if(is.null(t)){t<-1}
  if(is.null(r)){r<-1}
  if(is.null(b)){b<-10}
  if(is.null(l)){l<-1}
  
  ggplot2::theme_classic()+  
    ggplot2::theme(
      
      # axis text
      axis.text.x = element_text(size=ax.txt.siz,vjust=-1), # tick label size
      axis.text.y = element_text(size=ax.txt.siz,margin = margin(r=4)), # tick label size
      # axis title
      axis.title   =element_text(size=ax.tit.siz,face="bold"),# axis title size
      axis.title.y = element_text(margin = margin(r=10),vjust=-1),# spacing of y and tick
      axis.title.x = element_text(margin = margin(b=10),vjust=-4), # spacing of x and axis 
      # axis tick
      axis.ticks   =element_line(size = 1),# tick thickness
      axis.ticks.length=unit(.1, "cm"),# tick length
      # facet 
      strip.text.x   = element_text(size=strp.txt.siz),
      strip.text.y   = element_text(size=strp.txt.siz),
      strip.background = element_rect(colour = NA, fill=NA),
      # frame
      panel.border = element_rect(colour = NA, fill=NA, size=1),
      # axis.line    = element_line(size = 1, linetype = "solid"),# axis line
      # axis.line.x.top = element_line(size = 1, linetype = "solid"),# axis line
      # axis.line.y.right = element_line(size = 1, linetype = "solid"),# axis line
      # legend
      legend.title = element_text(size = lgd.tit.siz,face='bold'),# legend size
      legend.text  = element_text(size =lgd.txt.siz,face='bold'),# legend text
      # legend.justification=c(1,0), legend.position=c(.99,0.1),# legend position lower right 
      plot.margin = margin(t = t, r = r, b = b, l = l, unit = "pt"),
      legend.background = element_rect(fill=NA),
      plot.background = element_rect(
        fill = "transparent"
      ), ...
      
    )
}  
