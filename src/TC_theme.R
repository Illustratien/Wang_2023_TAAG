TC_theme <- function(ax.txt.siz=NULL,ax.tit.siz=NULL,lgd.txt.siz=NULL,lgd.tit.siz=NULL,strp.txt.siz=NULL,
                     t=NULL,r=NULL,b=NULL,l=NULL,frame=NULL,frame.tick=NULL,...){
  
  if (is.null(ax.txt.siz)){ax.txt.siz <-32}
  if (is.null(ax.tit.siz)){ax.tit.siz <- 32}
  if (is.null(lgd.txt.siz)){lgd.txt.siz <- 28}
  if (is.null(lgd.tit.siz)){lgd.tit.siz <- 28}
  if (is.null(strp.txt.siz)){strp.txt.siz <- 28}
  if(is.null(t)){t<-50}
  if(is.null(r)){r<-10}
  if(is.null(b)){b<-10}
  if(is.null(l)){l<-5}
  if(is.null(frame)){frame<-3}
  if(is.null(frame.tick)){frame.tick<-2}
  ggplot2::theme_classic()+  
    ggplot2::theme(
      
      # axis text
      axis.text.x = element_text(size=ax.txt.siz,face="bold",vjust=-2), # tick label size
      axis.text.y = element_text(size=ax.txt.siz,face="bold",margin = margin(r=8)), # tick label size
      # axis title
      axis.title   =element_text(size=ax.tit.siz,face="bold"),# axis title size
      axis.title.y = element_text(margin = margin(r=35),vjust=-2),# spacing of y and tick
      axis.title.x = element_text(margin = margin(b=35),vjust=-6), # spacing of x and axis 
      # axis tick
      axis.ticks   =element_line(size = frame.tick),# tick thickness
      axis.ticks.length=unit(.3, "cm"),# tick length
      # facet 
      strip.text.x   = element_text(size=strp.txt.siz,face="bold"),
      strip.text.y   = element_text(size=strp.txt.siz,face="bold"),
      strip.background = element_rect(colour = "White", fill=NA),
      # frame
      panel.border = element_rect(colour = "White", fill=NA, size=frame),
      # axis.line    = element_line(size = 2, linetype = "solid"),# axis line 
      axis.line.x.top = element_line(size = frame, linetype = "solid"),# axis line
      # axis.line.x.bottom = element_line(size = frame, linetype = "solid"),# axis line
      axis.line.y.right = element_line(size = frame, linetype = "solid"),# axis line
      # axis.line.y.left = element_line(size = frame, linetype = "solid"),# axis line
      # legend
      legend.title = element_text(size = lgd.tit.siz,face='bold'),# legend size
      legend.text  = element_text(size =lgd.txt.siz,face='bold'),# legend text
      legend.background = element_rect(fill=NA),
      # legend.justification=c(1,0), legend.position=c(.99,0.1),# legend position lower right 
      plot.margin = margin(t = t, r = r, b = b, l = l, unit = "pt"),
      plot.background = element_rect(
        fill = "white"
      ),
      ...
    )
}  
