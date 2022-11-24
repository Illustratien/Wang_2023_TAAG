pacman::p_load(igraph,tcltk,extrafont,RcolorBrewer,scales)
# dir ---------------------------------------------------------------------
src_dir <-dirname(rstudioapi::getSourceEditorContext()$path)
dat_dir <- sub('src.*','data',src_dir)
res_dir <- sub('src.*','result',src_dir)
windowsFonts("Arial"=windowsFont("Arial"))

# Function ----------------------------------------------------------------

add_label <- function(xfrac, yfrac, label, pos = 4, ...) {
  # for adding text in the graph *from web
  # https://seananderson.ca/2013/10/21/panel-letters/
    u <- par("usr")
    x <- u[1] + xfrac * (u[2] - u[1])
    y <- u[4] - yfrac * (u[4] - u[3])
    text(x, y, label, pos = pos,font=2,cex=2,...)
}

# read data-------------------------------------------------------------------------

Net <- readRDS(paste0(res_dir,'/Networkdata/figure3_net.rds'))
Link <- readRDS(paste0(res_dir,'/Networkdata/figure3_link.rds'))

plot.link <- data.frame(
  x=Link[,1],
  y=Link[c(seq(1+(nrow(Link)/3),nrow(Link)),1:(nrow(Link)/3)),1],
  s=c(rep('a',(nrow(Link)/3)),rep('b',(nrow(Link)/3)),rep('c',(nrow(Link)/3)))
)
# color setting for network drawing-------------------------------------------------------------------

line_colr <- c('#05840f','#C93838')
line_rgb <- col2rgb(line_colr)
line_clr <- rgb(line_rgb[1,],
                line_rgb[2,],
                line_rgb[3,],
                max=255,
                alpha=60*255/100)
#red blue yellow
node_colr <- c('#A58AFF','#1972f0','#FFBE00')
node_rgb <- col2rgb(node_colr)
node_clr <- rgb(node_rgb[1,],
                node_rgb[2,],
                node_rgb[3,],
                max=255,
                alpha=80*255/100)

unit.pi <-quote('ton'%.%'ha'^'-1')
thresh <- 0.33

pi.txt <-bquote(bold(Yield)~bolditalic(P)[i]~bold('('*.(unit.pi)*')'))

# Fig3 -------------------------------------------------------------------------
# arrangement setting for subplots in rplot
x1 <- c(0,.5,0,0.5)
x2 <- c(0.5,1,0.5,1)
y1 <- c(0.5,.5,0,0)
y2 <- c(1,1,0.5,0.5)
v <- c(T,T,T,T)

tiffname <- 'Fig.3'

tiff(filename=paste0(res_dir,'/',tiffname,'.tiff'),
     type="cairo",
     units="cm",
     width=17.4,
     height=11.6,
     compression = "lzw",
     pointsize=12,
     res=400,# dpi,
     family="Arial"
)

for ( i in 1:3){
  
  if (i>1){  par(fig=c(x1[i], x2[i], y1[i],y2[i]),
                 mai = c(0.1, 0.1, 0.1, 0.1),
                 oma = c(.01, .01, 0.01, 0.01),
                 lwd=2,new=v[i])}
  else{  par(fig=c(x1[i], x2[i], y1[i],y2[i]),
             mai = c(0.1, 0.1, 0.1, 0.1),
             oma = c(.01, .01, 0.01, 0.01),
             lwd=2)}
  
  net1 <- Net[[i]]
  igraph::E(net1)$width <- E(net1)$weight*8
  l <- layout.fruchterman.reingold(net1)
  nodeNam <- sub('Pi yield',as.expression(bquote(bolditalic(P)[bold('i, yield')])),V(net1)$Name)
  plot(net1,
       vertex.label.cex=0.7,
       vertex.label.font=2,
       vertex.size=35,
       vertex.label=parse(text=nodeNam) ,
       vertex.label.color='black',
       vertex.label.family='Arial',
       # rescale=FALSE,
       edge.color=ifelse(E(net1)$sign==1,
                         line_clr[1],
                         line_clr[2]),
       layout=l)
  
  add_label(0.02, 0.07, letters[i])
  if(i==3){
    legend(x=-1.83, y=-0.57,
           c(as.expression(bquote(bold(yield))),
             as.expression(bquote(
               bolditalic(P)[bold('i, yield')])),
             as.expression(bquote(bold(physiological~parameters)))),
           pch=21,
           title=as.expression(
             bquote(atop(bold('Threshold:'~'r >')~bold(.(toString(thresh))),
                         ~~~~~~~~~~bold('r > 0')~~~~~~~~~~bold('r < 0')))),
           col=NA, pt.bg=alpha(node_colr,.8), pt.cex=2, cex=.65, 
           bty="n", ncol=1, text.font=2,text.col = '#5a5a5a',
           title.adj=0)
    # create the green line legend manually
    # generate continous x 
    x.base <- -1.75
    x.step <- .01
    x0 <- seq(x.base,x.base+.15,x.step)
    y <- -.68# fix y
    lines(x0,rep(y,length(x0)),col=alpha('#05840f',.4),lty=1,lwd=8)
    # create the read line
    xbase1 <- x.base+.64
    x3 <- seq(xbase1,xbase1+.15,x.step)
    lines(x3,rep(y,length(x3)),col=alpha('#C93838',.4),lty=1,lwd=8)
    
    add_label(0.02, 0.07, letters[i])
    
  }
}
# scatter plot
i <- 4
par(fig=c(x1[i], x2[i], y1[i],y2[i]),
    mai = c(0.6, 0.6, 0.1, 0.1),
    oma = c(0,6, 2, 1),
    lwd=2,new=v[i])

plot(x~y,
     pch = c(16, 17, 18),  # different 'pch' types 
     col = alpha(c('#C93838','#FFBE00','#05840f'),.5),
     data=plot.link,
     cex=.5,
     cex.axis=1,
     font=1,
     xlim=c(-1,0.6),
     ylim=c(-1,0.6),
     xlab='',ylab='',cex.axis=1,cex.lab=1,
     font.lab=2,font.axis=2)
#diagnonal line
lines(seq(-1,0.6,.01),seq(-1,0.6,.01),col=alpha('#757d89',.8),lty=2,lwd=2)
# xtitle
title(xlab=expression(italic(r)), 
      line=2, cex.lab=1.2, family="Arial")
# ytitle
title(ylab=expression(italic(r)), 
      line=2, cex.lab=1.2, family="Arial")
legend(x=-0.5, y=-.6,
       c(expression(italic(r)*"-network A vs "*italic(r)*"-network B"),
         expression(italic(r)*"-network B vs "*italic(r)*"-network C"),
         expression(italic(r)*"-network A vs "*italic(r)*"-network C")),
       pch=c(16, 17, 18),
       col=alpha(c('#C93838','#FFBE00','#05840f'),.5),
       pt.cex=1,# symbol size 
       cex=.55, # text size
       y.intersp = .8,# vertical disntance between legend
       bty="n", ncol=1, text.font=2,text.col = '#5a5a5a',
       title.adj=0)
# add letters at the uuper left corner
mtext( letters[i],side = 3,font=2,cex=2,line=-.5, 
       at=par("usr")[1]-0.50*diff(par("usr")[1:2]))

dev.off()
