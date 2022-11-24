
rm(list=ls())

pacman::p_load(igraph,tcltk,extrafont,RcolorBrewer,scales)

# dir ---------------------------------------------------------------------
src_dir <-dirname(rstudioapi::getSourceEditorContext()$path)
dat_dir <- sub('src.*','data',src_dir)
res_dir <- sub('src.*','result',src_dir)
windowsFonts("Arial"=windowsFont("Arial"))

# color bar setting -------------------------------------------------------------------------

# color RampPalette always start from low value to high value
# for <0 blue to white
colfunc1 <- colorRampPalette(c('#072f5f',"#99efff"))(4)
# for >0 orange color start from white to orange
colfunc2<- colorRampPalette(c("#ffaf7a",'#c83200'))(4) 
# put the white in the middle
colfunc <- c(colfunc1,"#FFFFFF",colfunc2) %>%  rev(.)

# layout for the node position in networks-------------------------------------------------------------------------

l1 <-read.csv(paste0(res_dir,"/Networkdata/si_layout_circle.csv")) %>% as.matrix(.)
l2 <-read.csv(paste0(res_dir,"/Networkdata/t_layout_circle.csv")) %>% as.matrix(.)

# merge 2 plots -------------------------------------------------------------------

vec1 <- c("Trait_network","SI_network")
Net <- list()
Link <- list()
for( i in 1:2){
  Net[[i]] <- readr::read_rds(paste0(res_dir,"/Networkdata/Net_",vec1[i],"_fig6.rds"))
  Link[[i]] <- readr::read_rds(paste0(res_dir,"/Networkdata/Link_",vec1[i],"_fig6.rds"))
}
# create breaks for the image.plot function 
tiffname <- paste0('Fig.6')

for ( i in 1:1){# select the first replicates as example
  ##### net_si for si
  net_si <- Net[[2]][[i]]
  
  igraph::E(net_si)$width <- E(net_si)$weight*8
  nodeNam <-V(net_si)$Name
  tr.vec <- nodeNam[startsWith(nodeNam, "Pi ")]
  tr.vec <- sub('Pi ','',tr.vec)
  # little correction of ndoename
  tr.vec <- stringi::stri_replace_all_fixed(tr.vec, 
                                            pattern = c("flowering", "maturity","lai"),
                                            replacement = c("flowring_date", "maturity_date","LAI"),
                                            vectorize_all = FALSE)
  # for Pi yield prefix
  trait_column_name<-purrr::map_chr(tr.vec,~{
    paste0('bolditalic(P)[bold(\"i,',' ',.x,'\")]')
  }) 
  trait_columns <- 1:length(tr.vec)
  si_node_names <- nodeNam
  si_node_names[trait_columns] <- trait_column_name
  si_columns <- (length(tr.vec)+1):length(si_node_names)
  si_node_names[si_columns]<-purrr::map_chr(si_node_names[si_columns],~{
    paste0('italic(',.x,')')
  })
  
  # merge sign and number into one
  igraph::E(net_si)$line <- igraph::E(net_si)$weight*igraph::E(net_si)$sign
  ### assign group by interval
  # cut the edge vlaue by given range of interval factor
  # there will be one range merge together (-0.33:0.33)
  ii <- cut(E(net_si)$line, breaks = c(seq(-1,-.33,len = 5),
                                     seq(.33, 1,len = 5)),
            include.lowest = TRUE)
  ## Use bin indices, ii, to select color from vector of n-1 equally spaced colors
  edge_colors_si<- colfunc[ii]
  
  ###### net_trait for yield
  net_trait <- Net[[1]][[i]]
  
  igraph::E(net_trait)$width <- E(net_trait)$weight*8
  # merge sign and number into one
  igraph::E(net_trait)$line <- igraph::E(net_trait)$weight*igraph::E(net_trait)$sign
  trait_node_names <-V(net_trait)$Name
  trait_node_names <- stringi::stri_replace_all_fixed(trait_node_names, 
                                              pattern = c("flowering", "maturity","lai"),
                                              replacement = c("flowring_date", "maturity_date","LAI"),
                                              vectorize_all = FALSE)
  
  trait_node_names[si_columns]<-purrr::map_chr(trait_node_names[si_columns],~{
    paste0('italic(',.x,')')
  })
  trait_node_names[trait_columns]<-purrr::map_chr(trait_node_names[trait_columns],~{
    paste0('bold(',.x,')')
  }) 
  # split the range above and below absolute value from .33 to 1 into 5 groups
  ii <- cut(E(net_trait)$line, breaks = c(seq(-1,-.33,len = 5),
                                     seq(.33, 1,len = 5)),
            include.lowest = TRUE)
  
  breaks <- c(-1,as.numeric(sub("[^,]*,([^]]*)\\]", "\\1", levels(ii))))
  ## Use bin indices, ii, to select color from vector of n-1 equally spaced colors
  edge_colors_trait<- colfunc[ii]
  #start graph
  tiff(filename=paste0(hdir2,'/Paper/',tiffname,'.tiff'),
       type="cairo",
       units="cm",
       compression = "lzw",
       width=16.4,
       height=20,
       pointsize=12,
       res=300,# dpi,
       family="Arial")
  # start two rows plot
  par(mai = c(0.001, 0.1, 0.1, 0.1),
      oma = c(.001, .01, 0.01, 0.01),
      lwd=2, mfrow=c(2,1))
  
  #first row: Trait network #############
  plot(net_trait,
       vertex.label.cex=0.8,
       vertex.label.font=2,
       vertex.size=20,
       vertex.label=parse(text=trait_node_names) ,
       vertex.label.color='black',
       vertex.label.family='Arial',
       edge.color=edge_colors_trait,
       layout=as.matrix(l2))
  
  add_label(0, 0.07, letters[1])
  #second row  SI network #########

  plot(net_si,
       vertex.label.cex=0.8,
       vertex.label.font=2,
       vertex.size=20,
       vertex.label=parse(text=si_node_names) ,
       vertex.label.color='black',
       vertex.label.family='Arial',
       edge.color=edge_colors_si,
       layout=l1)
  
  # legend
  legend(x=1.3, y=.5,
         pch=21,
         c(as.expression(bquote(bold("Trait"))),
           as.expression(bquote(
             bolditalic(P)[bold('i, Trait')]))),
         y.intersp = 2,
         col=NA, pt.bg=alpha(node_colr[1:2],.8),
         pt.cex=3, cex=1, 
         bty="n", ncol=1, text.font=2,
         text.col = '#5a5a5a',
         title.adj=0)
  # subplot label
  add_label(0, 0.07, letters[2])
  
  image.plot( legend.only=TRUE, zlim=c(-1,1),
              col = colfunc,
              breaks = breaks,
              lab.breaks=breaks %>% round(.,2),
              # addjust margin
              smallplot=c(.85,.89, .08,.38)
              # smallplot=c(.02,.06, .08,.44)
  )
  text(1.47,-.15,bquote(bolditalic(r)),cex = 1)

  dev.off()
}
