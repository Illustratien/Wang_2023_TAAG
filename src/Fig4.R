rm(list = ls())
pacman::p_load(scales,ggplot2,magrittr,dplyr)
# dir ---------------------------------------------------------------------
src_dir <-dirname(rstudioapi::getSourceEditorContext()$path)
dat_dir <- sub('src.*','data',src_dir)
res_dir <- sub('src.*','result',src_dir)

# function ----------------------------------------------------------------
source(paste0(src_dir,"/TC_theme2.R"))

lookup<-function(ori.df,ori.id.vec,look.df,ldf.tar,ldf.id){
  # rename the column of data frame (ori.df) based on reference table (look.df)
  # input
  #dataframe with old name
  # output
  # data frame with new name 
  
  #find the target vector as reference
  tar.vec<-look.df[[ldf.tar]]
  # rename the column based on the match result of reference
  ori.df[,ori.id.vec]<-apply(ori.df[,ori.id.vec],2,
                             function(x){tar.vec[match(x, look.df[[ldf.id]])]})
  return(ori.df)
}
# trait vector -------------------------------------------------------------------------
#ouput stability index dataframe
Link.comp <- readRDS(paste0(res_dir,'/Networkdata/Fig4_method_compare_link.rds'))
# to generate the node name dataframe
nodenam <- readRDS(paste0(res_dir,'/Networkdata/Fig4_method_compare_nodenam.rds'))


pp.vec <- c("y_rue","y_sla","ll_modifier",
            "potential_grain_filling_rate","node_no_correction","tfac_slope")

source.vec <- c('even','top',"random")


physiological.parameter.df <-  Link.comp %>%
  dplyr::filter(.,weight>0.33) %>% 
  dplyr::group_by(from,to,Source) %>% 
  dplyr::summarise(n = dplyr::n(),
                   av.w=mean(weight),
                   sd.w=sd(weight))%>% 
  # replace the name 
  lookup(.,c("from","to"),
         nodenam,"Name","Id") %>% 
  dplyr::group_by(to,Source) %>% 
  # use the sum of two trait as sorting column
  dplyr::mutate(sor=sum(n)) %>% 
  dplyr::arrange(desc(sor),from,to) %>% 
  # split by sampling method
  split(.,.$Source) %>% 
  # select the top 5 physiological parameters based on the 
  purrr::map_dfr(.,~{
    # for each source based on the mean r between nodes, select top 5
    dplyr::arrange(.x,desc(av.w)) %>% 
      .[1:5,]
  })

physiological.parameters<-unique(physiological.parameter.df$to) %>% 
  .[!.%in%c('Pi yield','yield')]

# calculate ratio of r to yield and r to si
wides <-dplyr::filter(Link.comp,G%in%c('TP','SP')) %>% 
  dplyr::select(c("weight","to","from","Net",'Source')) %>% 
  reshape(.,
          v.names = "weight", 
          idvar = c("to","Net",'Source'),
          timevar = "from", direction = "wide") %>% 
  dplyr::group_by(to,Net) %>% 
  dplyr::mutate(ratio=weight.2/weight.1) %>% 
  lookup(.,c("to"),nodenam,"Name","Id")

sub6trait<-dplyr::filter(wides,to %in% physiological.parameters)

merge.data <- readRDS(paste0(res_dir,'/Networkdata/figure4_pp_geno.rds')) %>% 
  dplyr::filter(.,PP%in%pp.vec) %>% 
  mutate(pp.value =as.numeric(PP.value)) %>% 
  group_by(seed_number,source,PP)%>%
  summarise(mp=mean(pp.value),
            my=mean(yield),
            mpi=mean(pi.yield)) %>% 
  mutate(Net=paste0("100_",seed_number) ) %>% 
  rename("to"="PP", "Source"="source") %>% 
  # merge
  left_join(sub6trait,.,by=c("Net","Source","to")) %>% 
  mutate(to=factor(to,levels = pp.vec),
         Source=sub("top","top 20",Source) %>% 
                factor(.,levels = c("even","random","top 20")))

# Fig4 ---------------------------------------------------------------------

low = "#F21A00"
mid = "#00980b"
high = "#3498db"
ylab1<-bquote(bold("|"~bolditalic(r)~"|"~between~physiological~parameter~and)~bolditalic(P)[bold(i*", yield")])
xlab1<-bquote(bold("|"~bolditalic(r)~"|"~between~physiological~parameter~and)~bold(yield))

label_text <- data.frame(l=letters[1:18],weight.1=0.05,weight.2=.95,mp=-5,
                         expand.grid(to=merge.data$to %>% levels(),
                                     Source=merge.data$Source %>% levels()))
# Fig4-------------------------------------------------------------------------

fig4 <- ggplot(merge.data,
               aes(x=weight.1,y=weight.2,color=mp))+
  # create region for network selection
  annotate("rect", xmin = .33, xmax = 1,
           ymin = .33, ymax = 1,
           alpha = .2)+
  geom_point(size=1,alpha=.4)+
  geom_abline(intercept = 0, slope = 1,linetype = "dashed")+
  ggpmisc::stat_poly_eq(aes(label = after_stat(eq.label)),
                        coef.digits = 3,show.legend = FALSE,label.x= .95,label.y=.05,size=2) +
  ggpmisc::stat_poly_eq(show.legend = FALSE,label.x= .95,label.y=.2,size=2) +
  labs(x=xlab1,y=ylab1)+
  scale_x_continuous(limits = c(0,1),labels = c(0,0.25,0.5,.75,1))+
  scale_y_continuous(limits = c(0,1))+
  
  scale_color_gradient2("Mean value of physiological parameter",
                        low = low, mid = mid, high = high,
                        midpoint = 1, limits = c(0.92, 1.08))+
  
  guides(color = guide_colourbar(barwidth = 10, barheight = .5,title.vjust = .8))+
  TC_theme2(ax.tit.siz = 10,strp.txt.siz = 5,ax.txt.siz = 7,
            lgd.tit.siz = 8,lgd.txt.siz = 8,
            b=1,legend.position='bottom')+
  theme(strip.text.x = element_text( face = "bold.italic"),
        strip.text.y = element_text(face = "bold"))+
  facet_grid(Source~to)+
  geom_text( data=label_text,aes(label = l,x=weight.1,y=weight.2))


tiff(filename=paste0(res_dir,'/Fig.4.tiff'),
     units="cm",
     width=17.4,
     height=12,
     compression = "lzw",
     pointsize=12,
     res=330)

fig4
dev.off()

