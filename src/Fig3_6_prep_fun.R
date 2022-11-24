node.nam<- function(trait.vector,
                    stable_table,
                    physio.para)
{
  # Input
  # triat.vector: a vector used to indicate the   
  # stable_table : dataframe of SI from toolStability
  # physio.para : dataframe contains physiological parameters

  # output
  # dataframe with node name 
  
  trait.numb <- length(trait.vector)
  trait.id <- 1:trait.numb
  width.weight <- 5 #the weight that the node to be amplified at the end 
  
  # stability indices index
  
  si.id <- seq((trait.numb+2),(2*trait.numb)+1)
  
  # physiological parameters index (escape genotype) 
  para_id <- 1:(dim(physio.para)[2]-1)
  # build node
  node0 <- data.frame(
    # index
    Id=seq(1,(2*trait.numb+length(para_id))),
    # full name 
    Name=c(trait.vector,names(stable_table)[si.id],names(physio.para)[para_id]),
    # group
    Group=c(rep(1,trait.numb),rep(2,trait.numb),rep(3,length(para_id)))
  )
  
  return(node0)
}

link<- function(trait.vector,
                stable_table,
                physio.para,
                node.condi,
                thresh.additional=NULL)
{
  # Generate link from network object
  # triat.vector: a vector used to indicate the traits  
  # stable_table : SI table from toolStability
  # physio.para : physiological parameters
  # node.condi : pass from rlang::quo(), a condition variable to filter the unwanted node
  
  trait.numb <- length(trait.vector)
  trait.id <- 1:trait.numb
  width.weight <- 5 #the weight that the node to be amplified at the end 
  
  # stability indices index
  si.id <- seq((trait.numb+2),(2*trait.numb)+1)
  
  # physiological parameters index (escape genotype) 
  para_id <- 1:(dim(physio.para)[2]-1)
  # build node
  node0 <- data.frame(
    # index
    Id=seq(1,(2*trait.numb+length(para_id))),
    # full name 
    Name=c(trait.vector,names(stable_table)[si.id],names(physio.para)[para_id]),
    # group
    Group=c(rep(1,trait.numb),rep(2,trait.numb),rep(3,length(para_id)))
  )
  # node_table$Env <- 
  
  # combine trait, stability indices and physiological parameters 
  table <- dplyr::inner_join(stable_table,physio.para,by='genotype')
  uniform <- table[,-which(names(table) =='genotype')]
  if(testit::has_warning(cor(uniform))){
    stop('uniform!')
  }
  # calculate corelation matrix
  cormatrx <-cor(table[,-which(names(table) =='genotype')]) 
  #copy and replace the lower triangle to NA
  up <- cormatrx
  up[lower.tri(up,diag = T)] <- NA
  # transform the upper triangle into linear by row
  line <- as.data.frame(na.omit(as.vector(t(up))))
  
  colnames(line) <- 'weight'
  # label correlation coefficient by sign
  line$sign <- ifelse(line$weight>0,1,-1)
  #generate from-to index 
  mat <- as.data.frame(t(combn(dim(node0)[1],2)))
  colnames(mat) <- c('from','to')
  # combine from-to index and correlation 
  link <- cbind(mat,line)
  # label from, trait, stability indices and parameters for T,S and P 
  link$fL<- cut(link$from,breaks = c(0,trait.numb,2*trait.numb,Inf),
                labels=c('T','S','P'))
  if (trait.numb>1){
    # label to 
    link$tL<- cut(link$to,breaks = c(1,trait.numb,2*trait.numb,Inf),
                  labels=c('T','S','P'))
  }else if (trait.numb==1){
    link$tL<- cut(link$to,breaks = c(1,trait.numb+1,Inf),
                  labels=c('S','P'))
  }  
  # mix from to label
  link$G <- paste0(link$fL,link$tL)
  #select specific S & T combinations
  link$dis <- link$to-link$from
  # turn the weight into positive
  link$weight <- abs(link$weight)
  # only select the corresponding relation ship between trait and stability indices
  sub.link <- dplyr::filter(link,
                            !!node.condi)
  if (!is.null(thresh.additional)){
    condi.vec <- with(sub.link,which(!G%in%c("SP","TP")|(G%in%c("SP","TP")&weight>thresh.additional)))
    sub.link <- sub.link[condi.vec,]
  }
  return(sub.link)
}
multi_trait_net_prep1<- function(thresh,
                                 node.condi,
                                 trait.vector,
                                 stable_table,
                                 physio.para,
                                 node_colr=NULL,
                                 index_id=NULL,
                                 thresh.additional=NULL)
{
  # Generate network object
  # thresh : threshold to filter the link(correlation) below the threshold
  # node.condi : pass from rlang::quo(), a condition variable to filter the unwanted node
  # triat.vector: a vector used to indicate the traits  
  # stable_table : SI table from toolStability
  # physio.para : physiological parameters
  # thresh.additional: additional thresh hold condition for the display purpose
  
  # color settinig
  
  if(is.null(node_colr)){
    if (is.null(thresh.additional)){ # Fig3
      node_colr <- c('#A58AFF','#1972f0','#FFBE00')
    }else{ # Fig6
      node_colr <- c('#D91E18','#3161A3','#FFBE00')
    }
    
  }
  
  node_rgb <- col2rgb(node_colr)
  node_clr <- rgb(node_rgb[1,],
                  node_rgb[2,],
                  node_rgb[3,],
                  max=255,
                  alpha=80*255/100)
  
  
  #trait number and index 
  trait.numb <- length(trait.vector)
  trait.id <- 1:trait.numb
  width.weight <- 5 #the weight that the node to be amplified at the end 
  
  # stability indices index, for assessing the SI columns
  if (is.null(index_id)){
    si.id <- seq((trait.numb+2),(2*trait.numb)+1)
  }else{si.id <- index_id}
  
  # physiological parameters index (escape genotype) 
  para_id <- 1:(dim(physio.para)[2]-1)
  
  # build node
  node0 <- data.frame(
    # index
    Id=seq(1,(2*trait.numb+length(para_id))),
    # full name 
    Name=c(trait.vector,names(stable_table)[si.id],names(physio.para)[para_id]),
    # group
    Group=c(rep(1,trait.numb),rep(2,trait.numb),rep(3,length(para_id)))
  )
  # node_table$Env <- 
  
  # combine trait, stability indices and physiological parameters 
  table <- dplyr::inner_join(stable_table,physio.para,by='genotype')
  uniform <- table[,-which(names(table) =='genotype')]
  # check of correlation uniformity
  if(testit::has_warning(cor(uniform))){
    stop('uniform!')
  }
  # calculate corelation matrix
  cormatrx <-cor(table[,-which(names(table) =='genotype')]) 
  #copy and replace the lower triangle to NA
  up <- cormatrx
  up[lower.tri(up,diag = T)] <- NA
  # transform the upper triangle into linear by row
  line <- as.data.frame(na.omit(as.vector(t(up))))
  
  colnames(line) <- 'weight'
  # label correlation coefficient by sign
  line$sign <- ifelse(line$weight>0,1,-1)
  #generate from-to index 
  mat <- as.data.frame(t(combn(dim(node0)[1],2)))
  colnames(mat) <- c('from','to')
  # combine from-to index and correlation 
  link <- cbind(mat,line)
  # label from, trait, stability indices and parameters for T,S and P 
  link$fL<- cut(link$from,breaks = c(0,trait.numb,2*trait.numb,Inf),
                labels=c('T','S','P'))
  if (trait.numb>1){
    # label to 
    link$tL<- cut(link$to,breaks = c(1,trait.numb,2*trait.numb,Inf),
                  labels=c('T','S','P'))
  }else if (trait.numb==1){
    link$tL<- cut(link$to,breaks = c(1,trait.numb+1,Inf),
                  labels=c('S','P'))
  }  
  # mix from to label
  link$G <- paste0(link$fL,link$tL)
  #select specific S & T combinations
  link$dis <- link$to-link$from
  # turn the weight into positive
  link$weight <- abs(link$weight)
  # only select the corresponding relation ship between trait and stability indices
  sub.link <- dplyr::filter(link,
                            weight>thresh,
                            !!node.condi)
  
  if(dim(sub.link)[1]==0){
    stop('Your condition might be wrong,\n  there is no link match your conditions')}
  if (!is.null(thresh.additional)){
    # exclude PT or ST that smaller than threshold, otherwise, it will be too much for display
    condi.vec <- with(sub.link,which(!G%in%c("SP","TP")|(G%in%c("SP","TP")&weight>thresh.additional)))
    sub.link <- sub.link[condi.vec,]
  }
  
  node <- node0[unique(c(sub.link$from,sub.link$to)),]
  node <- node[order(node$Id),]
  # buildinig node
  net <- igraph::graph.data.frame(sub.link,node, directed=F) 
  
  igraph::V(net)$color <- node_clr[V(net)$Group]
  igraph::V(net)$frame.color <- NA
  igraph::E(net)$width <- E(net)$weight*width.weight
  return(net)
}


add_label <- function(xfrac, yfrac, label, pos = 4, ...) {
  # add character label for subplot
  # for adding text in the graph *from web
  # https://seananderson.ca/2013/10/21/panel-letters/
  u <- par("usr")
  x <- u[1] + xfrac * (u[2] - u[1])
  y <- u[4] - yfrac * (u[4] - u[3])
  text(x, y, label, pos = pos,font=2,cex=2,...)
}
