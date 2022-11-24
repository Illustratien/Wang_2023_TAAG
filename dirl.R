rm(list = ls())

pacman::p_load(data.tree,plyr,magrittr)
dir <- dirname(rstudioapi::getSourceEditorContext()$path)

pre <- sub("R_script","",dir)

path <- c(
  list.dirs(dir) %>% .[!grepl("Rproj.user|.git",.)],
  list.files(paste0(dir,"/src"),full.names = T),
  list.files(paste0(dir,"/result"),full.names = T,recursive = T),
  paste0(dir,"/Wang_2022_TAAG.Rproj"),
  paste0(dir,"/README.md"),
  paste0(dir,"/data/Wang_et_al_TAAG_2022_output_trait.rds"),
  paste0(dir,"/data/Wang_et_al_TAAG_2022_physiological_parameter.rds")
) %>% gsub(pre,"",.)


x <- lapply(strsplit(path, "/"), function(z) as.data.frame(t(z))) %>% 
  rbind.fill() 

x$pathString <- apply(x, 1, function(x) paste(trimws(na.omit(x)), collapse="/"))

(mytree <- data.tree::as.Node(x))

