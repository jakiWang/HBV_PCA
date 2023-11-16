# make a var name dictionary
cate.ncol <- do.call(cbind,lapply(lifespain_range_cate, ncol))
cate.namecol <- names(lifespain_range_cate)
var_name_raw <- gsub("_inrange$","",do.call(c,lapply(lifespain_range_cate, colnames)))
grp_var <- data.frame(grp_name = rep(cate.namecol,cate.ncol),var_name_raw)
grp_var$var_name <- paste0(grp_var$grp_name,".",var_name_raw)

var_name.list <- list()
var_name.list <- lapply(seq_along(level.list), function(i){
  var_name <- names(level.list)[[i]]
  dum_var_name <- level.list[[i]]
  data.frame(var_name = rep(var_name,length(dum_var_name)),dum_var_name)
})
var_vardum <- do.call(rbind,var_name.list)

var_name.dictionary <- var_vardum %>% left_join(grp_var, by = "var_name") 
# variable name dictionary created, dum_var_name - summary var name, grp_name - group name, var_vame_raw - var name

# single variable group
idx <- is.na(var_name.dictionary$var_name_raw)
var_name.dictionary$var_name_raw[idx] <- var_name.dictionary$grp_name[idx] <- var_name.dictionary$var_name[idx]

head(var_name.dictionary)

# lipid group
lipid <- data.frame(var_name = c("Lipids.CHOL","Lipids.HDLC","Lipids.LDLC","Lipids.TRIG"),dum_var_name = rep(NA,4), grp_name = rep("Lipids",4), var_name_raw = c("CHOL","HDLC","LDLC","TRIG"))

var_name.dictionary <- rbind(var_name.dictionary,lipid)
var_name.dictionary$var_name_up <- toupper(var_name.dictionary$var_name)

# PLOT!
ds.plt.raw <- data.frame(var$cos2) %>% mutate(dum_var_name = rownames(var$coord)) %>% 
  left_join(var_name.dictionary,by = "dum_var_name")

names(ds.plt.raw)


library(ggplot2)
library(ggrepel)

ggplot(ds.plt.raw,aes(x = Dim.1, y = Dim.2, 
                      color = grp_name,label = var_name_raw))+
  geom_point()+
  geom_label_repel(box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') 

  #stat_ellipse()
