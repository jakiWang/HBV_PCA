head(var_name.dictionary) # from 3_1 var by group
ds.plt.raw <- data.frame(var$coord) %>% mutate(var_name_up = toupper(rownames(var$coord))) %>% 
  left_join(var_name.dictionary %>% select(-dum_var_name) %>% unique(), by = "var_name_up")

nrow(ds.plt.raw)

library(ggplot2)
library(ggrepel)

ggplot(ds.plt.raw,aes(x = Dim.1, y = Dim.2, 
                      color = grp_name,label = var_name_raw))+
  geom_point()+
  geom_label_repel(box.padding   = 0.35, 
                   segment.color = 'grey50') +
  labs(title = "coord")+

  stat_ellipse()
