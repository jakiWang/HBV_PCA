library(dplyr)

load("lifespain_results.rda") #raw data called lifespain.df
load("group_names.rda") #group variable name vector called group_names

names(lifespain.df)[43] <- "Total Fatty Acids"
names(lifespain.df)[50] <- "Acetoacetic acid"
names(lifespain.df)[47] <- "2-hydroxyisobutyric acid"

head(df)
head(group.names)

table(lifespain.df$Timepoint) # count the time point
life.num <- lifespain.df %>% select(where(is.numeric))
dim(life.num)

## exluse all zero column
sum_lifespan <- bind_rows(life.num %>% 
  summarise(across(everything(), mean)) ,
  life.num %>% 
  summarise(across(everything(), sd))) %>% 
  t() %>% 
  `colnames<-`(c("Mean","SD")) # mean ans sd

(zero_var <- rownames(sum_lifespan)[sum_lifespan[,2] == 0]) # zero sd column
life.num.n0 <- life.num %>% select(-zero_var)

## in list form
lifespan.list <- list(length = sum(!is.na(group.names)))

lifespan.list[[1]] <- life.num.n0 %>% select(1:7)
lifespan.list[[2]] <- life.num.n0 %>% select(8:9)
lifespan.list[[3]] <- life.num.n0 %>% select(10:13)
lifespan.list[[4]] <- life.num.n0 %>% select(14:34)
lifespan.list[[5]] <- life.num.n0 %>% select(35:37)
lifespan.list[[6]] <- life.num.n0 %>% select(38)
lifespan.list[[7]] <- life.num.n0 %>% select(39)
lifespan.list[[8]] <- life.num.n0 %>% select(40:54)
lifespan.list[[9]] <- life.num.n0 %>% select(55)
lifespan.list[[10]] <- life.num.n0 %>% select(56:58)
lifespan.list[[11]] <- life.num.n0 %>% select(59:61)
lifespan.list[[12]] <- life.num.n0 %>% select(62:65)

names(lifespan.list) <- group.names[!is.na(group.names)]

## overall correlation

for (p in c("A","B","C")) {
  phase.idx <- lifespain.df$Timepoint == p
  tiff(paste0("./plot/corr_acrossgroup_phase",p,".tiff"),units = "cm",
       width = 17.35, height = 23.35, res=500,pointsize=7)
  corrplot::corrplot(corr = cor(life.num.n0[phase.idx,]), type = "upper")
  dev.off()
  
  lapply(seq_along(lifespan.list)[-13], function(i) {
    tiff(paste0("./plot/corr_",names(lifespan.list)[i],"_phase",p,".tiff"))
    corrplot::corrplot(corr = cor(lifespan.list[[i]][phase.idx,]), type = "upper")
    dev.off()
  })
}

person.idx <- substr(lifespain.df$`Report number`,1,1)
phase <- substr(lifespain.df$`Report number`,2,2)
outcome <- lifespain.df$`clinical group`
