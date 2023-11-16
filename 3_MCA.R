library(FactoMineR)
library(factoextra)
library(dplyr)

cate.data <- do.call(cbind,lifespain_range_cate) %>% select(- "Organic Acids.ASCORBIC ACID_inrange") # subject 2 ommited Organic Acids.ASCORBIC ACID_inrange
names(cate.data) <- gsub("_inrange$","",names(cate.data))

cate.data %>% summary() 
cate.data$people <- lifespain_range$Alcohols$people
cate.data$phase <- lifespain_range$Alcohols$phase
cate.data$outcome <- lifespain_range$Alcohols$outcome


res <- MCA(cate.data[,1:60],graph = F)

summary(res)

get_eigenvalue(res)

# results
var <- get_mca_var(res)

head(var$coord) # what is coordinate
head(var$cos2) # represents the quality of representation for variables on the factor map, var.cos2 = var.coord * var.coord.
head(var$contrib) #(var.cos2 * 100) / (total cos2 of the component).

# scree plot
fviz_screeplot(res, addlabels = TRUE, ylim = c(0, 21))
# group by var
fviz_screeplot(res, addlabels = TRUE)

fviz_contrib(res, choice = "var", axes = 1:3, top = 15)

fviz_mca_var(res, choice = "mca.cor", 
             repel = TRUE, 
             ggtheme = theme_minimal())

fviz_mca_var(res, col.var = "cos2", select.var = list(cos2 = 0.75),
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, 
             ggtheme = theme_minimal())

fviz_mca_var(res, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, 
             ggtheme = theme_minimal()
)

fviz_mca_var(res,
             pointsize = "cos2", 
             select.var = list(cos2 = 0.75),
             repel = TRUE, 
             ggtheme = theme_minimal())

fviz_mca_biplot(res, 
                select.var = list(contrib = 5),
                ggtheme = theme_minimal())



# individual
fviz_mca_ind(res, 
             #label = "none", 
             habillage = as.factor(cate.data$phase), 
             addEllipses = T,
             ggtheme = theme_minimal()) 

fviz_mca_ind(res, 
             #label = "none", 
             habillage = as.factor(cate.data$outcome), 
             addEllipses = T,
             ggtheme = theme_minimal()) 


# eigenvalue

eig <- res$eig[,1] # from package FactoMineR

# adjust according to Huerve's paper
K <- 60
J <- dim(res$call$Xtot)[2]

eig.ori <- ifelse(eig == 0, 0, sqrt(eig)*(K-1)/K + 1/K)

phi <- (K/(K-1)) * (sum(eig.ori^2) - (J-K)/(K*K))

(tau <- eig*100/phi)

  
# 3d plot
rc = var$coord

library(plotly)

p = plot_ly() 
p = add_trace(p, x = rc[,1], y = rc[,2], z = rc[,3],
              mode = 'text', text = rownames(rc),
              textfont = list(color = "red"), showlegend = FALSE) 
p <- config(p, displayModeBar = FALSE)
p <- layout(p, scene = list(xaxis = list(title = colnames(rc)[1]),
                            yaxis = list(title = colnames(rc)[2]),
                            zaxis = list(title = colnames(rc)[3]),
                            aspectmode = "data"),
            margin = list(l = 0, r = 0, b = 0, t = 0))
p$sizingPolicy$browser$padding <- 0
my.3d.plot = p



#calculate dim

res <- MCA(cate.data[,1:60],ncp = 9, graph = F)
var <- get_mca_var(res)

var.matrix <- as.matrix(res$call$Xtot)
coef <- var$coord
new.ds <- var.matrix %*% coef


# evaluation
set.seed(123)
train.idx <- sample(1:nrow(cate.data),round(nrow(cate.data)*0.6))

train.ds <- data.frame(new.ds[train.idx,])
train.ds$outcome <- factor(cate.data$outcome[train.idx],levels = c("No flare","Flare","Flare+Sloss"),labels = c(1,2,3))
train.ds$outcome <- relevel(train.ds$outcome,ref = 1)

library(nnet)
mod <- multinom(outcome ~ .,data = train.ds) # outcome 3 vs outcome 1; outcome 2 vs outcome 1
summary(mod)

coef(mod)
pred <- predict(mod,newdata = data.frame(new.ds[-train.idx,]))
true <- factor(cate.data$outcome[-train.idx],levels = c("No flare","Flare","Flare+Sloss"),labels = c(1,2,3))
cbind(true,pred)
