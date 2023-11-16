cont.data <- do.call(cbind,lifespan.list)
cont.data$people <- lifespain_range$Alcohols$people
cont.data$phase <- lifespain_range$Alcohols$phase
cont.data$outcome <- lifespain_range$Alcohols$outcome

dim(cont.data)

res.pca <- PCA(cont.data[,1:65],graph = FALSE)
var <- get_pca_var(res.pca)

fviz_eig(res.pca, addlabels=TRUE, hjust = -0.3)+
  theme_minimal()

fviz_pca_var(res.pca,  
             repel = TRUE, 
             geom = c( "text"),
             ggtheme = theme_minimal())

fviz_contrib(res.pca, choice="var", axes = 1:2)+
  labs(title = "Contributions to Dim 1+2")

fviz_pca_ind(res.pca, repel = TRUE, col.ind = "cos2",habillage = as.factor(cate.data$phase), 
             addEllipses = T,
             ggtheme = theme_minimal())


# 3d plot
rc <- get_pca_var(res.pca)$coord
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
my.3d.plot

# pred
library(nnet)
var <- get_pca_var(res.pca)
var$cos2

attributes(as.matrix(cate.data[,1:60]))
attributes(var$coord)

pc.data <- as.matrix(cate.data[,1:60]) %*% var$coord

dt.fit <- data.frame(pc.data,outcome = cate.data$outcome)
fit <- multinom(outcome ~ Dim.1 + Dim.2 + Dim.3 + Dim.4 + Dim.5, data = dt.fit)

table(predict(fit) == dt.fit$outcome)
hist(residuals(fit)[,3])

