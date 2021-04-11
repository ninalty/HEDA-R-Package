
# NMDS Analysis -----------------------------------------------------------
# Aim for stress < 0.1
# k - number of dimensions
# distDf is a distance matrix (for example, euclidean distance)
nmds <- metaMDS(distDf, k=2, try=100, maxit = 2000, plot=FALSE, 
                 autotransform = FALSE, noshare = FALSE)



# Principle Component Analysis --------------------------------------------
# rescale_df is a rescaled (0 to 1) dataframe
pcVectors <- function(rescale_df) {
  pc_analysis<-prcomp(rescale_df) # performs principal component analysis
  env_vectors<-envfit(pc_analysis, rescale_df) # fits vectors of variables
  return(list(env_vectors, pc_analysis))
}

pcOut <- pcVectors(rescale_df = rescaleDf)
envVectors <- pcOut[[1]]
pcaAnalysis <- pcOut[[2]]

# Plots NMDS with site names
x <- nmds$points[,1]
y <- nmds$points[,2]
plot(x,y, xlab="NMDS1", ylab="NMDS2", type="n", pch=16) 
points(x,y,pch=16,cex=.8)
plot(envVectors,col=c("black"),cex=.8, labels = nmdsLabels)



# Classification Tree -----------------------------------------------------

# Run cart analysis
library(vegan)
set.seed(84) # To remain consistent typically set this this to the same number always
# Create formula for classification tree 
# (Cluster groups) ~ (are a function of these attributes)
# cartAtts - which attributes you want the classification tree to split by
# dataDf - the dataframe with site data
# groups - a vector with the cluster groups corresponding to site order
cartAtts <- c() # insert list of attribute names
cartFormula <- paste("groups", "~", paste(cartAtts, collapse = "+"))
cartOut <- rpart(as.formula(cartFormula),
                 data=dataDf, method="class",minsplit=3)

# Plot figure to help choose appropriate number of branches
plotcp(cartOut)

# Enter value for appropriate number of branches
cpValue <- 0.021 #readline("CP value: ")

# Prune tree to appropriate number of branches
prune_cart<-prune(cartOut, cpValue)

# Evaluate misclassification rate
pred_cart <- predict(prune_cart, dataDf, type=c("class"))
misclass <- cbind(groups, pred_cart)
perc_class <- nrow(misclass[misclass[,1]==misclass[,2],]) / 
  length(misclass[,1]) * 100
cat("Percent correctly classified =", perc_class, "\n")

# Calculate cross-validation percentage
rootNE <- cartOut$frame[1, 'dev']/cartOut$frame[1, 'n']
cpDf <- as.data.frame(cartOut$cptable)
for (i in 2:nrow(cpDf)) {
  if (cpDf$CP[i] < cpValue & cpDf$CP[i-1] > cpValue) {
    xvalError <- cpDf$xerror[i]
  }
}
cartCrossVal <- round((1-rootNE*xvalError)*100, digits=1)
cat("Cross-validation percentage =", cartCrossVal, "\n")

# Print cart performance statistics
printcp(cartOut, digits = 3)

# Plot tree
library(rpart.plot)
rpart.plot(x = prune_cart, type = 0, extra=0, tweak = 1, box.palette = 0, 
           roundint = FALSE)


