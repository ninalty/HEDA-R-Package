# Nonparametric statistics ------------------------------------------------

### Dunn analysis
# Herve's function
SignificativeDiffBoxPlot <- function(melted, type = "Wilcox", padjmeth = "holm"){
  stat.test <- switch(type, 
                      "Dunn" = melted %>% dunn_test(value ~ groups, p.adjust.method = padjmeth),
                      "Tukey" = melted %>% tukey_hsd(value ~ groups, p.adjust.method = padjmeth),
                      "Wilcox" = melted %>% wilcox_test(value ~ groups, p.adjust.method = padjmeth)
  )
  stat.test <- merge(stat.test, summarize_at(melted, "value", max) %>% 
                       dplyr::rename(y.position = value))
  p <- ggboxplot(melted,  #check the liba
                 x = "groups", 
                 y = "value", 
                 color = "groups", 
                 facet.by = "variable",
                 scales = "free",
                 title = paste(type, padjmeth, sep = ", ")) +
    stat_pvalue_manual(stat.test, 
                       label = "p.adj.signif", 
                       step.increase = 0.1,
                       step.group.by = "variable", 
                       hide.ns = TRUE)
  return(list(p = p, stats = stat.test))	
}

data_mat <- # Enter dataframe with original data here
statAtts <- # Enter variables/attributes that are used in clustering
groups <- as.factor(groups) # change groups to cluster groups

statDf <- data_mat[,statAtts]
statDf <- cbind(statDf, groups)

expList <- statAtts # Could change if you want different labels than statAtts

melted <- melt(statDf, id.vars = "groups") %>% group_by(variable)

dt <- SignificativeDiffBoxPlot(melted, type = "Dunn")
dunnStats <- as.data.frame(dt$stats)

plot(dunnStats$p)

# If the above plot is enough, the code below isn't necessary

#### Determine level of significance

numGroups <- max(as.numeric(groups))
numAtts <- length(statAtts)

statDfCreate <- function(colHeaders, numRows, numCols) {
  outDf <- data.frame(matrix(ncol = numCols, nrow = numRows),
                      stringsAsFactors = FALSE)
  colnames(outDf) <- colHeaders
  return(outDf)
}

stat_groups <- statDfCreate(statAtts, numGroups, numAtts)
stat_category <- statDfCreate(statAtts, numGroups, numAtts)
stat_color <- statDfCreate(statAtts, numGroups, numAtts)

groups_stat <- statDfCreate(statAtts, length(groups), numAtts)

for (att_name in statAtts) {
  
  dunnAtt <- dunnStats[dunnStats$variable==att_name,]
  
  for (k in 1:numGroups) {
    
    dunnAtt$group1 <- as.numeric(dunnAtt$group1)
    dunnAtt$group2 <- as.numeric(dunnAtt$group2)
    
    k.result <- dunnAtt[dunnAtt$group1==k | dunnAtt$group2==k,]
    
    k.sig <- nrow(k.result[k.result$p.adj <= 0.05,])
    
    k.sig.nums <- c(k.result$group1[k.result$group2==k & k.result$p.adj<0.05],
                    k.result$group2[k.result$group1==k & k.result$p.adj<0.05])
    
    stat_groups[k, att_name] <- paste(k.sig.nums, collapse = ",")
    
    if (k.sig > 1) {
      stat_category[k, att_name] <- "Multiple"
      stat_color[k, att_name] <- "#998ec3"
    } else if (k.sig == 1) {
      stat_category[k, att_name] <- "One"
      stat_color[k, att_name] <- "#f1a340"
    } else {
      stat_category[k, att_name] <- "None"
      stat_color[k, att_name] <- "#f7f7f7"
    }
  }
  
  for (n in 1:length(groups)) {
    
    n.grp <- as.numeric(as.character(groups[n]))
    
    groups_stat[n, att_name] <- stat_category[n.grp, att_name]
    
  }
  
}

# Box and whisker plots ---------------------------------------------------

bp <- list()

for (j in 1:ncol(data_mat)) {
  
  att_name <- names(data_mat[j])
  f <- paste(att_name, "~", "groups")
  bp.fill <- stat_color[,att_name]
  # if (j != 8) {
  
  bp[[j]] <- ggplot(data_mat, aes_string(x = "groups", y = att_name)) +
    geom_boxplot(fill = bp.fill) +
    xlab("Group") +
    ylab(expList[j]) +
    theme(legend.position="none", 
          axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          panel.background = element_blank())
}

library(gridExtra)
grid.arrange(grobs = bp, ncol=3)

