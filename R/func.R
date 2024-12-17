
### Pearl Ante-Testard


### Functions
### This is based on the article by Belias et al. (2022): https://onlinelibrary.wiley.com/doi/10.1002/jrsm.1546

### The following function will calculate the risk differences between interventions
### The output is a data.frame with the risk differences

risk.diff.creator =  function(dataframe = NULL,          ### Provide the data-set with the predicted outcomes
                              treatment = NULL,       ### Provide the name of the intervention variable
                              matching.variables=  NULL, ### Provide the names of any other predictors 
                              outcome= NULL,             ### Provide the name of the outcome 
                              predicted.outcome = NULL,  ### Provide the name of the predicted outcome
                              predicted.CI = NULL){      ### Provide the name of the confidence intervals of the predicted outcome
  
  
  
  dataframe = dataframe[,c(outcome,treatment,matching.variables, predicted.outcome, predicted.CI)]
  
  
  
  split= dataframe%>%
    group_by(!!as.name(treatment) )%>%
    group_split()
  
  
  split.df= full_join(x = split[[1]],y = split[[2]], by= matching.variables  )
  
  
  
  p1 =  as.matrix(split.df[, paste(predicted.outcome,"y",sep = ".")])
  p2 =  as.matrix(split.df[, paste(predicted.outcome,"x",sep = ".")])
  l1 =  as.matrix(split.df[, paste(predicted.CI[1],"y",sep = ".")])
  l2 =  as.matrix(split.df[, paste(predicted.CI[1],"x",sep = ".")])
  u1 =  as.matrix(split.df[, paste(predicted.CI[2],"y",sep = ".")])
  u2 =  as.matrix(split.df[, paste(predicted.CI[2],"x",sep = ".")])
  
  split.df$fit.diff = p1 - p2 
  
  split.df$diff.lower =  split.df$fit.diff - sqrt((p1-l1)^2 + (u2-p2)^2)
  
  split.df$diff.upper =  split.df$fit.diff +  sqrt((p2-l2)^2 + (u1-p1)^2)
  
  split.df= as.data.frame(split.df)
  return(split.df)
  
  
}



#### expit function to back-transform from logit scale

expit<-function(rs) {1/(1+exp(-rs))}


#### The following function is an assisting function that performs a pointwise meta-analysis per X
#### Note that in order to calculate the predicted outcome correctly you need to have matching IWI 
#### values in at least 2 Studies.
####
#### The output is a data-set with the results of 3 meta-analyses (Fixed effects, Random effects and Random effects with HKSJ correction)
#### You can choose based on your assumptions which method you wish to apply for your meta-analysis.


pointwise.ma <- function(data, 
                         clustering.variable = "study", 
                         combining.variables = NULL, 
                         predicted.outcome = NULL, 
                         predicted.outcome.se = NULL, 
                         predicted.outcome.CI = c("lower", "upper"), 
                         tau.method = "REML") {
  
  # Split the data by the clustering variable
  split <- data %>%
    group_by(!!as.name(clustering.variable)) %>%
    group_split()
  
  # Custom merge function
  merge_custom <- function(x, y) {
    merge(x, y, by = combining.variables, all = TRUE, no.dups = TRUE)
  }
  
  # Merge all data frames in the split list
  data <- Reduce(merge_custom, split)
  
  # Initialize columns for meta-analysis results
  meta_columns <- c("FE.meta", "FE.meta.upper", "FE.meta.lower", "FE.se",
                    "RE.meta", "RE.meta.upper", "RE.meta.lower", "RE.se",
                    "RE.meta.HKSJ", "RE.meta.HKSJ.upper", "RE.meta.HKSJ.lower", "RE.HKSJ.se",
                    "Q", "H", "H.HKSJ", "Q.HKSJ", "tau", "pval.Q")
  data[meta_columns] <- NA
  
  # Perform meta-analysis for each row
  for (i in seq_len(nrow(data))) {
    TE <- data[i, ] %>%
      select(contains(predicted.outcome)) %>%
      t() %>%
      na.omit() %>%
      as.vector()
    
    if (!is.null(predicted.outcome.se)) {
      seTE <- data[i, ] %>%
        select(contains(predicted.outcome.se)) %>%
        t() %>%
        na.omit() %>%
        as.vector()
      
      meta1 <- metagen(TE = TE, seTE = seTE, method.tau = tau.method, hakn = TRUE, adhoc.hakn = "ci")
      meta2 <- metagen(TE = TE, seTE = seTE, method.tau = tau.method, hakn = FALSE)
      
    } else if (!is.null(predicted.outcome.CI)) {
      lower <- data[i, ] %>%
        select(contains(predicted.outcome.CI[1])) %>%
        t() %>%
        na.omit() %>%
        as.vector()
      
      upper <- data[i, ] %>%
        select(contains(predicted.outcome.CI[2])) %>%
        t() %>%
        na.omit() %>%
        as.vector()
      
      meta1 <- metagen(TE = TE, lower = lower, upper = upper, method.tau = tau.method, hakn = TRUE, adhoc.hakn = "ci")
      meta2 <- metagen(TE = TE, lower = lower, upper = upper, method.tau = tau.method, hakn = FALSE)
    }
    
    # Store the Heterogeneity measures
    data[i, c("Q", "Q.HKSJ", "H", "H.HKSJ")] <- c(meta2$Q, meta1$Q, meta2$H, meta1$H)
    
    # Store the Fixed-effects model results
    data[i, c("FE.meta", "FE.meta.upper", "FE.meta.lower", "FE.se")] <- c(meta2$TE.fixed, meta2$upper.fixed, meta2$lower.fixed, meta1$seTE.fixed)
    
    # Store the Random-effects model results
    data[i, c("RE.meta", "RE.meta.upper", "RE.meta.lower", "RE.se")] <- c(meta2$TE.random, meta2$upper.random, meta2$lower.random, meta2$seTE.random)
    
    # Store the Random-effects model with HKSJ results
    data[i, c("RE.meta.HKSJ", "RE.meta.HKSJ.upper", "RE.meta.HKSJ.lower", "RE.HKSJ.se")] <- c(meta1$TE.random, meta1$upper.random, meta1$lower.random, meta1$seTE.random)
    
    # Store tau and p-value for Q
    data[i, c("tau", "pval.Q")] <- c(meta2$tau2, meta2$pval.Q)
    
    # Print progress
    if (round(i * 100 / nrow(data)) != round((i - 1) * 100 / nrow(data))) {
      cat("\014")
      print(paste0(round(i * 100 / nrow(data)), "%"))
    }
  }
  
  return(data)
}


