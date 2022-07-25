################################################################################
#
# Analysis script for the manuscript
# "NFC and burnout in teachers - A replication and extension study"
# by Zerna, J., Engelmann, N., Strobel, A., & Strobel., A. (2022)
#
# Corresponding author: Josephine Zerna (josephine.zerna@tu-dresden.de)
#
# Please download the repsitory from github.com/JZerna/NFC-Burnout
#
#
################################################################################

##### Setup ####################################################################

  # load here-library for path-independent project structure (install here by calling install.packages("here") if necessary)
  # set top level directory to source file
  
  library(here)
  
  here::i_am("flag_top_level_NFCBurnout.txt")
  
  # load reproducible environment library to get all packages with correct versions form the renv.lock file
  # install renv by calling install.packages("renv") if necessary
  
  #library(renv)

  # the required packages are:
  # "bibtex", "here", "tidyverse", "bayestestR", "papaja", "lavaan", "psych", "MVN", "MBESS",
  # "kableExtra", "Hmisc", "semPlot", "ggplot2", "knitr", "ppcor", "stats", "apa", "GGally"
  
  # Seed for random number generation
  set.seed(13)
  knitr::opts_chunk$set(cache.extra = knitr::rand_seed)
  
  # Set a number for the bootstrapping procedures
  bootnum <- 5000
  
##### Data import ##############################################################
  
  # Load the data file
  
  base::load(here("Data", "data.Rda"))
  
  # Load the internal consistencies (bootstrapping has been suppressed to save time on knitting)
  consistencies <- readRDS(here("Data", "consistencies.rds"))
  outlierconsistencies <- readRDS(here("Data", "outlierconsistencies.rds"))
  
  # create a vector of consistencies to be used as the diagonal in a table later
  consist_diag <- cbind(format(round(t(consistencies[2,2:11]), digits = 2), nsmall = 2), rep("(",10),
                        format(round(t(consistencies[5,2:11]), digits = 2), nsmall = 2), rep(")",10))
  # paste together to create the output "alpha(omega)"
  consist_diag <- apply(consist_diag[,c(1:4)], 1 ,paste, collapse = "")
  
##### Split large data frame ###################################################
  
  # Put questionnaire data into different data frames for easy access
  
  mbi <- data[,grep("MBI", colnames(data))]   # Maslach Burnout Inventory
  erq <- data[,grep("ERQ", colnames(data))]   # Emotion Regulation Questionnaire
  scs <- data[,grep("SCS", colnames(data))]   # Self Control Scale
  ws <- data[,grep("WS", colnames(data))]     # Work Satisfaction
  ncs <- data[,grep("NCS", colnames(data))]   # Need for Cognition Scale
  ncs <- ncs[,-9]                             # remove the dummy item
  covb <- data[,grep("COV", colnames(data))]  # Covid Burden Items
  dth <- ws[,c(4,8,9)]                        # the Work Satisfaction items for the latent variable Demands Too High
  dtl <- ws[,c(12,26,27)]                     # the Work Satisfaction items for the latent variable Demands Too Low
  drf <- ws[,c(17,21,24)]                     # the Work Satisfaction items for the latent variable Demand-Resource-Fit
  
  # Create a list with keys for the questionnaire subscales/latent variables/item parcels
  
  keylist <- list(mbi_ee = c(5,7,10,11,14,18,19,20,21), mbi_dp = c(1,6,9,15,17), mbi_rpe = c(2,3,4,8,12,13,16),
                  erq_supp = c(2,4,6,9), erq_reap = c(1,3,5,7,8,10),
                  scs = c(2,3,4,5,6,7,8,10,11),
                  ncs = c(4,6,7,8,9,10,11,12,15,16),
                  ws = c(4,5,6,8,9,10,11,13,16,19,20,33))
  
  # Recode all the reverse coded items
  
  mbi[,keylist$mbi_rpe] <- 7 - mbi[,keylist$mbi_rpe]  # Recode from 1-6 to 6-1
  scs[,keylist$scs] <- 6 - scs[,keylist$scs]          # Recode from 1-5 to 5-1
  scs <- scs - 3                                      # Recode all from 1-5 to -2-+2
  ncs[,keylist$ncs] <- 8 - ncs[,keylist$ncs]          # Recode from 1-7 to 7-1
  ncs <- ncs - 4                                      # Recode all from 1-7 to -3-+3
  ws[,keylist$ws] <- 6 - ws[,keylist$ws]              # Recode from 1-5 to 5-1
  ws <- ws - 3                                        # Recode all from 1-5 to -2-+2
  covb[,c(2,3,4,9)] <- 4 - covb[,c(2,3,4,9)]          # Recode from 1-3 to 3-1
  covb[,8] <- 3 - covb[,8]                            # Recode from 1-2 to 2-1
  # The following recoding is opposite to what is described in the methods section of the paper, which
  # is due to the fact that the questionnaire is originally coded as 1 (false) to 5 (true), but it was
  # coded as 1 (true) to 5 (false) in the SoSciSurvey of the present study.
  dth <- 6 - dth                                      # Recode all from 1-5 to 5-1
  dth <- dth - 3                                      # Recode all from 1-5 to -2-+2
  dtl <- dtl - 3                                      # Recode all from 1-5 to -2-+2
  drf <- 6 - drf                                      # Recode all from 1-5 to 5-1
  drf <- drf -3                                       # Recode all from 1-5 to -2-+2
  
##### Parcelling of Need for Cognition items ###################################
  
  # Parcelling preparation: Compute factor loadings on the first PCA component
  
  ncs_pca <- princomp(ncs, cor = FALSE, scores = TRUE)
  ncs_load <- ncs_pca$loadings[,1]
  
  # Parcelling function
  
  parcel.gen <- function(loadings, parcels = 4,seed = 242,iterations = 10000) {
    
    p = parcels                   # number of parcels
    l = length(loadings)          # number of items
    set.seed(seed)              # seed for reproducibility
    
    out = parcels = NULL            # set up containers
    for (k in 1:iterations) {
      
      s = sample(1:l,l,replace = F) # random item order
      m = matrix(s,ncol = 4)        # separate into parcels
      
      # temporarily convert item numbers per parcel to concatenated strings
      parcels.i = NULL
      for (i in 1:p) {
        parcels.i = c(parcels.i,paste0(sort(m[,i]),collapse = ","))
      }
      # write to container
      parcels = rbind(parcels,parcels.i)
      
      # compute average item loadings per parcel
      avg.h = colMeans(matrix(loadings[m],ncol = 4))
      
      # compute differences between all average item loadings
      h.diff = NULL
      for (i in 1:(p-1)) {
        for (j in (i+1):p) {
          h.diff = c(h.diff,abs(avg.h[i]-avg.h[j]))
        }
      }
      
      # sum up differences
      h.diff.sum = sum(h.diff)
      # write summed differences to container
      out = c(out,h.diff.sum)
    }
    
    # find minimum summed difference and generate matrix or respective items
    final.parcels = matrix(as.numeric(unlist(strsplit(parcels[which.min(out),],','))),ncol = 4)
    # obtain corresponding loadings per parcel
    final.h = matrix(loadings[final.parcels],ncol = 4)
    
    # return result as list
    return(list(items = final.parcels,loadings = final.h,avg.loadings = colMeans(final.h),summed.difference = min(out)))
    
  }
  
  ncs_parcels <- parcel.gen(loadings = ncs_load) # execute function
  
  # Add parcelling result to the key list
  
  keylist <- c(keylist, parcel1 = list(ncs_parcels$items[,1]), parcel2 = list(ncs_parcels$items[,2]),
               parcel3 = list(ncs_parcels$items[,3]), parcel4 = list(ncs_parcels$items[,4]))

##### Questionnaire scores #####################################################
  
  # Create data frame to feed the values of every subject into
  
  score_data <- data.frame(id = c(1:180),
                           years = data$yearsteaching,
                           mbi = rep(0,180),
                           mbi_ee = rep(0,180), mbi_dp = rep(0,180), mbi_rpe = rep(0,180),
                           erq = rep(0,180),
                           erq_supp = rep(0,180), erq_reap = rep(0,180),
                           scs = rep(0,180),
                           ncs = rep(0,180),
                           nfc1 = rep(0,180), nfc2 = rep(0,180), nfc3 = rep(0,180), nfc4 = rep(0,180),
                           dth = rep(0,180),
                           dth1 = rep(0,180), dth2 = rep(0,180), dth3 = rep(0,180),
                           dtl = rep(0,180),
                           dtl1 = rep(0,180), dtl2 = rep(0,180), dtl3 = rep(0,180),
                           drf = rep(0,180),
                           drf1 = rep(0,180), drf2 = rep(0,180), drf3 = rep(0,180),
                           covb = rep(0,180))
  
  # Compute individual scores or (for the latent variable indicators) feed in raw values
  
  for (i in 1:nrow(data)) {
    score_data[i,c(3:ncol(score_data))] <- c(rowSums(mbi[i,]),
                                             rowSums(mbi[i,keylist$mbi_ee]), rowSums(mbi[i,keylist$mbi_dp]), rowSums(mbi[i,keylist$mbi_rpe]),
                                             rowSums(erq[i,]),
                                             rowSums(erq[i,keylist$erq_supp]), rowSums(erq[i,keylist$erq_reap]),
                                             rowSums(scs[i,]),
                                             rowSums(ncs[i,]),
                                             rowSums(ncs[i,keylist$parcel1]), rowSums(ncs[i,keylist$parcel2]), rowSums(ncs[i,keylist$parcel3]), rowSums(ncs[i,keylist$parcel4]),
                                             rowSums(dth[i,]),
                                             dth[i,1], dth[i,2], dth[i,3],
                                             rowSums(dtl[i,]),
                                             dtl[i,1], dtl[i,2], dtl[i,3],
                                             rowSums(drf[i,]),
                                             drf[i,1], drf[i,2], drf[i,3],
                                             rowSums(covb[i,]))
  }
  
  # This function was taken from an answer on a Stackoverflow question on August 30th 2021
  # https://stackoverflow.com/questions/35085261/how-to-use-loess-method-in-ggallyggpairs-using-wrap-function/35088740
  
  my_fn <- function(data, mapping, pts=list(), smt=list(), ...){
    ggplot(data = data, mapping = mapping, ...) +
      do.call(geom_point, pts) +
      do.call(geom_smooth, smt)
  }
  
  # Plot a big correlational matrix to check for non-linear associations and unexpected outliers
  multicorrplot <- GGally::ggpairs(score_data[,c("years","mbi","mbi_ee","mbi_dp","mbi_rpe","erq","erq_supp","erq_reap",
                                         "scs","ncs","dth","dtl","drf","covb")], lower = list(continuous = my_fn))
  
  # It looked like there was an outlier in the data, especially visible in the MBI data
  # Check for the outlier in particular, using the data involved in the mediation
  outlierplot <- psych::outlier(score_data[ ,c(6,8:11)], plot = TRUE)
  
  # Remove participant 161 in a second version of the data frame (for the supplementary material analysis)
  score_data_no_outlier <- score_data[score_data$mbi < 100, ]
  
##### Descriptive data analysis ################################################
  
  # Tests for normal distribution
  
  # using Mardia's test for multivariate normal distribution
  # results will be fed into the desc_met data frame in the next step
  
  isnorm_test <- MVN::mvn(score_data[c("mbi","mbi_ee","mbi_dp","mbi_rpe","erq","erq_supp","erq_reap","scs","ncs","dth","dtl","drf","covb")],
                     mvnTest = "mardia", covariance = TRUE, desc = TRUE, univariateTest = "SW")  
  
  # Descriptive statistics
  
  # Categorical descriptives: data frame with percentages of responses per category
  
  desc_cat <- data.frame(answer = c(1,2,3,4,5),
                         age = c(nrow(data[data$age == 1,])/nrow(data), nrow(data[data$age == 2,])/nrow(data), nrow(data[data$age == 3,])/nrow(data),
                                 nrow(data[data$age == 4,])/nrow(data), nrow(data[data$age == 5,])/nrow(data)),
                         gender = c(nrow(data[data$gender == 1,])/nrow(data), nrow(data[data$gender == 2,])/nrow(data), nrow(data[data$gender == 3,])/nrow(data),0,0),
                         schooltype = c(nrow(data[data$schooltype == 1,])/nrow(data), nrow(data[data$schooltype == 2,])/nrow(data), nrow(data[data$schooltype == 3,])/nrow(data),
                                        nrow(data[data$schooltype == 4,])/nrow(data), nrow(data[data$schooltype == 5,])/nrow(data)))
  
  # Metric descriptives: data frame with descriptive values of questionnaires
  
  desc_met <- data.frame(Variable = c("MBI","MBI EE","MBI DP","MBI RPE","ERQ","ERQ S","ERQ R","SCS","NFC","DTH","DTL","DRF","COV"),
                         Minimum = sapply(score_data[,c("mbi","mbi_ee","mbi_dp","mbi_rpe","erq","erq_supp","erq_reap","scs","ncs","dth","dtl","drf","covb")], min),
                         Maximum = sapply(score_data[,c("mbi","mbi_ee","mbi_dp","mbi_rpe","erq","erq_supp","erq_reap","scs","ncs","dth","dtl","drf","covb")], max),
                         Mean = isnorm_test$Descriptives$Mean,
                         SD = isnorm_test$Descriptives$Std.Dev,
                         Normality = isnorm_test$univariateNormality$Normality,
                         Skewness = isnorm_test$Descriptives$Skew,
                         Kurtosis = isnorm_test$Descriptives$Kurtosis)
  # Drop the row names
  
  rownames(desc_met) <- NULL
  
  # Replace the capital YESes and NOs with small ones
  
  desc_met$Normality[grep("NO", desc_met$Normality)] <- "No"
  desc_met$Normality[grep("YES", desc_met$Normality)] <- "Yes"
  
  # Round the values
  
  desc_met[ ,c("Mean","SD","Skewness","Kurtosis")] <- round(desc_met[ ,c("Mean","SD","Skewness","Kurtosis")], digits = 2)
  
  
##### Tests for internal consistency ###########################################
  
  # This part takes quite a long time, so if you quickly want to run the analysis I recommend leaving this part out

  # # Create data frame to feed values into
  # 
  # consistencies <- data.frame(value = c("lower CI alpha","raw alpha","upper CI alpha",
  #                                       "lower CI omega","raw omega","upper CI omega"))
  # 
  # # Compute Cronbach's Alpha and MacDonald's Omega for questionnaires and demand-resource-ratios
  # 
  # # Maslach Burnout Inventory
  # 
  # cronalph <- alpha(mbi, check.keys = TRUE)
  # macomega <- ci.reliability(data=mbi, type="omega", conf.level = 0.95, interval.type="bca", B=bootnum)
  # consistencies$mbi <- round(c(cronalph$total$raw_alpha - 1.96 * cronalph$total$ase, cronalph$total$raw_alpha,
  #                              cronalph$total$raw_alpha + 1.96 * cronalph$total$ase,
  #                              macomega$ci.lower, macomega$est, macomega$ci.upper), digits = 2)
  # 
  # # MBI emotional exhaustion
  # 
  # cronalph <- alpha(subset(mbi, select = keylist$mbi_ee), check.keys = TRUE)
  # macomega <- ci.reliability(data=subset(mbi, select = keylist$mbi_ee), type="omega", conf.level = 0.95, interval.type="bca", B=bootnum)
  # consistencies$mbi_ee <- round(c(cronalph$total$raw_alpha - 1.96 * cronalph$total$ase, cronalph$total$raw_alpha,
  #                                 cronalph$total$raw_alpha + 1.96 * cronalph$total$ase,
  #                                 macomega$ci.lower, macomega$est, macomega$ci.upper), digits = 2)
  # 
  # # MBI depersonalization
  # 
  # cronalph <- alpha(subset(mbi, select = keylist$mbi_dp), check.keys = TRUE)
  # macomega <- ci.reliability(data=subset(mbi, select = keylist$mbi_dp), type="omega", conf.level = 0.95, interval.type="bca", B=bootnum)
  # consistencies$mbi_dp <- round(c(cronalph$total$raw_alpha - 1.96 * cronalph$total$ase, cronalph$total$raw_alpha,
  #                                 cronalph$total$raw_alpha + 1.96 * cronalph$total$ase,
  #                                 macomega$ci.lower, macomega$est, macomega$ci.upper), digits = 2)
  # 
  # # MBI reduced personal efficacy
  # 
  # cronalph <- alpha(subset(mbi, select = keylist$mbi_rpe), check.keys = TRUE)
  # macomega <- ci.reliability(data=subset(mbi, select = keylist$mbi_rpe), type="omega", conf.level = 0.95, interval.type="bca", B=bootnum)
  # consistencies$mbi_rpe <- round(c(cronalph$total$raw_alpha - 1.96 * cronalph$total$ase, cronalph$total$raw_alpha,
  #                                  cronalph$total$raw_alpha + 1.96 * cronalph$total$ase,
  #                                  macomega$ci.lower, macomega$est, macomega$ci.upper), digits = 2)
  # 
  # # ERQ
  # 
  # cronalph <- alpha(erq, check.keys = TRUE)
  # macomega <- ci.reliability(data=erq, type="omega", conf.level = 0.95, interval.type="bca", B=bootnum)
  # consistencies$erq <- round(c(cronalph$total$raw_alpha - 1.96 * cronalph$total$ase, cronalph$total$raw_alpha,
  #                              cronalph$total$raw_alpha + 1.96 * cronalph$total$ase,
  #                              macomega$ci.lower, macomega$est, macomega$ci.upper), digits = 2)
  # 
  # # ERQ suppression
  # 
  # cronalph <- alpha(subset(erq, select = keylist$erq_supp), check.keys = TRUE)
  # macomega <- ci.reliability(data=subset(erq, select = keylist$erq_supp), type="omega", conf.level = 0.95, interval.type="bca", B=bootnum)
  # consistencies$erq_supp <- round(c(cronalph$total$raw_alpha - 1.96 * cronalph$total$ase, cronalph$total$raw_alpha,
  #                                   cronalph$total$raw_alpha + 1.96 * cronalph$total$ase,
  #                                   macomega$ci.lower, macomega$est, macomega$ci.upper), digits = 2)
  # 
  # # ERQ reappraisal
  # 
  # cronalph <- alpha(subset(erq, select = keylist$erq_reap), check.keys = TRUE)
  # macomega <- ci.reliability(data=subset(erq, select = keylist$erq_reap), type="omega", conf.level = 0.95, interval.type="bca", B=bootnum)
  # consistencies$erq_reap <- round(c(cronalph$total$raw_alpha - 1.96 * cronalph$total$ase, cronalph$total$raw_alpha,
  #                                   cronalph$total$raw_alpha + 1.96 * cronalph$total$ase,
  #                                   macomega$ci.lower, macomega$est, macomega$ci.upper), digits = 2)
  # 
  # # Self Control Scale
  # 
  # cronalph <- alpha(scs, check.keys = TRUE)
  # macomega <- ci.reliability(data=scs, type="omega", conf.level = 0.95, interval.type="bca", B=bootnum)
  # consistencies$scs <- round(c(cronalph$total$raw_alpha - 1.96 * cronalph$total$ase, cronalph$total$raw_alpha,
  #                              cronalph$total$raw_alpha + 1.96 * cronalph$total$ase,
  #                              macomega$ci.lower, macomega$est, macomega$ci.upper), digits = 2)
  # 
  # # Need for Cognition Scale
  # 
  # cronalph <- alpha(ncs, check.keys = TRUE)
  # macomega <- ci.reliability(data=ncs, type="omega", conf.level = 0.95, interval.type="bca", B=bootnum)
  # consistencies$ncs <- round(c(cronalph$total$raw_alpha - 1.96 * cronalph$total$ase, cronalph$total$raw_alpha,
  #                              cronalph$total$raw_alpha + 1.96 * cronalph$total$ase,
  #                              macomega$ci.lower, macomega$est, macomega$ci.upper), digits = 2)
  # 
  # # Covid burden
  # 
  # cronalph <- alpha(covb, check.keys = TRUE)
  # macomega <- ci.reliability(data=covb, type="omega", conf.level = 0.95, interval.type="bca", B=bootnum)
  # consistencies$covb <- round(c(cronalph$total$raw_alpha - 1.96 * cronalph$total$ase, cronalph$total$raw_alpha,
  #                               cronalph$total$raw_alpha + 1.96 * cronalph$total$ase,
  #                               macomega$ci.lower, macomega$est, macomega$ci.upper), digits = 2)
  # 
  # # Demands Too High
  # 
  # cronalph <- alpha(dth, check.keys = FALSE)
  # macomega <- ci.reliability(data=dth, type="omega", conf.level = 0.95, interval.type="bca", B=bootnum)
  # consistencies$dth <- round(c(cronalph$total$raw_alpha - 1.96 * cronalph$total$ase, cronalph$total$raw_alpha,
  #                              cronalph$total$raw_alpha + 1.96 * cronalph$total$ase,
  #                              macomega$ci.lower, macomega$est, macomega$ci.upper), digits = 2)
  # 
  # # Demands Too Low
  # 
  # cronalph <- alpha(dtl, check.keys = FALSE)
  # macomega <- ci.reliability(data=dtl, type="omega", conf.level = 0.95, interval.type="bca", B=bootnum)
  # consistencies$dtl <- round(c(cronalph$total$raw_alpha - 1.96 * cronalph$total$ase, cronalph$total$raw_alpha,
  #                              cronalph$total$raw_alpha + 1.96 * cronalph$total$ase,
  #                              macomega$ci.lower, macomega$est, macomega$ci.upper), digits = 2)
  # 
  # # Demand Resource Fit
  # 
  # cronalph <- alpha(drf, check.keys = FALSE)
  # macomega <- ci.reliability(data=drf, type="omega", conf.level = 0.95, interval.type="bca", B=bootnum)
  # consistencies$drf <- round(c(cronalph$total$raw_alpha - 1.96 * cronalph$total$ase, cronalph$total$raw_alpha,
  #                              cronalph$total$raw_alpha + 1.96 * cronalph$total$ase,
  #                              macomega$ci.lower, macomega$est, macomega$ci.upper), digits = 2)
  # 
  # remove(cronalph,macomega) # delete the temporary variable
  # 

##### Correlations #############################################################
  
  # The following function for nicely formatted correlation output was (modified and) taken from
  # https://paulvanderlaken.com/2020/07/28/publication-ready-correlation-matrix-significance-r/
  # on the 5th of August 2021
  
  correlation_matrix <- function(df,
                                 type = "spearman",
                                 digits = 3,
                                 decimal.mark = ".",
                                 use = "all",
                                 show_significance = TRUE,
                                 replace_diagonal = FALSE,
                                 replacement = ""){
    # check arguments
    stopifnot({
      is.numeric(digits)
      digits >= 0
      use %in% c("all", "upper", "lower")
      is.logical(replace_diagonal)
      is.logical(show_significance)
      is.character(replacement)
    })
    # we need the Hmisc package for this
    require(Hmisc)
    
    # retain only numeric and boolean columns
    isNumericOrBoolean = vapply(df, function(x) is.numeric(x) | is.logical(x), logical(1))
    if (sum(!isNumericOrBoolean) > 0) {
      cat('Dropping non-numeric/-boolean column(s):', paste(names(isNumericOrBoolean)[!isNumericOrBoolean], collapse = ', '), '\n\n')
    }
    df = df[isNumericOrBoolean]
    
    # transform input data frame to matrix
    x <- as.matrix(df)
    
    # run correlation analysis using Hmisc package
    correlation_matrix <- Hmisc::rcorr(x, type = type)
    R <- correlation_matrix$r # Matrix of correlation coefficients
    p <- correlation_matrix$P # Matrix of p-value 
    
    # transform correlations to specific character format
    Rformatted = formatC(R, format = 'f', digits = digits, decimal.mark = decimal.mark)
    
    # if there are any negative numbers, we want to put a html space before the positives to align all
    if (sum(!is.na(R) & R < 0) > 0) {
      Rformatted = ifelse(!is.na(R) & R > 0, paste0(" ", Rformatted), Rformatted)
    }
    
    # add significance levels if desired
    if (show_significance) {
      # define notions for significance levels; spacing is important.
      stars <- ifelse(is.na(p), "", ifelse(p < .001, "***", ifelse(p < .01, "**", ifelse(p < .05, "*", ""))))
      Rformatted = paste0(Rformatted, stars)
    }
    
    # make all character strings equally long
    max_length = max(nchar(Rformatted))
    Rformatted = vapply(Rformatted, function(x) {
      current_length = nchar(x)
      difference = max_length - current_length
      return(paste0(x, paste(rep(" ", difference), collapse = ''), sep = ''))
    }, FUN.VALUE = character(1))
    
    # build a new matrix that includes the formatted correlations and their significance stars
    Rnew <- matrix(Rformatted, ncol = ncol(x))
    rownames(Rnew) <- colnames(Rnew) <- colnames(x)
    
    # replace undesired values
    if (use == 'upper') {
      Rnew[lower.tri(Rnew, diag = replace_diagonal)] <- replacement
    } else if (use == 'lower') {
      Rnew[upper.tri(Rnew, diag = replace_diagonal)] <- replacement
    } else if (replace_diagonal) {
      diag(Rnew) <- replacement
    }
    
    return(Rnew)
  }
  
  
  # correlate variables
  correlations <- correlation_matrix(score_data[c("mbi","mbi_ee","mbi_dp","mbi_rpe","erq","erq_supp","erq_reap",
                                                  "scs","ncs","dth","dtl","drf","covb")],
                                     type = "spearman",
                                     use = "lower",
                                     digits = 2,
                                     show_significance = TRUE,
                                     replace_diagonal = TRUE,
                                     replacement = "")
  
  # reorder the columns of the consistencies data frame to match the correlation columns
  consist_diag <- consistencies[c(colnames(correlations))]
  
  # create a vector of consistencies to be used as the diagonal in a table later
  consist_diag <- cbind(format(round(t(consist_diag[2,]), digits = 2), nsmall = 2), rep("(",13),
                        format(round(t(consist_diag[5,]), digits = 2), nsmall = 2), rep(")",13))
  
  # paste together to create the output "alpha(omega)"
  consist_diag <- apply(consist_diag[,c(1:4)], 1 ,paste, collapse = "")
  
  # add previously assembled diagonal with alpha and omega values
  diag(correlations) <- consist_diag
  
  # remove all leading zeros
  correlations <- data.frame(apply(correlations, 2, function(x) {x <- gsub("0.", ".", x, fixed = TRUE)}))
  
  # set row and column names for better table printing
  row.names(correlations) <- c("1. MBI","2. MBI EE","3. MBI DP","4. MBI RPE","5. ERQ","6. ERQ S","7. ERQ R",
                               "8. SCS","9. NFC","10. DTH","11. DTL","12. DRF","13. COV")
  colnames(correlations) <- c("1","2","3","4","5","6","7","8","9","10","11","12","13")
  
  names(correlations) <- paste("{", names(correlations), "}")

##### Replication of Grass et al. (2018) #######################################
  
  # model syntax
  
  repli_model <- '
  # direct paths
    scs       ~   a1 * ncs
    erq_reap  ~   a2 * ncs
    erq_supp  ~   a3 * ncs
    mbi_rpe   ~   b1 * scs + b2 * erq_reap + b3 * erq_supp + c * ncs
    
  # indirect effects
    Indirect1 := a1 * b1
    Indirect2 := a2 * b2
    Indirect3 := a3 * b3

  # contrast (if significant, the effects differ)
    Contrast := Indirect1 - Indirect2 - Indirect3
         
  # total effect
    Total := c + (a1 * b1) + (a2 * b2) + (a3 * b3)
'
  
  # Determine the model fit
  
  fit <- lavaan::sem(
    model = repli_model,
    data  = score_data,
    se = "bootstrap",
    bootstrap = 2000
  )
  
  # Get summary
  
  fit_summ <- lavaan::summary(fit, fit.measures = TRUE, standardize = TRUE, rsquare = TRUE,
                      estimates = TRUE, ci = TRUE)
  
##### Prepare table ############################################################
  
  # put relevant data from the mediation into a table for reporting
  
  # put data into data frame
  
  replic_table <- data.frame("Path" = c("NFC on Self Control", "NFC on Reappraisal", "NFC on Suppression",
                                        "Self Control on RPE", "Reappraisal on RPE", "Suppression on RPE", "NFC on RPE",
                                        "NFC on RPE via Self Control", "NFC on RPE via Reappraisal", "NFC on RPE via Suppression",
                                        "Total Effect"),
                             "$B$" = c(fit_summ$PE$est[fit_summ$PE$label != "" & fit_summ$PE$label != "Contrast"]),
                             "$SE$" = c(fit_summ$PE$se[fit_summ$PE$label != "" & fit_summ$PE$label != "Contrast"]),
                             "$z$-value" = c(fit_summ$PE$z[fit_summ$PE$label != "" & fit_summ$PE$label != "Contrast"]),
                             "$p$-value" = c(fit_summ$PE$pvalue[fit_summ$PE$label != "" & fit_summ$PE$label != "Contrast"]),
                             "CI Lower" = c(fit_summ$PE$ci.lower[fit_summ$PE$label != "" & fit_summ$PE$label != "Contrast"]),
                             "CI Upper" = c(fit_summ$PE$ci.upper[fit_summ$PE$label != "" & fit_summ$PE$label != "Contrast"]),
                             "$\\beta$" = c(fit_summ$PE$std.all[fit_summ$PE$label != "" & fit_summ$PE$label != "Contrast"]),
                             check.names = FALSE)
  
  # format the numbers to three digits after the decimal point
  
  replic_table[2:ncol(replic_table)] <- format(round(replic_table[2:ncol(replic_table)], digits = 3), nsmall = 2)
  
##### Replication of Grass et al. (2018) including years spent teaching ########
  
  # Replication of Grass et al. (2018) using years spent teaching as a variable influencing self-control
  # (as indicated by the correlation of it with SCS but not with the other variables)
  
  repli_model_years <- '
  # direct paths
    scs       ~   a1 * ncs + y * years
    erq_reap  ~   a2 * ncs
    erq_supp  ~   a3 * ncs
    mbi_rpe   ~   b1 * scs + b2 * erq_reap + b3 * erq_supp + c * ncs
    
  # indirect effects
    Indirect1 := (a1 * b1) + (y * b1)
    Indirect2 := a2 * b2
    Indirect3 := a3 * b3

  # contrast (if significant, the effects differ)
    Contrast := Indirect1 - Indirect2 - Indirect3
         
  # total effect
    Total := c + (a1 * b1) + (a2 * b2) + (a3 * b3) + (y * b1)
'
  
  # Determine the model fit
  
  fityears <- lavaan::sem(
    model = repli_model_years,
    data  = score_data,
    se = "bootstrap",
    bootstrap = 2000
  )
  
  # Get summary
  
  fityears_summ <- lavaan::summary(fityears, fit.measures = TRUE, standardize = TRUE, rsquare = TRUE,
                           estimates = TRUE, ci = TRUE)
  
##### Prepare table ############################################################
  
  # put relevant data from the mediation into a table for reporting
  
  # put data into data frame
  
  replic_years_table <- data.frame("Path" = c("NFC on Self Control", "Years spent teaching on Self Control",
                                              "NFC on Reappraisal", "NFC on Suppression",
                                              "Self Control on RPE", "Reappraisal on RPE", "Suppression on RPE", "NFC on RPE",
                                              "NFC and years spent teaching on RPE via Self Control",
                                              "NFC on RPE via Reappraisal", "NFC on RPE via Suppression",
                                              "Total Effect"),
                                   "$B$" = c(fityears_summ$PE$est[fityears_summ$PE$label != "" & fityears_summ$PE$label != "Contrast"]),
                                   "$SE$" = c(fityears_summ$PE$se[fityears_summ$PE$label != "" & fityears_summ$PE$label != "Contrast"]),
                                   "$z$-value" = c(fityears_summ$PE$z[fityears_summ$PE$label != "" & fityears_summ$PE$label != "Contrast"]),
                                   "$p$-value" = c(fityears_summ$PE$pvalue[fityears_summ$PE$label != "" & fityears_summ$PE$label != "Contrast"]),
                                   "CI Lower" = c(fityears_summ$PE$ci.lower[fityears_summ$PE$label != "" & fityears_summ$PE$label != "Contrast"]),
                                   "CI Upper" = c(fityears_summ$PE$ci.upper[fityears_summ$PE$label != "" & fityears_summ$PE$label != "Contrast"]),
                                   "$\\beta$" = c(fityears_summ$PE$std.all[fityears_summ$PE$label != "" & fityears_summ$PE$label != "Contrast"]),
                                   check.names = FALSE)
  
  # format the numbers to three digits after the decimal point
  
  replic_years_table[2:ncol(replic_years_table)] <- format(round(replic_years_table[2:ncol(replic_years_table)], digits = 3), nsmall = 2)
  
##### Demand-resource-ratio model ##############################################
  
  # SEM with demand-resource-ratios
  
  sem_model <- '
  # measurement model
    NFC =~ nfc1 + nfc2 + nfc3 + nfc4
    DTH =~ dth1 + dth2 + dth3
    DTL =~ dtl1 + dtl2 + dtl3
    DRF =~ drf1 + drf2 + drf3
    MBI =~ mbi_ee + mbi_dp + mbi_rpe

  # structural model
    DTH ~ a1 * NFC
    DTL ~ a2 * NFC
    DRF ~ a3 * NFC
    MBI ~ c * NFC + b1 * DTH + b2 * DTL + b3 * DRF

  # indirect effects
    Indirect1 := a1 * b1
    Indirect2 := a2 * b2
    Indirect3 := a3 * b3

  # contrast (if significant, the effects differ)
    Contrast := Indirect1 - Indirect2 - Indirect3
         
  # total effect
    Total := c + (a1 * b1) + (a2 * b2) + (a3 * b3)'
  
  # Determine the model fit
  
  fit2 <- lavaan::sem(
    model = sem_model,
    data  = score_data,
    estimator = "MLR"
  )
  
  # Get summary
  
  fit2_summ <- lavaan::summary(fit2, fit.measures = TRUE, standardize = TRUE, rsquare = TRUE,
                       estimates = TRUE, ci = TRUE)
  
##### Prepare table ############################################################
  
  # put relevant data from the mediation into a table for reporting
  
  # put data into data frame
  
  drr_table <- data.frame("Path" = c("NFC on DTH", "NFC on DTL", "NFC on DRF", "NFC on MBI",
                                     "DTH on MBI", "DTL on MBI", "DRF on MBI",
                                     "NFC on MBI via DTH", "NFC on MBI via DTL", "NFC on MBI via DRF",
                                     "Total Effect"),
                          "$B$" = c(fit2_summ$PE$est[fit2_summ$PE$label != "" & fit2_summ$PE$label != "Contrast"]),
                          "$SE$" = c(fit2_summ$PE$se[fit2_summ$PE$label != "" & fit2_summ$PE$label != "Contrast"]),
                          "$z$-value" = c(fit2_summ$PE$z[fit2_summ$PE$label != "" & fit2_summ$PE$label != "Contrast"]),
                          "$p$-value" = c(fit2_summ$PE$pvalue[fit2_summ$PE$label != "" & fit2_summ$PE$label != "Contrast"]),
                          "CI Lower" = c(fit2_summ$PE$ci.lower[fit2_summ$PE$label != "" & fit2_summ$PE$label != "Contrast"]),
                          "CI Upper" = c(fit2_summ$PE$ci.upper[fit2_summ$PE$label != "" & fit2_summ$PE$label != "Contrast"]),
                          "$\\beta$" = c(fit2_summ$PE$std.all[fit2_summ$PE$label != "" & fit2_summ$PE$label != "Contrast"]),
                          check.names = FALSE)
  
  # format the numbers to three digits after the decimal point
  
  drr_table[2:ncol(drr_table)] <- format(round(drr_table[2:ncol(drr_table)], digits = 3), nsmall = 2)
  
##### Exploring bad fit ########################################################
  
  # Use function from http://www.sthda.com/english/wiki/correlation-matrix-a-
  # quick-start-guide-to-analyze-format-and-visualize-a-correlation-matrix-
  # using-r-software#a-simple-function-to-format-the-correlation-matrix
  # accessed on September 23rd 2021
  
  flattenCorrMatrix <- function(cormat, pmat) {
    ut <- upper.tri(cormat)
    data.frame(
      row = rownames(cormat)[row(cormat)[ut]],
      column = rownames(cormat)[col(cormat)[ut]],
      cor  =(cormat)[ut],
      p = pmat[ut]
    )
  }
  
  # Compute data frame of partial correlations
  
  partialcorr <- flattenCorrMatrix(pcor(score_data[c("years","mbi","mbi_ee","mbi_dp","mbi_rpe",
                                                     "erq","erq_supp","erq_reap","scs","ncs",
                                                     "dth","dtl","drf","covb")], method = "spearman")$estimate,
                                   pcor(score_data[c("years","mbi","mbi_ee","mbi_dp","mbi_rpe",
                                                     "erq","erq_supp","erq_reap","scs","ncs",
                                                     "dth","dtl","drf","covb")], method = "spearman")$p.value)
  # Add easy-to-read significance column
  
  partialcorr$sign <- ifelse(partialcorr$p < 0.05, "sign", "ns")
  
##### Exploratory model 1 ######################################################
  
  # Exploratory analysis I: MBI subscale instead of sum score
  
  explor1_model <- '
  # measurement model
    NFC =~ nfc1 + nfc2 + nfc3 + nfc4
    DTH =~ dth1 + dth2 + dth3
    DTL =~ dtl1 + dtl2 + dtl3
    DRF =~ drf1 + drf2 + drf3
    RPE =~ mbi_rpe

  # structural model
    DTH ~ a1 * NFC
    DTL ~ a2 * NFC
    DRF ~ a3 * NFC
    RPE ~ c * NFC + b1 * DTH + b2 * DTL + b3 * DRF

  # indirect effects
    Indirect1 := a1 * b1
    Indirect2 := a2 * b2
    Indirect3 := a3 * b3

  # contrast (if significant, the effects differ)
    Contrast := Indirect1 - Indirect2 - Indirect3
         
  # total effect
    Total := c + (a1 * b1) + (a2 * b2) + (a3 * b3)'
  
  # Determine the model fit
  
  fit_explor1 <- lavaan::sem(
    model = explor1_model,
    data  = score_data,
    estimator = "MLR"
  )
  
  # Get summary
  
  fit_explor1_summ <- lavaan::summary(fit_explor1, fit.measures = TRUE, standardize = TRUE, rsquare = TRUE,
                              estimates = TRUE, ci = TRUE)

##### Prepare table ############################################################
  
  # put relevant data from the mediation into a table for reporting
  
  # put data into data frame
  
  drr_rpe_table <- data.frame("Path" = c("NFC on DTH", "NFC on DTL", "NFC on DRF", "NFC on RPE",
                                         "DTH on RPE", "DTL on RPE", "DRF on RPE",
                                         "NFC on RPE via DTH", "NFC on RPE via DTL", "NFC on RPE via DRF",
                                         "Total Effect"),
                              "$B$" = c(fit_explor1_summ$PE$est[fit_explor1_summ$PE$label != "" & fit_explor1_summ$PE$label != "Contrast"]),
                              "$SE$" = c(fit_explor1_summ$PE$se[fit_explor1_summ$PE$label != "" & fit_explor1_summ$PE$label != "Contrast"]),
                              "$z$-value" = c(fit_explor1_summ$PE$z[fit_explor1_summ$PE$label != "" & fit_explor1_summ$PE$label != "Contrast"]),
                              "$p$-value" = c(fit_explor1_summ$PE$pvalue[fit_explor1_summ$PE$label != "" & fit_explor1_summ$PE$label != "Contrast"]),
                              "CI Lower" = c(fit_explor1_summ$PE$ci.lower[fit_explor1_summ$PE$label != "" & fit_explor1_summ$PE$label != "Contrast"]),
                              "CI Upper" = c(fit_explor1_summ$PE$ci.upper[fit_explor1_summ$PE$label != "" & fit_explor1_summ$PE$label != "Contrast"]),
                              "$\\beta$" = c(fit_explor1_summ$PE$std.all[fit_explor1_summ$PE$label != "" & fit_explor1_summ$PE$label != "Contrast"]),
                              check.names = FALSE)
  
  # format the numbers to three digits after the decimal point
  
  drr_rpe_table[2:ncol(drr_rpe_table)] <- format(round(drr_rpe_table[2:ncol(drr_rpe_table)], digits = 3), nsmall = 2)
  
##### Exploratory model 2 ######################################################
  
  # Exploratory analysis II: Covid burden as a covariate
  
  explor2_model <- '
  # measurement model
    NFC =~ nfc1 + nfc2 + nfc3 + nfc4
    DTH =~ dth1 + dth2 + dth3
    DRF =~ drf1 + drf2 + drf3

  # structural model
    covb ~ yc * years
    scs ~ ys * years
    NFC ~~ scs
    NFC ~~ covb
    DTH ~ cdth * covb + sdth * scs + ndth* NFC
    DRF ~ sdrf * scs + ndrf * NFC
    DTH ~~ DRF
    mbi_ee ~ dthe * DTH + ce * covb
    mbi_rpe ~ drfr * DRF
    
    mbi_ee ~~ mbi_rpe
    NFC ~~ years

    dth1 ~~ dth2
    dth1 ~~ dth3
    dth2 ~~ dth3

    drf1 ~~ drf2
    drf1 ~~ drf3
    drf2 ~~ drf3
    
  # Indirect effects
    Indirect1 := (ndrf * drfr) + (ys * sdrf * drfr)
    Indirect2 := (ndth * dthe) + (ys * sdth * dthe) + (yc * cdth * dthe)

  # contrast (if significant, the effects differ)
    Contrast := Indirect1 - Indirect2
         
  # total effect
    Total := (ndrf * drfr) + (ys * sdrf * drfr) + (ndth * dthe) + (ys * sdth * dthe) + (yc * cdth * dthe)
'
  
  # Determine the model fit
  
  fit_explor2 <- lavaan::sem(
    model = explor2_model,
    data  = score_data,
    estimator = "MLR"
  )
  
  # Get summary
  
  fit_explor2_summ <- lavaan::summary(fit_explor2, fit.measures = TRUE, standardize = TRUE, rsquare = TRUE,
                              estimates = TRUE, ci = TRUE)
  
##### Prepare table ############################################################
  
  # put relevant data from the mediation into a table for reporting
  
  # put data into data frame
  
  explor2_table <- data.frame("Path" = c("Years on COVB", "Years on SCS",
                                         "COVB on DTH", "SCS on DTH", "NFC on DTH",
                                         "SCS on DRF", "NFC on DRF",
                                         "DTH on EE", "COVB on EE", "DRF on RPE",
                                         "NFC and Years on RPE via SCS and DRF",
                                         "NFC and Years on EE via SCS, COVB, and DTH",
                                         "Total Effect"),
                              "$B$" = c(fit_explor2_summ$PE$est[fit_explor2_summ$PE$label != "" & fit_explor2_summ$PE$label != "Contrast"]),
                              "$SE$" = c(fit_explor2_summ$PE$se[fit_explor2_summ$PE$label != "" & fit_explor2_summ$PE$label != "Contrast"]),
                              "$z$-value" = c(fit_explor2_summ$PE$z[fit_explor2_summ$PE$label != "" & fit_explor2_summ$PE$label != "Contrast"]),
                              "$p$-value" = c(fit_explor2_summ$PE$pvalue[fit_explor2_summ$PE$label != "" & fit_explor2_summ$PE$label != "Contrast"]),
                              "CI Lower" = c(fit_explor2_summ$PE$ci.lower[fit_explor2_summ$PE$label != "" & fit_explor2_summ$PE$label != "Contrast"]),
                              "CI Upper" = c(fit_explor2_summ$PE$ci.upper[fit_explor2_summ$PE$label != "" & fit_explor2_summ$PE$label != "Contrast"]),
                              "$\\beta$" = c(fit_explor2_summ$PE$std.all[fit_explor2_summ$PE$label != "" & fit_explor2_summ$PE$label != "Contrast"]),
                              check.names = FALSE)
  
  # format the numbers to three digits after the decimal point
  
  explor2_table[2:ncol(explor2_table)] <- format(round(explor2_table[2:ncol(explor2_table)], digits = 3), nsmall = 2)
  
#### Fully saturated model #####################################################
  
  fullysat_model <- '
  # measurement model
    NFC =~ nfc1 + nfc2 + nfc3 + nfc4
    DTH =~ dth1 + dth2 + dth3
    DRF =~ drf1 + drf2 + drf3
    DTL =~ dtl1 + dtl2 + dtl3
    MBI =~ mbi_ee + mbi_dp + mbi_rpe
    SC =~ scs
    COV =~ covb
    YST =~ years

  # structural model
    
    DTH ~ hnfc * NFC + hsc * SC + hcov * COV + hyst * YST
    DTL ~ lnfc * NFC + lsc * SC + lcov * COV + lyst * YST
    DRF ~ fnfc * NFC + fsc * SC + fcov * COV + fyst * YST
    MBI ~ mdth * DTH + mdtl * DTL + mdrf * DRF
    
    mbi_ee ~~ mbi_rpe
    mbi_ee ~~ mbi_dp
    mbi_dp ~~ mbi_rpe
    
    NFC ~~ YST
    NFC ~~ SC
    NFC ~~ COV
    SC ~~ COV
    SC ~~ YST
    YST ~~ COV

    DTH ~~ DTL
    DTH ~~ DRF
    DRF ~~ DTL
    
    dth1 ~~ dth2
    dth1 ~~ dth3
    dth2 ~~ dth3

    drf1 ~~ drf2
    drf1 ~~ drf3
    drf2 ~~ drf3
    
    dtl1 ~~ dtl2
    dtl1 ~~ dtl3
    dtl2 ~~ dtl3
    
  # Indirect effects
    Indirect1 := hnfc * mdth + hsc * mdth + hcov * mdth + hyst * mdth
    Indirect2 := lnfc * mdtl + lsc * mdtl + lcov * mdtl + lyst * mdtl
    Indirect3 := fnfc * mdrf + fsc * mdrf + fcov * mdrf + fyst * mdrf

  # contrast (if significant, the effects differ)
    Contrast := Indirect1 - Indirect2 - Indirect3
         
  # total effect
    Total := Indirect1 + Indirect2 + Indirect3
'
  
  # Determine the model fit
  
  fit_fullysat <- lavaan::sem(
    model = fullysat_model,
    data  = score_data,
    estimator = "MLR"
  )
  
  # Get summary
  
  fit_fullysat_summ <- lavaan::summary(fit_fullysat, fit.measures = TRUE, standardize = TRUE, rsquare = TRUE,
                                      estimates = TRUE, ci = TRUE)
  
##### Additional analyses without the outlier
  
  # Again, this part should be left out if you want to go home within the next hour
  
  # # Tests for internal consistency
  # 
  # # Create data frame to feed values into
  # 
  # outlierconsistencies <- data.frame(value = c("lower CI alpha","raw alpha","upper CI alpha",
  #                                              "lower CI omega","raw omega","upper CI omega"))
  # 
  # # Compute Cronbach's Alpha and MacDonald's Omega for questionnaires and demand-resource-ratios
  # 
  # # Maslach Burnout Inventory
  # 
  # cronalph <- alpha(mbi, check.keys = TRUE)
  # macomega <- ci.reliability(data=mbi[-161,], type="omega", conf.level = 0.95, interval.type="bca", B=bootnum)
  # outlierconsistencies$mbi <- round(c(cronalph$total$raw_alpha - 1.96 * cronalph$total$ase, cronalph$total$raw_alpha,
  #                                     cronalph$total$raw_alpha + 1.96 * cronalph$total$ase,
  #                                     macomega$ci.lower, macomega$est, macomega$ci.upper), digits = 2)
  # 
  # # MBI emotional exhaustion
  # 
  # cronalph <- alpha(subset(mbi, select = keylist$mbi_ee), check.keys = TRUE)
  # macomega <- ci.reliability(data=subset(mbi[-161,], select = keylist$mbi_ee), type="omega", conf.level = 0.95, interval.type="bca", B=bootnum)
  # outlierconsistencies$mbi_ee <- round(c(cronalph$total$raw_alpha - 1.96 * cronalph$total$ase, cronalph$total$raw_alpha,
  #                                        cronalph$total$raw_alpha + 1.96 * cronalph$total$ase,
  #                                        macomega$ci.lower, macomega$est, macomega$ci.upper), digits = 2)
  # 
  # # MBI depersonalization
  # 
  # cronalph <- alpha(subset(mbi, select = keylist$mbi_dp), check.keys = TRUE)
  # macomega <- ci.reliability(data=subset(mbi[-161,], select = keylist$mbi_dp), type="omega", conf.level = 0.95, interval.type="bca", B=bootnum)
  # outlierconsistencies$mbi_dp <- round(c(cronalph$total$raw_alpha - 1.96 * cronalph$total$ase, cronalph$total$raw_alpha,
  #                                        cronalph$total$raw_alpha + 1.96 * cronalph$total$ase,
  #                                        macomega$ci.lower, macomega$est, macomega$ci.upper), digits = 2)
  # 
  # # MBI reduced personal efficacy
  # 
  # cronalph <- alpha(subset(mbi, select = keylist$mbi_rpe), check.keys = TRUE)
  # macomega <- ci.reliability(data=subset(mbi[-161,], select = keylist$mbi_rpe), type="omega", conf.level = 0.95, interval.type="bca", B=bootnum)
  # outlierconsistencies$mbi_rpe <- round(c(cronalph$total$raw_alpha - 1.96 * cronalph$total$ase, cronalph$total$raw_alpha,
  #                                         cronalph$total$raw_alpha + 1.96 * cronalph$total$ase,
  #                                         macomega$ci.lower, macomega$est, macomega$ci.upper), digits = 2)
  # 
  # # ERQ
  # 
  # cronalph <- alpha(erq, check.keys = TRUE)
  # macomega <- ci.reliability(data=erq[-161,], type="omega", conf.level = 0.95, interval.type="bca", B=bootnum)
  # outlierconsistencies$erq <- round(c(cronalph$total$raw_alpha - 1.96 * cronalph$total$ase, cronalph$total$raw_alpha,
  #                                     cronalph$total$raw_alpha + 1.96 * cronalph$total$ase,
  #                                     macomega$ci.lower, macomega$est, macomega$ci.upper), digits = 2)
  # 
  # # ERQ suppression
  # 
  # cronalph <- alpha(subset(erq, select = keylist$erq_supp), check.keys = TRUE)
  # macomega <- ci.reliability(data=subset(erq[-161,], select = keylist$erq_supp), type="omega", conf.level = 0.95, interval.type="bca", B=bootnum)
  # outlierconsistencies$erq_supp <- round(c(cronalph$total$raw_alpha - 1.96 * cronalph$total$ase, cronalph$total$raw_alpha,
  #                                          cronalph$total$raw_alpha + 1.96 * cronalph$total$ase,
  #                                          macomega$ci.lower, macomega$est, macomega$ci.upper), digits = 2)
  # 
  # # ERQ reappraisal
  # 
  # cronalph <- alpha(subset(erq, select = keylist$erq_reap), check.keys = TRUE)
  # macomega <- ci.reliability(data=subset(erq[-161,], select = keylist$erq_reap), type="omega", conf.level = 0.95, interval.type="bca", B=bootnum)
  # outlierconsistencies$erq_reap <- round(c(cronalph$total$raw_alpha - 1.96 * cronalph$total$ase, cronalph$total$raw_alpha,
  #                                          cronalph$total$raw_alpha + 1.96 * cronalph$total$ase,
  #                                          macomega$ci.lower, macomega$est, macomega$ci.upper), digits = 2)
  # 
  # # Self Control Scale
  # 
  # cronalph <- alpha(scs, check.keys = TRUE)
  # macomega <- ci.reliability(data=scs[-161,], type="omega", conf.level = 0.95, interval.type="bca", B=bootnum)
  # outlierconsistencies$scs <- round(c(cronalph$total$raw_alpha - 1.96 * cronalph$total$ase, cronalph$total$raw_alpha,
  #                                     cronalph$total$raw_alpha + 1.96 * cronalph$total$ase,
  #                                     macomega$ci.lower, macomega$est, macomega$ci.upper), digits = 2)
  # 
  # # Need for Cognition Scale
  # 
  # cronalph <- alpha(ncs, check.keys = TRUE)
  # macomega <- ci.reliability(data=ncs[-161,], type="omega", conf.level = 0.95, interval.type="bca", B=bootnum)
  # outlierconsistencies$ncs <- round(c(cronalph$total$raw_alpha - 1.96 * cronalph$total$ase, cronalph$total$raw_alpha,
  #                                     cronalph$total$raw_alpha + 1.96 * cronalph$total$ase,
  #                                     macomega$ci.lower, macomega$est, macomega$ci.upper), digits = 2)
  # 
  # # Covid burden
  # 
  # cronalph <- alpha(covb, check.keys = TRUE)
  # macomega <- ci.reliability(data=covb[-161,], type="omega", conf.level = 0.95, interval.type="bca", B=bootnum)
  # outlierconsistencies$covb <- round(c(cronalph$total$raw_alpha - 1.96 * cronalph$total$ase, cronalph$total$raw_alpha,
  #                                      cronalph$total$raw_alpha + 1.96 * cronalph$total$ase,
  #                                      macomega$ci.lower, macomega$est, macomega$ci.upper), digits = 2)
  # 
  # # Demands Too High
  # 
  # cronalph <- alpha(dth, check.keys = FALSE)
  # macomega <- ci.reliability(data=dth[-161,], type="omega", conf.level = 0.95, interval.type="bca", B=bootnum)
  # outlierconsistencies$dth <- round(c(cronalph$total$raw_alpha - 1.96 * cronalph$total$ase, cronalph$total$raw_alpha,
  #                                     cronalph$total$raw_alpha + 1.96 * cronalph$total$ase,
  #                                     macomega$ci.lower, macomega$est, macomega$ci.upper), digits = 2)
  # 
  # # Demands Too Low
  # 
  # cronalph <- alpha(dtl, check.keys = FALSE)
  # macomega <- ci.reliability(data=dtl[-161,], type="omega", conf.level = 0.95, interval.type="bca", B=bootnum)
  # outlierconsistencies$dtl <- round(c(cronalph$total$raw_alpha - 1.96 * cronalph$total$ase, cronalph$total$raw_alpha,
  #                                     cronalph$total$raw_alpha + 1.96 * cronalph$total$ase,
  #                                     macomega$ci.lower, macomega$est, macomega$ci.upper), digits = 2)
  # 
  # # Demand Resource Fit
  # 
  # cronalph <- alpha(drf, check.keys = FALSE)
  # macomega <- ci.reliability(data=drf[-161,], type="omega", conf.level = 0.95, interval.type="bca", B=bootnum)
  # outlierconsistencies$drf <- round(c(cronalph$total$raw_alpha - 1.96 * cronalph$total$ase, cronalph$total$raw_alpha,
  #                                     cronalph$total$raw_alpha + 1.96 * cronalph$total$ase,
  #                                     macomega$ci.lower, macomega$est, macomega$ci.upper), digits = 2)
  # 
  # remove(cronalph,macomega) # delete the temporary variable
  
  
  
  # correlate variables
  outliercorrelations <- correlation_matrix(score_data_no_outlier[c("mbi","mbi_ee","mbi_dp","mbi_rpe","erq","erq_supp","erq_reap",
                                                  "scs","ncs","dth","dtl","drf","covb")],
                                     type = "spearman",
                                     use = "lower",
                                     digits = 2,
                                     show_significance = TRUE,
                                     replace_diagonal = TRUE,
                                     replacement = "")
  
  # reorder the columns of the consistencies data frame to match the correlation columns
  outlier_consist_diag <- outlierconsistencies[c(colnames(outliercorrelations))]
  
  # create a vector of consistencies to be used as the diagonal in a table later
  outlier_consist_diag <- cbind(format(round(t(outlier_consist_diag[2,]), digits = 2), nsmall = 2), rep("(",13),
                                format(round(t(outlier_consist_diag[5,]), digits = 2), nsmall = 2), rep(")",13))
  
  # paste together to create the output "alpha(omega)"
  outlier_consist_diag <- apply(outlier_consist_diag[,c(1:4)], 1 ,paste, collapse = "")
  
  # add previously assembled diagonal with alpha and omega values
  diag(outliercorrelations) <- outlier_consist_diag
  
  # remove all leading zeros
  outliercorrelations <- data.frame(apply(outliercorrelations, 2, function(x) {x <- gsub("0.", ".", x, fixed = TRUE)}))
  
  # set row and column names for better table printing
  row.names(outliercorrelations) <- c("1. MBI","2. MBI EE","3. MBI DP","4. MBI RPE","5. ERQ","6. ERQ S","7. ERQ R",
                               "8. SCS","9. NFC","10. DTH","11. DTL","12. DRF","13. COV")
  colnames(outliercorrelations) <- c("1","2","3","4","5","6","7","8","9","10","11","12","13")
  
  names(outliercorrelations) <- paste("{", names(outliercorrelations), "}")
  
  
  # Replication of Grass et al. (2018)
  
  outlierrepli_model <- '
  # direct paths
    scs       ~   a1 * ncs
    erq_reap  ~   a2 * ncs
    erq_supp  ~   a3 * ncs
    mbi_rpe   ~   b1 * scs + b2 * erq_reap + b3 * erq_supp + c * ncs
    
  # indirect effects
    Indirect1 := a1 * b1
    Indirect2 := a2 * b2
    Indirect3 := a3 * b3

  # contrast (if significant, the effects differ)
    Contrast := Indirect1 - Indirect2 - Indirect3
         
  # total effect
    Total := c + (a1 * b1) + (a2 * b2) + (a3 * b3)
'
  
  # Determine the model fit
  
  outlierfit <- lavaan::sem(
    model = outlierrepli_model,
    data  = score_data_no_outlier,
    se = "bootstrap",
    bootstrap = 2000
  )
  
  # Get summary
  
  outlierfit_summ <- lavaan::summary(outlierfit, fit.measures = TRUE, standardize = TRUE, rsquare = TRUE,
                             estimates = TRUE, ci = TRUE)
  
  
  # put relevant data from the mediation into a table for reporting
  
  # put data into data frame
  
  outlierreplic_table <- data.frame("Path" = c("NFC on Self Control", "NFC on Reappraisal", "NFC on Suppression",
                                               "Self Control on RPE", "Reappraisal on RPE", "Suppression on RPE", "NFC on RPE",
                                               "NFC on RPE via Self Control", "NFC on RPE via Reappraisal", "NFC on RPE via Suppression",
                                               "Total Effect"),
                                    "$B$" = c(outlierfit_summ$PE$est[outlierfit_summ$PE$label != "" & outlierfit_summ$PE$label != "Contrast"]),
                                    "$SE$" = c(outlierfit_summ$PE$se[outlierfit_summ$PE$label != "" & outlierfit_summ$PE$label != "Contrast"]),
                                    "$z$-value" = c(outlierfit_summ$PE$z[outlierfit_summ$PE$label != "" & outlierfit_summ$PE$label != "Contrast"]),
                                    "$p$-value" = c(outlierfit_summ$PE$pvalue[outlierfit_summ$PE$label != "" & outlierfit_summ$PE$label != "Contrast"]),
                                    "CI Lower" = c(outlierfit_summ$PE$ci.lower[outlierfit_summ$PE$label != "" & outlierfit_summ$PE$label != "Contrast"]),
                                    "CI Upper" = c(outlierfit_summ$PE$ci.upper[outlierfit_summ$PE$label != "" & outlierfit_summ$PE$label != "Contrast"]),
                                    "$\\beta$" = c(outlierfit_summ$PE$std.all[outlierfit_summ$PE$label != "" & outlierfit_summ$PE$label != "Contrast"]),
                                    check.names = FALSE)
  
  # format the numbers to three digits after the decimal point
  
  outlierreplic_table[2:ncol(outlierreplic_table)] <- format(round(outlierreplic_table[2:ncol(outlierreplic_table)], digits = 3), nsmall = 2)
  
  
  # SEM with demand-resource-ratios
  
  outliersem_model <- '
  # measurement model
    NFC =~ nfc1 + nfc2 + nfc3 + nfc4
    DTH =~ dth1 + dth2 + dth3
    DTL =~ dtl1 + dtl2 + dtl3
    DRF =~ drf1 + drf2 + drf3
    MBI =~ mbi_ee + mbi_dp + mbi_rpe

  # structural model
    DTH ~ a1 * NFC
    DTL ~ a2 * NFC
    DRF ~ a3 * NFC
    MBI ~ c * NFC + b1 * DTH + b2 * DTL + b3 * DRF

  # indirect effects
    Indirect1 := a1 * b1
    Indirect2 := a2 * b2
    Indirect3 := a3 * b3

  # contrast (if significant, the effects differ)
    Contrast := Indirect1 - Indirect2 - Indirect3
         
  # total effect
    Total := c + (a1 * b1) + (a2 * b2) + (a3 * b3)'
  
  # Determine the model fit
  
  outlierfit2 <- lavaan::sem(
    model = outliersem_model,
    data  = score_data_no_outlier,
    estimator = "MLR"
  )
  
  # Get summary
  
  outlierfit2_summ <- lavaan::summary(outlierfit2, fit.measures = TRUE, standardize = TRUE, rsquare = TRUE,
                              estimates = TRUE, ci = TRUE)
  
  
  # put relevant data from the mediation into a table for reporting
  
  # put data into data frame
  
  outlierdrr_table <- data.frame("Path" = c("NFC on DTH", "NFC on DTL", "NFC on DRF", "NFC on MBI",
                                            "DTH on MBI", "DTL on MBI", "DRF on MBI",
                                            "NFC on MBI via DTH", "NFC on MBI via DTL", "NFC on MBI via DRF",
                                            "Total Effect"),
                                 "$B$" = c(outlierfit2_summ$PE$est[outlierfit2_summ$PE$label != "" & outlierfit2_summ$PE$label != "Contrast"]),
                                 "$SE$" = c(outlierfit2_summ$PE$se[outlierfit2_summ$PE$label != "" & outlierfit2_summ$PE$label != "Contrast"]),
                                 "$z$-value" = c(outlierfit2_summ$PE$z[outlierfit2_summ$PE$label != "" & outlierfit2_summ$PE$label != "Contrast"]),
                                 "$p$-value" = c(outlierfit2_summ$PE$pvalue[outlierfit2_summ$PE$label != "" & outlierfit2_summ$PE$label != "Contrast"]),
                                 "CI Lower" = c(outlierfit2_summ$PE$ci.lower[outlierfit2_summ$PE$label != "" & outlierfit2_summ$PE$label != "Contrast"]),
                                 "CI Upper" = c(outlierfit2_summ$PE$ci.upper[outlierfit2_summ$PE$label != "" & outlierfit2_summ$PE$label != "Contrast"]),
                                 "$\\beta$" = c(outlierfit2_summ$PE$std.all[outlierfit2_summ$PE$label != "" & outlierfit2_summ$PE$label != "Contrast"]),
                                 check.names = FALSE)
  
  # format the numbers to three digits after the decimal point
  
  outlierdrr_table[2:ncol(outlierdrr_table)] <- format(round(outlierdrr_table[2:ncol(outlierdrr_table)], digits = 3), nsmall = 2)

#### DRR model with covariance as suggested by reviewer ####################
  
  sem_cov_model <- '
  # measurement model
    NFC =~ nfc1 + nfc2 + nfc3 + nfc4
    DTH =~ dth1 + dth2 + dth3
    DTL =~ dtl1 + dtl2 + dtl3
    DRF =~ drf1 + drf2 + drf3
    MBI =~ mbi_ee + mbi_dp + mbi_rpe

  # structural model
    DTH ~ a1 * NFC
    DTL ~ a2 * NFC
    DRF ~ a3 * NFC
    MBI ~ c * NFC + b1 * DTH + b2 * DTL + b3 * DRF
  
  # covariances
    DTH ~~ DTL
    DTL ~~ DRF
    DRF ~~ DTH

  # indirect effects
    Indirect1 := a1 * b1
    Indirect2 := a2 * b2
    Indirect3 := a3 * b3

  # contrast (if significant, the effects differ)
    Contrast := Indirect1 - Indirect2 - Indirect3
         
  # total effect
    Total := c + (a1 * b1) + (a2 * b2) + (a3 * b3)'
  
  # Determine the model fit
  
  fit2_cov <- lavaan::sem(
    model = sem_cov_model,
    data  = score_data,
    estimator = "MLR"
  )
  
  # Get summary
  
  fit2_cov_summ <- lavaan::summary(fit2_cov, fit.measures = TRUE, standardize = TRUE, rsquare = TRUE,
                               estimates = TRUE, ci = TRUE)
  
