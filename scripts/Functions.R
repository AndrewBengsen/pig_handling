# Functions

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
jama_palette3 <- c("#00a1d5", "#df8f44", "black")

HT <- function(x){list(HEAD = head(x), TAIL = tail(x))}

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

mod_results <- function(x){
  # use changeq = T if the upper CI in the summary table is spec as 98% not 97.5%
  require(gridExtra)
  Rhat <- gelman.diag(x, transform = T, multivariate = F)
  sum <- summary(x)
  ES <- effectiveSize(x)
  S <- ggs(x)
  
  modmat <- as.matrix(x, chains=T) %>%
    as.data.frame()
  modes <- medians <- numeric(ncol(modmat)-1)
  for(i in c(1:ncol(modmat)-1)){
    modes[i] <- getmode(modmat[,i+1])
  }
  for(i in c(1:ncol(modmat)-1)){
    medians[i] <- median(modmat[,i+1])
  }
  if(class(sum$statistics) == "numeric"){
    sum_statistics <- data.frame(t(sum$statistics))
    sum_quantiles <- data.frame(t(sum$quantiles))
    names(sum_quantiles) <- names(sum$quantiles)
  } else {
    sum_statistics <- sum$statistics
    sum_quantiles <- sum$quantiles
  }
  sumTab <- as.data.frame(cbind(sum_statistics, sum_quantiles)) %>%
    mutate(Par = row.names(sum_statistics), 
           ESS = ES,
           Rhat = Rhat$psrf[,1] ) %>%
    mutate(MCSE = SD/sqrt(ESS)) %>%
    mutate(Mode = modes,
           Median = round(medians,3)) %>%
    dplyr::select(Par, Mean,  Median, Mode, lcri = "2.5%", ucri ="97.5%", SD, Rhat, ESS) #, ESS, MCSE
  
  out <- list(Rhat = Rhat, sum = sum, sumTab = sumTab, modMat = modmat, S = S)
} 


mod_results_90 <- function(x){
  # use changeq = T if the upper CI in the summary table is spec as 98% not 97.5%
  require(gridExtra)
  Rhat <- gelman.diag(x, transform = T, multivariate = F)
  sum <- summary(x)
  ES <- effectiveSize(x)
  S <- ggs(x)
  
  hpdi <- HDInterval::hdi(x, credMass=0.90) # Highest posterior density interval
  
  modmat <- as.matrix(x, chains=T) %>%
    as.data.frame()
  #modes <- medians <- numeric(ncol(modmat)-1)
  #for(i in c(1:ncol(modmat)-1)){
  #  modes[i] <- getmode(modmat[,i+1])
  #}
  #for(i in c(1:ncol(modmat)-1)){
  #  medians[i] <- median(modmat[,i+1])
  #}
  sumTab <- as.data.frame(cbind(sum$statistics, sum$quantiles)) %>%
    mutate(Par = row.names(sum$statistics), 
           ESS = ES,
           Rhat = Rhat$psrf[,1] ) %>%
    mutate(MCSE = SD/sqrt(ESS)) %>%
    mutate(#Mode = modes,
      #Median = round(medians,3),
      HPDIlower = hpdi[1,],
      HPDIupper = hpdi[2,]) %>%
    dplyr::select(Par, Mean,  #Median, Mode, 
                  HPDIlower, HPDIupper, SD, Rhat, ESS) #, ESS, MCSE
  
  out <- list(Rhat = Rhat, sum = sum, sumTab = sumTab, modMat = modmat, S = S)
} 

mod_results_95 <- function(x){
  # use changeq = T if the upper CI in the summary table is spec as 98% not 97.5%
  require(gridExtra)
  Rhat <- gelman.diag(x, transform = T, multivariate = F)
  sum <- summary(x)
  ES <- effectiveSize(x)
  S <- ggs(x)
  
  hpdi <- HDInterval::hdi(x, credMass=0.95) # Highest posterior density interval
  
  modmat <- as.matrix(x, chains=T) %>%
    as.data.frame()
  #modes <- medians <- numeric(ncol(modmat)-1)
  #for(i in c(1:ncol(modmat)-1)){
  #  modes[i] <- getmode(modmat[,i+1])
  #}
  #for(i in c(1:ncol(modmat)-1)){
  #  medians[i] <- median(modmat[,i+1])
  #}
  sumTab <- as.data.frame(cbind(sum$statistics, sum$quantiles)) %>%
    mutate(Par = row.names(sum$statistics), 
           ESS = ES,
           Rhat = Rhat$psrf[,1] ) %>%
    mutate(MCSE = SD/sqrt(ESS)) %>%
    mutate(#Mode = modes,
      #Median = round(medians,3),
      HPDIlower = hpdi[1,],
      HPDIupper = hpdi[2,]) %>%
    dplyr::select(Par, Mean,  #Median, Mode, 
                  HPDIlower, HPDIupper, SD, Rhat, ESS) #, ESS, MCSE
  
  out <- list(Rhat = Rhat, sum = sum, sumTab = sumTab, modMat = modmat, S = S)
} 

