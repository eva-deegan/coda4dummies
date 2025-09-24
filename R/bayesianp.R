#' bayesian p value calculation
#'
#' Function to calculate percentage of coda samples positive and negative for each parameter
#' It takes a jagsUI object, maybe will add rjags in the future idk and number of chains default 3
#' New function written by Eva tested once
#'
#' @param jagsobj jagsUI or rjags object
#' @param n.chains number of chains
#' @return A data frame of columns for parameters, % positive and negative samples, significance (>95%) and direction

bayesian_p=function(jagsobj, n.chains=3){
  
#converting jags obj to coda sample form
coda=jagsobj$samples
#getting number of samples
samples=dim(coda[[1]])[1]
#and number of parameters
param_num=dim(coda[[1]])[2]
n.chains=n.chains

p_df=matrix(nrow=param_num, ncol=5)
colnames(p_df)=c("param", "percent_pos", "percent_neg", "sig", "direction")


for(p in 1:param_num){
  bins=c()
  for(c in 1:n.chains){
  param=coda[[c]][,p]
  bin=param>0
  bin["TRUE"]=1
  bin["FALSE"]=0
  bins=c(bins, bin)
  }
  pos=length(which(bins==1))
  neg=length(which(bins==0))
  p_df[p,"param"]=attributes(coda[[1]][1,p])$names
  p_df[p, "percent_pos"]=round(pos/(pos+neg)*100,2)
  p_df[p, "percent_neg"]=round(neg/(pos+neg)*100,2)
  if(as.numeric(p_df[p, "percent_pos"])>=95 | as.numeric(p_df[p, "percent_neg"])>=95){
    p_df[p, "sig"]="TRUE"
  }else{
    p_df[p, "sig"]="FALSE"
  }
  if(p_df[p, "percent_pos"]>p_df[p, "percent_neg"]){
    p_df[p, "direction"]="positive"
  }else{
    p_df[p, "direction"]="negative"
  }
  
}
p_df=as.data.frame(p_df)
p_df=p_df[-which(substr(p_df$param, 1,3)=="sig" | p_df$param=="deviance"),]

return(p_df)
}
