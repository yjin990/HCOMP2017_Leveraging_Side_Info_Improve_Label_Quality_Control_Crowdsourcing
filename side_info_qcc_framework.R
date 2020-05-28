require(methods)
args = commandArgs(trailingOnly=TRUE)
setwd(args[1])#set the path to the directory containing all the data and side info files
#the following four variables are used to indicate which type of side info has been activated
#in this case, we could implement models with different combinations of side info in one script
L_info_en <- as.numeric(args[2]) #labeller info enabled
I_info_en <- as.numeric(args[3]) #item info enabled
R_info_en <- as.numeric(args[4]) #response info enabled
S_info_en <- as.numeric(args[5]) #session info enabled

r <- read.csv("r.csv") #responses
uv <- read.csv("uv.csv",sep=",") #user-item pairs corresponding to responses
gt <- read.csv("gt.csv",sep=",") #ground truths of items
x_u <- as.matrix(read.csv("dfs.csv",sep=",")) #labeler feature value matrix
x_v <- as.matrix(read.csv("qfs.csv",sep=",")) #item feature value matrix
x_uv <- as.matrix(read.csv("rfs.csv",sep=",")) #response feature value matrix
x_s <- as.matrix(read.csv("sfs.csv",sep=",")) #session feature value matrix

NUV <- length(r[,])#number of responses
NU <- max(unique(uv[,1]))#number of labelers
NV <- max(unique(uv[,2]))#number of items
NK <- max(unique(gt$true_label))#number of label categories
NDU <- ncol(x_u)#number of labeller features
NDV <- ncol(x_v)#number of item features
NDS <- ncol(x_s)#number of session features
NDUV <- ncol(x_uv)#number of response features
Nburnin <- as.numeric(args[6])#number of burn-in iterations
Nkeep <- as.numeric(args[7])#number of item true label samples to keep after burn-in
NRU <- as.numeric(args[8])#number of responses per labeller
NRV <- as.numeric(args[9])#number of responses per item

calculate_sigma_sq <- function(sd) {#heuristics to compute prior variances of different side-info coefficients 
  sigma_sq <- sd^2
  dn <- 0#number of digits
  while (sigma_sq < 1) {
    sigma_sq <- sigma_sq * 10
    dn <- dn + 1
  }
  return(10^(-dn))
}

#initialize item true label variables using Majority Vote
l <- c(rep(0, NV))
uv_r <- cbind(uv,r)
for (id in 1:NV) {
  rs <- uv_r[which(uv_r$new_itemid == id),"response"]
  count <- table(factor(rs, levels=1:NK))
  if (length(unique(count)) > 1) {
    winner <- tail(names(sort(count)),1)
  } else {
    #if there is a tie, then randomly choose the correct answer
    winner <- sample(1:NK,size=1)
  }
  l[id] <- as.numeric(winner)
}

#initialization 
dv <- c(rnorm(NV,mean=1,sd=sqrt(1)))
eu <- c(rnorm(NU,mean=1,sd=sqrt(1)))
#dv <- c(rep(1, NV))#initialize easiness values for all the items
#eu <- c(rep(0.5, NU))#initialize expertise values for all the labelers
l <- c(sample(c(1:NK),NV,replace=TRUE))#initalize item true labels
beta_U <- c(rep(0,NDU))#initalize coefficients regressed on labeler info features
beta_V <- c(rep(0,NDV))#initalize coefficients regressed on item info features
beta_S <- c(rep(0,NDS))#initalize coefficients regressed on session info features
alpha_u <- matrix(rep(1,NU*NDUV),NU)
#initalize labeler-specific coefficients regressed on response info features
alpha_v <- matrix(rep(0,NV*NDUV),NV)
#initalize item-specific coefficients regressed on response info features

mu_dv <- 0.1 #prior mean of item easiness
#prior variance of item easiness, we assume majority of items have similar degrees of easiness a-priori. In other words, majority of items tend to be labeled correctly by labellers.
sigma_sq_dv <- 0.01 
dv <- c(rnorm(NV,mean=mu_dv,sd=sqrt(sigma_sq_dv)))#initialize item easiness

#prior mean of labeler expertise
mu_eu <- 2.5
#prior variance of labeler expertise, we assume majority of labelers have similar degrees of expertise a-priori due to the presence of test questions.
sigma_sq_eu <- 0.25
eu <- c(rnorm(NU,mean=mu_eu,sd=sqrt(sigma_sq_eu)))

#prior mean of beta_U
mu_bu <- 0 
#sigma_sq_bu <- (sqrt(sigma_sq_eu) / (2 * NDU))^2 #prior variance of beta_U
sigma_sq_bu <- calculate_sigma_sq(sqrt(sigma_sq_eu) / (2 * NDU))
#prior variance of beta_U. The sum of the prior variance of each feature should be comparable to the prior variance of expertise eu
#sigma_sq_bu <- (sqrt(sigma_sq_eu) / NDU)^2
beta_U <- c(rnorm(NDU,mean=mu_bu,sd=sqrt(sigma_sq_bu)))#initalize coefficients regressed on labeller info features
#beta_U <- c(rep(-0.01,NDU))

mu_bv <- 0 #prior mean of beta_V
sigma_sq_bv <- calculate_sigma_sq(sqrt(sigma_sq_dv) / NDV) #prior variance of beta_V
#sigma_sq_bv <- (sqrt(sigma_sq_dv) / (2*NDV))^2 #prior variance of beta_V
#prior variance of beta_V. The sum of the prior variance of each feature should be comparable to the prior variance of expertise dv
#sigma_sq_bv <- (sqrt(sigma_sq_dv) / NDV)^2
beta_V <- c(rnorm(NDV,mean=mu_bv,sd=sqrt(sigma_sq_bv)))

mu_bs <- 0 #prior mean of beta_S
#sigma_sq_bs <- (sqrt(sigma_sq_eu) / (2*NDS))^2 #prior variance of beta_S
sigma_sq_bs <- calculate_sigma_sq(sqrt(sigma_sq_eu) / NDS)
#prior variance of beta_S. The sum of the prior variance of each feature should be comparable to the prior variance of expertise eu
sigma_sq_bs <- (sqrt(sigma_sq_eu) / NDS)^2
beta_S <- c(rnorm(NDS,mean=mu_bs,sd=sqrt(sigma_sq_bs)))

mu_au <- 0 #prior mean of alpha_u
sigma_sq_au <- calculate_sigma_sq(sqrt(sigma_sq_eu) / NDUV) #prior variance of alpha_u
#sigma_sq_au <- (sqrt(sigma_sq_eu) / (NDUV))^2
alpha_u <- matrix(rnorm(NU*NDUV,mean=mu_au,sd=sqrt(sigma_sq_au)),NU)
mu_av <- 1 #prior mean of alpha_v
#sigma_sq_av <- (sqrt(sigma_sq_dv) / NDUV)^2 #prior variance of alpha_v
sigma_sq_av <- calculate_sigma_sq(sqrt(sigma_sq_dv) / NDUV) #prior variance of alpha_u
alpha_v <- matrix(rnorm(NV*NDUV,mean=mu_av,sd=sqrt(sigma_sq_av)),NV)

eta <- 0.001 #basic learning rate
eta_dv <- eta / NRV #learning rate for item easiness
eta_eu <- eta / NRU #learning rate for labeler expertise
eta_bu <- eta / NUV #learning rate for beta_U
eta_bv <- eta / NUV #learning rate for beta_V
eta_bs <- eta / NUV #learning rate for beta_S
eta_av <- eta_dv #learning rate for alpha_v
eta_au <- eta_eu #learning rate for alpha_u
gamma <- 0.5 #dirichlet prior for item true labels

#negative log conditional probability distribution over all the parameters given all the priors and the current true label assignments
objective <- function() {
  obj <- 0
  for (n in 1:NUV) {
	#composite labeler factor fu
	fu <- eu[uv[n,1]] + ifelse(L_info_en == 1,sum(beta_U * x_u[n,]),0) + ifelse(S_info_en == 1,sum(beta_S * x_s[n,]),0)
	#composite item factor fv
	fv <- exp(dv[uv[n,2]] + ifelse(I_info_en == 1,sum(beta_V * x_v[n,]),0))
	#logistic regression with different combinations of side info indicated by "L_info_en", "I_info_en", "R_info_en" and "S_info_en"
	tmp <- 1/(1+exp(-fu * fv - ifelse(R_info_en == 1,sum(alpha_u[uv[n,1],]*alpha_v[uv[n,2],]*x_uv[n,]),0)))
	#indicator of whether response equals item true label
    delta <- ifelse(r[n,]==l[uv[n,2]],1,0)
	#log-likelihood of each response
	if ((tmp == 0)|(tmp == 1)|is.infinite(tmp)) next
    obj <- obj - delta*log(tmp) - (1-delta)*log((1-tmp)/(NK-1))
  }
  #log-prior of all the parameters depended on "L_info_en", "I_info_en", "R_info_en" and "S_info_en".
  obj <- obj + .5 * sum((dv-mu_dv)^2)/sigma_sq_dv + .5 * sum((eu-mu_eu)^2)/sigma_sq_eu + ifelse(L_info_en == 1,.5 * sum((beta_U-mu_bu)^2)/sigma_sq_bu,0) 
	+ ifelse(I_info_en == 1,.5 * sum((beta_V-mu_bv)^2)/sigma_sq_bv,0) + ifelse(S_info_en == 1,.5 * sum((beta_S-mu_bs)^2)/sigma_sq_bs,0) 
	+ ifelse(R_info_en == 1,.5 * sum((alpha_u-mu_au)^2)/sigma_sq_au + .5 * sum((alpha_v-mu_av)^2)/sigma_sq_av,0) 
  return(obj)
}

accuracy <- function(keep) {
	
	#compute accuracy by checking the difference between estimated gt and gt
	est_truth <- apply(keep,1,which.max)
	est_truth <- data.frame(est_truth)
	est_truth <- cbind(seq_len(NV),est_truth)
	colnames(est_truth) <- c("new_itemid","est_truth")
	compare_params <- merge(est_truth,gt,by="new_itemid")
	correct_ans <- length(which(compare_params$true_label==compare_params$est_truth))
	accuracy <- correct_ans/nrow(compare_params)
	accuracy <- round(accuracy,digits=5)
	return(accuracy)

}

gradient <- function() {
  dQddv <- c(rep(0, NV)) #gradient of dv
  dQdeu <- c(rep(0, NU)) #gradient of eu
  dQdbv <- c(rep(0, NDV)) # gradient of beta_V
  dQdbu <- c(rep(0, NDU)) # gradient of beta_U
  dQdbs <- c(rep(0, NDS)) # gradient of beta_S
  dQdav <- matrix(0,NV,NDUV) # gradient of alpha_v
  dQdau <- matrix(0,NU,NDUV) # gradient of alpha_u
   
  for (n in 1:NUV) {
  
	#same as in the objective function
	fu <- eu[uv[n,1]] + ifelse(L_info_en == 1,sum(beta_U * x_u[n,]),0) + ifelse(S_info_en == 1,sum(beta_S * x_s[n,]),0)
	fv <- exp(dv[uv[n,2]] + ifelse(I_info_en == 1,sum(beta_V * x_v[n,]),0))
	tmp <- 1/(1+exp(-fu * fv - ifelse(R_info_en == 1,sum(alpha_u[uv[n,1],]*alpha_v[uv[n,2],]*x_uv[n,]),0)))
    delta <- ifelse(r[n,]==l[uv[n,2]],1,0)
	
	#calculate the gradient of each parameter, please refer to the paper draft
    dQdeu[uv[n,1]] <- dQdeu[uv[n,1]] - (delta * (1-tmp) - (1-delta) * tmp) * fv
    dQddv[uv[n,2]] <- dQddv[uv[n,2]] - (delta * (1-tmp) - (1-delta) * tmp) * fu * fv
	if (L_info_en == 1) dQdbu <- dQdbu - (delta * (1-tmp) - (1-delta) * tmp) * fv * x_u[n,] 
    if (I_info_en == 1) dQdbv <- dQdbv - (delta * (1-tmp) - (1-delta) * tmp) * fu * fv * x_v[n,]
    if (S_info_en == 1) dQdbs <- dQdbs - (delta * (1-tmp) - (1-delta) * tmp) * fv * x_s[n,]
	if (R_info_en == 1) {
		dQdau[uv[n,1],] <- dQdau[uv[n,1],] - (delta * (1-tmp) - (1-delta) * tmp) * alpha_v[uv[n,2],] * x_uv[n,]
		dQdav[uv[n,2],] <- dQdav[uv[n,2],] - (delta * (1-tmp) - (1-delta) * tmp) * alpha_u[uv[n,1],] * x_uv[n,]
	}
  }
  
  dQdeu <- dQdeu + (eu - mu_eu) / sigma_sq_eu
  dQddv <- dQddv + (dv - mu_dv) / sigma_sq_dv  
  eu <- eu - eta_eu * dQdeu
  dv <- dv - eta_dv * dQddv
  
  #update the current estimates of all the parameters depended on their gradient descents as well as "L_info_en", "I_info_en", "R_info_en" and "S_info_en"
  if (L_info_en == 1) {dQdbu <- dQdbu + (beta_U - mu_bu) / sigma_sq_bu;beta_U <- beta_U - eta_bu * dQdbu}
  if (I_info_en == 1) {dQdbv <- dQdbv + (beta_V - mu_bv) / sigma_sq_bv;beta_V <- beta_V - eta_bv * dQdbv}
  if (S_info_en == 1) {dQdbs <- dQdbs + (beta_S - mu_bs) / sigma_sq_bs;beta_S <- beta_S - eta_bs * dQdbs}
  if (R_info_en == 1) {
	dQdau <- dQdau + (alpha_u - mu_au) / sigma_sq_au
	alpha_u <- alpha_u - eta_au * dQdau
	dQdav <- dQdav + (alpha_v - mu_av) / sigma_sq_av
	alpha_v <- alpha_v - eta_av * dQdav
  }

  #update prior means and variances of parameters eu, dv, alpha_u, alpha_V using Maximum-likelihood estimation.
  #mu_dv <- sum(dv) / NV
  #mu_eu <- sum(eu) / NU
  #sigma_sq_dv <- sum((dv - mu_dv)^2) / (NV-1)
  #sigma_sq_eu <- sum((eu - mu_eu)^2) / (NU-1)
  #mu_au <- sum(alpha_u) / NU
  #mu_av <- sum(alpha_v) / NV
  #sigma_sq_au <- sum((alpha_u - mu_au)^2) / (NU-1)
  #sigma_sq_av <- sum((alpha_v - mu_av)^2) / (NV-1)
  
  #pack the updated parameters
  params <- list("eu"=eu,"dv"=dv,"mu_dv"=mu_dv,"mu_eu"=mu_eu,"sigma_sq_dv"=sigma_sq_dv,"sigma_sq_eu"=sigma_sq_eu)
  if (L_info_en == 1) params[["beta_U"]] <- beta_U
  if (I_info_en == 1) params[["beta_V"]] <- beta_V
  if (S_info_en == 1) params[["beta_S"]] <- beta_S
  if (R_info_en == 1) {
		params[["alpha_u"]] <- alpha_u
		params[["mu_au"]] <- mu_au
		params[["sigma_sq_au"]] <- sigma_sq_au
		params[["alpha_v"]] <- alpha_v
		params[["mu_av"]] <- mu_av
		params[["sigma_sq_av"]] <- sigma_sq_av
	}
  return(params)
}


r1 <- c(r[,])
uv_uid <- c(uv[,1])
#samples of item true label to keep after burn-in
keep <- matrix(0,NV,NK)
keep1 <- matrix(0,NV,NK)
for (iter in 1:(Nburnin+Nkeep)) {
  
  #monitor objective function
  obj <- objective()
  obj <- round(obj,digits=3)
  #print(paste("obj: ",obj))
  #print(beta_V)
  params <- gradient()
  #unpack parameters
  eu <- params$eu
  dv <- params$dv
  mu_dv <- params$mu_dv
  mu_eu <- params$mu_eu
  sigma_sq_dv <- params$sigma_sq_dv
  sigma_sq_eu <- params$sigma_sq_eu
  if (L_info_en == 1) beta_U <- params$beta_U
  if (I_info_en == 1) beta_V <- params$beta_V
  if (S_info_en == 1) beta_S <- params$beta_S
  if (R_info_en == 1) {
		alpha_u <- params$alpha_u
		mu_au <- params$mu_au
		sigma_sq_au <- params$sigma_sq_au
		alpha_v <- params$alpha_v
		mu_av <- params$mu_av
		sigma_sq_av <- params$sigma_sq_av
	}
  
  count <- table(factor(l, levels=1:NK))
  #Collapsed Gibbs-sampling for item true labels 
  for (j in 1:NV) {
    count[l[j]] <- count[l[j]] - 1
    rinds <- rownames(uv[which(uv$new_itemid==j), ])
    ll <- c(rep(0,NK))
	#compute conditional probability distribution of item true labels depended on "L_info_en", "I_info_en", "R_info_en" and "S_info_en"
    for (rind in rinds) {
		rind <- as.integer(rind)
		delta <- c(rep(0,NK))
		delta[r1[rind]] <- 1
		i <- uv_uid[rind]
		fu <- eu[uv[rind,1]] + ifelse(L_info_en == 1,sum(beta_U * x_u[rind,]),0) + ifelse(S_info_en == 1,sum(beta_S * x_s[rind,]),0)
		fv <- exp(dv[uv[rind,2]] + ifelse(I_info_en == 1,sum(beta_V * x_v[rind,]),0))
		tmp <- 1/(1+exp(-fu * fv - ifelse(R_info_en == 1,sum(alpha_u[uv[rind,1],]*alpha_v[uv[rind,2],]*x_uv[rind,]),0)))
		ll <- ll + delta * log(tmp) + (1-delta)*log((1-tmp)/(NK-1))
    }
 	
    ll <- ll + log((count + gamma)/(NV-1+NK*gamma))
	
	if (anyNA(ll)) {
		
		ind <- which(!(is.na(ll)|is.infinite(ll)))
		if (length(ind) > 0) {
			nind <- which(is.na(ll))
			ll[nind] <- max(ll[ind])-1000
		} else {
			ll <- c(rep(0,NK))
		}
	}
    shift <- max(ll)
    ll <- exp(ll - shift)
    ll_sum <- sum(ll)
    ll <- ll / ll_sum
	l[j] <- sample(1:NK, 1, prob = ll)
    count[l[j]] <- count[l[j]] + 1
    if (iter > Nburnin) {
      keep[j,l[j]] <- keep[j,l[j]] + 1
    }
	keep1[j,l[j]] <- keep1[j,l[j]] + 1
  }
  
  #compute model likelihood for monitoring the convergence of the model learning process
  mll <- 0
  for (j in 1:NV) {
    rinds <- rownames(uv[which(uv$new_itemid==j), ])
    ll <- c(rep(0,NK))
    for (rind in rinds) {
		rind <- as.integer(rind)
		delta <- c(rep(0,NK))
		delta[r1[rind]] <- 1
		i <- uv_uid[rind]
		fu <- eu[uv[rind,1]] + ifelse(L_info_en == 1,sum(beta_U * x_u[rind,]),0) + ifelse(S_info_en == 1,sum(beta_S * x_s[rind,]),0)
		fv <- exp(dv[uv[rind,2]] + ifelse(I_info_en == 1,sum(beta_V * x_v[rind,]),0))
		tmp <- 1/(1+exp(-fu * fv - ifelse(R_info_en == 1,sum(alpha_u[uv[rind,1],]*alpha_v[uv[rind,2],]*x_uv[rind,]),0)))
		ll <- ll + delta * log(tmp) + (1-delta)*log((1-tmp)/(NK-1))
    }
	
    ll <- ll + log((count + gamma)/(NV+NK*gamma))
	
	if (anyNA(ll)) {
		
		ind <- which(!(is.na(ll)|is.infinite(ll)))
		if (length(ind) > 0) {
			nind <- which(is.na(ll))
			ll[nind] <- max(ll[ind])-1000
		} else {
			ll <- c(rep(0,NK))
		}
	}
	
    shift <- max(ll)
    ll <- exp(ll - shift)
    ll_sum <- sum(ll)
    mll <- mll + log(ll_sum)+shift
  }
  
  mll <- round(mll,digits=3)
  accu <- accuracy(keep1)
  eu_mean <- round(mean(eu),digits=3)
  dv_mean <- round(mean(dv),digits=3)
  eu_sd <- round(sd(eu),digits=3)
  dv_sd <- round(sd(dv),digits=3)
  print(paste("mll:",mll,"obj:",obj,"accu:",accu,"eu_mean:",eu_mean,"eu_sd:",eu_sd,"dv_mean:",dv_mean,"dv_sd:",dv_sd,sep=" "))
}
#print(keep)
print(paste("accuracy:", accuracy(keep)))
#output
#line <- paste("accuracy:", toJSON(accuracy), "args:", toJSON(args))
#print(paste("accuracy:", accuracy))
#print(accuracy)
#write(line,file="results.txt",append=TRUE)
