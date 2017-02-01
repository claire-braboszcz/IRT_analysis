
			###-------------------User Input Section-------------------

#In the user Input section, you will enter the relevant information to run your model
#These parameters should be updated to match the parameters of the model you wish to fit to the data


setwd('/home/claire/DATA/Etude Benjamin/')
			
pth<-"/home/claire/DATA/Etude Benjamin/"	
f_nme<-"n48_PrePostSuivi.csv"
f_nmeo<-"data_out_IERS_glob.csv"


#Identify which variable names correspond to the particular elements of the model
#depicted in Figure 1. enclose variable names in "" as indicated.
#x_v is the focal independent variable. 
#Multiple co-variates may be controlled for by entering them for cov_v, separated by commas as indicated.
#m_v is the mediator variable and y_v is the dependent variable.
#Note: R is case sensitive


x_v<-"NombreCauchemarParMois.PRE"    #Don't forget to leave the quotes around the names
cov_v<-c("") #To run a model without covariates, use c(""); 
	#additional coviariates can be entered; enclose each name in quotes & separate with commas

# Mediator
m_v<-"IERS.Global.PRE"

# VD
y_v<-"NombreCauchemarParMois.POST"

		##---------------Cross Method Input-------------------

#Values input in this section will be shared across methods
#Input your allowable type-I error rate parameter
#Syntax conducts two-tailed test (e.g. type_1 = 0.05 -> z = +/-1.96 for confidence inveral endpoints)
type_1<-0.05 

#input the starting value for the pseudo-random number generator (for replicability)
#Do not need to change to run syntax
#For more on the nature of this "starting value" (i.e., random seed)
#see Koopman, Howe & Hollenbeck (2014) chapter in "More Statistical and Methodological Myths and Urban Legends
ks<-0

#Increase default memory allocation. Requested limit in MB. For 64bit R/64bit Windows, limit is 8Tb
#when either is 32 bit, limit is ~4Gb. See ?memory.size for more information
memory.limit(size=12000)

		##---------------End Cross Method Input-------------------

		##---------------Method Specific Input-------------------

	#---------------Bootstrapping Specific Input---------------

boot_qty<-5000 #enter number of bootstrap samples used to estimate sampling distribution
			#Must be > or = to sample size in order to compute BCA intervals
			#5,000 bootstrap iterations per Preacher & Hayes (2004)

	#---------------End Bootstrapping Specific Input---------------

	#---------------Permutation Specific Input---------------

permu<-2000 #enter number of permutations used to estimate sampling distribution
		#2,000 iterations per Taylor & MacKinnon (2012)

	#---------------End Permutation Specific Input---------------

	#---------------Bayes Specific Input---------------

#Simulation Parameters
burn<-1000 # number of initial MCMC steps to discard (burn-in)
			#typically a few hundred is sufficient (Yuan & MacKinnon, 2009)
stps<-10000 # number of MCMC steps that are retained

#Because the Bayesian method requires a greater deal of user input, in order to simplify model specification, 
#by default, results will be provided for a standardized version of the data in the input file. This allows for
#user specification of informative priors in terms of standardized effect sizes. However, this default may be 
#overwritten with the specification below, by setting bys_std<-0. Doing this also requires the respecification of
#the priors in terms of the raw scales of data used in the model. See Yuan & MacKinnon (2009) for more specific
#information pertaining to the specification of appropriate priors.

#Toggle indicating whether standardized (set equal to 1) or raw (set equal to 0) data is to be used for the Bayesian
#analysis. Be sure that the priors specified below are input in the same units as the intended analysis.
bys_std<-1

#Prior Distribution Parameters 
#By default, these are set as an uniformative prior (with a mean of zero and a standard deviation of 1000). 
#When warranted, an informative prior can be specified for any/all of the relationships. See Yuan & MacKinnon (2009).

#Model 2: Y ~ M + X
m2_b0_m<-0 #mean of the prior distribution for the intercept for Model 2
m2_b0_sd<-1000 #standard deviation of the prior distribution for the intercept for Model 2

m2_b1_m<-0 #mean of the prior distribution for the relationship between M&Y for Model 2
m2_b1_sd<-1000 #standard deviation of the prior distribution for the relationship between M&Y for Model 2

m2_b2_m<-0 #mean of the prior distribution for the relationship between X&Y for Model 2
m2_b2_sd<-1000 #standard deviation of the prior distribution for the relationship between X&Y for Model 2

m2_b3_m<-c(0,0) #means of the prior distributions for the relationship between Y&Covariates for Model 2
m2_b3_sd<-c(1000,1000) #standard deviations of the  prior distributions for the relationship between Y&Covariates for Model 2
	#You must specify a prior distribution for each covariate. List in the same order as the covariates are
	#listed above when named in the cov_v variable specification. Separate each entry with a comma. 
	#If you did not specify any priors above in the initial model specification (i.e. cov_v), you can ignore these inputs

#Model 3: M ~ X
m3_b0_m<-0 #mean of the prior distribution for the intercept for Model 3
m3_b0_sd<-1000 #standard deviation of the prior distribution for the intercept for Model 3

m3_b2_m<-0 #mean of the prior distribution for the relationship between X&M for Model 3
m3_b2_sd<-1000 #standard deviation of the prior distribution for the relationship between X&M for Model 3

m3_b3_m<-c(0,0) #means of the prior distributions for the relationship between M&Covariates for Model 3
m3_b3_sd<-c(1000,1000) #standard deviations of the  prior distributions for the relationship between M&Covariates for Model 3
	#You must specify a prior distribution for each covariate. List in the same order as the covariates are
	#listed above when named in the cov_v variable specification. Separate each entry with a comma. 
	#If you did not specify any priors above in the initial model specification (i.e. cov_v), you can ignore these inputs

#Note: Bayesian prior specification is often in terms of precision (1/variance). For ease of entry, the sd values 
#above are converted to precision values automatically. 

	#---------------End Bayes Specific Input---------------		

		##---------------End Method Specific Input-------------------

				###---------------End User Input Section-------------------


#Output Start Time
print("Start Time")
print(Sys.time())

#flag to indicate presence of covariates
if(nchar(cov_v)[1]>=1) covp<-1 else covp<-0

#Read in sample data
#by default, this command assumes a .csv file with column names and missing data is NA
#to modify these assumptions, type ?read.table for additional information
setwd(pth)
dfo<-read.table(f_nme, header=TRUE, sep=",", na="NA", dec=".",as.is = FALSE,skip=0)
dim(dfo) #ensure that number of rows and columns are as expected for the whole data-set

#subset of data with only the variables comprising the desired model
if(covp==1) df<-dfo[,c(x_v,cov_v,m_v,y_v)] else df<-dfo[,c(x_v,m_v,y_v)]

#listwise deletion of cases with missing data
df<-na.omit(df)
df_dim<-dim(df)

#assign proper variables to each component of Figure 1
if(covp==1) {
	x<-as.data.frame(df[,c(x_v,cov_v)])
	nme_x<-c(x_v,cov_v)} else {
	x<-as.data.frame(df[,x_v])
	nme_x<-x_v}
names(x)<-nme_x
dim_x<-dim(x)
m<-as.data.frame(df[,m_v])
names(m)<-m_v
y<-as.data.frame(df[,y_v])
names(y)<-y_v

#generate standardized variables
require(psych)
x_vs<-paste(nme_x,"_n",sep="")
m_vs<-paste(m_v,"_n",sep="")
y_vs<-paste(y_v,"_n",sep="")
x_n<-as.data.frame(scale(x))
names(x_n)<-x_vs
m_n<-as.data.frame(scale(m))
names(m_n)<-m_vs
y_n<-as.data.frame(scale(y))
names(y_n)<-y_vs

#format output data_frame
df_o<-data.frame(x_v,stringsAsFactors=FALSE)
names(df_o)<-"X"
if(covp==1) df_o$Covar<-paste(cov_v,sep="",collapse=",") else df_o$Covar<-"none"
df_o$M<-m_v
df_o$Y<-y_v
df_o$Type_I_alpha<-type_1
df_o$Num_Obs<-df_dim[1]
df_o$Start_time<-Sys.time()

#calculate control limits
llt<-qnorm((type_1)/2)
ult<-qnorm(1-(type_1)/2)

#Modify Sobel function from multilevel package (Bliese, 2013) to allow for multiple x variables
#essentially reorder the model 2 regression so that the coefficient for the mediator is after the intercept
#instead of after a variable number of x & covar coefficients
sbl<-function (pred, med, out) {
    NEWDAT <- data.frame(pred = pred, med = med, out = out)
    NEWDAT <- na.exclude(NEWDAT)
    model1 <- lm(out ~ pred, data = NEWDAT)
    model2 <- lm(out ~ med + pred, data = NEWDAT)
    model3 <- lm(med ~ pred, data = NEWDAT)
    mod1.out <- summary(model1)$coef
    mod2.out <- summary(model2)$coef
    mod3.out <- summary(model3)$coef
    indir <- mod3.out[2, 1] * mod2.out[2, 1]
    effvar <- (mod3.out[2, 1])^2 * (mod2.out[2, 2])^2 + (mod2.out[2, 
        1])^2 * (mod3.out[2, 2])^2
    serr <- sqrt(effvar)
    zvalue = indir/serr
    out <- list(`Mod1: Y~X` = mod1.out, `Mod2: Y~M+X` = mod2.out, 
        `Mod3: M~X` = mod3.out, Indirect.Effect = indir, SE = serr, 
        z.value = zvalue, N = nrow(NEWDAT),YX=model1,YM=model2,MX=model3)
    return(out)
}

##########------SOBEL-------------------##########
sb<-sbl(as.matrix(x),as.matrix(m),as.matrix(y))
sb_n<-sbl(as.matrix(x_n),as.matrix(m_n),as.matrix(y_n))

#Report base model relationships (a,b & c paths)
x_nme<-paste(x_v,"_to_",y_v,sep="")
y_nme<-paste(m_v,"_to_",y_v,sep="")
m_nme<-paste(x_v,"_to_",m_v,sep="")

#a
m_nme_s<-paste(m_nme,"_Std",sep="")
m_nme_se<-paste(m_nme,"_SE",sep="")
m_nme_t<-paste(m_nme,"_t",sep="")
m_nme_p<-paste(m_nme,"_p",sep="")
df_o[1,m_nme]<-sb$Mod3[2,1] #effect of X on M, controlling for Covar
df_o[1,m_nme_s]<-sb_n$Mod3[2,1] #Standardized effect of X on M, controlling for Covar
df_o[1,m_nme_se]<-sb$Mod3[2,2] #standard error of effect of X on M, controlling for Covar
df_o[1,m_nme_t]<-sb$Mod3[2,3] #t-value for effect of X on M, controlling for Covar
df_o[1,m_nme_p]<-sb$Mod3[2,4] #p-value for effect of X on M, controlling for Covar

if(covp==1) {
	cov_num<-length(cov_v)
	cov_nme2<-paste(cov_v,"_to_",m_v,sep="")
	cov_nme2_s<-paste(cov_nme2,"_Std",sep="")
	cov_nme2_se<-paste(cov_nme2,"_SE",sep="")
	cov_nme2_t<-paste(cov_nme2,"_t",sep="")
	cov_nme2_p<-paste(cov_nme2,"_p",sep="")
	df_o[,cov_nme2]<-sb$Mod3[(3:(cov_num+2)),1] #effect of each covar on M, controlling for other covar & X
	df_o[,cov_nme2_s]<-sb_n$Mod3[(3:(cov_num+2)),1] #Standardized effect of each covar on M, controlling for other covar & X
	df_o[,cov_nme2_se]<-sb$Mod3[(3:(cov_num+2)),2] #standard error of effect of each covar on M, controlling for other covar & X
	df_o[,cov_nme2_t]<-sb$Mod3[(3:(cov_num+2)),3] #t-value for effect of each covar on M, controlling for other covar & X
	df_o[,cov_nme2_p]<-sb$Mod3[(3:(cov_num+2)),4] #p-value for effect of each covar on M, controlling for other covar & X
}

#b
y_nme_s<-paste(y_nme,"_Std",sep="")
y_nme_se<-paste(y_nme,"_SE",sep="")
y_nme_t<-paste(y_nme,"_t",sep="")
y_nme_p<-paste(y_nme,"_p",sep="")
df_o[1,y_nme]<-sb$Mod2[2,1] #effect of M on Y, controlling for X & Covar
df_o[1,y_nme_s]<-sb_n$Mod2[2,1] #standardized effect of M on Y, controlling for X & Covar
df_o[1,y_nme_se]<-sb$Mod2[2,2] #standard error of effect of M on Y, controlling for X & Covar
df_o[1,y_nme_t]<-sb$Mod2[2,3] #t-value for effect of M on Y, controlling for X & Covar
df_o[1,y_nme_p]<-sb$Mod2[2,4] #p-value for effect of M on Y, controlling for X & Covar

#c
x_nme_s<-paste(x_nme,"_Std",sep="")
x_nme_se<-paste(x_nme,"_SE",sep="")
x_nme_t<-paste(x_nme,"_t",sep="")
x_nme_p<-paste(x_nme,"_p",sep="")

df_o[1,x_nme]<-sb$Mod1[2,1] #effect of X on Y, controlling for Covar
df_o[1,x_nme_s]<-sb_n$Mod1[2,1] #standardized effect of X on Y, controlling for Covar
df_o[1,x_nme_se]<-sb$Mod1[2,2] #standard error of effect of X on Y, controlling for Covar
df_o[1,x_nme_t]<-sb$Mod1[2,3] #t-value for effect of X on Y, controlling for Covar
df_o[1,x_nme_p]<-sb$Mod1[2,4] #p-value for effect of X on Y, controlling for Covar

if(covp==1) {
	cov_nme<-paste(cov_v,"_to_",y_v,sep="")
	cov_nme_s<-paste(cov_nme,"_Std",sep="")
	cov_nme_se<-paste(cov_nme,"_SE",sep="")
	cov_nme_t<-paste(cov_nme,"_t",sep="")
	cov_nme_p<-paste(cov_nme,"_p",sep="")
	df_o[,cov_nme]<-sb$Mod1[(3:(cov_num+2)),1] #effect of each covar on Y, controlling for other covar & X
	df_o[,cov_nme_s]<-sb_n$Mod1[(3:(cov_num+2)),1] #standardized effect of each covar on Y, controlling for other covar & X
	df_o[,cov_nme_se]<-sb$Mod1[(3:(cov_num+2)),2] #standard error of effect of each covar on Y, controlling for other covar & X
	df_o[,cov_nme_t]<-sb$Mod1[(3:(cov_num+2)),3] #t-value for effect of each covar on Y, controlling for other covar & X
	df_o[,cov_nme_p]<-sb$Mod1[(3:(cov_num+2)),4] #p-value for effect of each covar on Y, controlling for other covar & X
}

#c'
x_nmep<-paste(x_nme,"_Prime",sep="")
x_nmep_s<-paste(x_nmep,"_Std",sep="")
x_nmep_se<-paste(x_nmep,"_SE",sep="")
x_nmep_t<-paste(x_nmep,"_t",sep="")
x_nmep_p<-paste(x_nmep,"_p",sep="")

df_o[1,x_nmep]<-sb$Mod2[3,1] #effect of X on Y, controlling for Covar
df_o[1,x_nmep_s]<-sb_n$Mod2[3,1] #standardized effect of X on Y, controlling for Covar
df_o[1,x_nmep_se]<-sb$Mod2[3,2] #standard error of effect of X on Y, controlling for Covar
df_o[1,x_nmep_t]<-sb$Mod2[3,3] #t-value for effect of X on Y, controlling for Covar
df_o[1,x_nmep_p]<-sb$Mod2[3,4] #p-value for effect of X on Y, controlling for Covar


#Report out Sobel effects
df_o$Indir_Effect<-sb$Indirect.Effect #estimated indirect effect
df_o$Sobel_SE<-sb$SE #estimated standard error
df_o$Sobel_z<-sb$z.value #estimated z value for indirect effect
df_o$Sobel_p<-(1-pnorm(abs(sb$z.value)))*2 #estimated p-value for indirect effect
df_o$Sobel_LCI<-sb$Indirect.Effect+llt*sb$SE #LCL for indirect effect
df_o$Sobel_UCI<-sb$Indirect.Effect+ult*sb$SE #UCL for indirect effect
df_o$Indir_Effect_Std<-sb_n$Indirect.Effect #estimated indirect effect (Standardized)
df_o$Sobel_SE_Std<-sb_n$SE #estimated standard error (Standardized)
df_o$Sobel_z_Std<-sb_n$z.value #estimated z value for indirect effect
df_o$Sobel_p_Std<-(1-pnorm(abs(sb_n$z.value)))*2 #estimated p-value for indirect effect
df_o$Sobel_LCI_Std<-sb_n$Indirect.Effect+llt*sb_n$SE #LCL for indirect effect (Standardized)
df_o$Sobel_UCI_Std<-sb_n$Indirect.Effect+ult*sb_n$SE #UCL for indirect effect (Standardized)

##########------BOOTSTRAPPING-------------------##########
#create function that calculates indirect effect for a given set of data
medi<-function(idf,i,...)
{
	dt<-idf[i,]
	bsmdl<-sbl(dt[,(1:l_x)],dt[,(l_x+1)],dt[,(l_x+2)])
	bsmdl$Indirect.Effect
}

#Generate input data set
rvar_mtr<-as.matrix(cbind(x,m,y))
svar_mtr<-as.matrix(cbind(x_n,m_n,y_n))
rownames(rvar_mtr)<-NULL
rownames(svar_mtr)<-NULL
#set variable to count number of IVs
l_x<-length(nme_x)

#Run bootstrap (Data, function, Num of iterations, ordinary type, using indicies)
	#bs$t contains actual indirect effect for each bootstrap sample
	#bs$t0 contains actual indirect effect for the original sample
	#Look at bootstrapped samples (num of rows = num of iterations; num of columns=sample size)
	#indx<-boot.array(bs, indices=FALSE) #each row sums to original data size
	#If set Indices to TRUE, lists index number used to create bootstrapped iteration
require(boot)
set.seed(ks) #remove effects from changes to starting point
bs<-boot(rvar_mtr,medi,R=boot_qty,sim="ordinary", stype="i") #Raw input
set.seed(ks) #remove effects from changes to starting point
bs_n<-boot(svar_mtr,medi,R=boot_qty,sim="ordinary", stype="i") #Raw input

#Calculate Confidence Intervals. Gives middle "conf" percentage (e.g. 0.95=2.5%-97.5%)
	#values stored in bc$bca
#create weight vector for BC confidence intervals
	#the boot package does not do BC intervals by default. Using this weight vector allows for a concise
	#means of estimating the BC intervals. A much more expansive syntax that explicitly uses the methods of Efron (1987)
	#is available from the authors upon request.
l<-c(rep(1,times=ceiling(df_dim[1]/2)),rep(-1,times=floor(df_dim[1]/2)))

bc_bca<-boot.ci(bs,conf=(1-type_1),type="bca")
bc_bc<-boot.ci(bs,conf=(1-type_1),type="bca", L=l)
bc_pcnt<-boot.ci(bs,conf=(1-type_1),type="perc")

bc_bca_n<-boot.ci(bs_n,conf=(1-type_1),type="bca")
bc_bc_n<-boot.ci(bs_n,conf=(1-type_1),type="bca", L=l)
bc_pcnt_n<-boot.ci(bs_n,conf=(1-type_1),type="perc")

#bootstrap output - Raw
df_o$Bstrap_Number<-boot_qty
df_o$BStrap_Median_Indir_Eff<-median(bs$t) #Average Indirect Effect
df_o$BStrap_Mean_Indir_Eff<-mean(bs$t) #Average Indirect Effect
df_o$BStrap_Indir_Eff_SE<-sd(bs$t) #Standard Deviation of estimated sampling distribution
df_o$BStrap_Indir_Eff_Bias<-df_o$BStrap_Mean_Indir_Eff-bs$t0 
	#difference between average of estimated sampling distribution and indirect effect for original sample
df_o$BStrap_BCA_LCI<-bc_bca$bca[4] 		#BCA Lower CI limit
df_o$BStrap_BCA_UCI<-bc_bca$bca[5] 		#BCA Upper CI limit
df_o$BStrap_BC_LCI<-bc_bc$bca[4] 		#BC Lower CI limit
df_o$BStrap_BC_UCI<-bc_bc$bca[5]		#BC Upper CI limit
df_o$BStrap_PCNT_LCI<-bc_pcnt$percent[4]	#Percentile Lower CI limit
df_o$BStrap_PCNT_UCI<-bc_pcnt$percent[5]	#Percentile Upper CI limit

#bootstrap output - Standardized
df_o$BStrap_Median_Indir_Eff_Std<-median(bs_n$t) #Average Indirect Effect
df_o$BStrap_Mean_Indir_Eff_Std<-mean(bs_n$t) #Average Indirect Effect
df_o$BStrap_Indir_Eff_SE_Std<-sd(bs_n$t) #Standard Deviation of estimated sampling distribution
df_o$BStrap_Indir_Eff_Bias_Std<-df_o$BStrap_Mean_Indir_Eff_Std-bs_n$t0 
	#difference between average of estimated sampling distribution and indirect effect for original sample
df_o$BStrap_BCA_LCI_Std<-bc_bca_n$bca[4] 		#BCA Lower CI limit
df_o$BStrap_BCA_UCI_Std<-bc_bca_n$bca[5] 		#BCA Upper CI limit
df_o$BStrap_BC_LCI_Std<-bc_bc_n$bca[4] 		#BC Lower CI limit
df_o$BStrap_BC_UCI_Std<-bc_bc_n$bca[5]		#BC Upper CI limit
df_o$BStrap_PCNT_LCI_Std<-bc_pcnt_n$percent[4]	#Percentile Lower CI limit
df_o$BStrap_PCNT_UCI_Std<-bc_pcnt_n$percent[5]	#Percentile Upper CI limit


##########------PERMUTATION-------------------##########
#Reference: Taylor & MacKinnon, 2012
#define count variable
pl<-permu-1
#Generate string of random numbers to permute residuals		
inlst<-(1:(2*pl*df_dim[1]))
set.seed(ks) #set random seed for replicability
rv<-sample(inlst,length(inlst),replace=FALSE) #generate random string of numbers by sampling

#Structure data frame with observational values
dfp<-as.data.frame(rep((1:pl),each=df_dim[1]))
names(dfp)<-"perm"
dim_dfp<-dim(dfp)
dfp[,((dim_dfp[2]+1):(dim_dfp[2]+dim_x[2]))]<-x
dfp[,"M"]<-m
dfp[,"Y"]<-y

##extract necessary data from regression equations embedded in "sbl" Sobel function
#Unscaled variables
alpha_res<-sb$MX$residuals #extract M~X+COV model residuals
alpha_hat<-sb$MX$fitted.values #extract M~X+COV model fitted values
beta_res<-sb$YM$residuals #extract Y~M+X+COV model residuals
beta_hat<-sb$YM$fitted.values #extract Y~M+X+COV model fitted values

#Add fitted values to permutation data frame
dfp$alpha_hat<-rep(alpha_hat,times=pl) #m model fitted values for each permutation
dfp$beta_hat<-rep(beta_hat,times=pl) #y model fitted values for each permutation

#Generate vectors of residuals
alpha_res_l<-rep(alpha_res,times=pl) #m model residuals for each permutation
beta_res_l<-rep(beta_res, times=pl) #y model residuals for each permutation

#Add re-ordered residuals to permutation data frame
#Seperate the list of numbers and combine with residuals from both models
ol_a<-as.matrix(cbind(dfp$perm,rv[1:(pl*df_dim[1])])) #first half w/permutation number
ol_b<-as.matrix(cbind(dfp$perm,rv[(pl*df_dim[1]+1):(2*pl*df_dim[1])])) #second half w/permutation number
#Generate vector of re-order indices
oi_a<-order(ol_a[,1],ol_a[,2]) #sort order alpha; by permutation number then random number
oi_b<-order(ol_b[,1],ol_b[,2]) #sort order beta; by permutation number then random number
dfp$alpha_res<-alpha_res_l[oi_a] #reorder alpha residuals (permute)
dfp$beta_res<-beta_res_l[oi_b] #reorder beta residuals (permute)
#Calculate new DV's for subsequent regression modeling
dfp$m_new<-dfp$alpha_hat+dfp$alpha_res #calculate new m (m_hat (predicted) + permuted residuals)
dfp$y_new<-dfp$beta_hat+dfp$beta_res #calculate new y (y_hat (predicted) + permuted residuals)

#perform permu-1 regressions for each model
#set-up appropriate formula objects for the independent variables included in the model
#mediator
for_m<-paste("m_new ~ ",paste(nme_x,collapse="+"),"|perm",sep="")
for_m<-as.formula(for_m)

#dv
for_y<-paste("y_new ~ ","M +",paste(nme_x,collapse="+"),"|perm",sep="")
for_y<-as.formula(for_y)


#Run multiple regressions per formulas specified above
require(nlme)
reg_m<-lmList(for_m,pool=FALSE,data=dfp) #run series of regressions m on x; group by permutation number
reg_y<-lmList(for_y,pool=FALSE,data=dfp) #run series of regressions y on x & m; group by permutation number

regc_m<-coef(reg_m) #extract m regression coefficients
regc_y<-coef(reg_y) #extract y regression coefficients

dfp<-NULL
reg_m<-NULL
reg_y<-NULL

#Add standardized values
dfp<-as.data.frame(rep((1:pl),each=df_dim[1]))
names(dfp)<-"perm"
dim_dfp<-dim(dfp)
dfp[,((dim_dfp[2]+1):(dim_dfp[2]+dim_x[2]))]<-x_n
dfp[,"M_n"]<-m_n
dfp[,"Y_n"]<-y_n

##extract necessary data from regression equations embedded in "sbl" Sobel function
#Standardized variables
alpha_res_n<-sb_n$MX$residuals #extract M~X+COV model residuals
alpha_hat_n<-sb_n$MX$fitted.values #extract M~X+COV model fitted values
beta_res_n<-sb_n$YM$residuals #extract Y~M+X+COV model residuals
beta_hat_n<-sb_n$YM$fitted.values #extract Y~M+X+COV model fitted values

#Add fitted values to permutation data frame
dfp$alpha_hat_n<-rep(alpha_hat_n,times=pl) #standardized m model fitted values for each permutation
dfp$beta_hat_n<-rep(beta_hat_n,times=pl) #standardized y model fitted values for each permutation

#Generate vectors of residuals
alpha_res_l_n<-rep(alpha_res_n,times=pl) #standardized m model residuals for each permutation
beta_res_l_n<-rep(beta_res_n, times=pl) #standardized y model residuals for each permutation

#Add re-ordered residuals to permutation data frame
dfp$alpha_res_n<-alpha_res_l_n[oi_a] #reorder standardized alpha residuals (permute)
dfp$beta_res_n<-beta_res_l_n[oi_b] #reorder standardized beta residuals (permute)

#Calculate new DV's for subsequent regression modeling
dfp$m_new_n<-dfp$alpha_hat_n+dfp$alpha_res_n #calculate new standardized m (m_hat (predicted) + permuted residuals)
dfp$y_new_n<-dfp$beta_hat_n+dfp$beta_res_n #calculate new y (y_hat (predicted) + permuted residuals)

#perform permu-1 regressions for each model
#set-up appropriate formula objects for the independent variables included in the model
#mediator
for_m_n<-paste("m_new_n ~ ",paste(x_vs,collapse="+"),"|perm",sep="")
for_m_n<-as.formula(for_m_n)
#dv
for_y_n<-paste("y_new_n ~ ","M_n +",paste(x_vs,collapse="+"),"|perm",sep="")
for_y_n<-as.formula(for_y_n)

#Run multiple regressions per formulas specified above
require(nlme)
reg_m_n<-lmList(for_m_n,pool=FALSE,data=dfp) #run series of regressions for standardized m on x; group by permutation number
reg_y_n<-lmList(for_y_n,pool=FALSE,data=dfp) #run series of regressions for standardized y on x & m; group by number

regc_m_n<-coef(reg_m_n) #extract m regression coefficients
regc_y_n<-coef(reg_y_n) #extract y regression coefficients

dfp<-NULL
reg_m_n<-NULL
reg_y_n<-NULL

#Create estimate of indirect effect
#Raw
alpha_smpdist<-c(sb$MX$coefficients[2],regc_m[,2]) #add-back initial m regression coeff
beta_smpdist<-c(sb$YM$coefficients[2],regc_y[,2]) #add-back initial y regression coeff
alphb<-alpha_smpdist*beta_smpdist #multiply alpha and beta to get indirect effect
#Scaled
alpha_smpdist_n<-c(sb_n$MX$coefficients[2],regc_m_n[,2]) #add-back initial m regression coeff
beta_smpdist_n<-c(sb_n$YM$coefficients[2],regc_y_n[,2]) #add-back initial y regression coeff
alphb_n<-alpha_smpdist_n*beta_smpdist_n #multiply alpha and beta to get indirect effect

#Write-out indirect effect control limits based on permutation results
df_o$Permute_Number<-permu
df_o$Permute_Median_Ind_Eff<-median(alphb)
df_o$Permute_Mean_Ind_Eff<-mean(alphb)
df_o$Permute_SE_Ind_Eff<-sd(alphb)
df_o$Permute_LCI<-quantile(alphb,probs=type_1/2,type=4)
df_o$Permute_UCI<-quantile(alphb,probs=(1-type_1/2),type=4)
df_o$Permute_Median_Ind_Eff_Std<-median(alphb_n)
df_o$Permute_Mean_Ind_Eff_Std<-mean(alphb_n)
df_o$Permute_SE_Ind_Eff_Std<-sd(alphb_n)
df_o$Permute_LCI_Std<-quantile(alphb_n,probs=type_1/2,type=4)
df_o$Permute_UCI_Std<-quantile(alphb_n,probs=(1-type_1/2),type=4)
#Note: The robustness of the significance of these effects can be explored manually by changing the value selected for probs
#e.g. setting probs equal to 0.005 gives the Lower control limit for two-tail alpha = 0.01 
	#(i.e., useful for testing significance at p=0.01)
	

##########------BAYES-------------------##########
#Reference: Yuan & MacKinnon, 2009

#Output general model info
df_o$Bayes_Std_Toggle<-bys_std #Toggle to indicate whether prior specification, analysis & results are standardized (1=YES)
df_o$Bayes_burnin<-burn
df_o$Bayes_number<-stps

#Output prior distribution parameters
#Model 2
df_o$Bayes_Mod2_b0_m<-m2_b0_m
df_o$Bayes_Mod2_b0_sd<-m2_b0_sd
df_o$Bayes_Mod2_YM_m<-m2_b1_m
df_o$Bayes_Mod2_YM_sd<-m2_b1_sd
df_o$Bayes_Mod2_YX_m<-m2_b2_m
df_o$Bayes_Mod2_YX_sd<-m2_b2_sd
if (covp==1) {
	df_o$Bayes_Mod2_YCov_m<-paste(m2_b3_m,collapse=", ")
	df_o$Bayes_Mod2_YCov_sd<-paste(m2_b3_sd,collapse=", ")
	}

#Model 3
df_o$Bayes_Mod3_b0_m<-m3_b0_m
df_o$Bayes_Mod3_b0_sd<-m3_b0_sd
df_o$Bayes_Mod3_MX_m<-m3_b2_m
df_o$Bayes_Mod3_MX_sd<-m3_b2_sd

if (covp==1) {
	df_o$Bayes_Mod3_MCov_m<-paste(m3_b3_m,collapse=", ")
	df_o$Bayes_Mod3_MCov_sd<-paste(m3_b3_sd,collapse=", ")
	}

#Prior Distribution Parameters: Convert user input into vectors suitable for model entry
#Mean vectors
if(covp==1) by_m2_m<-c(m2_b0_m, m2_b1_m, m2_b2_m, m2_b3_m) else by_m2_m<-c(m2_b0_m, m2_b1_m, m2_b2_m)
if(covp==1) by_m3_m<-c(m3_b0_m, m3_b2_m, m3_b3_m) else by_m3_m<-c(m3_b0_m, m3_b2_m)

#Precision vectors
if(covp==1) by_m2_sd<-c(m2_b0_sd, m2_b1_sd, m2_b2_sd, m2_b3_sd) else by_m2_sd<-c(m2_b0_sd, m2_b1_sd, m2_b2_sd)
if(covp==1) by_m3_sd<-c(m3_b0_sd, m3_b2_sd, m3_b3_sd) else by_m3_sd<-c(m3_b0_sd, m3_b2_sd)

#convert from sd to precision
by_m2_p<-by_m2_sd^-2
by_m3_p<-by_m3_sd^-2

#Load Required Packages
require(MCMCpack)
if(bys_std==1) {
	y_h<-MCMCregress(as.matrix(y_n)~as.matrix(m_n)+as.matrix(x_n),burnin=burn,mcmc=stps,seed=ks,b0=by_m2_m,B0=by_m2_p)
	med<-MCMCregress(as.matrix(m_n)~as.matrix(x_n),burnin=burn,mcmc=stps,seed=ks,b0=by_m3_m,B0=by_m3_p)
	} else {
	y_h<-MCMCregress(as.matrix(y)~as.matrix(m)+as.matrix(x),burnin=burn,mcmc=stps,seed=ks,b0=by_m2_m,B0=by_m2_p)
	med<-MCMCregress(as.matrix(m)~as.matrix(x),burnin=burn,mcmc=stps,seed=ks,b0=by_m3_m,B0=by_m3_p)
}
		

ie_b<-med[,2]*y_h[,2] #estimate posterior distribution of indirect effect

#write out model parameters
df_o$Bayes_Median_Indir_Eff_Post<-median(ie_b)
df_o$Bayes_Mean_Indir_Eff_Post<-mean(ie_b)
df_o$Bayes_SE_Indir_Eff_post<-sd(ie_b)
if(sb$Indirect.Effect>=0) df_o$Bayes_Indir_Eff_One_Tail_p<-length(ie_b[ie_b<=0])/stps else {
	df_o$Bayes_Indir_Eff_One_Tail_p<-length(ie_b[ie_b>=0])/stps}
	#One tail p-value as calculated by: Muth?n, B. (2010). 
	#Bayesian analysis in Mplus: A brief introduction. Technical Report. Version 3.  
df_o$Bayes_LCI<-quantile(ie_b,probs=type_1/2,type=4)
df_o$Bayes_UCI<-quantile(ie_b,probs=(1-type_1/2),type=4)

##Screen Output
#Direct Effects
msca<-which(names(df_o)==x_nme)
meca<-which(names(df_o)==x_nmep_p)
df_o[,msca:meca]

#SOBEL OUTPUT
sb$Mod1
sb$Mod2
sb$Mod3
sb_n$Mod1
sb_n$Mod2
sb_n$Mod3
ssc<-which(names(df_o)=="Indir_Effect")
sec<-which(names(df_o)=="Sobel_UCI_Std")
df_o[,ssc:sec]

#BOOTSTRAP OUTPUT
ssc2a<-which(names(df_o)=="BStrap_BCA_LCI")
sec2a<-which(names(df_o)=="BStrap_PCNT_UCI")
ssc2b<-which(names(df_o)=="BStrap_BCA_LCI_Std")
sec2b<-which(names(df_o)=="BStrap_PCNT_UCI_Std")
df_o[,c(ssc2a:sec2a,ssc2b:sec2b)]

#PERMUTATION OUTPUT
ssc3a<-which(names(df_o)=="Permute_LCI")
sec3a<-which(names(df_o)=="Permute_UCI")
ssc3b<-which(names(df_o)=="Permute_LCI_Std")
sec3b<-which(names(df_o)=="Permute_UCI_Std")
df_o[,c(ssc3a:sec3a,ssc3b:sec3b)]

#BAYES OUTPUT
ssc4a<-which(names(df_o)=="Bayes_Std_Toggle")
ssc4<-ssc<-which(names(df_o)=="Bayes_LCI")
sec4<-ssc<-which(names(df_o)=="Bayes_UCI")
df_o[,c(ssc4a,ssc4:sec4)]

#Write_out_data_file
if(nchar(f_nmeo)>1) write.table(t(df_o),f_nmeo,sep=",", na="", dec=".", row.names = TRUE)

#View diagnostic plots of MCMC for Bayesian analysis
#see documentation for coda package for interpretation and other options
require(coda)
#Trace plots
plot(y_h)
plot(med)

#autocorrelation
autocorr.plot(y_h)
autocorr.plot(med)

#Geweke
geweke.diag(y_h)
geweke.diag(med)

#Heidel
heidel.diag(y_h)
heidel.diag(med)





