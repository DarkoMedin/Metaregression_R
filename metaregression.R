


#Load the required libraries for this tutorial
install.packages('metafor')
install.packages('readxl')
library(metafor)
library(readxl)




#Load the Artificial MEta-analysis dataset and explore it
mdata <- read_excel(
  "C:/Users/MEDIN/Desktop/Meta analysis/Meta-analysis/post5/mregdat.xlsx")
View(mregdat)



#We will use the Risk ratios and their  95% Confidence intervals to :
#derive log confidence intervals - logci=log(ci)
#derive logRRs - logRR=log(RR)
#derive standard errors of the log RRs  
#SE(logRR)=lowerci(logRR)-upperci(logRR)/3.92
mdata['logRR']=log(mdata$rr)
mdata['loglci']=log(mdata$lci)
mdata['loguci']=log(mdata$uci)
mdata['logRRse']=(mdata$loguci-mdata$loglci)/3.92
mdata['mod']=((mdata$logRR*1.75)+6+rnorm(19,0,0.1))



#Create DerSimonian&Laird meta-regression object 
#Summarize the results
meta_dl=rma(yi=mdata$logRR, 
            sei=mdata$logRRse,
            mods = mdata$mod,
            method = 'DL', measure = 'RR')

summary(meta_dl)






#Create DL meta-regression plot using logRR scale
#The meta-regression  plot will contain bubbles and regression lines
#Bubbles - individual studies - the larger the bubble the larger the weight
#of the study (inversely proportional to the variance and standard error)
mregplot=regplot(meta_dl, 
                  lcol='red', 
                  col = 'blue',
                  level=0.95)




#Added the labels of the studies to a meta-regression plot
#Added the transf=exp to convert logRR to RR scale
mregplotRR=regplot(meta_dl, 
                  lcol='red', 
                  col = 'blue',
                  level=0.95, 
                  label = TRUE, 
                  transf = exp )




