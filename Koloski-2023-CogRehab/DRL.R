###Data Processing for DRL Summary Data
SumData<-read.csv("DRLSummaryData.csv", na.strings="")
group<-read.csv("Groups.csv", na.strings="")
group<-group[,-c(1,2)]

#Bringing in group and week info
#names(SumData)[names(SumData) == "ï..Subject"] <- "Subject"
Session<-c(101:125, 201:210)
Week<-c(4,4,4,4,4, 5,5,5,5,5, 6,6,6,6,6, 7,7,7,7,7, 8,8,8,8,8, 9,9,9,9,9, 10,10,10,10,10)
df<-cbind(Session, Week)
SumData<-merge(SumData, df, by="Session")
SumData<-merge(SumData, group, by="Subject")

#Setting up variables and transformations
#Transformations based on Box-Cox estimate
SumData$PctReinf<-SumData$Correct/(SumData$Correct+SumData$Incorrect)
SumData$t.PctReinf<-sqrt(SumData$PctReinf)
SumData$t.TotReinf<-sqrt(SumData$Reinf)
SumData$t.TotResp<-1/log(SumData$TotResp)
SumData$t.5CSRT.time<-log(SumData$NPTime+1)
SumData$t.TrayTime<-log(SumData$TrayTime)

#Outlier/non-lesion animals:
SumData<-subset(SumData, Subject!="ABP-A.1" & Subject!="ABP-A.6" & Subject!="ABP-A.10" & Subject!="ABP-A.11")
SumData<-subset(SumData, Subject!="DRL-A.36")

#LMERs for Rehab portion
library(lmerTest)
library(MuMIn)
library(emmeans)
SumData$Injury<-as.factor(SumData$Injury)
SumData$Condition<-as.factor(SumData$Condition)
SumData$Subject<-as.factor(SumData$Subject)
SumData$Injury<-relevel(SumData$Injury, ref="Sham")
SumData$Condition<-relevel(SumData$Condition, ref="None")
lmer.PctReinf<-lmer(scale(t.PctReinf)~Injury*Condition*scale(Week) + (1|Subject), data=subset(SumData, Phase=="Rehab"))
lmer.TotResp<-lmer(scale(t.TotResp)~Injury*Condition*scale(Week) + (1|Subject), data=subset(SumData, Phase=="Rehab"))

anova(lmer.PctReinf)
anova(lmer.TotResp)

#Write data to file.
write.table("Percent Reinforced over Weeks During Rehab", "Results.DRL.csv", sep=",", col.names=NA)
write.table(anova(lmer.PctReinf), "Results.DRL.csv", sep=",", col.names=NA, append=T)
write.table(summary(lmer.PctReinf)$coefficients, "Results.DRL.csv", sep=",", col.names=NA, append=T)
SumData$Condition<-relevel(SumData$Condition, ref="Cue")
lmer.PctReinf<-lmer(scale(t.PctReinf)~Injury*Condition*scale(Week) + (1|Subject), data=subset(SumData, Phase=="Rehab"))
write.table(summary(lmer.PctReinf)$coefficients, "Results.DRL.csv", sep=",", col.names=NA, append=T)
write.table(r.squaredGLMM(lmer.PctReinf), "Results.DRL.csv", append=T, sep=",", col.names=NA)
write.table(print(VarCorr(lmer.PctReinf), comp=c("Variance", "Std.Dev")), "Results.DRL.csv", append=T, sep=",", col.names=NA)

write.table("Total Responses over Weeks During Rehab", "Results.DRL.csv", sep=",", col.names=NA, append=T)
write.table(anova(lmer.TotResp), "Results.DRL.csv", sep=",", col.names=NA, append=T)
write.table(summary(lmer.TotResp)$coefficients, "Results.DRL.csv", sep=",", col.names=NA, append=T)
SumData$Condition<-relevel(SumData$Condition, ref="Cue")
lmer.TotResp<-lmer(scale(t.TotResp)~Injury*Condition*scale(Week) + (1|Subject), data=subset(SumData, Phase=="Rehab"))
write.table(summary(lmer.TotResp)$coefficients, "Results.DRL.csv", sep=",", col.names=NA, append=T)
write.table(r.squaredGLMM(lmer.TotResp), "Results.DRL.csv", append=T, sep=",", col.names=NA)
write.table(print(VarCorr(lmer.TotResp), comp=c("Variance", "Std.Dev")), "Results.DRL.csv", append=T, sep=",", col.names=NA)

#T-Tests for efficacy
write.table("T test for final session after cue degraded, CueTBIvsTBI, Pct Reinf", "Results.DRL.csv", sep=",", col.names=NA, append=T)
ttest.PctReinf<-t.test(t.PctReinf~Condition, data=subset(SumData, Session == 210 & Injury=="TBI"))
write.table(ttest.PctReinf$statistic, "Results.DRL.csv", sep=",", col.names=NA, append=T)
write.table(ttest.PctReinf$parameter, "Results.DRL.csv", sep=",", col.names=NA, append=T)
write.table(ttest.PctReinf$p.value, "Results.DRL.csv", sep=",", col.names=NA, append=T)

write.table("T test for final session after cue degraded, CueTBIvsTBI, Tot Resp", "Results.DRL.csv", sep=",", col.names=NA, append=T)
ttest.TotResp<-t.test(t.TotResp~Condition, data=subset(SumData, Session == 210 & Injury=="TBI"))
write.table(ttest.TotResp$statistic, "Results.DRL.csv", sep=",", col.names=NA, append=T)
write.table(ttest.TotResp$parameter, "Results.DRL.csv", sep=",", col.names=NA, append=T)
write.table(ttest.TotResp$p.value, "Results.DRL.csv", sep=",", col.names=NA, append=T)


#for graphing in SigmaPlot:
write.csv(SumData, "FinalSesSummary.csv")






##More detailed analyses on response data - function fitting and parameter analysis

##load in data 
data<-read.csv("DRLPressData.csv", na.strings="")
data<-subset(data, IRT>0)
group<-read.csv("Groups.csv", na.strings="")
group<-group[,-c(1,2)]
library(plyr)

#Missing sessions for:
#110 - 6, 22
#114 - 6, 22, 38

library(reshape2)
library(lattice)

dropvars<-names(data) %in% c("MSN","StartDate","StartTime","Box", "Comment", "Collect.Latency")
data<-data[!dropvars]
dropvars<-names(data) %in% c("Cue","Reinforced","Lever.Hold","Tray", "NosePoke", "Burst", "Group","Trial","Time","TrialTimer")
data2<-data[!dropvars]

#Cut time up into standardized bins
bins<-0:1199
Bin<-cut(data2$IRT, bins, include.lowest=T)
temp<-cbind(data2, Bin)
temp$IRT<-1  #Converts all IRTs to count variable associated with proper bin
temp$Bin<-as.numeric(temp$Bin)

#Bringing all together
FinalData<-aggregate(IRT~Subject+Session+Bin+Study, sum, data=temp)

#Generate list of subjects/sessions to extend out bins to all and fill in missing values.
df2<-aggregate(Trial~Subject+Session+Study, max, data=data)
df2<-do.call("rbind", replicate(length(bins), df2, simplify=F))
df2<-df2[with(df2, order(Subject, Session)),]
df2<-cbind(df2, bins)
df2<-df2[,-4]
colnames(df2)[colnames(df2) == "bins"]<-"Bin"
df2$Bin<-df2$Bin+1
FinalData<-merge(FinalData, df2, by=c("Subject", "Session", "Bin", "Study"), all=T)
FinalData$IRT[is.na(FinalData$IRT)]<-0
	#Replace any NAs that were generated by new data with 0s

temp<-aggregate(IRT~Session+Subject, sum, data=FinalData)
colnames(temp)[colnames(temp)=="IRT"] <- "TotResp"
FinalData<-merge(FinalData, temp, by=c("Session","Subject"), all=T)
FinalData$PropIRT<-FinalData$IRT/FinalData$TotResp
FinalData<-subset(FinalData, PropIRT<1)


#Bringing in group and week info
FinalData<-merge(FinalData, group, by="Subject")
Session<-c(101:125, 201:210)
Week<-c(4,4,4,4,4, 5,5,5,5,5, 6,6,6,6,6, 7,7,7,7,7, 8,8,8,8,8, 9,9,9,9,9, 10,10,10,10,10)
df<-cbind(Session, Week)
FinalData<-merge(FinalData, df, by="Session")
temp<-subset(FinalData, Bin<101)
#ABP-A missing #110, DRL-A missing #125
#Missing sessions for:
#110 - 6, 22
#114 - 6, 22, 38
temp1<-subset(temp, Study=="ABP-A" & Session!=110)
temp2<-subset(temp, Study=="DRL-A" & Session!=125 & Subject!="DRL-A.6" & Subject!="DRL-A.22" & Subject!="DRL-A.38")
temp3<-subset(temp, Study=="DRL-A" & Session!=125 & Session!=110 &  Session!=114 & Subject=="DRL-A.6")
temp4<-subset(temp, Study=="DRL-A" & Session!=125 & Session!=110 &  Session!=114 & Subject=="DRL-A.22")
temp5<-subset(temp, Study=="DRL-A" & Session!=125 &  Session!=114 & Subject=="DRL-A.38")
temp<-rbind(temp1, temp2, temp3, temp4, temp5)
#Outlier/non-lesion animals:
temp<-subset(temp, Subject!="ABP-A.1" & Subject!="ABP-A.6" & Subject!="ABP-A.10" & Subject!="ABP-A.11")
temp<-subset(temp, Subject!="DRL-A.36")


#Some plots
#xyplot(PropIRT~Bin|GroupName*as.factor(Week), data=subset(temp, Week<=8), xlim=c(1,50), type="a", lwd=4, ylim=c(0,0.35))
#xyplot(PropIRT~Bin|GroupName*as.factor(Week), data=subset(temp, Week>=9), xlim=c(1,50), type="a", lwd=4, ylim=c(0,0.15))

#Function for fitting:
combined<-function(X, X0, k, a, b, t0) {(X0*(X^k))+ (a*exp(-0.5*((X-t0)/b)^2))} 

#Group-level Fits
GroupSummary<-data.frame(Group=double(), Week=double(), X0=double(), k=double(), a=double(), b=double(), t0=double(), R2=double())

#copy each of these then run loop below
Grp="ShamCue"
startlist<-c(X0=0.05,k=-1.5,a=0.2,b=1,t0=20)

Grp="TBICue"
startlist<-c(X0=0.1,k=-1.5,a=0.15,b=3,t0=15)

Grp="Sham"
startlist<-c(X0=0.05,k=-1.5,a=0.1,b=1,t0=15)

Grp="TBI"
startlist<-c(X0=0.1,k=-1.5,a=0.1,b=1,t0=15)


N<-min(temp$Week)
for (wk in min(temp$Week):max(temp$Week)){
		#Calculate fit for each group
		try(fit<-(nls(PropIRT~combined(Bin,X0,k,a,b,t0), data=subset(temp, GroupName==Grp & Week==N), start=c(X0=0.05,k=-1.5,a=0.2,b=1,t0=20), nls.control(maxiter=5000, warnOnly=T))))
		
		#R2Calc for each fit
		try(SSfit<-subset(temp, GroupName==Grp & Week==N, select=PropIRT)-predict(fit))
		SSfit<-SSfit^2
		SSfit<-sum(SSfit)
		
		try(SStot<-subset(temp, GroupName==Grp & Week==N, select=PropIRT)-mean(na.omit(temp$PropIRT)))
		SStot<-SStot^2
		SStot<-sum(SStot)
		
		try(R2<-1-SSfit/SStot)
		
		#write coefficients & R2 value
		GroupSummary[nrow(GroupSummary)+1,]<-c(Grp, N, coef(fit), R2)	
N = N+1
}
xyplot(PropIRT+predict(fit)~Bin|as.factor(Week), data=subset(temp, GroupName==Grp), xlim=c(-1,50), type="a", lwd=4, ylim=c(0,0.35))


#Refit one week
Grp="TBI"
startlist<-c(X0=0.15,k=-.75,a=0.02,b=1.5,t0=10)
N=4
try(fit<-(nls(PropIRT~combined(Bin,X0,k,a,b,t0), data=subset(temp, GroupName==Grp & Week==N), start=startlist, nls.control(maxiter=5000, warnOnly=T))))
xyplot(PropIRT+predict(fit)~Bin, data=subset(temp, GroupName==Grp & Week==N), xlim=c(-1,50), type="a", lwd=4, ylim=c(0,0.35))
GroupSummary[nrow(GroupSummary)+1,]<-c(Grp, N, coef(fit), R2)	
GroupSummary<-GroupSummary[-22,]


write.csv(GroupSummary, "GroupFits.csv")
GroupSummary<-read.csv("GroupFits.csv")
GroupSummary<-GroupSummary[,-1]

Decay<-function(X, X0, k) {(X0*(X^k))}
GroupSummary$Two<-Decay(2,GroupSummary$X0,GroupSummary$k)
GroupSummary$Three<-Decay(3,GroupSummary$X0,GroupSummary$k)
GroupSummary$Four<-Decay(4,GroupSummary$X0,GroupSummary$k)
GroupSummary$Five<-Decay(5,GroupSummary$X0,GroupSummary$k)

GroupSummary$AUC<-
	0.5*(GroupSummary$X0-GroupSummary$Two)+GroupSummary$Two + 
	0.5*(GroupSummary$Two-GroupSummary$Three)+GroupSummary$Three + 
	0.5*(GroupSummary$Three-GroupSummary$Four)+GroupSummary$Four + 
	0.5*(GroupSummary$Four-GroupSummary$Five)+GroupSummary$Five

GroupSummary<-GroupSummary[,-c(9:12)]	

write.csv(GroupSummary, "GroupFits.csv")





#Individual Subject Fits
Summary<-data.frame(Subject=double(), Week=double(), X0=double(), k=double(), a=double(), b=double(), t0=double(), R2=double())

N<-min(temp$Week)
for (N in min(temp$Week):max(temp$Week)){

	for (M in levels(as.factor(temp$Subject))){
		#Calculate fit for each subject
		try(fit<-(nls(PropIRT~combined(Bin,X0,k,a,b,t0), data=subset(temp, Subject==M & Week==N), 
		              start=c(X0=0.15,k=-1.5,a=0.1,b=5,t0=15), nls.control(maxiter=5000, warnOnly=T))))
		#R2Calc for each fit
		try(SSfit<-subset(temp, Subject==M & Week==N, select=PropIRT)-predict(fit))
		SSfit<-SSfit^2
		SSfit<-sum(SSfit)
		try(SStot<-subset(temp, Subject==M & Week==N, select=PropIRT)-mean(na.omit(temp$PropIRT)))
		SStot<-SStot^2
		SStot<-sum(SStot)
		try(R2<-1-SSfit/SStot)
		#write coefficients & R2 value
		Summary[nrow(Summary)+1,]<-c(M, N, coef(fit), R2)				   
	}
N = N+1
}
names<-c(2:8)
Summary[,names]<-lapply(Summary[,names], as.numeric)



#To rerun non-fits
#slower model fit, but can constrain parameters to not get nonsense
temp1<-subset(Summary, X0<=0 | a<0 | k>0 | b<0 |t0<0 | R2<0.5 | k< -10, select=c(1:2))
  #filter to get implausible values and really poor fits
temp1$delete<-1
Summary<-merge(Summary,temp1, by=c("Subject", "Week"), all=T)
Summary<-subset(Summary, is.na(Summary$delete), select=-delete) 

startlist<-c(X0=0.15,k=-.55,a=0.1,b=10,t0=22)

for (i in 1:length(temp1$Subject)){
  M<-temp1$Subject[i]
  N<-temp1$Week[i]
  #Calculate fit for each subject
  try(fit<-(nls(PropIRT~combined(Bin,X0,k,a,b,t0), data=subset(temp, Subject==M & Week==N), 
                start=startlist, nls.control(maxiter=5000, warnOnly=T), algorithm="port", 
                lower=c(X0=0.001,k=-10,a=0,b=0,t0=0), upper=c(X0=1,k=0,a=1,b=200,t0=40))))
  #R2Calc for each fit
  try(SSfit<-subset(temp, Subject==M & Week==N, select=PropIRT)-predict(fit))
  SSfit<-SSfit^2
  SSfit<-sum(SSfit)
  try(SStot<-subset(temp, Subject==M & Week==N, select=PropIRT)-mean(na.omit(temp$PropIRT)))
  SStot<-SStot^2
  SStot<-sum(SStot)
  try(R2<-1-SSfit/SStot)
  #write coefficients & R2 value
  Summary[nrow(Summary)+1,]<-c(M, N, coef(fit), R2)				   
  
  i = i+1
}
names<-c(2:8)
Summary[,names]<-lapply(Summary[,names], as.numeric)


#Check again for bad fits:
temp1<-subset(Summary, X0<=0 | a<0 | k>0 | b<0 |t0<0 | R2<0.5 | k< -10)
M="DRL-A.4"
N=4
startlist<-c(X0=0.2,k=-.5,a=0.05,b=4,t0=5)
try(fit<-(nls(PropIRT~combined(Bin,X0,k,a,b,t0), data=subset(temp, Subject==M & Week==N), start=startlist, nls.control(maxiter=5000, warnOnly=T), algorithm="port", lower=c(X0=0,k=-10,a=0,b=0,t0=0), upper=c(X0=1,k=10,a=1,b=200,t0=40))))
xyplot(PropIRT+predict(fit)~Bin, data=subset(temp, Subject==M & Week==N), xlim=c(-1,50), type="a", lwd=4, ylim=c(0,0.35))
fit
Summary<-subset(Summary, !(Subject==M & Week==N))
Summary[nrow(Summary)+1,]<-c(M, N, coef(fit), R2)		



names<-c(2:8)
Summary[,names]<-lapply(Summary[,names], as.numeric)
Summary$Subject<-as.factor(Summary$Subject)

#To calculate AUC
Summary$Two<-Decay(2,Summary$X0,Summary$k)
Summary$Three<-Decay(3,Summary$X0,Summary$k)
Summary$Four<-Decay(4,Summary$X0,Summary$k)
Summary$Five<-Decay(5,Summary$X0,Summary$k)

Summary$AUC<-
	0.5*(Summary$X0-Summary$Two)+Summary$Two + 
	0.5*(Summary$Two-Summary$Three)+Summary$Three + 
	0.5*(Summary$Three-Summary$Four)+Summary$Four + 
	0.5*(Summary$Four-Summary$Five)+Summary$Five
	
Summary<-Summary[,-c(9:12)]	

Summary<-merge(Summary, group, by="Subject")
write.csv(Summary, "Fits.csv")







#Data Analyses
library(lmerTest)
analysis<-read.csv("Fits.csv", na.strings="")

analysis$t.AUC<-log(analysis$AUC)
analysis$t.t0<-(analysis$t0)^2
analysis$Subject<-as.factor(analysis$Subject)
analysis$Condition<-as.factor(analysis$Condition)
analysis$Injury<-as.factor(analysis$Injury)
analysis$Condition<-relevel(analysis$Condition, ref="None")
analysis$Injury<-relevel(analysis$Injury, ref="Sham")


AUC<-lmer(scale(t.AUC)~Injury*Condition*scale(Week) + (1|Subject), data=subset(analysis, Week<9))
Time<-lmer(scale(t.t0)~Injury*Condition*scale(Week) + (1|Subject), data=subset(analysis, Week<9))

anova(AUC)
anova(Time)

write.table("Area under the curve during Rehab", "Results.DRL.csv", sep=",", col.names=NA, append=T)
write.table(anova(AUC), "Results.DRL.csv", sep=",", col.names=NA, append=T)
write.table(summary(AUC)$coefficients, "Results.DRL.csv", sep=",", col.names=NA, append=T)
analysis$Condition<-relevel(analysis$Condition, ref="Cue")
AUC<-lmer(scale(t.AUC)~Injury*Condition*scale(Week) + (1|Subject), data=subset(analysis, Week<9))
write.table(summary(AUC)$coefficients, "Results.DRL.csv", sep=",", col.names=NA, append=T)
write.table(r.squaredGLMM(AUC), "Results.DRL.csv", append=T, sep=",", col.names=NA)
write.table(print(VarCorr(AUC), comp=c("Variance", "Std.Dev")), "Results.DRL.csv", append=T, sep=",", col.names=NA)

write.table("Estimated Time - T0 parameter during Rehab", "Results.DRL.csv", sep=",", col.names=NA, append=T)
write.table(anova(Time), "Results.DRL.csv", sep=",", col.names=NA, append=T)
write.table(summary(Time)$coefficients, "Results.DRL.csv", sep=",", col.names=NA, append=T)
analysis$Condition<-relevel(analysis$Condition, ref="Cue")
Time<-lmer(scale(t.t0)~Injury*Condition*scale(Week) + (1|Subject), data=subset(analysis, Week<9))
write.table(summary(Time)$coefficients, "Results.DRL.csv", sep=",", col.names=NA, append=T)
write.table(r.squaredGLMM(Time), "Results.DRL.csv", append=T, sep=",", col.names=NA)
write.table(print(VarCorr(Time), comp=c("Variance", "Std.Dev")), "Results.DRL.csv", append=T, sep=",", col.names=NA)



###Setup for t-tests on last session - calculate fit for single session
combined<-function(X, X0, k, a, b, t0) {(X0*(X^k))+ (a*exp(-0.5*((X-t0)/b)^2))} 
temp$Subject<-as.factor(temp$Subject)
temp$Study<-as.factor(temp$Study)
temp$Subject<-droplevels(temp)$Subject

Summary<-data.frame(Subject=double(), Session=double(), X0=double(), k=double(), a=double(), b=double(), t0=double(), R2=double())
startlist<-c(X0=0.15,k=-.75,a=0.02,b=1.5,t0=10)
N=210
for (sbj in levels(temp$Subject))
	{	#Calculate fit for each subject
		try(fit<-(nls(PropIRT~combined(Bin,X0,k,a,b,t0), data=subset(temp, Subject==sbj & Session>=N), 
		              start=c(X0=0.15,k=-1.5,a=0.1,b=5,t0=15), nls.control(maxiter=5000, warnOnly=T))))			
		#R2Calc for each fit
		try(SSfit<-subset(temp, Subject==sbj & Session>=N, select=PropIRT)-predict(fit))
		SSfit<-SSfit^2
		SSfit<-sum(SSfit)		
		try(SStot<-subset(temp, Subject==sbj & Session>=N, select=PropIRT)-mean(na.omit(temp$PropIRT)))
		SStot<-SStot^2
		SStot<-sum(SStot)	
		try(R2<-1-SSfit/SStot)	
		#write coefficients & R2 value
		Summary[nrow(Summary)+1,]<-c(sbj, N, coef(fit), R2)				   
	}
names<-c(2:8)
Summary[,names]<-lapply(Summary[,names], as.numeric)


#To rerun non-fits
#slower model fit, but can constrain parameters to not get nonsense
temp1<-subset(Summary, X0<=0 | a<0 | k>0 | b<0 |t0<0 | R2<0.5 | k< -10, select=c(1:2))
temp1$delete<-1
Summary<-merge(Summary,temp1, by=c("Subject", "Session"), all=T)
Summary<-subset(Summary, is.na(Summary$delete), select=-delete) 
startlist<-c(X0=0.15,k=-.55,a=0.1,b=10,t0=10)


for (i in 1:length(temp1$Subject)){
  M<-temp1$Subject[i]
  N<-temp1$Session[i]
  #Calculate fit for each subject
  try(fit<-(nls(PropIRT~combined(Bin,X0,k,a,b,t0), data=subset(temp, Subject==M & Session>=N), 
                start=startlist, nls.control(maxiter=5000, warnOnly=T), algorithm="port", 
                lower=c(X0=0.001,k=-10,a=0,b=0,t0=0), upper=c(X0=1,k=0,a=1,b=200,t0=40))))
  #R2Calc for each fit
  try(SSfit<-subset(temp, Subject==M & Session>=N, select=PropIRT)-predict(fit))
  SSfit<-SSfit^2
  SSfit<-sum(SSfit)
  try(SStot<-subset(temp, Subject==M & Session>=N, select=PropIRT)-mean(na.omit(temp$PropIRT)))
  SStot<-SStot^2
  SStot<-sum(SStot)
  try(R2<-1-SSfit/SStot)
  #write coefficients & R2 value
  Summary[nrow(Summary)+1,]<-c(M, N, coef(fit), R2)				   
  i = i+1
}
names<-c(2:8)
Summary[,names]<-lapply(Summary[,names], as.numeric)

#Check again for bad fits:
temp1<-subset(Summary, X0<=0 | a<0 | k>0 | b<0 |t0<0 | R2<0.5 | k< -10)


#Calculate AUC
Decay<-function(X, X0, k) {(X0*(X^k))}
Summary$Two<-Decay(2,Summary$X0,Summary$k)
Summary$Three<-Decay(3,Summary$X0,Summary$k)
Summary$Four<-Decay(4,Summary$X0,Summary$k)
Summary$Five<-Decay(5,Summary$X0,Summary$k)
Summary$AUC<-
	0.5*(Summary$X0-Summary$Two)+Summary$Two + 
	0.5*(Summary$Two-Summary$Three)+Summary$Three + 
	0.5*(Summary$Three-Summary$Four)+Summary$Four + 
	0.5*(Summary$Four-Summary$Five)+Summary$Five
Summary<-Summary[,-c(9:12)]	


Summary<-merge(Summary, group, by="Subject")
names<-c(1,10:12)
Summary[,names]<-lapply(Summary[,names], as.factor)

write.table(Summary, "fits.csv", sep=",",col.names=NA, append=T)



#T-Tests
Summary$t.AUC<-log(Summary$AUC)
Summary$t0<-as.numeric(Summary$t0)
Summary$t.t0<-((Summary$t0)^2)
Summary$Injury<-as.factor(Summary$Injury)
Summary$Condition<-as.factor(Summary$Condition)

write.table("T test for final session after cue degraded, CueTBIvsTBI, AUC", "Results.DRL.csv", sep=",", col.names=NA, append=T)
ttest.AUC<-t.test(t.AUC~Condition, data=subset(Summary, Injury=="TBI"))
write.table(ttest.AUC$statistic, "Results.DRL.csv", sep=",", col.names=NA, append=T)
write.table(ttest.AUC$parameter, "Results.DRL.csv", sep=",", col.names=NA, append=T)
write.table(ttest.AUC$p.value, "Results.DRL.csv", sep=",", col.names=NA, append=T)

write.table("T test for final session after cue degraded, CueTBIvsTBI, T0 parameter", "Results.DRL.csv", sep=",", col.names=NA, append=T)
ttest.Time<-t.test(t0~Condition, data=subset(Summary, Injury=="TBI"))
write.table(ttest.Time$statistic, "Results.DRL.csv", sep=",", col.names=NA, append=T)
write.table(ttest.Time$parameter, "Results.DRL.csv", sep=",", col.names=NA, append=T)
write.table(ttest.Time$p.value, "Results.DRL.csv", sep=",", col.names=NA, append=T)





###Plots for use in pub/presentation
names<-c(8:10)
temp[,names]<-lapply(temp[,names], as.factor)

#Settings for consistency
stripargs<-list(fontface="bold", cex=2.2)
setargs<-list(strip.background=list(col="lightgrey"), strip.border=list(lwd=3), axis.line=list(lwd=3), axis.components=list(bottom=list(tck=3)))
scale.x<-list(tck=c(1,0), cex=2.2, labels=c("","","10","","20","","30","","40","",""), at=c(0,5,10,15,20,25,30,35,40,45,50))
scale.y.sm<-list(cex=2.2, labels=c("0.02","0.04","0.06","0.08","0.10"), at=c(.02,.04,.06,.08,.10))

#Single group at timepoint - can rerun for different weeks
#First week shams
xyplot(PropIRT~Bin, 
		data=subset(temp, GroupName=="Sham" & Week==4), 
		xlim=c(1,50),  ylim=c(0,0.12),
		xlab = list(label="IRT (s)", cex=3, fontface="bold"),
		ylab = list(label="Proportion IRTs", cex=3, fontface="bold"), 
		par.strip = stripargs,
		par.settings=setargs,
		scales=list(tck=c(2.5,0), fontface="bold", lwd=3, x=scale.x, y=scale.y.sm),
		panel = function(x,y,...){
		  panel.refline(v = c(20), lwd=6, lty=3, col="darkgrey")
		  panel.xyplot(x,y, type="a", lwd=8, col="black", ...)
		}
		)

#Final week for shams
png(filename="Raw.png", width=700, height=500)
xyplot(PropIRT~Bin, 
		data=subset(temp, GroupName=="Sham" & Week==8), 
		xlim=c(1,50),  ylim=c(0,0.08),
    xlab = list(label="IRT (s)", cex=3, fontface="bold"),
    ylab = list(label="Proportion IRTs", cex=3, fontface="bold"), 
    par.strip = stripargs,
		par.settings=setargs,
    scales=list(tck=c(2.5,0), fontface="bold", lwd=3, x=scale.x, y=scale.y.sm),
		panel = function(x,y,...){
            panel.refline(v = c(20), lwd=6, lty=3, col="darkgrey")
            panel.xyplot(x,y, type="a", lwd=8, col="black", ...)
			}
		)
dev.off()

#Functions used to calculate. Plot to show what fits look like
Decay<-function(X, X0, k) {(X0*(X^k))}
Gaus<-function(X, a, b, t0) {a*exp(-0.5*((X-t0)/b)^2)}
combined<-function(X, X0, k, a, b, t0) {(X0*(X^k))+ (a*exp(-0.5*((X-t0)/b)^2))} 

xyplot(Decay(Bin, 0.06, -1.25)~Bin, 
		data=subset(temp, GroupName=="Sham" & Week==5), 
		xlim=c(1,50),  ylim=c(0,0.08),
    xlab = list(label="IRT (s)", cex=3.5, fontface="bold"),
    ylab = list(label="Proportion IRTs", cex=3.5, fontface="bold"), 
		par.strip = stripargs,
		par.settings=setargs,
		scales=list(tck=c(2.5,0), fontface="bold", lwd=3, x=scale.x, y=scale.y.sm),
		panel = function(x,y,...){
		  panel.xyplot(x,y, type="a", lwd=8, col="ForestGreen", ...)
		}
		)

xyplot(Decay(Bin, 0.06, -1.25)+Gaus(Bin, 0.055, 5, 19.25)~Bin, 
		data=subset(temp, GroupName=="Sham" & Week==5), 
		xlim=c(1,50),  ylim=c(0,0.08),
		xlab = list(label="IRT (s)", cex=3.5, fontface="bold"),
		ylab = list(label="Proportion IRTs", cex=3.5, fontface="bold"), 
		par.strip = stripargs,
		par.settings=setargs,
		scales=list(tck=c(2.5,0), fontface="bold", lwd=3, x=scale.x, y=scale.y.sm),
		panel = function(x,y,...){
		  panel.xyplot(x,y, type="a", lwd=8, col=c("ForestGreen", "blue"), ...)
		}
		)

library(latticeExtra)

png(filename="fits.png", width=700, height=500)
xyplot(Decay(Bin, 0.06, -1.25)~Bin, 
		data=subset(temp, GroupName=="Sham" & Week==5), 
		xlim=c(1,50), ylim=c(0,0.08),
		xlab = list(label="IRT (s)", cex=3, fontface="bold"),
		ylab = list(label="Proportion IRTs", cex=3, fontface="bold"), 
		par.strip = stripargs,
		par.settings=setargs,
		lines=c(x=c(20,20), y=c(0, 0.05)),
		scales=list(tck=c(2.5,0), fontface="bold", lwd=3, x=scale.x, y=scale.y.sm),
		panel=function(x,y,...){
		  panel.xyplot(x,y, type="a", lwd=8, col=c("ForestGreen", "blue"), ...)
		  panel.xyarea(x,y,border="ForestGreen", col="ForestGreen", lwd=4)
			}
		)+
 as.layer(xyplot(Gaus(Bin, 0.055, 5, 19.25)~Bin, 
		data=subset(temp, GroupName=="Sham" & Week==5), 
        type="a", lwd=8, col="blue",
		x.same=TRUE, y.same=TRUE,))
dev.off()
	

#Actual plots to export and assemble in photoshop for multipanel figure		

NoCue<-subset(temp, Bin<60 & (GroupName=="Sham"|GroupName=="TBI"))
NoCue$GroupName<-droplevels(NoCue)$GroupName		
Cue<-subset(temp, Bin<60 & (GroupName=="ShamCue"|GroupName=="TBICue"))
Cue$GroupName<-droplevels(Cue)$GroupName
levels(Cue$GroupName)<-c("Sham-Cue", "TBI-Cue")
library(latticeExtra)
library(plotrix)

#Custom function for error bands
  my.panel.bands <- function(x, y, upper, lower, fill, col,
                             subscripts, ..., font, fontface){
    upper <- upper[subscripts]
    lower <- lower[subscripts]
    panel.polygon(c(x, rev(x)), c(upper, rev(lower)),
                  col = fill, border = FALSE,
                  ...)
  }

#Graphing parameters for consistency
  stripargs<-list(fontface="bold", cex=3.5)
  setargs<-list(strip.background=list(col="lightgrey"), strip.border=list(lwd=3), axis.line=list(lwd=3), axis.components=list(bottom=list(tck=3)))
  scale.x<-list(tck=c(1,0), cex=3.5, labels=c("","","10","","20","","30","","40","",""), at=c(0,5,10,15,20,25,30,35,40,45,50))
  scale.y.sm<-list(cex=3.5, labels=c("0.02","0.04","0.06","0.08","0.10"), at=c(.02,.04,.06,.08,.10))
  scale.y.lg<-list(cex=3.5, labels=c("","0.10","","0.20","","0.30"), at=c(.05,.1,.15,.2,.25,.3))
  nocue.cols<-c("black", "red")
  cue.cols<-c("#606060", "#ff7000")
  
#No cue groups first 
plotting<-aggregate(PropIRT~Bin+GroupName+Week+Subject, data=NoCue, FUN=mean)
plotting2<-aggregate(PropIRT~Bin+GroupName+Week, data=plotting, FUN=std.error)
colnames(plotting2)[4]<-"SEM"
plotting<-aggregate(PropIRT~Bin+GroupName+Week, data=NoCue, FUN=mean)
plotting<-merge(plotting, plotting2, by=c("Bin", "GroupName", "Week"))
plotting$upper<-plotting$PropIRT+plotting$SEM
plotting$lower<-plotting$PropIRT-plotting$SEM
plotting<-plotting[order(plotting$Week,plotting$Bin),]  
  
png(filename="NoCue.1.png", width=1050, height=1500)
useOuterStrips(
  xyplot(PropIRT~Bin|GroupName*as.factor(Week), 
         data=subset(plotting, Week<=8), groups=GroupName, 
         upper= plotting$upper, lower = plotting$lower,
         xlab=NULL, ylab=NULL,
         par.strip = stripargs, par.settings = setargs,
         xlim=c(1,50), ylim=c(0,0.12),
         index.cond = list(1:2,5:1),
         scales=list(tck=c(2.5,0), fontface="bold", lwd=3, alternating=1,
                     x=scale.x, y=scale.y.sm),
         panel = function(x,y,...){
           panel.refline(v = c(20), lwd=6, lty=3, col="darkgrey")
           panel.xyplot(x,y, type="a", lwd=8, col=nocue.cols,...)
           panel.superpose(x, y, panel.groups = my.panel.bands, type='l', alpha=c(0.4,0.5), fill=nocue.cols ,...)
         }
  )	)
dev.off()

plotting<-subset(plotting, Week>=9)
plotting<-plotting[order(plotting$Week,plotting$Bin),]
png(filename="NoCue.2.png", width=1050, height=650)
useOuterStrips(
  xyplot(PropIRT~Bin|GroupName*as.factor(Week), 
         data=subset(plotting, Week>=9), groups=GroupName, 
         upper= plotting$upper, lower = plotting$lower,
         xlab=NULL, ylab=NULL,
         par.strip = stripargs, par.settings = setargs,
         xlim=c(1,50), ylim=c(0,0.12),
         index.cond = list(1:2, 2:1),
         scales=list(tck=c(2.5,0), fontface="bold", lwd=3, alternating=1,
                     x=scale.x, y=scale.y.sm),
         panel = function(x,y,...){
           panel.refline(v = c(20), lwd=6, lty=3, col="darkgrey")
           panel.xyplot(x,y, type="a", lwd=8, col=nocue.cols,...)
           panel.superpose(x, y, panel.groups = my.panel.bands, type='l', alpha=c(0.4,0.5), fill=nocue.cols ,...)
         }
  ), strip=F	)
dev.off()

#Cue groups next
plotting<-aggregate(PropIRT~Bin+GroupName+Week+Subject, data=Cue, FUN=mean)
plotting2<-aggregate(PropIRT~Bin+GroupName+Week, data=plotting, FUN=std.error)
colnames(plotting2)[4]<-"SEM"
plotting<-aggregate(PropIRT~Bin+GroupName+Week, data=Cue, FUN=mean)
plotting<-merge(plotting, plotting2, by=c("Bin", "GroupName", "Week"))
plotting$upper<-plotting$PropIRT+plotting$SEM
plotting$lower<-plotting$PropIRT-plotting$SEM
plotting<-plotting[order(plotting$Week,plotting$Bin),]

png(filename="Cue.1.png", width=1050, height=1500)
useOuterStrips(
  xyplot(PropIRT~Bin|GroupName*as.factor(Week), 
         data=subset(plotting, Week<=8), groups=GroupName, 
         upper= plotting$upper, lower = plotting$lower,
         xlab=NULL, ylab=NULL,
         par.strip = stripargs, par.settings = setargs,
         xlim=c(1,50), ylim=c(0,0.33),
         index.cond = list(1:2, 5:1),
         scales=list(tck=c(2.5,0), fontface="bold", lwd=3, alternating=1,
                     x=scale.x, y=scale.y.lg),
         panel = function(x,y,...){
           panel.refline(v = c(20), lwd=6, lty=3, col="darkgrey")
           panel.xyplot(x,y, type="a", lwd=8, col=cue.cols,...)
           panel.superpose(x, y, panel.groups = my.panel.bands, type='l', alpha=c(0.55, 0.65), fill=cue.cols ,...)
         }
  )	)
dev.off()


plotting<-subset(plotting, Week>=9)
plotting<-plotting[order(plotting$Week,plotting$Bin),]
png(filename="Cue.2.png", width=1050, height=650)
useOuterStrips(
  xyplot(PropIRT~Bin|GroupName*as.factor(Week), 
         data=subset(plotting, Week>=9), groups=GroupName, 
         upper= plotting$upper, lower = plotting$lower,
         xlab=NULL, ylab=NULL,
         par.strip = stripargs, par.settings = setargs,
         xlim=c(1,50), ylim=c(0,0.12),
         index.cond = list(1:2, 2:1),
         scales=list(tck=c(2.5,0), fontface="bold", lwd=3, alternating=1,
                     x=scale.x, y=scale.y.sm),
         panel = function(x,y,...){
           panel.refline(v = c(20), lwd=6, lty=3, col="darkgrey")
           panel.xyplot(x,y, type="a", lwd=8, col=cue.cols,...)
           panel.superpose(x, y, panel.groups = my.panel.bands, type='l', alpha=c(0.55, 0.65), fill=cue.cols ,...)
         }
  ), strip=F	)
dev.off()

	


#Final session plots
plotting<-aggregate(PropIRT~Bin+GroupName+Subject+Session, data=subset(NoCue, Session==210), FUN=mean)
plotting2<-aggregate(PropIRT~Bin+GroupName+Session, data=plotting, FUN=std.error)
colnames(plotting2)[4]<-"SEM"
plotting<-aggregate(PropIRT~Bin+GroupName+Session, data=subset(NoCue, Session==210), FUN=mean)
plotting<-merge(plotting, plotting2, by=c("Bin", "GroupName", "Session"))
plotting$upper<-plotting$PropIRT+plotting$SEM
plotting$lower<-plotting$PropIRT-plotting$SEM
plotting<-plotting[order(plotting$Session, plotting$Bin),]
plotting$Session<-"Last Session"

png(filename="NoCue.3.png", width=1050, height=380)
useOuterStrips(
  xyplot(PropIRT~Bin|GroupName*as.factor(Session), 
         data=subset(plotting,), groups=GroupName, 
         upper= plotting$upper, lower = plotting$lower,
         xlab=NULL, ylab=NULL,
         par.strip = stripargs, par.settings = setargs,
         xlim=c(1,50), ylim=c(0,0.12),
         index.cond = list(1:2, 1:1),
         strip.left=T,
         scales=list(tck=c(2.5,0), fontface="bold", lwd=3, alternating=1,
                     x=scale.x, y=scale.y.sm),
         panel = function(x,y,...){
           panel.refline(v = c(20), lwd=6, lty=3, col="darkgrey")
           panel.xyplot(x,y, type="a", lwd=8, col=nocue.cols,...)
           panel.superpose(x, y, panel.groups = my.panel.bands, type='l', alpha=c(0.4,0.5), fill=nocue.cols ,...)
         }
  ), strip=F	)
dev.off()


plotting<-aggregate(PropIRT~Bin+GroupName+Subject+Session, data=subset(Cue, Session==210), FUN=mean)
plotting2<-aggregate(PropIRT~Bin+GroupName+Session, data=plotting, FUN=std.error)
colnames(plotting2)[4]<-"SEM"
plotting<-aggregate(PropIRT~Bin+GroupName+Session, data=subset(Cue, Session==210), FUN=mean)
plotting<-merge(plotting, plotting2, by=c("Bin", "GroupName", "Session"))
plotting$upper<-plotting$PropIRT+plotting$SEM
plotting$lower<-plotting$PropIRT-plotting$SEM
plotting<-plotting[order(plotting$Session, plotting$Bin),]
plotting$Session<-"Last Session"

png(filename="Cue.3.png", width=1050, height=380)
useOuterStrips(
  xyplot(PropIRT~Bin|GroupName*as.factor(Session), 
         data=subset(plotting,), groups=GroupName, 
         upper= plotting$upper, lower = plotting$lower,
         xlab=NULL, ylab=NULL,
         par.strip = stripargs, par.settings = setargs,
         xlim=c(1,50), ylim=c(0,0.12),
         index.cond = list(1:2, 1:1),
         strip.left=T,
         scales=list(tck=c(2.5,0), fontface="bold", lwd=3, alternating=1,
                     x=scale.x, y=scale.y.sm),
         panel = function(x,y,...){
           panel.refline(v = c(20), lwd=6, lty=3, col="darkgrey")
           panel.xyplot(x,y, type="a", lwd=8, col=cue.cols,...)
           panel.superpose(x, y, panel.groups = my.panel.bands, type='l', alpha=c(0.55, 0.65), fill=cue.cols ,...)
         }
  ), strip=F	)
dev.off()

