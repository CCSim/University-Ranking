install.packages("pmr")
install.packages("mlogit")
install.packages("dplyr")
install.packages("VGAM")
install.packages("oii")
install.packages("ggplot2")
library(pmr)
library(mlogit)
library(dplyr)
library(oii)
library(VGAM)
library(ggplot2)

#qs ranking notes:
#AR: Academic Reputation ER: Employer Repitation
#FS: Faculty Student CPF: Citations per Faculty
#IF: International Faculty IS: International Students
qs2013<-read.csv("C:/Users/ccsim09/Desktop/2012-2013.csv",header=T)
summary(qs2013)
qs2014<-read.csv("C:/Users/ccsim09/Desktop/2013-2014.csv",header=T)
summary(qs2014)
qs2015<-read.csv("C:/Users/ccsim09/Desktop/2014-2015.csv",header=T)
summary(qs2015)
qs2016<-read.csv("C:/Users/ccsim09/Desktop/2015-2016.csv",header=T)
summary(qs2016)
qs2017<-read.csv("C:/Users/ccsim09/Desktop/2016-2017.csv",header=T)
summary(qs2017)
qs2018<-read.csv("C:/Users/ccsim09/Desktop/2017-2018.csv",header=T)
summary(qs2018)
qs2018$rank.2018
numins=0

#QS file of combination
qs<-read.csv("C:/Users/ccsim09/Desktop/TotalQS.csv",header=T)
summary(qs$INSTITUTION)
qs$INSTITUTION[2]

#QS file with raw score only
qssc50<-read.csv("C:/Users/ccsim09/Desktop/QSSC_top50.csv",header=T,row.names=NULL)
summary(qssc50$INSTITUTION)

data("Game",package="mlogit")
head(Game,4)
data("Game2",package="mlogit")
head(Game2,21)
G <- mlogit.data(Game2, shape="long", choice="ch", alt.var='platform', ranked=TRUE)
G<-mlogit.data(Game,shape="wide",choice="ch",varying=1:12,ranked=TRUE)
head(G,105)

X1 <- c(1,1,2,2,3,3)
X2 <- c(2,3,1,3,1,2)
X3 <- c(3,2,3,1,2,1)
X4 <- c(6,5,4,3,2,1)
test <- data.frame(X1,X2,X3)
rol(test,X4)

#universities who always appear in top 100
uni100<-read.csv("C:/Users/ccsim09/Desktop/Book1.csv",header=TRUE)
uni100$INSTITUTION

#universities who always appear in top 50
uni50<-read.csv("C:/Users/ccsim09/Desktop/39 out of 50.csv",header=TRUE)
uni50$INSTITUTION


qs$INSTITUTION %in% uni100$INSTITUTION

newqs <- qs[qs$INSTITUTION %in% uni100$INSTITUTION,]
length(newqs$INSTITUTION)
newqs[1,]
head(newqs,1)

#mlogit for qssc50
newqssc50<-qssc50[qssc50$INSTITUTION %in% uni50$INSTITUTION,]
summary(newqssc50$INSTITUTION)

for (year in 1:8)
{
  for (i in 1:39)
  {
    newqssc50$RANK[(year-1)*39+i]=i
  }
}

write.csv(newqssc50, "C:/Users/ccsim09/Desktop/newqssc49.csv")
newqssc50$RANK

qssc49<-read.csv("C:/Users/ccsim09/Desktop/newqssc49.csv",header=T)

asd<vglm(mild~let,family=acat(reverse=TRUE,parallel=TRUE),data=xx)
qs50rol<-mlogit.data(qssc49,shape="long",choice="RANK",alt.var = 'INSTITUTION',ranked = TRUE)
summary(mlogit(RANK~AR.SCORE+ER.SCORE+FS.SCORE+CPF.SCORE+IF.SCORE+IS.SCORE
               +LAGRANK+TIMES.ranking+student_staff_ratio+international_students
               +female_male_ratio,data=qs50rol,reflevel = "THE UNIVERSITY OF SYDNEY"))

testx<-lm(AR.SCORE~(LAGRANK+ER.SCORE+FS.SCORE+CPF.SCORE+IF.SCORE+IS.SCORE+female_male_ratio)^2,data=qssc50)
summary(testx)

#to see the correlation
corre=cbind(qssc50$RANK,qssc50$LAGRANK,qssc50$AR.SCORE,qssc50$ER.SCORE,qssc50$FS.SCORE,qssc50$CPF.SCORE,qssc50$IF.SCORE,qssc50$IS.SCORE,qssc50$Overall.Score,qssc50$TIMES.ranking,qssc50$student_staff_ratio,qssc50$international_students,qssc50$female_male_ratio,qssc50$Unemployment)
round(cor(corre),2)
cor(qs50rol$LAGRANK,qs50rol$TIMES.ranking)
plot(qs50rol$LAGRANK,qs50rol$TIMES.ranking)

#Some more attempts
summary(mlogit(RANK~(LAGRANK+TIMES.ranking+student_staff_ratio+international_students
               +female_male_ratio)^2,data=qs50rol,reflevel = "THE UNIVERSITY OF SYDNEY"))
summary(mlogit(RANK~TIMES.ranking,data=qs50rol,reflevel = "COLUMBIA UNIVERSITY"))
summary(mlogit(RANK~(TIMES.ranking+LAGRANK)^2,data=qs50rol,reflevel = "COLUMBIA UNIVERSITY"))
modeltimes=mlogit(RANK~(TIMES.ranking+LAGRANK)^2,data=qs50rol,reflevel = "COLUMBIA UNIVERSITY")
modeltimes$probabilities
summary(mlogit(RANK~(LAGRANK+TIMES.ranking+Unemployment)^2,data=qs50rol,reflevel = "COLUMBIA UNIVERSITY"))
summary(mlogit(RANK~TIMES.ranking|gdpgrowth,data=qs50rol,reflevel = "COLUMBIA UNIVERSITY"))
summary(mlogit(RANK~FS.SCORE+IF.SCORE+IS.SCORE+TIMES.ranking,data=qs50rol,reflevel = "THE UNIVERSITY OF SYDNEY"))

#TIMES data
times50<-read.csv("C:/Users/ccsim09/Desktop/times50.csv",header=T)
summary(times50$university_name)

# Play with US universities
USqssc<-read.csv("C:/Users/ccsim09/Desktop/USqssc.csv",header=T)
USrol<-mlogit.data(USqssc,shape="long",choice="RANK",alt.var = 'INSTITUTION',ranked = TRUE)
summary(mlogit(RANK~LAGRANK+TIMES.ranking|Unemployment,data=USrol,reflevel = "COLUMBIA UNIVERSITY"))

#Make prediction by using ROL model
leave1out<-read.csv("C:/Users/ccsim09/Desktop/leave1out.csv",header=T)
newd<-read.csv("C:/Users/ccsim09/Desktop/newd.csv",header=T)
testx<-newd[c(1,2,3,13,25,27)]
testxrol<-mlogit.data(newd,shape="long",choice="RANK",alt.var = 'INSTITUTION',ranked = FALSE)
leave1outrol<-mlogit.data(leave1out,shape="long",choice="RANK",alt.var = 'INSTITUTION',ranked = TRUE)
rol1<-mlogit(RANK~(TIMES.ranking+LAGRANK+Unemployment_growth)^2|gdpgrowth_lag,data=leave1outrol,reflevel = "THE UNIVERSITY OF NEW SOUTH WALES (UNSW)")
pred1<-predict(rol1,newdata=testxrol)
summary(rol1)
prediction<-read.csv("C:/Users/ccsim09/Desktop/Prediction ROL1.csv",header=T)
cor(prediction$Rank,prediction$Real)
concordant.pairs(prediction$INSTITUTION,newd$INSTITUTION)
concordant.pairs(prediction$Rank,prediction$Real)
discordant.pairs(prediction$INSTITUTION,newd$INSTITUTION)
cor(cbind(prediction$Rank,prediction$Real),method="kendall",use="pairwise")
ggplot(prediction,aes(x=prediction$Rank,y=prediction$Real))+geom_point()+geom_smooth()


rol2<-mlogit(RANK~TIMES.ranking+Times_overall+Times_teaching+Times_research+
                     Times_citations+Times_Industryincome+
                     Times_international_outlook+Unemployment_growth+
                     student_staff_ratio+Number_FTE+Number_FTE*TIMES.ranking+student_staff_ratio*TIMES.ranking|gdpgrowth_lag,data=qs50rol,reflevel = "THE UNIVERSITY OF NEW SOUTH WALES (UNSW)")
summary(rol2)
pred2<-predict(rol2,newdata=testxrol)
prediction2<-read.csv("C:/Users/ccsim09/Desktop/Prediction ROL3 for 2019QS.csv",header=T)
cor(prediction2$RANK,prediction2$Real_Re)
concordant.pairs(prediction2$INSTITUTION,prediction2$INS_19)
concordant.pairs(prediction2$RANK,prediction2$Real_Re)
discordant.pairs(prediction2$INSTITUTION,prediction2$INS_19)
cor(cbind(prediction2$RANK,prediction2$Real_Re),method="kendall",use="pairwise")
ggplot(prediction2,aes(x=prediction2$RANK,y=prediction2$Real_Re))+geom_point()+geom_smooth()
prediction2$INSTITUTION[which(prediction2$RANK<prediction2$Real_Re)]


prediction3<-read.csv("C:/Users/ccsim09/Desktop/Prediction ROL2.csv",header=T)
cor(prediction3$RANK,prediction3$Real)
concordant.pairs(prediction3$RANK,prediction3$Real)
cor(cbind(prediction3$RANK,prediction3$Real),method="kendall",use="pairwise")
ggplot(prediction3,aes(x=prediction3$RANK,y=prediction3$Real))+geom_point()+geom_smooth()
prediction3$INSTITUTION[which(prediction3$RANK<prediction3$Real)]

rol3<-mlogit(RANK~AR.SCORE+ER.SCORE+FS.SCORE+CPF.SCORE+IF.SCORE+IS.SCORE,data=leave1outrol,reflevel = "THE UNIVERSITY OF SYDNEY")
summary(rol3)


qsc<-read.csv("C:/Users/User/Desktop/newqssc49_Change.csv",header=T)
qscr<-mlogit.data(qsc,shape="long",choice="RANK",alt.var = 'INSTITUTION',ranked = TRUE)
rol4<-mlogit(RANK~TIMES_ranking+LAGRANK+Unemployment_growth+LAGRANK*Unemployment_growth,data=qscr,reflevel = "THE UNIVERSITY OF SYDNEY")
write.csv(predict(rol4,newdata=testxrol),"C:/Users/ccsim09/Desktop/rol4.csv")
rol4pred<-read.csv("C:/Users/ccsim09/Desktop/rol4.csv",header=T)
cor(rol4pred$rank_pred,rol4pred$rank_real)
concordant.pairs(rol4pred$rank_pred,rol4pred$rank_real)
cor(cbind(rol4pred$rank_pred,rol4pred$rank_real),method="kendall",use="pairwise")

rolgdp1<-mlogit(RANK~LAGRANK|gdpgrowth_lag,data=qscr,reflevel = "THE UNIVERSITY OF SYDNEY")
summary(rolgdp1)
rolgdp2<-mlogit(RANK~LAGRANK|gdpgrowth_lag2,data=qscr,reflevel = "THE UNIVERSITY OF SYDNEY")
summary(rolgdp2)

rol4cv<-mlogit(RANK~TIMES.ranking+LAGRANK+Unemployment_growth+LAGRANK*Unemployment_growth,data=leave1outrol,reflevel = "THE UNIVERSITY OF SYDNEY")
write.csv(predict(rol4cv,newdata=testxrol),"C:/Users/ccsim09/Desktop/rol4cv.csv")
rol4cvpred<-read.csv("C:/Users/ccsim09/Desktop/rol4cv.csv",header=T)
cor(rol4cvpred$rank_pred,rol4cvpred$rank_real)
concordant.pairs(rol4cvpred$rank_pred,rol4cvpred$rank_real)
cor(cbind(rol4cvpred$rank_pred,rol4cvpred$rank_real),method="kendall",use="pairwise")

rol5<-mlogit(RANK~LAGRANK*RegionGDP_growth+LAGRANK|gdpgrowth_lag2,data=qscr,reflevel = "THE UNIVERSITY OF SYDNEY")
rol6<-mlogit(RANK~LAGRANK+IF_GB,data=qscr,reflevel = "THE UNIVERSITY OF SYDNEY")

rolarwu<-mlogit(RANK~LAGRANK+alumni+pub|gdpgrowth_lag2,data=qscr,reflevel = "THE UNIVERSITY OF SYDNEY")

#to see if the effect of lag rank is determinant
rollag<-mlogit(RANK~LAGRANK,data=qscr,reflevel = "THE UNIVERSITY OF SYDNEY")
lag2019<-read.csv("C:/Users/ccsim09/Desktop/lag2019.csv",header=T)
lag2019r<-mlogit.data(lag2019,shape="long",choice="RANK",alt.var = 'INSTITUTION')
write.csv(predict(rollag,newdata=lag2019r),"C:/Users/ccsim09/Desktop/rollag2019.csv")
lagpred<-read.csv("C:/Users/ccsim09/Desktop/rollag2019.csv",header=T)
cor(lagpred$rank_pred,lagpred$rank_real)
concordant.pairs(lagpred$rank_pred,lagpred$rank_real)
cor(cbind(lagpred$rank_pred,lagpred$rank_real),method="kendall",use="pairwise")

#Make prediction by using ACL Model
aclqs<-vglm(RANK~AR.SCORE+ER.SCORE+FS.SCORE+CPF.SCORE+IF.SCORE+IS.SCORE+LAGRANK+LAGRANK*AR.SCORE,
            family=acat(reverse=TRUE,parallel=TRUE),data=qssc49)
aclqs<-vglm(RANK~AR.SCORE,
            family=acat(reverse=TRUE,parallel=TRUE),data=qssc49)
aclpred<-read.csv("C:/Users/ccsim09/Desktop/SAS_pred.csv",header=T)
cor(aclpred$RANK,aclpred$predrank)
concordant.pairs(aclpred$INSTITUTION,aclpred$INSTITUTION1)
concordant.pairs(aclpred$predrank,aclpred$RANK)
rankcor<-cbind(aclpred$predrank,aclpred$RANK)
cor(rankcor,method="kendall",use="pairwise")
discordant.pairs(aclpred$INSTITUTION,aclpred$INSTITUTION1)
ggplot(aclpred,aes(x=aclpred$predrank,y=aclpred$RANK))+geom_point()+geom_smooth()

#log-likelihood using SAS genmod
genpred<-read.csv("C:/Users/ccsim09/Desktop/z.csv",header=T)
cor(genpred$Rank_pred,genpred$RANK_Real)
concordant.pairs(genpred$Rank_pred,genpred$RANK_Real)
cor(cbind(genpred$Rank_pred,genpred$RANK_Real),method="kendall",use="pairwise")

genpredcv<-read.csv("C:/Users/ccsim09/Desktop/zz.csv",header=T)
cor(genpredcv$rank_pred,genpredcv$rank_real)
concordant.pairs(genpredcv$rank_pred,genpredcv$rank_real)
cor(cbind(genpredcv$rank_pred,genpredcv$rank_real),method="kendall",use="pairwise")

#Predict individual scores
ARpred<-lm(AR.SCORE~Times_teaching+Unemployment_growth,data=leave1out)
ERpred<-lm(ER.SCORE~Times_Industryincome+gdpgrowth_lag,data=leave1out)
FSpred<-lm(FS.SCORE~student_staff_ratio+Number_FTE,data=leave1out)
CPFpred<-lm(CPF.SCORE~Times_citations+Times_research,data=leave1out)
IFpred<-lm(IF.SCORE~Times_international_outlook,data=leave1out)
ISpred<-lm(IS.SCORE~international_students+Times_international_outlook,data=leave1out)
AR<-predict(ARpred,newdata=newd)
ER<-predict(ERpred,newdata=newd)
FS<-predict(FSpred,newdata=newd)
CPF<-predict(CPFpred,newdata=newd)
IF<-predict(IFpred,newdata=newd)
IS<-predict(ISpred,newdata=newd)
newone<-cbind(AR,ER,FS,CPF,IF,IS)
write.csv(newone, "C:/Users/ccsim09/Desktop/newScore.csv")

lmpred<-read.csv("C:/Users/ccsim09/Desktop/newScore.csv",header=T)
cor(lmpred$RANK,lmpred$preRank)
concordant.pairs(prediction2$INSTITUTION,prediction2$INS_19)
concordant.pairs(lmpred$RANK,lmpred$preRank)
discordant.pairs(prediction2$INSTITUTION,prediction2$INS_19)
cor(cbind(lmpred$RANK,lmpred$preRank),method="kendall",use="pairwise")
ggplot(lmpred,aes(x=lmpred$preRank,y=lmpred$RANK))+geom_point()+geom_smooth()
aclpred$INSTITUTION[which(aclpred$predrank<aclpred$RANK)]


#Consider the change in each sub scores
qsc<-read.csv("C:/Users/ccsim09/Desktop/newqssc49_Change.csv",header=T)
qscr<-mlogit.data(qsc,shape="long",choice="RANK",alt.var = 'INSTITUTION',ranked = TRUE)
rolch<-mlogit(RANK~AR_change+ER_change+FS_change+CPF_change+IF_change+IS_change,data=qscr,reflevel = "THE UNIVERSITY OF SYDNEY")
summary(rolch)

summary(lm(Rank_Change~AR_change+ER_change+FS_change+CPF_change+IF_change+IS_change,data=qsc))
