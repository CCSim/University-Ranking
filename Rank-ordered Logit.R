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


qsc<-read.csv("C:/Users/ccsim09/Desktop/newqssc49_Change_09.csv",header=T)
qscr<-mlogit.data(qsc,shape="long",choice="RANK",alt.var = 'INSTITUTION',ranked = TRUE)
rol4<-mlogit(RANK~TIMES_ranking+LAGRANK+Unemployment_growth+LAGRANK*Unemployment_growth,data=qscr,reflevel = "THE UNIVERSITY OF SYDNEY")
write.csv(predict(rol4,newdata=testxrol),"C:/Users/ccsim09/Desktop/rol4.csv")
rol4pred<-read.csv("C:/Users/ccsim09/Desktop/rol4.csv",header=T)
cor(rol4pred$rank_pred,rol4pred$rank_real)
concordant.pairs(rol4pred$rank_pred,rol4pred$rank_real)
cor(cbind(rol4pred$rank_pred,rol4pred$rank_real),method="kendall",use="pairwise")

rolgdp1<-mlogit(RANK~LAGRANK+Country|gdpgrowth_lag,data=qscr,reflevel = "THE UNIVERSITY OF SYDNEY")
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

plot(qsc$RANK,qsc$alumni)
plot(qsc$RANK,qsc$award)
plot(qsc$RANK,qsc$hici)
plot(qsc$RANK,qsc$ns)
plot(qsc$RANK,qsc$pub)
plot(qsc$RANK,qsc$pcp)
ggplot(qsc, aes(x=RANK, y=alumni))+geom_point()+geom_smooth()
ggplot(qsc, aes(x=RANK, y=award))+geom_point()+geom_smooth()
ggplot(qsc, aes(x=RANK, y=hici))+geom_point()+geom_smooth()
ggplot(qsc, aes(x=RANK, y=ns))+geom_point()+geom_smooth()
ggplot(qsc, aes(x=RANK, y=pub))+geom_point()+geom_smooth()
ggplot(qsc, aes(x=RANK, y=pcp))+geom_point()+geom_smooth()
rolarwu<-mlogit(RANK~LAGRANK+ns+alumni+LAGRANK*alumni|gdpgrowth_lag2,data=qscr,reflevel = "THE UNIVERSITY OF SYDNEY")
summary(rolarwu)

qsc$arsub=qsc$AR_SCORE-mean(qsc$AR_SCORE)
qsc$ersub=qsc$ER_SCORE-mean(qsc$ER_SCORE)
qsc$fssub=qsc$FS_SCORE-mean(qsc$FS_SCORE)
qsc$cpfsub=qsc$CPF_SCORE-mean(qsc$CPF_SCORE)
qsc$ifsub=qsc$IF_SCORE-mean(qsc$IF_SCORE)
qsc$issub=qsc$IS_SCORE-mean(qsc$IS_SCORE)
qsc$nssub=qsc$ns-mean(qsc$ns)
qsc$alusub=qsc$alumni-mean(qsc$alumni)
qsc$pubsub=qsc$pub-mean(qsc$pub)

rolarwu1<-mlogit(RANK~LAGRANK+alusub+ns+ns+LAGRANK*alusub+pubsub*LAGRANK|gdpgrowth_lag2,data=qscr,reflevel = "THE UNIVERSITY OF SYDNEY")
summary(rolarwu1)


rolarwu2<-mlogit(RANK~LAGRANK+alumni+pub+LAGRANK*alumni+pub*LAGRANK|gdpgrowth_lag2,data=qscr,reflevel = "THE UNIVERSITY OF SYDNEY")
summary(rolarwu2)


qsc8<-read.csv("C:/Users/ccsim09/Desktop/newqssc49_Change.csv",header=T)
qsc8$timesscale<-scale(qsc8$TIMES_ranking)
qsc8$qsscale<-scale(qsc8$RANK)
qsc8$arwuscale<-scale(qsc8$ARWU_ranking)
qscr8<-mlogit.data(qsc8,shape="long",choice="RANK",alt.var = 'INSTITUTION',ranked = TRUE)
rolarwu8<-mlogit(RANK~LAGRANK+TIMES_ranking+ARWU_ranking+TIMES_ranking*ARWU_ranking+pub+alumni+alumni*ARWU_ranking|gdpgrowth_lag2,data=qscr8,reflevel = "THE UNIVERSITY OF SYDNEY")
summary(rolarwu8)

regionb<-mlogit(RANK~timesscale+IF_GB,data=qscr8,reflevel = "THE UNIVERSITY OF SYDNEY")

##bar plot of the variables
par(mfrow=c(2,3), mar=c(1,1,1,1))
boxplot(qsc$AR_SCORE~qsc$RANK, col=c("bisque4","bisque3"), 
        main="AR",horizontal=TRUE, axes=TRUE)
boxplot(qsc$ER_SCORE~qsc$RANK, col=c("bisque4","bisque3"), 
        main="ER",horizontal=TRUE, axes=TRUE)
boxplot(qsc$FS_SCORE~qsc$RANK, col=c("bisque4","bisque3"), 
        main="FS",horizontal=TRUE, axes=TRUE)
boxplot(qsc$CPF_SCORE~qsc$RANK, col=c("bisque4","bisque3"), 
        main="CPF",horizontal=TRUE, axes=TRUE)
boxplot(qsc$IF_SCORE~qsc$RANK, col=c("bisque4","bisque3"), 
        main="IF",horizontal=TRUE, axes=TRUE)
boxplot(qsc$IS_SCORE~qsc$RANK, col=c("bisque4","bisque3"), 
        main="IS",horizontal=TRUE, axes=TRUE)

par(mfrow=c(2,3), mar=c(1,1,1,1))
boxplot(qsc$alumni~qsc$RANK, col=c("bisque4","bisque3"), 
        main="alumni",horizontal=TRUE, axes=TRUE)
boxplot(qsc$pcp~qsc$RANK, col=c("bisque4","bisque3"), 
        main="pcp",horizontal=TRUE, axes=TRUE)
boxplot(qsc$ns~qsc$RANK, col=c("bisque4","bisque3"), 
        main="ns",horizontal=TRUE, axes=TRUE)
boxplot(qsc$pub~qsc$RANK, col=c("bisque4","bisque3"), 
        main="pub",horizontal=TRUE, axes=TRUE)
boxplot(qsc$award~qsc$RANK, col=c("bisque4","bisque3"), 
        main="award",horizontal=TRUE, axes=TRUE)
boxplot(qsc$hici~qsc$RANK, col=c("bisque4","bisque3"), 
        main="hici",horizontal=TRUE, axes=TRUE)

par(mfrow=c(2,3), mar=c(1,1,1,1))
boxplot(qsc8$Times_teaching~qsc8$RANK, col=c("bisque4","bisque3"), 
        main="Times_teaching",horizontal=TRUE, axes=TRUE)
boxplot(qsc8$Times_research~qsc8$RANK, col=c("bisque4","bisque3"), 
        main="Times_research",horizontal=TRUE, axes=TRUE)
boxplot(qsc8$Times_Industryincome~qsc8$RANK, col=c("bisque4","bisque3"), 
        main="Times_Industryincome",horizontal=TRUE, axes=TRUE)
boxplot(qsc8$Times_international_outlook~qsc8$RANK, col=c("bisque4","bisque3"), 
        main="Times_international_outlook",horizontal=TRUE, axes=TRUE)
boxplot(qsc8$Times_citations~qsc8$RANK, col=c("bisque4","bisque3"), 
        main="Times_citations",horizontal=TRUE, axes=TRUE)

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


## Examine if using Times ranking as the major input
times8<-read.csv("C:/Users/User/Desktop/Fortimes.csv",header=T)
times8$AR_SCORE<-scale(times8$AR_SCORE)
times8$ER_SCORE<-scale(times8$ER_SCORE)
times8$CPF_SCORE<-scale(times8$CPF_SCORE)
times8$FS_SCORE<-scale(times8$FS_SCORE)
times8$IF_SCORE<-scale(times8$IF_SCORE)
times8$IS_SCORE<-scale(times8$IS_SCORE)
times8$award<-scale(times8$award)
times8$hici<-scale(times8$hici)
times8$alumni<-scale(times8$alumni)
times8$ns<-scale(times8$ns)
times8$pcp<-scale(times8$pcp)
times8$pub<-scale(times8$pub)

times8r<-mlogit.data(times8,shape="long",choice="TIMES_ranking",alt.var = 'INSTITUTION',ranked = TRUE)
timesrol<-mlogit(TIMES_ranking~RANK+AR_SCORE+ER_SCORE+CPF_SCORE+FS_SCORE+IS_SCORE+IF_SCORE+hici+pcp+ns+alumni+pub+award|gdpgrowth_lag2,data=times8r,reflevel = "THE UNIVERSITY OF SYDNEY")
summary(timesrol)

timesrol2<-mlogit(TIMES_ranking~(RANK+pub+award+hici+ns+pcp+alumni+AR_SCORE+ER_SCORE+CPF_SCORE+FS_SCORE+IS_SCORE+IF_SCORE)^2|gdpgrowth_lag,data=times8r,reflevel = "THE UNIVERSITY OF SYDNEY")
summary(timesrol2)

timesrol4<-mlogit(TIMES_ranking~(RANK+RANK*(pub+award+hici+ns+pcp+alumni)+(pub+award+hici+ns+pcp+alumni)*(AR_SCORE+ER_SCORE+CPF_SCORE+FS_SCORE+IS_SCORE+IF_SCORE))|gdpgrowth_lag,data=times8r,reflevel = "THE UNIVERSITY OF SYDNEY")
summary(timesrol4)

timesrol3<-mlogit(TIMES_ranking~RANK+pub*RANK+hici*RANK
                  +award*RANK+pub+award+hici|gdpgrowth_lag2,data=times8r,reflevel = "THE UNIVERSITY OF SYDNEY")
summary(timesrol3)

cor(times8$Times_research,times8$pub)
cor(times8$Times_citations,times8$pub)

##plot the times ranking and other sub scores
par(mfrow=c(2,3), mar=c(1,1,1,1))
boxplot(times8$AR_SCORE~times8$TIMES_ranking, col=c("bisque4","bisque3"), 
        main="AR",horizontal=TRUE, axes=TRUE)
boxplot(times8$ER_SCORE~times8$TIMES_ranking, col=c("bisque4","bisque3"), 
        main="ER",horizontal=TRUE, axes=TRUE)
boxplot(times8$FS_SCORE~times8$TIMES_ranking, col=c("bisque4","bisque3"), 
        main="FS",horizontal=TRUE, axes=TRUE)
boxplot(times8$CPF_SCORE~times8$TIMES_ranking, col=c("bisque4","bisque3"), 
        main="CPF",horizontal=TRUE, axes=TRUE)
boxplot(times8$IF_SCORE~times8$TIMES_ranking, col=c("bisque4","bisque3"), 
        main="IF",horizontal=TRUE, axes=TRUE)
boxplot(times8$IS_SCORE~times8$TIMES_ranking, col=c("bisque4","bisque3"), 
        main="IS",horizontal=TRUE, axes=TRUE)

par(mfrow=c(2,3), mar=c(1,1,1,1))
boxplot(times8$alumni~times8$TIMES_ranking, col=c("bisque4","bisque3"), 
        main="alumni",horizontal=TRUE, axes=TRUE)
boxplot(times8$award~times8$TIMES_ranking, col=c("bisque4","bisque3"), 
        main="award",horizontal=TRUE, axes=TRUE)
boxplot(times8$pub~times8$TIMES_ranking, col=c("bisque4","bisque3"), 
        main="pub",horizontal=TRUE, axes=TRUE)
boxplot(times8$pcp~times8$TIMES_ranking, col=c("bisque4","bisque3"), 
        main="pcp",horizontal=TRUE, axes=TRUE)
boxplot(times8$hici~times8$TIMES_ranking, col=c("bisque4","bisque3"), 
        main="hici",horizontal=TRUE, axes=TRUE)
boxplot(times8$ns~times8$TIMES_ranking, col=c("bisque4","bisque3"), 
        main="ns",horizontal=TRUE, axes=TRUE)


# adverse selection in ROL for QS
qsc<-read.csv("C:/Users/ccsim09/Desktop/newqssc49_Change_09.csv",header=T)
qsc$arsub=qsc$AR_SCORE-mean(qsc$AR_SCORE)
qsc$ersub=qsc$ER_SCORE-mean(qsc$ER_SCORE)
qsc$fssub=qsc$FS_SCORE-mean(qsc$FS_SCORE)
qsc$cpfsub=qsc$CPF_SCORE-mean(qsc$CPF_SCORE)
qsc$ifsub=qsc$IF_SCORE-mean(qsc$IF_SCORE)
qsc$issub=qsc$IS_SCORE-mean(qsc$IS_SCORE)
qsc$nssub=qsc$ns-mean(qsc$ns)
qsc$alusub=qsc$alumni-mean(qsc$alumni)
qsc$pubsub=qsc$pub-mean(qsc$pub)
qsc$hicisub=qsc$hici-mean(qsc$hici)
qsc$awardsub=qsc$award-mean(qsc$award)
qsc$pcpsub=qsc$pcp-mean(qsc$pcp)
qscr<-mlogit.data(qsc,shape="long",choice="RANK",alt.var = 'INSTITUTION',ranked = TRUE)
adrol<-mlogit(RANK~LAGRANK+arsub+ersub+fssub+cpfsub+ifsub+issub+nssub+alusub+pubsub+awardsub+hicisub+pcpsub|gdpgrowth_lag2,data=qscr,reflevel = "THE UNIVERSITY OF SYDNEY")
coefad<-adrol$coefficients
write.csv(qsc,"C:/Users/ccsim09/Desktop/qscad.csv")
qscad<-read.csv("C:/Users/ccsim09/Desktop/qscad.csv")
summary(qscad)
boxplot(qscad$BLAG,qscad$bar,qscad$ber,qscad$bfs,qscad$bcpf,qscad$bif,qscad$bis,
        qscad$bns,qscad$balu,qscad$bpub,qscad$baward,qscad$bpcp,qscad$bhici,col="bisque4",
        names=c("LAG","AR","ER","FS","CPF","IF","IS","ns","alumni","pub","award","pcp","hici"))
###### plot by year
par(mfrow=c(2,3), mar=c(1,1,1,1))
boxplot(qscad$BLAG~qscad$Year,col="bisque4",main="LAG RANK")
boxplot(qscad$bar~qscad$Year,col="bisque4",main="AR")
boxplot(qscad$ber~qscad$Year,col="bisque4",main="ER")
boxplot(qscad$bfs~qscad$Year,col="bisque4",main="FS")
boxplot(qscad$bcpf~qscad$Year,col="bisque4",main="CPF")
boxplot(qscad$bif~qscad$Year,col="bisque4",main="IF")
boxplot(qscad$bis~qscad$Year,col="bisque4",main="IS")
par(mfrow=c(2,3), mar=c(1,1,1,1))
boxplot(qscad$bns~qscad$Year,col="bisque4",main="ns")
boxplot(qscad$balu~qscad$Year,col="bisque4",main="alu")
boxplot(qscad$bpub~qscad$Year,col="bisque4",main="pub")
boxplot(qscad$baward~qscad$Year,col="bisque4",main="award")
boxplot(qscad$bpcp~qscad$Year,col="bisque4",main="pcp")
boxplot(qscad$bhici~qscad$Year,col="bisque4",main="hici")

##### ggplot
qscadhku<-qscad[which(qscad$INSTITUTION=="UNIVERSITY OF HONG KONG (HKU)"),]

ggplot()+geom_boxplot(data=qscad,aes(x=Year,y=bar,group=Year))+
  geom_point(data=qscad[which(qscad$INSTITUTION=="UNIVERSITY OF HONG KONG (HKU)"),],aes(x=Year,y=bar),color="red")+
  labs(y = "AR Effect")+ggtitle("Plot of AR effect")
ggplot()+geom_boxplot(data=qscad,aes(x=Year,y=ber,group=Year))+
  geom_point(data=qscad[which(qscad$INSTITUTION=="UNIVERSITY OF HONG KONG (HKU)"),],aes(x=Year,y=ber),color="red")+
  labs(y = "ER Effect")+ggtitle("Plot of ER effect")
ggplot()+geom_boxplot(data=qscad,aes(x=Year,y=bfs,group=Year))+
  geom_point(data=qscad[which(qscad$INSTITUTION=="UNIVERSITY OF HONG KONG (HKU)"),],aes(x=Year,y=bfs),color="red")+
  labs(y = "FS Effect")+ggtitle("Plot of FS effect")
ggplot()+geom_boxplot(data=qscad,aes(x=Year,y=bcpf,group=Year))+
  geom_point(data=qscad[which(qscad$INSTITUTION=="UNIVERSITY OF HONG KONG (HKU)"),],aes(x=Year,y=bcpf),color="red")+
  labs(y = "CPF Effect")+ggtitle("Plot of CPF effect")
ggplot()+geom_boxplot(data=qscad,aes(x=Year,y=bif,group=Year))+
  geom_point(data=qscad[which(qscad$INSTITUTION=="UNIVERSITY OF HONG KONG (HKU)"),],aes(x=Year,y=bif),color="red")+
  labs(y = "IF Effect")+ggtitle("Plot of IF effect")
ggplot()+geom_boxplot(data=qscad,aes(x=Year,y=bis,group=Year))+
  geom_point(data=qscad[which(qscad$INSTITUTION=="UNIVERSITY OF HONG KONG (HKU)"),],aes(x=Year,y=bis),color="red")+
  labs(y = "IS Effect")+ggtitle("Plot of IS effect")

ggplot()+geom_boxplot(data=qscad,aes(x=Year,y=bns,group=Year))+
  geom_point(data=qscad[which(qscad$INSTITUTION=="UNIVERSITY OF HONG KONG (HKU)"),],aes(x=Year,y=bns),color="red")+
  labs(y = "ns Effect")+ggtitle("Plot of ns effect")
ggplot()+geom_boxplot(data=qscad,aes(x=Year,y=balu,group=Year))+
  geom_point(data=qscad[which(qscad$INSTITUTION=="UNIVERSITY OF HONG KONG (HKU)"),],aes(x=Year,y=balu),color="red")+
  labs(y = "alumni Effect")+ggtitle("Plot of alumni effect")
ggplot()+geom_boxplot(data=qscad,aes(x=Year,y=bpub,group=Year))+
  geom_point(data=qscad[which(qscad$INSTITUTION=="UNIVERSITY OF HONG KONG (HKU)"),],aes(x=Year,y=bpub),color="red")+
  labs(y = "pub Effect")+ggtitle("Plot of pub effect")
ggplot()+geom_boxplot(data=qscad,aes(x=Year,y=bhici,group=Year))+
  geom_point(data=qscad[which(qscad$INSTITUTION=="UNIVERSITY OF HONG KONG (HKU)"),],aes(x=Year,y=bhici),color="red")+
  labs(y = "hici Effect")+ggtitle("Plot of hici effect")
ggplot()+geom_boxplot(data=qscad,aes(x=Year,y=baward,group=Year))+
  geom_point(data=qscad[which(qscad$INSTITUTION=="UNIVERSITY OF HONG KONG (HKU)"),],aes(x=Year,y=baward),color="red")+
  labs(y = "award Effect")+ggtitle("Plot of award effect")
ggplot()+geom_boxplot(data=qscad,aes(x=Year,y=bpcp,group=Year))+
  geom_point(data=qscad[which(qscad$INSTITUTION=="UNIVERSITY OF HONG KONG (HKU)"),],aes(x=Year,y=bpcp),color="red")+
  labs(y = "pcp Effect")+ggtitle("Plot of pcp effect")


adrol2<-mlogit(RANK~LAGRANK+nssub+alusub|gdpgrowth_lag2,data=qscr,reflevel = "THE UNIVERSITY OF SYDNEY")
summary(adrol2)
qscad1<-read.csv("C:/Users/User/Desktop/qscad1.csv")
boxplot(qscad1$nsad,qscad1$aluad,col="bisque4")


par(mfrow=c(2,3), mar=c(1,1,1,1))
plot(qsc$RANK,qscad$bar,main ="AR")
plot(qsc$RANK,qscad$ber,main ="ER")
plot(qsc$RANK,qscad$bfs,main="FS")
plot(qsc$RANK,qscad$cpf,main="CPF")
plot(qsc$RANK,qscad$bif,main="IF")
plot(qsc$RANK,qscad$bis,main="IS")
par(mfrow=c(2,3), mar=c(1,1,1,1))
plot(qsc$RANK,qscad$bns,main="ns")
plot(qsc$RANK,qscad$balu,main="alumni")
plot(qsc$RANK,qscad$bpub,main="pub")
plot(qsc$RANK,qscad$baward,main="award")
plot(qsc$RANK,qscad$bpcp,main="pcp")
plot(qsc$RANK,qscad$bhici,main="hici")


##adverse selection for QSC8
qsc8<-read.csv("C:/Users/ccsim09/Desktop/newqssc49_Change.csv",header=T)
qsc8$arsub=qsc8$AR_SCORE-mean(qsc8$AR_SCORE)
qsc8$ersub=qsc8$ER_SCORE-mean(qsc8$ER_SCORE)
qsc8$fssub=qsc8$FS_SCORE-mean(qsc8$FS_SCORE)
qsc8$cpfsub=qsc8$CPF_SCORE-mean(qsc8$CPF_SCORE)
qsc8$ifsub=qsc8$IF_SCORE-mean(qsc8$IF_SCORE)
qsc8$issub=qsc8$IS_SCORE-mean(qsc8$IS_SCORE)
qsc8$nssub=qsc8$ns-mean(qsc8$ns)
qsc8$alusub=qsc8$alumni-mean(qsc8$alumni)
qsc8$pubsub=qsc8$pub-mean(qsc8$pub)
qsc8$hicisub=qsc8$hici-mean(qsc8$hici)
qsc8$awasub=qsc8$award-mean(qsc8$award)
qsc8$pcpsub=qsc8$pcp-mean(qsc8$pcp)
qsc8$tteachsub=qsc8$Times_teaching-mean(qsc8$Times_teaching)
qsc8$trshsub=qsc8$Times_research-mean(qsc8$Times_research)
qsc8$tcitsub=qsc8$Times_citations-mean(qsc8$Times_citations)
qsc8$tininsub=qsc8$Times_Industryincome-mean(qsc8$Times_Industryincome)
qsc8$tiosub=qsc8$Times_international_outlook-mean(qsc8$Times_international_outlook)



qsc8r<-mlogit.data(qsc8,shape="long",choice="RANK",alt.var = 'INSTITUTION',ranked = TRUE)
ad8rol<-mlogit(RANK~LAGRANK*(alusub+awasub+nssub+pcpsub+pubsub+hicisub)|gdpgrowth_lag2,data=qsc8r,reflevel = "THE UNIVERSITY OF SYDNEY")
ad8rol1<-mlogit(RANK~LAGRANK+alusub+awasub+nssub+pcpsub+pubsub+hicisub|gdpgrowth_lag2,data=qsc8r,reflevel = "THE UNIVERSITY OF SYDNEY")
summary(ad8rol)
summary(ad8rol1)

whole<-mlogit(RANK~LAGRANK+tteachsub+trshsub+tcitsub+tininsub+tiosub+alusub+awasub+nssub+pcpsub+pubsub+hicisub|gdpgrowth_lag2,data=qsc8r,reflevel = "THE UNIVERSITY OF SYDNEY")
summary(whole)

ad8rol2<-mlogit(RANK~LAGRANK*(tteachsub+trshsub+tcitsub+tininsub+tiosub)|gdpgrowth_lag2,data=qsc8r,reflevel = "THE UNIVERSITY OF SYDNEY")
ad8rol3<-mlogit(RANK~LAGRANK+tteachsub*top-top+tteachsub+trshsub+tcitsub+tininsub+tiosub|gdpgrowth_lag2,data=qsc8r,reflevel = "THE UNIVERSITY OF SYDNEY")
summary(ad8rol2)
summary(ad8rol3)
coefad<-ad8rol$coefficients

qsc8$top=0
for (i in 1:length(qsc8$RANK)){
if (qsc8$RANK[i]<20) {
  qsc8$top[i]=1
} 
}
qsc8$top

qsc8$lagtop=0
for (i in 1:length(qsc8$LAGRANK)){
  if (qsc8$LAGRANK[i]<20) {
    qsc8$lagtop[i]=1
  } 
}
qsc8$lagtop

#if_top indicator
whole1<-mlogit(RANK~top*(LAGRANK+arsub+ersub+fssub+cpfsub+ifsub+issub)+tteachsub+trshsub+tcitsub+tininsub+tiosub+alusub+awasub+nssub+pcpsub+pubsub+hicisub|gdpgrowth_lag2,data=qsc8r,reflevel = "THE UNIVERSITY OF SYDNEY")
summary(whole1)

whole2<-mlogit(RANK~LAGRANK+top*(tteachsub+trshsub+tcitsub+tininsub+tiosub)+alusub+awasub+nssub+pcpsub+pubsub+hicisub|gdpgrowth_lag2,data=qsc8r,reflevel = "THE UNIVERSITY OF SYDNEY")
summary(whole2)

whole3<-mlogit(RANK~LAGRANK+tteachsub+trshsub+tcitsub+tininsub+tiosub+top*(alusub+awasub+nssub+pcpsub+pubsub+hicisub)|gdpgrowth_lag2,data=qsc8r,reflevel = "THE UNIVERSITY OF SYDNEY")
summary(whole3)

####GB_AU_CA indicator
ad8rolREGION<-mlogit(RANK~LAGRANK+IF_GB_AU_CA*TIMES_ranking-IF_GB_AU_CA+tteachsub+tteachsub+trshsub+tcitsub+tininsub+tiosub|gdpgrowth_lag2,data=qsc8r,reflevel = "THE UNIVERSITY OF SYDNEY")
summary(ad8rolREGION)

ad8rol2REGION<-mlogit(RANK~LAGRANK*IF_GB_AU_CA-IF_GB_AU_CA+alusub+awasub+nssub+pcpsub+pubsub+hicisub|gdpgrowth_lag2,data=qsc8r,reflevel = "THE UNIVERSITY OF SYDNEY")
summary(ad8rol2REGION)

whole4<-mlogit(RANK~LAGRANK+IF_GB_AU_CA*(arsub+ersub+fssub+cpfsub+ifsub+issub)+tteachsub+trshsub+tcitsub+tininsub+tiosub+alusub+awasub+nssub+pcpsub+pubsub+hicisub|gdpgrowth_lag2,data=qsc8r,reflevel = "THE UNIVERSITY OF SYDNEY")
summary(whole4) ### computationally singular

#### interatcion between LAGRANK and subscores
whole5<-mlogit(RANK~LAGRANK*(arsub+ersub+fssub+cpfsub+ifsub+issub)+tteachsub+trshsub+tcitsub+tininsub+tiosub+alusub+awasub+nssub+pcpsub+pubsub+hicisub|gdpgrowth_lag2,data=qsc8r,reflevel = "THE UNIVERSITY OF SYDNEY")
summary(whole5)

whole6<-mlogit(RANK~LAGRANK*(tteachsub+trshsub+tcitsub+tininsub+tiosub)+alusub+awasub+nssub+pcpsub+pubsub+hicisub|gdpgrowth_lag2,data=qsc8r,reflevel = "THE UNIVERSITY OF SYDNEY")
summary(whole6)

whole7<-mlogit(RANK~LAGRANK+tteachsub+trshsub+tcitsub+tininsub+tiosub+LAGRANK*(alusub+awasub+nssub+pcpsub+pubsub+hicisub)|gdpgrowth_lag2,data=qsc8r,reflevel = "THE UNIVERSITY OF SYDNEY")
summary(whole7)

##Adverse selection on ROL for Times
times8<-read.csv("C:/Users/ccsim09/Desktop/Fortimes.csv",header=T)
times8$arsub=times8$AR_SCORE-mean(times8$AR_SCORE)
times8$ersub=times8$ER_SCORE-mean(times8$ER_SCORE)
times8$fssub=times8$FS_SCORE-mean(times8$FS_SCORE)
times8$cpfsub=times8$CPF_SCORE-mean(times8$CPF_SCORE)
times8$ifsub=times8$IF_SCORE-mean(times8$IF_SCORE)
times8$issub=times8$IS_SCORE-mean(times8$IS_SCORE)
times8$nssub=times8$ns-mean(times8$ns)
times8$alusub=times8$alumni-mean(times8$alumni)
times8$pubsub=times8$pub-mean(times8$pub)
times8$pcpsub=times8$pcp-mean(times8$pcp)
times8$awasub=times8$award-mean(times8$award)
times8$hicisub=times8$hici-mean(times8$hici)
times8$tteachsub=times8$Times_teaching-mean(times8$Times_teaching)
times8$trshsub=times8$Times_research-mean(times8$Times_research)
times8$tcitsub=times8$Times_citations-mean(times8$Times_citations)
times8$tininsub=times8$Times_Industryincome-mean(times8$Times_Industryincome)
times8$tiosub=times8$Times_international_outlook-mean(times8$Times_international_outlook)

times8$top=0
for (i in 1:length(times8$TIMES_ranking)){
  if (times8$TIMES_ranking[i]<20) {
    times8$top[i]=1
  } 
}
times8$top


sbss<-mlogit(TIMES_ranking~LAGRANK+arsub+ersub+fssub+cpfsub+ifsub+issub+pcpsub+nssub+alusub+pubsub+hicisub*top-top+awasub|gdpgrowth_lag2,data=timesr,reflevel = "THE UNIVERSITY OF SYDNEY")
summary(sbss)

timesr<-mlogit.data(times8,shape="long",choice="TIMES_ranking",alt.var = 'INSTITUTION',ranked = TRUE)
timesadrol<-mlogit(TIMES_ranking~LAGRANK+tteachsub+trshsub+tcitsub+tininsub+tiosub+arsub+ersub+fssub+cpfsub+ifsub+issub+pcpsub+nssub+alusub+pubsub+hicisub+awasub|gdpgrowth_lag2,data=timesr,reflevel = "THE UNIVERSITY OF SYDNEY")
summary(timesadrol)
write.csv(times8,"C:/Users/ccsim09/Desktop/times888.csv")
timescad1<-read.csv("C:/Users/ccsim09/Desktop/times888.csv")
boxplot(timescad1$bAR,timescad1$bER,timescad1$bfs,timescad1$bcpf,timescad1$bif,timescad1$bis,
        timescad1$bns,timescad1$balu,timescad1$bpub,timescad1$bawa,timescad1$bpcp,timescad1$bhici,col="bisque4",
        names=c("AR","ER","FS","CPF","IF","IS","ns","alumni","pub","award","pcp","hici"))
boxplot(timescad1$bAR,timescad1$bER,timescad1$bfs,timescad1$bcpf,timescad1$bif,timescad1$bis,
        col="bisque4",
        names=c("AR","ER","FS","CPF","IF","IS"))
boxplot(timescad1$bns,timescad1$balu,timescad1$bpub,timescad1$bawa,timescad1$bpcp,timescad1$bhici,
        col="bisque4",
        names=c("ns","alumni","pub","award","pcp","hici"))


par(mfrow=c(1,1),mar=c(2,2,2,2))
boxplot(timescad1$bteach,timescad1$btrch,timescad1$btcit,timescad1$btinin,timescad1$btio,
        timescad1$bar,timescad1$ber,timescad1$bfs,timescad1$bcpf,timescad1$bif,timescad1$bis,
        timescad1$bns,timescad1$balu,timescad1$bpub,timescad1$bawa,timescad1$bpcp,timescad1$bhici,col="bisque4",
        names=c("teaching","research","citations","industry","international","AR","ER","FS","CPF","IF","IS","ns","alumni","pub","award","pcp","hici"))
boxplot(timescad1$bteach,timescad1$btrch,timescad1$btcit,timescad1$btinin,timescad1$btio,
        col="bisque4",
        names=c("teaching","research","citations","industry","international"))
boxplot(timescad1$bar,timescad1$ber,timescad1$bfs,timescad1$bcpf,timescad1$bif,timescad1$bis,
        timescad1$bns,timescad1$balu,timescad1$bpub,timescad1$bawa,timescad1$bpcp,timescad1$bhici,col="bisque4",
        names=c("AR","ER","FS","CPF","IF","IS","ns","alumni","pub","award","pcp","hici"))

adtimesREGION<-mlogit(TIMES_ranking~RANK+RANK*IF_GB_AU_CA-IF_GB_AU_CA+arsub+ersub+fssub+cpfsub+ifsub+issub,data=timesr,reflevel = "THE UNIVERSITY OF SYDNEY")
summary(adtimesREGION)

adtimesREGION1<-mlogit(TIMES_ranking~RANK+RANK*IF_GB-IF_GB+arsub+ersub+fssub+cpfsub+ifsub+issub,data=timesr,reflevel = "THE UNIVERSITY OF SYDNEY")
summary(adtimesREGION1)

ggplot()+geom_boxplot(data=timescad1,aes(x=Year,y=bar,group=Year))+
  geom_point(data=timescad1[which(timescad1$INSTITUTION=="UNIVERSITY OF HONG KONG (HKU)"),],aes(x=Year,y=bar),color="red")+
  labs(y = "AR Effect")+ggtitle("Plot of AR effect")
ggplot()+geom_boxplot(data=timescad1,aes(x=Year,y=ber,group=Year))+
  geom_point(data=timescad1[which(timescad1$INSTITUTION=="UNIVERSITY OF HONG KONG (HKU)"),],aes(x=Year,y=ber),color="red")+
  labs(y = "ER Effect")+ggtitle("Plot of ER effect")
ggplot()+geom_boxplot(data=timescad1,aes(x=Year,y=bfs,group=Year))+
  geom_point(data=timescad1[which(timescad1$INSTITUTION=="UNIVERSITY OF HONG KONG (HKU)"),],aes(x=Year,y=bfs),color="red")+
  labs(y = "FS Effect")+ggtitle("Plot of FS effect")
ggplot()+geom_boxplot(data=timescad1,aes(x=Year,y=bcpf,group=Year))+
  geom_point(data=timescad1[which(timescad1$INSTITUTION=="UNIVERSITY OF HONG KONG (HKU)"),],aes(x=Year,y=bcpf),color="red")+
  labs(y = "CPF Effect")+ggtitle("Plot of CPF effect")
ggplot()+geom_boxplot(data=timescad1,aes(x=Year,y=bif,group=Year))+
  geom_point(data=timescad1[which(timescad1$INSTITUTION=="UNIVERSITY OF HONG KONG (HKU)"),],aes(x=Year,y=bif),color="red")+
  labs(y = "IF Effect")+ggtitle("Plot of IF effect")
ggplot()+geom_boxplot(data=timescad1,aes(x=Year,y=bis,group=Year))+
  geom_point(data=timescad1[which(timescad1$INSTITUTION=="UNIVERSITY OF HONG KONG (HKU)"),],aes(x=Year,y=bis),color="red")+
  labs(y = "IS Effect")+ggtitle("Plot of IS effect")

ggplot()+geom_boxplot(data=timescad1,aes(x=Year,y=bns,group=Year))+
  geom_point(data=timescad1[which(timescad1$INSTITUTION=="UNIVERSITY OF HONG KONG (HKU)"),],aes(x=Year,y=bns),color="red")+
  labs(y = "ns Effect")+ggtitle("Plot of ns effect")
ggplot()+geom_boxplot(data=timescad1,aes(x=Year,y=balu,group=Year))+
  geom_point(data=timescad1[which(timescad1$INSTITUTION=="UNIVERSITY OF HONG KONG (HKU)"),],aes(x=Year,y=balu),color="red")+
  labs(y = "alumni Effect")+ggtitle("Plot of alumni effect")
ggplot()+geom_boxplot(data=timescad1,aes(x=Year,y=bpub,group=Year))+
  geom_point(data=timescad1[which(timescad1$INSTITUTION=="UNIVERSITY OF HONG KONG (HKU)"),],aes(x=Year,y=bpub),color="red")+
  labs(y = "pub Effect")+ggtitle("Plot of pub effect")
ggplot()+geom_boxplot(data=timescad1,aes(x=Year,y=bhici,group=Year))+
  geom_point(data=timescad1[which(timescad1$INSTITUTION=="UNIVERSITY OF HONG KONG (HKU)"),],aes(x=Year,y=bhici),color="red")+
  labs(y = "hici Effect")+ggtitle("Plot of hici effect")
ggplot()+geom_boxplot(data=timescad1,aes(x=Year,y=baward,group=Year))+
  geom_point(data=timescad1[which(timescad1$INSTITUTION=="UNIVERSITY OF HONG KONG (HKU)"),],aes(x=Year,y=baward),color="red")+
  labs(y = "award Effect")+ggtitle("Plot of award effect")
ggplot()+geom_boxplot(data=timescad1,aes(x=Year,y=bpcp,group=Year))+
  geom_point(data=timescad1[which(timescad1$INSTITUTION=="UNIVERSITY OF HONG KONG (HKU)"),],aes(x=Year,y=bpcp),color="red")+
  labs(y = "pcp Effect")+ggtitle("Plot of pcp effect")

#(timescad1$bteach,timescad1$btrch,timescad1$btcit,timescad1$btinin,timescad1$btio,
ggplot()+geom_boxplot(data=timescad1,aes(x=Year,y=bteach,group=Year))+
  geom_point(data=timescad1[which(timescad1$INSTITUTION=="UNIVERSITY OF HONG KONG (HKU)"),],aes(x=Year,y=bteach),color="red")+
  labs(y = "teaching Effect")+ggtitle("Plot of teaching effect")
ggplot()+geom_boxplot(data=timescad1,aes(x=Year,y=btrch,group=Year))+
  geom_point(data=timescad1[which(timescad1$INSTITUTION=="UNIVERSITY OF HONG KONG (HKU)"),],aes(x=Year,y=btrch),color="red")+
  labs(y = "research Effect")+ggtitle("Plot of research effect")
ggplot()+geom_boxplot(data=timescad1,aes(x=Year,y=btcit,group=Year))+
  geom_point(data=timescad1[which(timescad1$INSTITUTION=="UNIVERSITY OF HONG KONG (HKU)"),],aes(x=Year,y=btcit),color="red")+
  labs(y = "citations Effect")+ggtitle("Plot of citations effect")
ggplot()+geom_boxplot(data=timescad1,aes(x=Year,y=btinin,group=Year))+
  geom_point(data=timescad1[which(timescad1$INSTITUTION=="UNIVERSITY OF HONG KONG (HKU)"),],aes(x=Year,y=btinin),color="red")+
  labs(y = "industry_income Effect")+ggtitle("Plot of industry_income effect")
ggplot()+geom_boxplot(data=timescad1,aes(x=Year,y=btio,group=Year))+
  geom_point(data=timescad1[which(timescad1$INSTITUTION=="UNIVERSITY OF HONG KONG (HKU)"),],aes(x=Year,y=btio),color="red")+
  labs(y = "international_outlook Effect")+ggtitle("Plot of international_outlook effect")


### Restart by using last-year subscore to predict
qs_test<-read.csv("C:/Users/ccsim09/Desktop/QS_testing.csv",header=T)
qs_train<-read.csv("C:/Users/ccsim09/Desktop/QS_training.csv",header=T)
times_test<-read.csv("TIMES_testing.csv",header=T)
times_train<-read.csv("TIMES_training.csv",header=T)
summary(times_test)

qstrainr<-mlogit.data(qs_train,shape="long",choice="RANK",alt.var = 'INSTITUTION',ranked = TRUE)
qstestr<-mlogit.data(qs_test,shape="long",choice="RANK",alt.var = 'INSTITUTION',ranked = TRUE)
qsrol<-mlogit(RANK~LAGRANK+AR_SCORE+ER_SCORE+FS_SCORE+CPF_SCORE+IF_SCORE+IS_SCORE|gdpgrowth_lag,data=qstrainr,reflevel = "THE UNIVERSITY OF SYDNEY")
summary(qsrol)

qslagr<-mlogit(RANK~LAGRANK,data=qstrainr,reflevel = "THE UNIVERSITY OF SYDNEY")
summary(qslagr)
predlag<-predict(qslagr,newdata=qstestr)
write.csv(predlag,"/Users/simon/Desktop/Predictionlag.csv")
predictionlag<-read.csv("/Users/simon/Desktop/Predictionlag.csv",header=T)
cor(predictionlag$Rank,predictionlag$Real)
concordant.pairs(predictionlag$Rank,predictionlag$Real)
cor(cbind(predictionlag$Rank,predictionlag$Real),method="kendall",use="pairwise")

qs1<-mlogit(RANK~LAGRANK+AR_SCORE+ER_SCORE+FS_SCORE+CPF_SCORE+IS_SCORE+Times_teaching+Times_research+
              Times_citations+Times_Industryincome+
              Times_international_outlook+alumni+award+ns+pcp+pub+hici|gdpgrowth_lag,data=qstrainr,reflevel = "THE CHINESE UNIVERSITY OF HONG KONG (CUHK)")
summary(qs1)
pred1<-predict(qs1,newdata=qstestr)
write.csv(pred1,"/Users/simon/Desktop/Predict1.csv")
predict1<-read.csv("/Users/simon/Desktop/Predict1.csv",header=T)
cor(predict1$Rank,predict1$Real)
concordant.pairs(predict1$Rank,predict1$Real)
cor(cbind(predict1$Rank,predict1$Real),method="kendall",use="pairwise")


qs2<-mlogit(RANK~LAGRANK+AR_SCORE+ER_SCORE+FS_SCORE+CPF_SCORE+IF_SCORE+IS_SCORE+
              alumni+pub+Unemployment,data=qstrainr,reflevel = "THE CHINESE UNIVERSITY OF HONG KONG (CUHK)")
summary(qs2)
pred2<-predict(qs2,newdata=qstestr)
write.csv(pred2,"/Users/simon/Desktop/Predict2.csv")
predict2<-read.csv("/Users/simon/Desktop/Predict2.csv",header=T)
cor(predict2$Rank,predict2$Real)
concordant.pairs(predict2$Rank,predict2$Real)
cor(cbind(predict2$Rank,predict2$Real),method="kendall",use="pairwise")


qs3<-mlogit(RANK~LAGRANK+AR_SCORE+ER_SCORE+FS_SCORE+CPF_SCORE+
              RegionGDP_growth|gdpgrowth_lag,data=qstrainr,reflevel = "THE CHINESE UNIVERSITY OF HONG KONG (CUHK)")
summary(qs3)
pred3<-predict(qs3,newdata=qstestr)
write.csv(pred3,"/Users/simon/Desktop/Predict3.csv")
predict3<-read.csv("/Users/simon/Desktop/Predict3.csv",header=T)
cor(predict3$Rank,predict3$Real)
concordant.pairs(predict3$Rank,predict3$Real)
cor(cbind(predict3$Rank,predict3$Real),method="kendall",use="pairwise")

qs4<-mlogit(RANK~AR_SCORE+ER_SCORE+FS_SCORE+CPF_SCORE+
              ns+pub+RegionGDP_growth,data=qstrainr,reflevel = "THE CHINESE UNIVERSITY OF HONG KONG (CUHK)")
summary(qs4)
pred4<-predict(qs4,newdata=qstestr)
write.csv(pred4,"/Users/simon/Desktop/Predict4.csv")
predict4<-read.csv("/Users/simon/Desktop/Predict4.csv",header=T)
cor(predict4$Rank,predict4$Real)
concordant.pairs(predict4$Rank,predict4$Real)
cor(cbind(predict4$Rank,predict4$Real),method="kendall",use="pairwise")

qs5<-mlogit(RANK~LAGRANK+AR_SCORE+ER_SCORE+FS_SCORE+CPF_SCORE+IF_SCORE+pub+
              RegionGDP_growth|gdpgrowth_lag,data=qstrainr,reflevel = "THE CHINESE UNIVERSITY OF HONG KONG (CUHK)")
summary(qs5)
pred5<-predict(qs5,newdata=qstestr)
write.csv(pred5,"/Users/simon/Desktop/Predict5.csv")
predict5<-read.csv("/Users/simon/Desktop/Predict5.csv",header=T)
cor(predict5$Rank,predict5$Real)
concordant.pairs(predict5$Rank,predict5$Real)
cor(cbind(predict5$Rank,predict5$Real),method="kendall",use="pairwise")

qs6<-mlogit(RANK~LAGRANK+AR_SCORE+ER_SCORE+FS_SCORE+CPF_SCORE+IF_SCORE+IS_SCORE+
              alumni+pub+Unemployment,data=qstrainr,reflevel = "THE CHINESE UNIVERSITY OF HONG KONG (CUHK)")
summary(qs6)
pred6<-predict(qs6,newdata=qstestr)
write.csv(pred6,"/Users/simon/Desktop/Predict6.csv")
predict6<-read.csv("/Users/simon/Desktop/Predict6.csv",header=T)
cor(predict6$Rank,predict6$Real)
concordant.pairs(predict6$Rank,predict6$Real)
cor(cbind(predict6$Rank,predict6$Real),method="kendall",use="pairwise")

qs_train$arsub=qs_train$AR_SCORE-mean(qs_train$AR_SCORE)
qs_train$ersub=qs_train$ER_SCORE-mean(qs_train$ER_SCORE)
qs_train$fssub=qs_train$FS_SCORE-mean(qs_train$FS_SCORE)
qs_train$cpfsub=qs_train$CPF_SCORE-mean(qs_train$CPF_SCORE)
qs_train$ifsub=qs_train$IF_SCORE-mean(qs_train$IF_SCORE)
qs_train$issub=qs_train$IS_SCORE-mean(qs_train$IS_SCORE)
qs_train$alusub=qs_train$alumni-mean(qs_train$alumni)
qs_train$pubsub=qs_train$pub-mean(qs_train$pub)
write.csv(qs_train,"C:/Users/ccsim09/Desktop/qssub.csv")
qssub<-read.csv("/Users/simon/Desktop/qssub.csv",header=T)
ggplot()+geom_boxplot(data=qssub,aes(x=Year,y=bar,group=Year))+
  geom_point(data=qssub[which(qssub$INSTITUTION=="UNIVERSITY OF HONG KONG (HKU)"),],aes(x=Year,y=bar),color="red",size=2)+
  geom_point(data=qssub[which(qssub$INSTITUTION=="NATIONAL UNIVERSITY OF SINGAPORE (NUS)"),],aes(x=Year,y=bar),color="blue",size=2)+
  labs(y = "AR Effect")+ggtitle("Plot of AR effect")
ggplot()+geom_boxplot(data=qssub,aes(x=Year,y=ber,group=Year))+
  geom_point(data=qssub[which(qssub$INSTITUTION=="UNIVERSITY OF HONG KONG (HKU)"),],aes(x=Year,y=ber),color="red",size=2)+
  geom_point(data=qssub[which(qssub$INSTITUTION=="NATIONAL UNIVERSITY OF SINGAPORE (NUS)"),],aes(x=Year,y=ber),color="blue",size=2)+
  labs(y = "ER Effect")+ggtitle("Plot of ER effect")
ggplot()+geom_boxplot(data=qssub,aes(x=Year,y=bfs,group=Year))+
  geom_point(data=qssub[which(qssub$INSTITUTION=="UNIVERSITY OF HONG KONG (HKU)"),],aes(x=Year,y=bfs),color="red",size=2)+
  geom_point(data=qssub[which(qssub$INSTITUTION=="NATIONAL UNIVERSITY OF SINGAPORE (NUS)"),],aes(x=Year,y=bfs),color="blue",size=2)+
  labs(y = "FS Effect")+ggtitle("Plot of FS effect")
ggplot()+geom_boxplot(data=qssub,aes(x=Year,y=bcpf,group=Year))+
  geom_point(data=qssub[which(qssub$INSTITUTION=="UNIVERSITY OF HONG KONG (HKU)"),],aes(x=Year,y=bcpf),color="red",size=2)+
  geom_point(data=qssub[which(qssub$INSTITUTION=="NATIONAL UNIVERSITY OF SINGAPORE (NUS)"),],aes(x=Year,y=bcpf),color="blue",size=2)+
  labs(y = "CPF Effect")+ggtitle("Plot of CPF effect")
ggplot()+geom_boxplot(data=qssub,aes(x=Year,y=bif,group=Year))+
  geom_point(data=qssub[which(qssub$INSTITUTION=="UNIVERSITY OF HONG KONG (HKU)"),],aes(x=Year,y=bif),color="red",size=3)+
  geom_point(data=qssub[which(qssub$INSTITUTION=="NATIONAL UNIVERSITY OF SINGAPORE (NUS)"),],aes(x=Year,y=bif),color="blue",size=2)+
  labs(y = "IF Effect")+ggtitle("Plot of IF effect")
ggplot()+geom_boxplot(data=qssub,aes(x=Year,y=bis,group=Year))+
  geom_point(data=qssub[which(qssub$INSTITUTION=="UNIVERSITY OF HONG KONG (HKU)"),],aes(x=Year,y=bis),color="red",size=2)+
  geom_point(data=qssub[which(qssub$INSTITUTION=="NATIONAL UNIVERSITY OF SINGAPORE (NUS)"),],aes(x=Year,y=bis),color="blue",size=2)+
  labs(y = "IS Effect")+ggtitle("Plot of IS effect")
ggplot()+geom_boxplot(data=qssub,aes(x=Year,y=balu,group=Year))+
  geom_point(data=qssub[which(qssub$INSTITUTION=="UNIVERSITY OF HONG KONG (HKU)"),],aes(x=Year,y=balu),color="red",size=3)+
  geom_point(data=qssub[which(qssub$INSTITUTION=="NATIONAL UNIVERSITY OF SINGAPORE (NUS)"),],aes(x=Year,y=balu),color="blue",size=2)+
  labs(y = "alumni Effect")+ggtitle("Plot of alumni effect")
ggplot()+geom_boxplot(data=qssub,aes(x=Year,y=bpub,group=Year))+
  geom_point(data=qssub[which(qssub$INSTITUTION=="UNIVERSITY OF HONG KONG (HKU)"),],aes(x=Year,y=bpub),color="red",size=2)+
  geom_point(data=qssub[which(qssub$INSTITUTION=="NATIONAL UNIVERSITY OF SINGAPORE (NUS)"),],aes(x=Year,y=bpub),color="blue",size=2)+
  labs(y = "pub Effect")+ggtitle("Plot of pub effect")


asia_test<-read.csv("/Users/Simon/Desktop/Asia_testing.csv",header=T)
asia_train<-read.csv("/Users/Simon/Desktop/Asia_training.csv",header=T)
summary(asia_train)
asiatrainr<-mlogit.data(asia_train,shape="long",choice="RANK",alt.var = 'INSTITUTION',ranked = TRUE)
asiatestr<-mlogit.data(asia_test,shape="long",choice="RANK",alt.var = 'INSTITUTION',ranked = TRUE)
asiarol<-mlogit(RANK~LAGRANK+AR_SCORE+ER_SCORE+FS_SCORE+CPF_SCORE+IF_SCORE+IS_SCORE|gdpgrowth_lag2,data=asiatrainr,reflevel = "THE CHINESE UNIVERSITY OF HONG KONG (CUHK)")
summary(asiarol)

asia_train$arsub=asia_train$AR_SCORE-mean(asia_train$AR_SCORE)
asia_train$ersub=asia_train$ER_SCORE-mean(asia_train$ER_SCORE)
asia_train$fssub=asia_train$FS_SCORE-mean(asia_train$FS_SCORE)
asia_train$cpfsub=asia_train$CPF_SCORE-mean(asia_train$CPF_SCORE)
asia_train$ifsub=asia_train$IF_SCORE-mean(asia_train$IF_SCORE)
asia_train$issub=asia_train$IS_SCORE-mean(asia_train$IS_SCORE)
asia_train$nssub=asia_train$ns-mean(asia_train$ns)
asia_train$alusub=asia_train$alumni-mean(asia_train$alumni)
asia_train$pubsub=asia_train$pub-mean(asia_train$pub)
asia_train$hicisub=asia_train$hici-mean(asia_train$hici)
asia_train$awasub=asia_train$award-mean(asia_train$award)
asia_train$pcpsub=asia_train$pcp-mean(asia_train$pcp)
asia_train$tteachsub=asia_train$Times_teaching-mean(asia_train$Times_teaching)
asia_train$trshsub=asia_train$Times_research-mean(asia_train$Times_research)
asia_train$tcitsub=asia_train$Times_citations-mean(asia_train$Times_citations)
asia_train$tininsub=asia_train$Times_Industryincome-mean(asia_train$Times_Industryincome)
asia_train$tiosub=asia_train$Times_international_outlook-mean(asia_train$Times_international_outlook)

asiatrainr<-mlogit.data(asia_train,shape="long",choice="RANK",alt.var = 'INSTITUTION',ranked = TRUE)

asiawhole<-mlogit(RANK~LAGRANK+arsub+ersub+fssub+cpfsub+ifsub+issub+tteachsub+trshsub+tcitsub+tininsub+tiosub+alusub+awasub+nssub+pcpsub+pubsub+hicisub|gdpgrowth_lag,data=asiatrainr,reflevel = "THE CHINESE UNIVERSITY OF HONG KONG (CUHK)")
summary(asiawhole)
asiawhole$coefficients
write.csv(asia_train,"/Users/Simon/Desktop/asia_sub.csv")
asia_sub<-read.csv("/Users/Simon/Desktop/asia_sub.csv",header=T)
ggplot()+geom_boxplot(data=asia_sub,aes(x=Year,y=bar,group=Year))+
  geom_point(data=asia_sub[which(asia_sub$INSTITUTION=="UNIVERSITY OF HONG KONG (HKU)"),],aes(x=Year,y=bar),color="red")+
  geom_point(data=asia_sub[which(asia_sub$INSTITUTION=="NANYANG TECHNOLOGICAL UNIVERSITY (NTU)"),],aes(x=Year,y=bteach),color="blue")+
  geom_point(data=asia_sub[which(asia_sub$INSTITUTION=="NATIONAL UNIVERSITY OF SINGAPORE (NUS)"),],aes(x=Year,y=bteach),color="yellow")+
  labs(y = "AR Effect")+ggtitle("Plot of AR effect")
ggplot()+geom_boxplot(data=asia_sub,aes(x=Year,y=ber,group=Year))+
  geom_point(data=asia_sub[which(asia_sub$INSTITUTION=="UNIVERSITY OF HONG KONG (HKU)"),],aes(x=Year,y=ber),color="red")+
  geom_point(data=asia_sub[which(asia_sub$INSTITUTION=="NANYANG TECHNOLOGICAL UNIVERSITY (NTU)"),],aes(x=Year,y=bteach),color="blue")+
  geom_point(data=asia_sub[which(asia_sub$INSTITUTION=="NATIONAL UNIVERSITY OF SINGAPORE (NUS)"),],aes(x=Year,y=bteach),color="yellow")+
  labs(y = "ER Effect")+ggtitle("Plot of ER effect")
ggplot()+geom_boxplot(data=asia_sub,aes(x=Year,y=bfs,group=Year))+
  geom_point(data=asia_sub[which(asia_sub$INSTITUTION=="UNIVERSITY OF HONG KONG (HKU)"),],aes(x=Year,y=bfs),color="red")+
  geom_point(data=asia_sub[which(asia_sub$INSTITUTION=="NANYANG TECHNOLOGICAL UNIVERSITY (NTU)"),],aes(x=Year,y=bteach),color="blue")+
  geom_point(data=asia_sub[which(asia_sub$INSTITUTION=="NATIONAL UNIVERSITY OF SINGAPORE (NUS)"),],aes(x=Year,y=bteach),color="yellow")+
  labs(y = "FS Effect")+ggtitle("Plot of FS effect")
ggplot()+geom_boxplot(data=asia_sub,aes(x=Year,y=bcpf,group=Year))+
  geom_point(data=asia_sub[which(asia_sub$INSTITUTION=="UNIVERSITY OF HONG KONG (HKU)"),],aes(x=Year,y=bcpf),color="red")+
  geom_point(data=asia_sub[which(asia_sub$INSTITUTION=="NANYANG TECHNOLOGICAL UNIVERSITY (NTU)"),],aes(x=Year,y=bteach),color="blue")+
  geom_point(data=asia_sub[which(asia_sub$INSTITUTION=="NATIONAL UNIVERSITY OF SINGAPORE (NUS)"),],aes(x=Year,y=bteach),color="yellow")+
  labs(y = "CPF Effect")+ggtitle("Plot of CPF effect")
ggplot()+geom_boxplot(data=asia_sub,aes(x=Year,y=bif,group=Year))+
  geom_point(data=asia_sub[which(asia_sub$INSTITUTION=="UNIVERSITY OF HONG KONG (HKU)"),],aes(x=Year,y=bif),color="red")+
  geom_point(data=asia_sub[which(asia_sub$INSTITUTION=="NANYANG TECHNOLOGICAL UNIVERSITY (NTU)"),],aes(x=Year,y=bteach),color="blue")+
  geom_point(data=asia_sub[which(asia_sub$INSTITUTION=="NATIONAL UNIVERSITY OF SINGAPORE (NUS)"),],aes(x=Year,y=bteach),color="yellow")+
  labs(y = "IF Effect")+ggtitle("Plot of IF effect")
ggplot()+geom_boxplot(data=asia_sub,aes(x=Year,y=bis,group=Year))+
  geom_point(data=asia_sub[which(asia_sub$INSTITUTION=="UNIVERSITY OF HONG KONG (HKU)"),],aes(x=Year,y=bis),color="red")+
  geom_point(data=asia_sub[which(asia_sub$INSTITUTION=="NANYANG TECHNOLOGICAL UNIVERSITY (NTU)"),],aes(x=Year,y=bteach),color="blue")+
  geom_point(data=asia_sub[which(asia_sub$INSTITUTION=="NATIONAL UNIVERSITY OF SINGAPORE (NUS)"),],aes(x=Year,y=bteach),color="yellow")+
  labs(y = "IS Effect")+ggtitle("Plot of IS effect")

ggplot()+geom_boxplot(data=asia_sub,aes(x=Year,y=bns,group=Year))+
  geom_point(data=asia_sub[which(asia_sub$INSTITUTION=="UNIVERSITY OF HONG KONG (HKU)"),],aes(x=Year,y=bns),color="red")+
  geom_point(data=asia_sub[which(asia_sub$INSTITUTION=="NANYANG TECHNOLOGICAL UNIVERSITY (NTU)"),],aes(x=Year,y=bteach),color="blue")+
  geom_point(data=asia_sub[which(asia_sub$INSTITUTION=="NATIONAL UNIVERSITY OF SINGAPORE (NUS)"),],aes(x=Year,y=bteach),color="yellow")+
  labs(y = "ns Effect")+ggtitle("Plot of ns effect")
ggplot()+geom_boxplot(data=asia_sub,aes(x=Year,y=balu,group=Year))+
  geom_point(data=asia_sub[which(asia_sub$INSTITUTION=="UNIVERSITY OF HONG KONG (HKU)"),],aes(x=Year,y=balu),color="red")+
  geom_point(data=asia_sub[which(asia_sub$INSTITUTION=="NANYANG TECHNOLOGICAL UNIVERSITY (NTU)"),],aes(x=Year,y=bteach),color="blue")+
  geom_point(data=asia_sub[which(asia_sub$INSTITUTION=="NATIONAL UNIVERSITY OF SINGAPORE (NUS)"),],aes(x=Year,y=bteach),color="yellow")+
  labs(y = "alumni Effect")+ggtitle("Plot of alumni effect")
ggplot()+geom_boxplot(data=asia_sub,aes(x=Year,y=bpub,group=Year))+
  geom_point(data=asia_sub[which(asia_sub$INSTITUTION=="UNIVERSITY OF HONG KONG (HKU)"),],aes(x=Year,y=bpub),color="red")+
  geom_point(data=asia_sub[which(asia_sub$INSTITUTION=="NANYANG TECHNOLOGICAL UNIVERSITY (NTU)"),],aes(x=Year,y=bteach),color="blue")+
  geom_point(data=asia_sub[which(asia_sub$INSTITUTION=="NATIONAL UNIVERSITY OF SINGAPORE (NUS)"),],aes(x=Year,y=bteach),color="yellow")+
  labs(y = "pub Effect")+ggtitle("Plot of pub effect")
ggplot()+geom_boxplot(data=asia_sub,aes(x=Year,y=bhici,group=Year))+
  geom_point(data=asia_sub[which(asia_sub$INSTITUTION=="UNIVERSITY OF HONG KONG (HKU)"),],aes(x=Year,y=bhici),color="red")+
  geom_point(data=asia_sub[which(asia_sub$INSTITUTION=="NANYANG TECHNOLOGICAL UNIVERSITY (NTU)"),],aes(x=Year,y=bteach),color="blue")+
  geom_point(data=asia_sub[which(asia_sub$INSTITUTION=="NATIONAL UNIVERSITY OF SINGAPORE (NUS)"),],aes(x=Year,y=bteach),color="yellow")+
  labs(y = "hici Effect")+ggtitle("Plot of hici effect")
ggplot()+geom_boxplot(data=asia_sub,aes(x=Year,y=baward,group=Year))+
  geom_point(data=asia_sub[which(asia_sub$INSTITUTION=="UNIVERSITY OF HONG KONG (HKU)"),],aes(x=Year,y=baward),color="red")+
  geom_point(data=asia_sub[which(asia_sub$INSTITUTION=="NANYANG TECHNOLOGICAL UNIVERSITY (NTU)"),],aes(x=Year,y=bteach),color="blue")+
  geom_point(data=asia_sub[which(asia_sub$INSTITUTION=="NATIONAL UNIVERSITY OF SINGAPORE (NUS)"),],aes(x=Year,y=bteach),color="yellow")+
  labs(y = "award Effect")+ggtitle("Plot of award effect")
ggplot()+geom_boxplot(data=asia_sub,aes(x=Year,y=bpcp,group=Year))+
  geom_point(data=asia_sub[which(asia_sub$INSTITUTION=="UNIVERSITY OF HONG KONG (HKU)"),],aes(x=Year,y=bpcp),color="red")+
  geom_point(data=asia_sub[which(asia_sub$INSTITUTION=="NANYANG TECHNOLOGICAL UNIVERSITY (NTU)"),],aes(x=Year,y=bteach),color="blue")+
  geom_point(data=asia_sub[which(asia_sub$INSTITUTION=="NATIONAL UNIVERSITY OF SINGAPORE (NUS)"),],aes(x=Year,y=bteach),color="yellow")+
  labs(y = "pcp Effect")+ggtitle("Plot of pcp effect")

#(asia_sub$bteach,asia_sub$btrch,asia_sub$btcit,asia_sub$btinin,asia_sub$btio,
ggplot()+geom_boxplot(data=asia_sub,aes(x=Year,y=bteach,group=Year))+
  geom_point(data=asia_sub[which(asia_sub$INSTITUTION=="UNIVERSITY OF HONG KONG (HKU)"),],aes(x=Year,y=bteach),color="red")+
  geom_point(data=asia_sub[which(asia_sub$INSTITUTION=="NANYANG TECHNOLOGICAL UNIVERSITY (NTU)"),],aes(x=Year,y=bteach),color="blue")+
  geom_point(data=asia_sub[which(asia_sub$INSTITUTION=="NATIONAL UNIVERSITY OF SINGAPORE (NUS)"),],aes(x=Year,y=bteach),color="yellow")+
  labs(y = "teaching Effect")+ggtitle("Plot of teaching effect")
ggplot()+geom_boxplot(data=asia_sub,aes(x=Year,y=btrch,group=Year))+
  geom_point(data=asia_sub[which(asia_sub$INSTITUTION=="UNIVERSITY OF HONG KONG (HKU)"),],aes(x=Year,y=btrch),color="red")+
  geom_point(data=asia_sub[which(asia_sub$INSTITUTION=="NANYANG TECHNOLOGICAL UNIVERSITY (NTU)"),],aes(x=Year,y=bteach),color="blue")+
  geom_point(data=asia_sub[which(asia_sub$INSTITUTION=="NATIONAL UNIVERSITY OF SINGAPORE (NUS)"),],aes(x=Year,y=bteach),color="yellow")+
  labs(y = "research Effect")+ggtitle("Plot of research effect")
ggplot()+geom_boxplot(data=asia_sub,aes(x=Year,y=btcit,group=Year))+
  geom_point(data=asia_sub[which(asia_sub$INSTITUTION=="UNIVERSITY OF HONG KONG (HKU)"),],aes(x=Year,y=btcit),color="red")+
  geom_point(data=asia_sub[which(asia_sub$INSTITUTION=="NANYANG TECHNOLOGICAL UNIVERSITY (NTU)"),],aes(x=Year,y=bteach),color="blue")+
  geom_point(data=asia_sub[which(asia_sub$INSTITUTION=="NATIONAL UNIVERSITY OF SINGAPORE (NUS)"),],aes(x=Year,y=bteach),color="yellow")+
  labs(y = "citations Effect")+ggtitle("Plot of citations effect")
ggplot()+geom_boxplot(data=asia_sub,aes(x=Year,y=btinin,group=Year))+
  geom_point(data=asia_sub[which(asia_sub$INSTITUTION=="UNIVERSITY OF HONG KONG (HKU)"),],aes(x=Year,y=btinin),color="red")+
  geom_point(data=asia_sub[which(asia_sub$INSTITUTION=="NANYANG TECHNOLOGICAL UNIVERSITY (NTU)"),],aes(x=Year,y=bteach),color="blue")+
  geom_point(data=asia_sub[which(asia_sub$INSTITUTION=="NATIONAL UNIVERSITY OF SINGAPORE (NUS)"),],aes(x=Year,y=bteach),color="yellow")+
  labs(y = "industry_income Effect")+ggtitle("Plot of industry_income effect")
ggplot()+geom_boxplot(data=asia_sub,aes(x=Year,y=btio,group=Year))+
  geom_point(data=asia_sub[which(asia_sub$INSTITUTION=="UNIVERSITY OF HONG KONG (HKU)"),],aes(x=Year,y=btio),color="red")+
  geom_point(data=asia_sub[which(asia_sub$INSTITUTION=="NANYANG TECHNOLOGICAL UNIVERSITY (NTU)"),],aes(x=Year,y=bteach),color="blue")+
  geom_point(data=asia_sub[which(asia_sub$INSTITUTION=="NATIONAL UNIVERSITY OF SINGAPORE (NUS)"),],aes(x=Year,y=bteach),color="yellow")+
  labs(y = "international_outlook Effect")+ggtitle("Plot of international_outlook effect")


asia1<-mlogit(RANK~LAGRANK+AR_SCORE+ER_SCORE+FS_SCORE+CPF_SCORE+IS_SCORE+Times_international_outlook+alumni+award+ns+pcp+pub|gdpgrowth_lag,data=asiatrainr,reflevel = "THE CHINESE UNIVERSITY OF HONG KONG (CUHK)")
summary(asia1)

asia2<-mlogit(RANK~LAGRANK,data=asiatrainr,reflevel = "THE CHINESE UNIVERSITY OF HONG KONG (CUHK)")
summary(asia2)

asia3<-mlogit(RANK~LAGRANK+AR_SCORE+ER_SCORE+FS_SCORE+CPF_SCORE+IS_SCORE|gdpgrowth_lag,data=asiatrainr,reflevel = "THE CHINESE UNIVERSITY OF HONG KONG (CUHK)")
summary(asia3)

asia4<-mlogit(RANK~LAGRANK+ER_SCORE+FS_SCORE+CPF_SCORE+IS_SCORE+ns+pcp+pub|gdpgrowth_lag,data=asiatrainr,reflevel = "THE CHINESE UNIVERSITY OF HONG KONG (CUHK)")
summary(asia4)

pred1<-predict(asia1,newdata=asiatestr)
write.csv(pred1,"/Users/simon/Desktop/Prediction1.csv")
prediction<-read.csv("/Users/simon/Desktop/Prediction1.csv",header=T)
cor(prediction$Rank,prediction$Real)
concordant.pairs(prediction$INSTITUTION,newd$INSTITUTION)
concordant.pairs(prediction$Rank,prediction$Real)
discordant.pairs(prediction$INSTITUTION,newd$INSTITUTION)
cor(cbind(prediction$Rank,prediction$Real),method="kendall",use="pairwise")

pred2<-predict(asia3,newdata=asiatestr)
write.csv(pred2,"/Users/simon/Desktop/Prediction2.csv")
prediction2<-read.csv("/Users/simon/Desktop/Prediction2.csv",header=T)
cor(prediction2$Rank,prediction2$Real)
concordant.pairs(prediction2$Rank,prediction2$Real)

pred3<-predict(asia2,newdata=asiatestr)
write.csv(pred3,"/Users/simon/Desktop/Prediction3.csv")
prediction3<-read.csv("/Users/simon/Desktop/Prediction3.csv",header=T)
cor(prediction3$Rank,prediction3$Real)
concordant.pairs(prediction3$Rank,prediction3$Real)
