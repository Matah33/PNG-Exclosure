# Data upload ####

mergePNG<-read.delim("clipboard", as.is=FALSE) #PNGmerge v souboru PNGmerge
summary(mergePNG)
mergePNG$Tree<-as.factor(mergePNG$Tree)

PNG<-read.delim("clipboard",as.is=FALSE) ## LeafHerbivory sheet
summary(PNG)
PNG$Tree<-as.factor(PNG$Tree)
PNG$Code<-as.factor(PNG$Code)


PNG2<-read.delim("clipboard", as.is=FALSE) ## guildy - size+abundance z listu "Guild merge"
summary(PNG2)
PNG2$Ele<-as.factor(PNG2$Ele)
PNG2$Rep<-as.factor(PNG2$Rep)

PNG3<-read.delim("clipboard", as.is=FALSE) #all from sheet TotalLeafArea
summary(PNG3)
PNG3$Rep<-as.factor(PNG3$Rep)

PNG4<-read.delim("clipboard",as.is=FALSE) #Guild_size+abu
summary(PNG4)
PNG4$Tree<-as.factor(PNG4$Tree)
PNG4[is.na(PNG4)] <- 0

PNG5<-read.delim("clipboard",as.is=FALSE) # procento herbivorie vypoctene v excelu na strom
# sheet 3

# Libraries ####
library(dplyr)
library(ggplot2)
library(plyr)
library(lme4)
library (car)
library(emmeans)
library(xlsx)
library(tidyverse)
# Tools ####
PNG[PNG$Percentage > 100,] ### Najde mi ty data kde je herbivorie vic nez 100 procent
PNG[PNG$Tree==is.na,]

#### DATA cleaning ####

distinct(PNG)
PNGdis<-PNG %>% distinct(Plot,Treatment,Tree,Code,Ideal.Area, .keep_all = TRUE)
dPNG<-duplicated(PNG)
unique(PNG)
#### LISTOVA PLOCHA ####
ggplot(mergePNG, aes(x= Treatment, M2))+
  geom_boxplot(width=0.5)+
  theme_classic() + scale_fill_brewer(palette="RdYlGn")+
  xlab('Locality')+ylab(' Tota leaf area per tree individual')

ggplot(mergePNG, aes(x= Plot, M2))+
  geom_boxplot(width=0.5)+
  theme_classic() + scale_fill_brewer(palette="RdYlGn")+
  xlab('Locality')+ylab(' Total leaf area per tree individual')


hist(asin(mergePNG$M2), breaks=15) #nejlepsi je to asi bez transformace

area3<-lmer(log(M2)~Treatment+Plot+Spec+(1|Tree),data=mergePNG) #bez vystrah
drop1 (area3, test="Chisq")

summary(area3)

# nejlepsi je to bez transformace, takze lmer

### HERBIVORIE Vypocet prumernych hodnot na strom ####

PNG$int <- interaction(PNG$Plot, PNG$Treatment,PNG$Tree)

col1 <- ddply(PNG, .(int), summarise,
              mean=mean(Tpercentage),
              n=length(Tpercentage),
              se=sd(Tpercentage/sqrt(n), na.rm=T))
col1[
  order( col1[,1] ),] #zkopirovano a ulozeno do souboru, z nehoz se pak zpetne zkopirovalo pro col1a

col1a<-read.delim("clipboard",as.is=FALSE)

### GRAFIKA HERBIVORIE#####
ggplot(mergePNG, aes(x= Spec, Tpercentage))+
  geom_boxplot(width=0.5)+
  theme_classic() + scale_fill_brewer(palette="RdYlGn")+
  xlab('Locality')+ylab('log( % Herbivory)')

#Nefunguje dobre - ukazuje errorbars pro vsechny stromy
ggplot(col1a, aes(int, mean, fill=X))+
  geom_col(position = position_dodge(0.8))+
   geom_errorbar(aes(ymin = mean-se, ymax = mean+se),
                position = position_dodge(0.8), width=0.3, size=1)+
  theme_classic() + scale_fill_brewer(palette="RdYlGn")+xlab('Treatment')+ylab('Species diversity (H)')

#### VYPOCET HERBIVORIE ####



hist(log(mergePNG$Tpercentage),breaks=15) #ma vicemene normalni rozdeleni

eat2<-lmer(log(Tpercentage)~Plot+Treatment+Spec+(1|Tree),data=mergePNG) #bez vystrah
anova(eat1,eat2) #lmer je lepsi
drop1(eat2,test="Chisq")
summary(eat2)

eat3<-lmer(log(Tpercentage)~Plot+Treatment+Plot:Treatment+(1|Tree),data=mergePNG)
?isSingular #prilis mnoho parametru
drop1(eat3,test="Chisq") #interakce neprukazna

eat4<-lmer(log(Tpercentage)~Plot+Treatment+Spec+Treatment:Spec:Plot+(1|Tree),data=mergePNG)
drop1(eat4,test="Chisq")
# vsechny kombinace interakci neprukazne.

#Tukeyho test
eatmean1<-emmeans(eat2, specs = trt.vs.ctrl ~ Treatment|Spec)
summary(eatmean1)
plot(eatmean1,horizontal=FALSE)
eatmean2<-emmeans(eat2, specs = trt.vs.ctrl ~ Treatment)
summary(eatmean2)
plot(eatmean2,horizontal=FALSE)

emeat1<-emmeans(eat2,specs = pairwise ~ Treatment:Plot,type='response')

plot(emeat1,horizontal=FALSE)
summary(emeat1) #TUKEY

### GRAFIKA GUILDY OVERALL ####


#### boxplot from PNG2 Overall insect
ggplot(PNG2, aes(x= Plot, total_inv, fill=Treatment))+
  geom_boxplot(width=0.5)+
        theme_classic() + scale_fill_brewer(palette="RdYlGn")+xlab('Treatment')+ylab('Invertebrate abundances')

facet_wrap(~Plot)+
stat_boxplot(geom="errorbar", width=0.5
               ) 
  
  
#### column plot from mean+se stat on PNG2 Overall

PNG2$int <- interaction(PNG2$Plot, PNG2$Treatment)

col2 <- ddply(PNG2, .(int), summarise,
              mean=mean(total_inv),
              n=length(total_inv),
              se=sd(total_inv/sqrt(n), na.rm=T))

View(col2)
plot(col2)

col2$Plot <- rep(c('Baiteta', 'Wanang1', 'Wanang3', 'Yawan'), 2 )


col2$treat <- rep(c('CON', 'EXP'), each=4)



ggplot(col2, aes(treat, mean))+
  geom_bar(position = position_dodge(0.8)) +
  facet_wrap(~Plot)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se),
                position = position_dodge(0.8), width=0.3, size=1)+
  theme_classic() + scale_fill_brewer(palette="RdYlGn")+xlab('Treatment')+ylab('Species diversity (H)')

### VYPOCTY GUILDY OVERALL ####

glmer1<- glmer.nb(total_inv~Treatment+Plot+(1|Tree), data=PNG2)
drop1(glmer1, test='Chisq')
summary(glmer1)


glmer2<- update(glmer1, .~.+Ele -Plot,data=PNG2)
glmer2<-glmer.nb(total_inv~Treatment+Ele+(1|Rep), data=PNG2)
Anova(glmer1)
drop1(glmer2, test="Chisq")
summary(glmer2)

anova(glmer1,glmer2,glmer3)
glmer3<-update(glmer2, .~.+Hab,data=PNG2)
glmer3<-glmer.nb(total_inv~Treatment+Hab +(1|Rep), data=PNG2)
drop1(glmer3, test="Chisq")

#rozdily v hmyzacich nejlip popisuje glmer1 
# nebude tedy jednodussi model lepsi?

glmer1a<-glmer.nb(total_inv~Treatment+(1|Rep), data=PNG2)
anova(glmer1,glmer1a)

glmer4<-glmer.nb(total_inv~Treatment+Plot+Hab+(1|Rep), data=PNG2)
drop1(glmer4,test="Chisq")


#jednodussi model neni lepsi

#Tukeyho test
em1<-emmeans(glmer1,specs = pairwise ~ Treatment:Plot,type='response')
plot(em1,horizontal=FALSE)
summary(em1) #TUKEY
em1df<-as.data.frame(em1)


class(em1)
ggplot(em1df1, aes(x= Plot,rate, fill=Treatment))+
  geom_point()+
  geom_errorbar(aes(ymin = rate-SE, ymax = rate+SE),
                width=0.3, size=1)+
  theme_classic() + scale_fill_brewer(palette="RdYlGn")+xlab('Treatment')+ylab('Invertebrate abundances')

ggplot(em1df1, aes(x=Plot, rate,fill=Treatment))+
    geom_col(position = position_dodge(0.8)) +
  geom_errorbar(aes(ymin = rate-SE, ymax = rate+SE), position = position_dodge(0.8),
                width=0.3, size=1)+
  theme_classic() + scale_fill_brewer(palette="Spectral")+xlab('Locality')+ylab('log Invertebrate abundances')
  
  rlang::last_error()
rlang::last_trace()
facet_wrap(~Plot)
  
em1df1<-read.delim("clipboard")
class(em1df1)
summary(em1df1)

#### Correlation Herbivory vs Insect abundance ####



with(mergePNG,cor.test(mean,CHEW, alternative="greater",na.rm=TRUE))

#### Herbivory vs size+number of herbivores

with(mergePNG,cor.test(Tpercentage,MS_Chew, alternative="greater"))


lmer1<-lmer (log(Tpercentage)~log(CHEW+0.001)+log(MS_Chew+0.001)+(1|Tree),data=mergePNG,,na.rm=TRUE)
drop1(lmer1,test='Chisq')


#### GRAFIKA GUILDY ZVLAST ####

### Abundance
ggplot(PNG4, aes(x= Guild, MA, fill=Treatment))+
  geom_boxplot(width=0.5)+
  theme_classic() + scale_fill_brewer(palette="RdYlGn")+xlab('Guild')+ylab('Mean abundances')

### Locality abundance separe
ggplot(PNG4, aes(x= Guild, MA, fill=Treatment))+
  geom_boxplot(width=0.5)+ facet_wrap(~Plot) +
  theme_classic() + scale_fill_brewer(palette="RdYlGn")+xlab('Guild')+ylab('e bundances')

### Size 
ggplot(PNG4, aes(x= Guild, MS, fill=Treatment))+
  geom_boxplot(width=0.5)+
  theme_classic() + scale_fill_brewer(palette="RdYlGn")+xlab('Guild')+ylab('Mean size')

### Locality size separe
ggplot(PNG4, aes(x= Guild, MS, fill=Treatment))+
  geom_boxplot(width=0.5)+ facet_wrap(~Plot) +
  theme_classic() + scale_fill_brewer(palette="RdYlGn")+xlab('Guild')+ylab('Mean size')

#### VYPOCTY GUILDY ZVLAST ####

### Abundance
abu1<-glmer(MA~Guild+Treatment+Plot+(1|Tree), data=PNG4,family=poisson) #overdispersion
abu2<-glmer.nb(MA~Guild+Treatment+Plot+(1|Tree), data=PNG4)
drop1(abu2,test="Chisq")
summary(abu2)

#horsi nez abu2
abu3<-glmer.nb(MA~Guild+Treatment+(1|Plot)+(1|Tree), data=PNG4)
drop1(abu3,test="Chisq")
summary(abu2)

#Neni prukazne lepsi ani horsi
abu5<-glmer.nb(MA~Guild+Treatment+Guild:Treatment+Plot+(1|Tree), data=PNG4)
drop1(abu5,test="Chisq")
summary(abu6)
abu6<-glmer.nb(MA~Guild+Treatment+Plot+Guild:Treatment:Plot+(1|Tree), data=PNG4)
drop1(abu6,test="Chisq")

abumean<-emmeans(abu6,specs = pairwise ~ Guild:Treatment:Plot,type='response')
plot(abumean,horizontal=FALSE)
summary(abumean) # TUKEY
em1df<-as.data.frame(em1)
abumean$contrasts
abumean2<-emmeans(abu6, specs = trt.vs.ctrl ~ Treatment|Guild|Plot)
abumean3<-emmeans(abu6, specs = trt.vs.ctrl ~ Plot|Guild|Treatment)
summary(abumean3)
plot(abumean2,horizontal=FALSE)
abumean2<-emmeans (abu2,  ~ )
summary(abumean2)
plot(abumean2)
anova(abu2, abu5)

abumean2data<-read.delim("clipboard",as.is=FALSE)

### abumean2 grafika
ggplot(abumean2data, aes(x= Guild, emmean, fill=Treatment))+
  geom_boxplot(
    aes(ymin = asymp.LCL-SE, lower = asymp.LCL, middle = emmean, upper = asymp.UCL, ymax = asymp.UCL+SE),
    stat = "identity"
  )+facet_wrap(~Plot)+
  theme_classic() + scale_fill_brewer(palette="Greys")+xlab('Guild')+ylab('Mean abundances')

### Reorder the data for ggplot
levels(abumean2data$Guild) = c()

### Body size

hist(log(PNG4$MS)) # ma normalni rozdeleni

size1<-lmer(log(MS+0.01)~Treatment+Plot+(1|Tree),data=PNG4)
#dvojta interakce neprukazna p=0.1408
drop1(size1,test="Chisq")
summary(size1)

size2<-lmer(log(MS+0.001)~Guild+Treatment+Plot+Guild:Treatment+(1|Tree),data=PNG4)
drop1(size2,test="Chisq")


#Tukeyho test
emsize<-emmeans(size2,specs = pairwise ~ Treatment:Guild,type='response')
plot(emsize,horizontal=FALSE)
summary(emsize) #TUKEY
em1df<-as.data.frame(em1)
#interakce sice prukazna, ale jen diky nesmzslnym kombinacim


### VYPOCTY CHEWING HERBIVORES abu/m2####


drop1(herb1, test='Chisq')
summary(glmer1)

ggplot(PNG3, aes(x= Plot,Abundances.m2 , fill=Treatment), na.rm=TRUE)+
  geom_boxplot(width=0.5)+
  theme_classic() + scale_fill_brewer(palette="RdYlGn")+xlab('Locality')+ylab('Abundances/m2')

herb1<-glmer.nb(Abundances.m2~Plot+Treatment+(1|Rep), data=PNG3)
warnings()
herb2<-glmer.nb(Abundances.m2~Plot+Treatment+Plot:Treatment+(1|Rep), data=PNG3)
drop1(herb1, test='Chisq')
anova(herb1,herb2)

#Tukeyho test
emher1<-emmeans(herb2,specs = pairwise ~ Treatment:Plot,type='response')
plot(emher1,horizontal=FALSE)
summary(emher1) # TUKEY
em1df<-as.data.frame(em1)

### VYPOCTY CHEWING abu ####
abum2<-read.delim("clipboard")
summary(abum2)
hist(abum2$Abundances.m2)




#### DHARMA Overdispersion test ####
library(DHARMa)
plotResiduals (herb1)
plotResiduals (glmer1)
plotResiduals (eat1)
plotResiduals (abu1)
plotResiduals (abu2)
plotResiduals (size1)
plotResiduals (size2)
plotResiduals (area1)
plotResiduals (area2)
plotResiduals (area3)

#### write CSV ####
write.csv(PNGdis,"C:\Users\vince\Downloads\PNGdis.csv")
write.csv(PNGdis,'PNGdis.csv')
