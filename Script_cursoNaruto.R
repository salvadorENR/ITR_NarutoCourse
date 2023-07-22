################################################################################
library(ltm)       # IRT
library(psych)     # VSS.scree
library(subscore)  # Yen.Q3
library(plink)     # plink
library(ggplot2)   # ggplot
library(ggrepel)   # ggplot
library(gridExtra) # ggplot

################################################################################
setwd("C:/Users/kowat/OneDrive/?f?X?N?g?b?v") # C:/Users/kowat/OneDrive/?f?X?N?g?b?v

d20 <- read.csv("d2020.csv",header=TRUE)
d23 <- read.csv("d2023.csv",header=TRUE)

head(d20)
nrow(d20)
ncol(d20)

head(d23)
nrow(d23)
ncol(d23)

################################################################################
########## Step 1 ##########
VSS.scree(d20)
eigen(cor(d20))

VSS.scree(d23)
eigen(cor(d23))

################################################################################
########## Step 2 ##########
d20.yen.Q3.1plm <- Yen.Q3(d20, IRT.model="1pl")
d20.yen.Q3.2plm <- Yen.Q3(d20, IRT.model="2pl")

d20.yen.Q3.1plm
d20.yen.Q3.2plm

d23.yen.Q3.1plm <- Yen.Q3(d23, IRT.model="1pl")
d23.yen.Q3.2plm <- Yen.Q3(d23, IRT.model="2pl")

d23.yen.Q3.1plm
d23.yen.Q3.2plm

################################################################################
########## Step 3 ##########
d20.des    <- descript(d20)
d23.des    <- descript(d23)

### scatter plot of correct answer rate and discrimination ###
# d2020 #
d20.type   <- c("a","a","a","a","a","a","a","a","a","a","a","a","a","a","a","a","a",
                "c","c","c","c","c","c","c","c","c","c","c","c","c")
d20.label  <- c("a1","a2","a3","a4","a5","a6","a7","a8","a9","a10","a11","a12","a13","a14","a15","a16","a17",
                "c1","c2","c3","c4","c5","c6","c7","c8","c9","c10","c11","c12","c13")

d20.plot.d <- data.frame(d20.type, d20.label, cbind(d20.des$perc[,2], d20.des$ExBisCorr))
colnames(d20.plot.d) <- c("type","label","car","dscrmn")

p1 <- 
  ggplot(d20.plot.d, aes(x=car, y=dscrmn, label=label, color=type, shape=type)) + 
  theme_bw(base_size=20)      +
  geom_point(size=4.0)        +
  geom_text_repel(size=6.0)   +
  xlim(c(0.05,0.85))          +
  ylim(c(0.1 ,0.7 ))          +
  ggtitle("Data 2020")        +
  xlab("Correct Answer Rate") +
  ylab("Discrimination")
print(p1)

# d2023 #
d23.type   <- c("c","c","c","c","c","c","c","c","c","c","c","c","c",
                "b","b","b","b","b","b","b","b","b","b","b","b","b","b","b")
d23.label  <- c("c1","c2","c3","c4","c5","c6","c7","c8","c9","c10","c11","c12","c13",
                "b1","b2","b3","b4","b5","b6","b7","b8","b9","b10","b11","b12","b13","b14","b15")

d23.plot.d <- data.frame(d23.type, d23.label, cbind(d23.des$perc[,2], d23.des$ExBisCorr))
colnames(d23.plot.d) <- c("type","label","car","dscrmn")

p2 <- 
  ggplot(d23.plot.d, aes(x=car, y=dscrmn, label=label, color=type, shape=type)) + 
  theme_bw(base_size=20)      +
  geom_point(size=4.0)        +
  geom_text_repel(size=6.0)   +
  xlim(c(0.05,0.85))          +
  ylim(c(0.1 ,0.7 ))          +
  ggtitle("Data 2023")        +
  xlab("Correct Answer Rate") +
  ylab("Discrimination")
print(p2)

# combining scatter plot #
grid.arrange(p1,p2,ncol=2)

################################################################################
########## Step 4 ##########
### d2020 ###
# 1plm #
m <- length(d20)
d20.1plm  <- rasch(d20, IRT.param=TRUE, constraint=cbind(m+1, 1.702))
summary(d20.1plm)
summary(coef(d20.1plm))
plot(d20.1plm, type="ICC", items=c(1:m))
plot(d20.1plm, type="IIC", items=c(0))

# 2plm #
d20.2plm  <- ltm(d20~z1, IRT.param=TRUE)
summary(d20.2plm)
summary(coef(d20.2plm))
plot(d20.2plm, type="ICC", items=c(1:30))
plot(d20.2plm, type="IIC", items=c(0))

### d2023 ###
# 1plm #
m <- length(d23)
d23.1plm  <- rasch(d23, IRT.param=TRUE, constraint=cbind(m+1, 1.702))
summary(d23.1plm)
summary(coef(d23.1plm))
plot(d23.1plm, type="ICC", items=c(1:m))
plot(d23.1plm, type="IIC", items=c(0))

# 2plm #
d23.2plm  <- ltm(d23~z1, IRT.param=TRUE)
summary(d23.2plm)
summary(coef(d23.2plm))
plot(d23.2plm, type="ICC", items=c(1:m))
plot(d23.2plm, type="IIC", items=c(0))

### scatter plot of item difficulty and discrimination ###
# d2020 #
d20.type   <- c("a","a","a","a","a","a","a","a","a","a","a","a","a","a","a","a","a",
                "c","c","c","c","c","c","c","c","c","c","c","c","c")
d20.label  <- c("a1","a2","a3","a4","a5","a6","a7","a8","a9","a10","a11","a12","a13","a14","a15","a16","a17",
                "c1","c2","c3","c4","c5","c6","c7","c8","c9","c10","c11","c12","c13")

d20.plot.d <- data.frame(d20.type, d20.label, coef(d20.2plm))
colnames(d20.plot.d) <- c("type","label","dffclt","dscrmn")

p1 <- 
  ggplot(d20.plot.d, aes(x=dffclt, y=dscrmn, label=label, color=type, shape=type)) + 
  theme_bw(base_size=20)      +
  geom_point(size=4.0)        +
  geom_text_repel(size=6.0)   +
  xlim(c(-2.5,3.0))           +
  ylim(c(0.2 ,2.8 ))          +
  ggtitle("Data 2020")        +
  xlab("Difficulty")          +
  ylab("Discrimination")
print(p1)

# d2023 #
d23.type   <- c("c","c","c","c","c","c","c","c","c","c","c","c","c",
                "b","b","b","b","b","b","b","b","b","b","b","b","b","b","b")
d23.label  <- c("c1","c2","c3","c4","c5","c6","c7","c8","c9","c10","c11","c12","c13",
                "b1","b2","b3","b4","b5","b6","b7","b8","b9","b10","b11","b12","b13","b14","b15")

d23.plot.d <- data.frame(d23.type, d23.label, coef(d23.2plm))
colnames(d23.plot.d) <- c("type","label","dffclt","dscrmn")

p2 <- 
  ggplot(d23.plot.d, aes(x=dffclt, y=dscrmn, label=label, color=type, shape=type)) + 
  theme_bw(base_size=20)      +
  geom_point(size=4.0)        +
  geom_text_repel(size=6.0)   +
  xlim(c(-2.5,3.0))           +
  ylim(c(0.2 ,2.8 ))          +
  ggtitle("Data 2023")        +
  xlab("Difficulty")          +
  ylab("Discrimination")
print(p2)

# combining scatter plot #
grid.arrange(p1,p2,ncol=2)

################################################################################
########## Step 5 ##########
### d2020 ###
# 1plm #
d20.1plm.ts <- factor.scores.rasch(d20.1plm, resp.pattern=d20)
hist(d20.1plm.ts$score.dat$z1)

# 2plm #
d20.2plm.ts <- factor.scores.ltm(d20.2plm, resp.patterns=d20)
hist(d20.2plm.ts$score.dat$z1)

### d2023 ###
# 1plm #
d23.1plm.ts <- factor.scores.rasch(d23.1plm, resp.pattern=d23)
hist(d23.1plm.ts$score.dat$z1)

# 2plm #
d23.2plm.ts <- factor.scores.ltm(d23.2plm, resp.patterns=d23)
hist(d23.2plm.ts$score.dat$z1)

################################################################################
########## Step 6 ##########
### 1plm ###
d20.1plm.dffclt <- coef(d20.1plm)[,1]

d23.1plm.dffclt <- coef(d23.1plm)[,1]

d20.1plm.para <- data.frame(cbind(d20.1plm.dffclt))
d23.1plm.para <- data.frame(cbind(d23.1plm.dffclt))

colnames(d20.1plm.para) <- c("Dffclt")
colnames(d23.1plm.para) <- c("Dffclt")

d20.1plm.theta <- d20.1plm.ts$score.dat$z1
d23.1plm.theta <- d23.1plm.ts$score.dat$z1

d20.cat <- rep(x=2, times=ncol(d20))
d23.cat <- rep(x=2, times=ncol(d23))

d20.pm <- as.poly.mod(n=ncol(d20))
d23.pm <- as.poly.mod(n=ncol(d23))

d20.common <- c(18:30)
d23.common <- c(1 :13)

para.d   <- list(d20.1plm.para , d23.1plm.para)
theta.d  <- list(d20.1plm.theta, d23.1plm.theta)
cat.d    <- list(d20.cat       , d23.cat)
pm.d     <- list(d20.pm        , d23.pm)
common.d <- cbind(d20.common   , d23.common)

pars.d   <- as.irt.pars(x=para.d, common=common.d, cat=cat.d, poly.mod=pm.d)
out.d1    <- plink(x=pars.d, ability=theta.d, rescale="MS", symmetric=TRUE, base.grp=2)
summary(out.d1)

link.pars(out.d1)
link.ability(out.d1)

# organizing data before-after equating #
para.equated <- cbind(d20.1plm.para, link.pars(out.d)[[1]])
colnames(para.equated) <- c("before-diffclt","after-dscrmn","after-diffclt")
head(para.equated)

theta.equated <- cbind(d20.1plm.theta, link.ability(out.d)[[1]])
colnames(theta.equated) <- c("before-equating","after-equating")
head(theta.equated)

# visualzing with histgram #
d.ts <- data.frame(Group=c(rep("d2020",nrow(d20)),rep("d2023",nrow(d23))),value=c(theta.equated[,2],d23.1plm.theta))

ggplot((d.ts),aes(value,fill=Group))                         +
  theme_bw(base_size=23)                                     +
  geom_histogram(binwidth=0.3,position="identity",alpha=0.7) +
  xlab("Equated test score by IRT")                                  +
  ylab(" ")

### 2plm ###
d20.2plm.dscrmn <- coef(d20.2plm)[,2]
d20.2plm.dffclt <- coef(d20.2plm)[,1]

d23.2plm.dscrmn <- coef(d23.2plm)[,2]
d23.2plm.dffclt <- coef(d23.2plm)[,1]

d20.2plm.para <- data.frame(cbind(d20.2plm.dscrmn, d20.2plm.dffclt))
d23.2plm.para <- data.frame(cbind(d23.2plm.dscrmn, d23.2plm.dffclt))

colnames(d20.2plm.para) <- c("Dscrmn","Dffclt")
colnames(d23.2plm.para) <- c("Dscrmn","Dffclt")

d20.2plm.theta <- d20.2plm.ts$score.dat$z1
d23.2plm.theta <- d23.2plm.ts$score.dat$z1

d20.cat <- rep(x=2, times=ncol(d20))
d23.cat <- rep(x=2, times=ncol(d23))

d20.pm <- as.poly.mod(n=ncol(d20))
d23.pm <- as.poly.mod(n=ncol(d23))

d20.common <- c(18:30)
d23.common <- c(1 :13)

para.d   <- list(d20.2plm.para , d23.2plm.para)
theta.d  <- list(d20.2plm.theta, d23.2plm.theta)
cat.d    <- list(d20.cat       , d23.cat)
pm.d     <- list(d20.pm        , d23.pm)
common.d <- cbind(d20.common   , d23.common)

pars.d   <- as.irt.pars(x=para.d, common=common.d, cat=cat.d, poly.mod=pm.d)
out.d2    <- plink(x=pars.d, ability=theta.d, rescale="MS", symmetric=TRUE, base.grp=2)
summary(out.d2)

link.pars(out.d2)
link.ability(out.d2)

# organizing data before-after equating #
para.equated <- cbind(d20.2plm.para, link.pars(out.d)[[1]])
colnames(para.equated) <- c("before-dscrmn","before-diffclt","after-dscrmn","after-diffclt")
head(para.equated)

theta.equated <- cbind(d20.2plm.theta, link.ability(out.d)[[1]])
colnames(theta.equated) <- c("before-equating","after-equating")
head(theta.equated)

# visualzing with histgram #
d.ts <- data.frame(Group=c(rep("d2020",nrow(d20)),rep("d2023",nrow(d23))),value=c(theta.equated[,2],d23.2plm.theta))

ggplot((d.ts),aes(value,fill=Group))                         +
  theme_bw(base_size=23)                                     +
  geom_histogram(binwidth=0.15,position="identity",alpha=0.7) +
  xlab("Equated test score by IRT")                                  +
  ylab(" ")

################################################################################




