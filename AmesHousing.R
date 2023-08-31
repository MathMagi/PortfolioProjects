#Ames Housing Data
#
#

library(ggplot2)
library(plyr)
library(dplyr)
library(corrplot)
library(caret)
library(gridExtra)
library(Rmisc)
library(ggrepel)
library(e1071)
#library()
#library()
#
#
#
#
#
#
comma <- scales :: comma
dat <- read.csv("training.csv")
class(dat)
#View(dat)
head(dat)

# Missing data
# Identify variables with missing values
missing_vars <- dat %>%
  select_if(~any(is.na(.))) %>%
  colnames()

cat("Variables with missing values:\n")
cat(paste(missing_vars, collapse = ", "))

#Sale Price summary + Graph, showing skeweness 
summary(dat$SalePrice)
ggplot(data=dat[!is.na(dat$SalePrice),], aes(x=SalePrice)) +
  geom_histogram(fill="blue", binwidth = 10000) +
  scale_x_continuous(breaks= seq(0, 800000, by=100000), labels = scales :: comma)
skewness(dat$SalePrice)
qqnorm(dat$SalePrice)
qqline(dat$SalePrice)

LogSP <- log(dat$SalePrice)
skewness(LogSP)
qqnorm(LogSP)
qqline(LogSP)

# Checking for numeric variables
numericVars <- which(sapply(dat, is.numeric)) #index vector numeric variables
numericVarNames <- names(numericVars) #saving names vector for use later on
cat('There are', length(numericVars), 'numeric variables')


# Correlation Matrix setup
dat_numVars <- dat[, numericVars]
cor_numVars <- cor(dat_numVars, use="pairwise.complete.obs") #correlations of all numeric variables

#sort on decreasing correlations with SalePrice
cor_sorted <- as.matrix(sort(cor_numVars[,'SalePrice'], decreasing = TRUE))
#select only high corelations
CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.5)))
cor_numVars <- cor_numVars[CorHigh, CorHigh]

corrplot.mixed(cor_numVars, tl.col="black", tl.pos = "lt")



#Correlation matrix variables with high correlation

#Overall Quality has the highest correlation with SalePrice among the numeric variables (0.79). 
# It rates the overall material and finish of the house on a scale from 1 (very poor) to 10 (very excellent).
#The positive correlation is certainly there indeed, and seems to be a slightly upward curve. 
# Regarding outliers, there do not see any extreme values. 
# If there is any to take out as an outlier later on, it seems to be the expensive house with grade 4.
ggplot(data=dat[!is.na(dat$SalePrice),], aes(x=factor(OverallQual), y=SalePrice))+
  geom_boxplot(col='blue') + labs(x='Overall Quality') +
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = scales:: comma)


#The numeric variable with the second highest correlation with SalesPrice is the Above Grade Living Area. 
# This make a lot of sense; big houses are generally more expensive.

ggplot(data=dat[!is.na(dat$SalePrice),], aes(x=GrLivArea, y=SalePrice))+
  geom_point(col='blue') + geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1)) +
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma) +
  geom_text_repel(aes(label = ifelse(dat$GrLivArea[!is.na(dat$SalePrice)]>4500, rownames(dat), '')))

dat[c(524, 1299), c('SalePrice', 'GrLivArea', 'OverallQual')]
#Especially the two houses with really big living areas and low SalePrices seem outliers (houses 524 and 1299, see labels in graph). 
# I will not take them out yet, as taking outliers can be dangerous. For instance, a low score on the Overall Quality could explain a low price. 
# However, as you can see below, these two houses actually also score maximum points on Overall Quality. 
#Therefore, I will keep houses 1299 and 524 in mind as prime candidates to take out as outliers.



# Missing data
# Identify variables with missing values
missing_vars <- dat %>%
  select_if(~any(is.na(.))) %>%
  colnames()

cat("Variables with missing values:\n")
cat(paste(missing_vars, collapse = ", "))

#########################################################################

plot(dat$SalePrice)
plot(log(dat$SalePrice))


######################################################### RECODE MISSING DATA

# Pool Quality
dat$PoolQC[is.na(dat$PoolQC)] <- 'None'
dat$PoolQC<-as.integer(revalue(dat$PoolQC, c('None' = 0, 'Fa' = 1, 'TA' = 2, 'Gd' = 3, 'Ex' = 4)))
table(dat$PoolQC)

# Miscelaneous Features
dat$MiscFeature[is.na(dat$MiscFeature)] <- 'None'
dat$MiscFeature <- as.factor(dat$MiscFeature)
ggplot(dat[!is.na(dat$SalePrice),], aes(x=MiscFeature, y=SalePrice)) +
  stat_summary(geom="bar", fun = "median", fill='red') +
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..))
table(dat$MiscFeature)

# Alley 
dat$Alley[is.na(dat$Alley)] <- 'None'
dat$Alley <- as.factor(dat$Alley)
ggplot(dat[!is.na(dat$SalePrice),], aes(x=Alley, y=SalePrice)) +
  stat_summary( fun.y = "median", geom = "bar", fill='blue')+
  scale_y_continuous(breaks= seq(0, 200000, by=50000), labels = comma)
table(dat$Alley)

#Fireplaces 
dat$FireplaceQu[is.na(dat$FireplaceQu)] <- 'None'
dat$FireplaceQu<-as.integer(revalue(dat$FireplaceQu, c('None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)))
table(dat$FireplaceQu)

########################## Garage
length(which(is.na(dat$GarageType) & is.na(dat$GarageFinish) & is.na(dat$GarageCond) & is.na(dat$GarageQual)))
# Fixes the year built
dat$GarageYrBlt[is.na(dat$GarageYrBlt)] <- dat$YearBuilt[is.na(dat$GarageYrBlt)]
# Garage types, Finish, quality, condition
dat$GarageType[is.na(dat$GarageType)] <- 'No Garage'
dat$GarageType <- as.factor(dat$GarageType)
table(dat$GarageType)
dat$GarageFinish[is.na(dat$GarageFinish)] <- 'None'
Finish <- c('None'=0, 'Unf'=1, 'RFn'=2, 'Fin'=3)
dat$GarageFinish<-as.integer(revalue(dat$GarageFinish, Finish))
table(dat$GarageFinish)
dat$GarageQual[is.na(dat$GarageQual)] <- 'None'
dat$GarageQual<-as.integer(revalue(dat$GarageQual, c('None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)))
table(dat$GarageQual)
dat$GarageCond[is.na(dat$GarageCond)] <- 'None'
dat$GarageCond<-as.integer(revalue(dat$GarageCond, c('None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)))
table(dat$GarageCond)


###################Basement Variables
length(which(is.na(dat$BsmtQual) & is.na(dat$BsmtCond) & is.na(dat$BsmtExposure) & is.na(dat$BsmtFinType1) & is.na(dat$BsmtFinType2)))
dat[!is.na(dat$BsmtFinType1) & (is.na(dat$BsmtCond)|is.na(dat$BsmtQual)|is.na(dat$BsmtExposure)|is.na(dat$BsmtFinType2)), c('BsmtQual', 'BsmtCond', 'BsmtExposure', 'BsmtFinType1', 'BsmtFinType2')]
dat$BsmtFinType2[333] <- names(sort(-table(dat$BsmtFinType2)))[1]
dat$BsmtExposure[949] <- names(sort(-table(dat$BsmtExposure)))[1]
# Quality
dat$BsmtQual[is.na(dat$BsmtQual)] <- 'None'
dat$BsmtQual<-as.integer(revalue(dat$BsmtQual, c('None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)))
table(dat$BsmtQual)
#Cond
dat$BsmtCond[is.na(dat$BsmtCond)] <- 'None'
dat$BsmtCond<-as.integer(revalue(dat$BsmtCond, c('None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)))
table(dat$BsmtCond)
# Exposure
dat$BsmtExposure[is.na(dat$BsmtExposure)] <- 'None'
Exposure <- c('None'=0, 'No'=1, 'Mn'=2, 'Av'=3, 'Gd'=4)
dat$BsmtExposure<-as.integer(revalue(dat$BsmtExposure, Exposure))
table(dat$BsmtExposure)
# Finish Type 1
dat$BsmtFinType1[is.na(dat$BsmtFinType1)] <- 'None'
FinType <- c('None'=0, 'Unf'=1, 'LwQ'=2, 'Rec'=3, 'BLQ'=4, 'ALQ'=5, 'GLQ'=6)
dat$BsmtFinType1<-as.integer(revalue(dat$BsmtFinType1, FinType))
table(dat$BsmtFinType1)
# Finish Type 2
dat$BsmtFinType2[is.na(dat$BsmtFinType2)] <- 'None'
FinType <- c('None'=0, 'Unf'=1, 'LwQ'=2, 'Rec'=3, 'BLQ'=4, 'ALQ'=5, 'GLQ'=6)
dat$BsmtFinType2<-as.integer(revalue(dat$BsmtFinType2, FinType))
table(dat$BsmtFinType2)


###############Masonory
length(which(is.na(dat$MasVnrType) & is.na(dat$MasVnrArea)))
dat$MasVnrType[is.na(dat$MasVnrType)] <- 'None'
Masonry <- c('None'=0, 'BrkCmn'=0, 'BrkFace'=1, 'Stone'=2)
dat$MasVnrType<-as.integer(revalue(dat$MasVnrType, Masonry))
table(dat$MasVnrType)
dat$MasVnrArea[is.na(dat$MasVnrArea)] <-0

#MS Zoning
dat$MSZoning[is.na(dat$MSZoning)] <- names(sort(-table(dat$MSZoning)))[1]
dat$MSZoning <- as.factor(dat$MSZoning)
table(dat$MSZoning)

#Kitchen Quality
dat$KitchenQual[is.na(dat$KitchenQual)] <- 'TA' #replace with most common value
dat$KitchenQual<-as.integer(revalue(dat$KitchenQual, c('None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)))
table(dat$KitchenQual)

#Utility
table(dat$Utilities)
#useless as all have the same except one house so we will disregard this Variable
dat$Utilities <- NULL

# Home Functionality
dat$Functional[is.na(dat$Functional)] <- names(sort(-table(dat$Functional)))[1]
dat$Functional <- as.integer(revalue(dat$Functional, c('Sal'=0, 'Sev'=1, 'Maj2'=2, 'Maj1'=3, 'Mod'=4, 'Min2'=5, 'Min1'=6, 'Typ'=7)))
table(dat$Functional)

#####################Exterior
dat$Exterior1st[is.na(dat$Exterior1st)] <- names(sort(-table(dat$Exterior1st)))[1]
dat$Exterior1st <- as.factor(dat$Exterior1st)
table(dat$Exterior1st)
# Exterior 2
dat$Exterior2nd[is.na(dat$Exterior2nd)] <- names(sort(-table(dat$Exterior2nd)))[1]
dat$Exterior2nd <- as.factor(dat$Exterior2nd)
table(dat$Exterior2nd)
# exterior Quality
dat$ExterQual<-as.integer(revalue(dat$ExterQual, c('None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)))
table(dat$ExterQual)
#Exterior Condition
dat$ExterCond<-as.integer(revalue(dat$ExterCond, c('None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)))
table(dat$ExterCond)


#Electrical
dat$Electrical[is.na(dat$Electrical)] <- names(sort(-table(dat$Electrical)))[1]
dat$Electrical <- as.factor(dat$Electrical)
table(dat$Electrical)

#####Sales
# Type
dat$SaleType[is.na(dat$SaleType)] <- names(sort(-table(dat$SaleType)))[1]
dat$SaleType <- as.factor(dat$SaleType)
table(dat$SaleType)
# Condition
dat$SaleCondition <- as.factor(dat$SaleCondition)
table(dat$SaleCondition)

################Lot Variables
#Lot Shape
dat$LotShape<-as.integer(revalue(dat$LotShape, c('IR3'=0, 'IR2'=1, 'IR1'=2, 'Reg'=3)))
table(dat$LotShape)
#Lot configuration
dat$LotConfig <- as.factor(dat$LotConfig)
table(dat$LotConfig)
# Lot Frontage
table(dat$LotFrontage)
table(is.na(dat$LotFrontage))
# For the missing values it is best we can either disregard the data or we can use the median lot frontage
# From the respective Neighboorhoods to use in its place. Could this alter our outcome greatly?
ggplot(dat[!is.na(dat$LotFrontage),], aes(x=Neighborhood, y=LotFrontage)) +
  stat_summary( fun.y = "median", geom = "bar", fill='blue')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#For loop if we want the median 
for (i in 1:nrow(dat)){
  if(is.na(dat$LotFrontage[i])){
    dat$LotFrontage[i] <- as.integer(median(dat$LotFrontage[dat$Neighborhood==dat$Neighborhood[i]], na.rm=TRUE)) 
  }
}


###################################### Other Char Variables

# Fence
dat$Fence[is.na(dat$Fence)] <- 'None'
table(dat$Fence)
dat$Fence <- as.factor(dat$Fence)

# Roof
dat$RoofMatl <- as.factor(dat$RoofMatl)
table(dat$RoofMatl)
dat$RoofStyle <- as.factor(dat$RoofStyle)
table(dat$RoofStyle)

#Foundation
dat$Foundation <- as.factor(dat$Foundation)
table(dat$Foundation)

#Heating
dat$Heating <- as.factor(dat$Heating)
table(dat$Heating)
dat$HeatingQC<-as.integer(revalue(dat$HeatingQC,  c('None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)))
table(dat$HeatingQC)
dat$CentralAir<-as.integer(revalue(dat$CentralAir, c('N'=0, 'Y'=1)))
table(dat$CentralAir)

#land
dat$LandContour <- as.factor(dat$LandContour)
table(dat$LandContour)
dat$LandSlope<-as.integer(revalue(dat$LandSlope, c('Sev'=0, 'Mod'=1, 'Gtl'=2)))
table(dat$LandSlope)

#Dwelling
dat$BldgType <- as.factor(dat$BldgType)
table(dat$BldgType)
dat$HouseStyle <- as.factor(dat$HouseStyle)
table(dat$HouseStyle)

#Neighborhood and Condition
dat$Neighborhood <- as.factor(dat$Neighborhood)
table(dat$Neighborhood)
dat$Condition1 <- as.factor(dat$Condition1)
table(dat$Condition1)
dat$Condition2 <- as.factor(dat$Condition2)
table(dat$Condition2)

# Pavements Ordinal
dat$Street<-as.integer(revalue(dat$Street, c('Grvl'=0, 'Pave'=1)))
table(dat$Street)
dat$PavedDrive<-as.integer(revalue(dat$PavedDrive, c('N'=0, 'P'=1, 'Y'=2)))
table(dat$PavedDrive)


############################## Numerics into Factors
str(dat$MoSold)
str(dat$YrSold)
dat$MoSold <- as.factor(dat$MoSold)

str(dat$MSSubClass)
dat$MSSubClass <- as.factor(dat$MSSubClass)

#revalue for better readability
dat$MSSubClass<-revalue(dat$MSSubClass, c('20'='1 story 1946+', '30'='1 story 1945-', '40'='1 story unf attic', '45'='1,5 story unf', '50'='1,5 story fin', '60'='2 story 1946+', '70'='2 story 1945-', '75'='2,5 story all ages', '80'='split/multi level', '85'='split foyer', '90'='duplex all style/age', '120'='1 story PUD 1946+', '150'='1,5 story PUD all', '160'='2 story PUD 1946+', '180'='PUD multilevel', '190'='2 family conversion'))

#############################################################################################################

# New visualisation

numericVars <- which(sapply(dat, is.numeric)) #index vector numeric variables
factorVars <- which(sapply(dat, is.factor)) #index vector factor variables
CharVars <- which(sapply(dat, is.character)) #index vector factor variables

cat('There are', length(numericVars), 'numeric variables, and', length(factorVars), 'categoric variables',length(CharVars), 'character variables' )

# Correlation matrix 2

#The number of variables with a correlation of at least 0.5 with the SalePrice has increased from 10 to 16.
dat_numVar <- dat[, numericVars]
cor_numVar <- cor(dat_numVar, use="pairwise.complete.obs") #correlations of all numeric variables

#sort on decreasing correlations with SalePrice
cor_sorted <- as.matrix(sort(cor_numVar[,'SalePrice'], decreasing = TRUE))
#select only high corelations
CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.5)))
cor_numVar <- cor_numVar[CorHigh, CorHigh]

corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt", tl.cex = 0.7,cl.cex = .7, number.cex=.7)


###################


# Ground Living Area and other related variables
cor(dat$GrLivArea, (dat$X1stFlrSF + dat$X2ndFlrSF + dat$LowQualFinSF))
# SF and GRLIVAREA are excatly correlated so do not use these
s1 <- ggplot(data= dat, aes(x=GrLivArea)) +
  geom_density() + labs(x='Square feet living area')
s2 <- ggplot(data=dat, aes(x=as.factor(TotRmsAbvGrd))) +
  geom_histogram(stat='count') + labs(x='Rooms above Ground')
s3 <- ggplot(data= dat, aes(x=TotalBsmtSF)) +
  geom_density() + labs(x='Square feet basement')
s4 <- ggplot(data= dat[dat$LotArea<100000,], aes(x=LotArea)) +
  geom_density() + labs(x='Square feet lot')
s5 <- ggplot(data= dat, aes(x=LotFrontage)) +
  geom_density() + labs(x='Linear feet lot frontage')

layout <- matrix(c(1,2,3,4,5,6),3,2,byrow=TRUE)
multiplot(s1, s2, s3, s4, s5, NULL, layout=layout)


#Neighborhood
# need to make bins
#n1 is by sale price
#n2 is by frequency 
n1 <- ggplot(dat[!is.na(dat$SalePrice),], aes(x=Neighborhood, y=SalePrice)) +
  geom_bar(stat='summary', fun.y = "median", fill='blue') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(breaks= seq(0, 800000, by=50000), labels = comma) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..), size=3) +
  geom_hline(yintercept=163000, linetype="dashed", color = "red") #dashed line is median SalePrice
n2 <- ggplot(data=dat, aes(x=Neighborhood)) +
  geom_histogram(stat='count')+
  geom_label(stat = "count", aes(label = ..count.., y = ..count..), size=3)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
grid.arrange(n1, n2)

# Both the median and mean Saleprices agree on 3 neighborhoods with substantially higher saleprices.
#The separation of the 3 relatively poor neighborhoods is less clear, but at least both graphs agree on the same 3 poor neighborhoods.
#Since I do not want to ‘overbin’, I am only creating categories for those ‘extremes’.

nb1 <- ggplot(dat[!is.na(dat$SalePrice),], aes(x=reorder(Neighborhood, SalePrice, FUN=median), y=SalePrice)) +
  geom_bar(stat='summary', fun.y = "median", fill='blue') + labs(x='Neighborhood', y='Median SalePrice') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(breaks= seq(0, 800000, by=50000), labels = comma) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..), size=3) +
  geom_hline(yintercept=163000, linetype="dashed", color = "red") #dashed line is median SalePrice
nb2 <- ggplot(dat[!is.na(dat$SalePrice),], aes(x=reorder(Neighborhood, SalePrice, FUN=mean), y=SalePrice)) +
  geom_bar(stat='summary', fun.y = "mean", fill='blue') + labs(x='Neighborhood', y="Mean SalePrice") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(breaks= seq(0, 800000, by=50000), labels = comma) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..), size=3) +
  geom_hline(yintercept=163000, linetype="dashed", color = "red") #dashed line is median SalePrice
grid.arrange(nb1, nb2)
#Binning them
dat$NeighRich[dat$Neighborhood %in% c('StoneBr', 'NridgHt', 'NoRidge')] <- 2
dat$NeighRich[!dat$Neighborhood %in% c('MeadowV', 'IDOTRR', 'BrDale', 'StoneBr', 'NridgHt', 'NoRidge')] <- 1
dat$NeighRich[dat$Neighborhood %in% c('MeadowV', 'IDOTRR', 'BrDale')] <- 0
table(dat$NeighRich)




# Qaulities
q1 <- ggplot(data=dat, aes(x=as.factor(OverallQual))) +
  geom_histogram(stat='count')
q2 <- ggplot(data=dat, aes(x=as.factor(ExterQual))) +
  geom_histogram(stat='count')
q3 <- ggplot(data=dat, aes(x=as.factor(BsmtQual))) +
  geom_histogram(stat='count')
q4 <- ggplot(data=dat, aes(x=as.factor(KitchenQual))) +
  geom_histogram(stat='count')
q5 <- ggplot(data=dat, aes(x=as.factor(FireplaceQu))) +
  geom_histogram(stat='count')

# Garage quality and Pool Quality do not seem worth using as they both have a high frequency in only one category
# High correlation with some of the variables as well
layout <- matrix(c(1,2,3,4,5,6),3,2,byrow=TRUE)
multiplot(q1, q2, q3, q4, q5, NULL, layout=layout)
cor(dat$OverallQual, (dat$ExterQual))
cor(dat$OverallQual, (dat$KitchenQual))
cor(dat$OverallQual, (dat$BsmtQual))
cor(dat$OverallQual, (dat$FireplaceQu))


#MS Sub Class
# need to make bins
#ms1 is by sale price
#ms2 is by frequency 
ms1 <- ggplot(dat[!is.na(dat$SalePrice),], aes(x=MSSubClass, y=SalePrice)) +
  geom_bar(stat='summary', fun.y = "median", fill='blue') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(breaks= seq(0, 800000, by=50000), labels = comma) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..), size=3) +
  geom_hline(yintercept=163000, linetype="dashed", color = "red") #dashed line is median SalePrice
ms2 <- ggplot(data=dat, aes(x=MSSubClass)) +
  geom_histogram(stat='count')+
  geom_label(stat = "count", aes(label = ..count.., y = ..count..), size=3) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
grid.arrange(ms1, ms2)

# Garage Variables 
g1 <- ggplot(data=dat[dat$GarageCars !=0,], aes(x=GarageYrBlt)) +
  geom_histogram()
g2 <- ggplot(data=dat, aes(x=as.factor(GarageCars))) +
  geom_histogram(stat='count')
g3 <- ggplot(data= dat, aes(x=GarageArea)) +
  geom_density()
g4 <- ggplot(data=dat, aes(x=as.factor(GarageCond))) +
  geom_histogram(stat='count')
g5 <- ggplot(data=dat, aes(x=GarageType)) +
  geom_histogram(stat='count')
g6 <- ggplot(data=dat, aes(x=as.factor(GarageQual))) +
  geom_histogram(stat='count')
g7 <- ggplot(data=dat, aes(x=as.factor(GarageFinish))) +
  geom_histogram(stat='count')

layout <- matrix(c(1,5,5,2,3,8,6,4,7),3,3,byrow=TRUE)
multiplot(g1, g2, g3, g4, g5, g6, g7, layout=layout)
#GarageCars and GarageArea are highly correlated.
# GarageQual and GarageCond also seem highly correlated, and both are dominated by level =3.
# 3 variables should be sufficient (possibly GarageCars, GarageType, and a Quality measurement)




# Basement Variables
# Similar the garage variables, multiple basement variables are important in the correlations matrix and the Top 20 RF predictors list. 
# However, 11 basement variables seems like overkill. Visualizing 8 of them below. 
b1 <- ggplot(data=dat, aes(x=BsmtFinSF1)) +
  geom_histogram() + labs(x='Type 1 finished square feet')
b2 <- ggplot(data=dat, aes(x=BsmtFinSF2)) +
  geom_histogram()+ labs(x='Type 2 finished square feet')
b3 <- ggplot(data=dat, aes(x=BsmtUnfSF)) +
  geom_histogram()+ labs(x='Unfinished square feet')
b4 <- ggplot(data=dat, aes(x=as.factor(BsmtFinType1))) +
  geom_histogram(stat='count')+ labs(x='Rating of Type 1 finished area')
b5 <- ggplot(data=dat, aes(x=as.factor(BsmtFinType2))) +
  geom_histogram(stat='count')+ labs(x='Rating of Type 2 finished area')
b6 <- ggplot(data=dat, aes(x=as.factor(BsmtQual))) +
  geom_histogram(stat='count')+ labs(x='Height of the basement')
b7 <- ggplot(data=dat, aes(x=as.factor(BsmtCond))) +
  geom_histogram(stat='count')+ labs(x='Rating of general condition')
b8 <- ggplot(data=dat, aes(x=as.factor(BsmtExposure))) +
  geom_histogram(stat='count')+ labs(x='Walkout or garden level walls')

layout <- matrix(c(1,2,3,4,5,9,6,7,8),3,3,byrow=TRUE)
multiplot(b1, b2, b3, b4, b5, b6, b7, b8, layout=layout)

cor(dat$TotalBsmtSF, (dat$BsmtFinSF1 + dat$BsmtUnfSF + dat$BsmtFinSF2))
#So it seemed as if the Total Basement Surface in square feet (TotalBsmtSF) is further broken down into finished areas, and unfinished area. 
# I did a check between the correlation of total of those 3 variables, and TotalBsmtSF. 
# The correlation is exactly 1, so that’s a good thing (no errors or small discrepancies)!


#Bathrooms 
#combining to see if it will be a releavant variable since sepearated may prove ot be insufficeint
dat$TotBathrooms <- dat$FullBath + (dat$HalfBath*0.5) + dat$BsmtFullBath + (dat$BsmtHalfBath*0.5)
tb1 <- ggplot(data=dat[!is.na(dat$SalePrice),], aes(x=as.factor(TotBathrooms), y=SalePrice))+
  geom_point(col='blue') + geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1)) +
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma)
tb2 <- ggplot(data=dat, aes(x=as.factor(TotBathrooms))) +
  geom_histogram(stat='count')
grid.arrange(tb1, tb2)



#As the total living space generally is very important when people buy houses, 
# adding a predictors that adds up the living space above and below ground.

dat$TotalSqFeet <- dat$GrLivArea + dat$TotalBsmtSF
ggplot(data=dat[!is.na(dat$SalePrice),], aes(x=TotalSqFeet, y=SalePrice))+
  geom_point(col='blue') + geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1)) +
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma) +
  geom_text_repel(aes(label = ifelse(dat$GrLivArea[!is.na(dat$SalePrice)]>4500, rownames(dat), '')))

# Hgh correlation as well
cor(dat$SalePrice, dat$TotalSqFeet, use= "pairwise.complete.obs")
# The two potential outliers seem to ‘outlie’ even more than before. 
#By taking out these two outliers, the correlation increases by 5%.
cor(dat$SalePrice[-c(524, 1299)], dat$TotalSqFeet[-c(524, 1299)], use= "pairwise.complete.obs")
 #Porch
dat$TotalPorchSF <- dat$OpenPorchSF + dat$EnclosedPorch + dat$X3SsnPorch + dat$ScreenPorch

# Age and remodeling
dat$Remod <- ifelse(dat$YearBuilt==dat$YearRemodAdd, 0, 1) #0=No Remodeling, 1=Remodeling
dat$Age <- as.numeric(dat$YrSold)-dat$YearRemodAdd
cor(dat$SalePrice[!is.na(dat$SalePrice)], dat$Age[!is.na(dat$SalePrice)])
dat$IsNew <- ifelse(dat$YrSold==dat$YearBuilt, 1, 0)
table(dat$IsNew)


################################################
################################################## Preparing data for modeling
###Dropping highly correlated variables
#First of all, I am dropping a variable if two variables are highly correlated. 
#To find these correlated pairs, I have used the correlations matrix again 
#For instance: GarageCars and GarageArea have a correlation of 0.89. 
# Of those two, I am dropping the variable with the lowest correlation with SalePrice 
#(which is GarageArea with a SalePrice correlation of 0.62. GarageCars has a SalePrice correlation of 0.64).

dropVars <- c('YearRemodAdd', 'GarageYrBlt', 'GarageArea', 'GarageCond', 'TotalBsmtSF', 'TotalRmsAbvGrd', 'BsmtFinSF1')

dat <- dat[,!(names(dat) %in% dropVars)]


#Removing outliers
# removing the two really big houses with low SalePrice  
dat <- dat[-c(524, 1299),]

#PreProcessing predictor variables
#Before modeling it is necessary to center and scale the ‘true numeric’ predictors 
# (so not variables that have been label encoded), 
# and create dummy variables for the categorical predictors. 
# Below, I am splitting the dataframe into one with all (true) numeric variables, 
# and another dataframe holding the (ordinal) factors.

numericVarNames <- numericVarNames[!(numericVarNames %in% c('MSSubClass', 'MoSold', 'YrSold', 'SalePrice', 'OverallQual', 'OverallCond'))] #numericVarNames was created before having done anything
numericVarNames <- append(numericVarNames, c('Age', 'TotalPorchSF', 'TotBathrooms', 'TotalSqFeet'))

DFnumeric <- dat[, names(dat) %in% numericVarNames]

DFfactors <- dat[, !(names(dat) %in% numericVarNames)]
DFfactors <- DFfactors[, names(DFfactors) != 'SalePrice']

cat('There are', length(DFnumeric), 'numeric variables, and', length(DFfactors), 'factor variables')


#######################################################
#Skewness 
#Dealing with skewness of response variable
#The skew of 1.87 indicates a right skew that is too high, 
# and the Q-Q plot shows that sale prices are also not normally distributed. 
# To fix this I am taking the log of SalePrice.

skewness(dat$SalePrice)
## [1] 1.877427
qqnorm(dat$SalePrice)
qqline(dat$SalePrice)

LogSP <- log(dat$SalePrice)
skewness(LogSP)
qqnorm(LogSP)
qqline(LogSP)
##############

set.seed(777)

# Split the data into train and test sets (70/30 ratio)
train_index <- createDataPartition(dat$SalePrice, p = 0.7, list = FALSE)
train <- dat[train_index, ]
test <- dat[-train_index, ]

# Specify the number of folds for cross-validation
k <- 10

# Create a repeated k-fold cross-validation control
cv_control <- trainControl(method = "repeatedcv", number = k, repeats = 3)

# Train and evaluate a model using k-fold cross-validation
Kmodel <- train(SalePrice~ TotalSqFeet + OverallQual  +
                  GarageCars + YearBuilt +  KitchenQual +  FireplaceQu + 
                  GarageFinish + LotArea + OverallCond +
                  BsmtQual + ExterQual + GrLivArea 
                 + MSZoning + BsmtUnfSF + TotalPorchSF  
                , data = train, method = "lm", trControl = cv_control)
# Make predictions on the test set
predictions_test <- predict(Kmodel, newdata = test)
# Calculate predicted values on training data
predictions_train <- predict(Kmodel, newdata = train)

# Calculate residuals
residuals_test <- predictions_test - test$SalePrice
# Calculate residuals on training data
residuals_train <- predictions_train - train$SalePrice

# Evaluate the model on the test set
# Calculate RMSE on training data
train_rmse <- sqrt(mean(residuals_train^2))
test_rmse <- sqrt(mean(residuals_test^2))
# Print the test RMSE
cat("Test RMSE:", test_rmse, "\n")
cat("train RMSE:", train_rmse, "\n")
#Calculate SSE and SST
SSE <- sum(residuals_test^2)
SST <- sum((test$SalePrice - mean(test$SalePrice))^2)

# Calculate R^2
KR2 <- 1 - (SSE / SST)

#Adjusted 
KAR2 <- 1-((1-KR2) * (435-1)/ (435-81-1))


# Print R^2
cat("R^2:", KR2, "\n")
cat("Adjsuted R^2:", KAR2, "\n")
summary(Kmodel)

Lmodel <- train(log(SalePrice) ~ TotalSqFeet + OverallQual  +
                          GarageCars + YearBuilt +  KitchenQual +  FireplaceQu + 
                          GarageFinish + LotArea + OverallCond  +
                          BsmtQual + ExterQual + GrLivArea  
                        + MSZoning + BsmtUnfSF  + TotalPorchSF
                        , data = train, method = "lm", trControl = cv_control)

# Calculate predicted values on test set
predictions_test2 <- predict(Lmodel, newdata = test)
# Calculate predicted values on training data
predictions_train2 <- predict(Lmodel, newdata = train)

# Calculate residuals
residuals_test2 <- predictions_test2 - log(test$SalePrice)
# Calculate residuals on training data
residuals_train2 <- predictions_train2 - log(train$SalePrice)

# Calculate RMSE
test_rmse2 <- sqrt(mean(residuals_test2^2))
# Calculate RMSE on training data
train_rmse2 <- sqrt(mean(residuals_train2^2))

# Print the test RMSE
cat("Test RMSE:", test_rmse2, "\n")
# Print the training RMSE
cat("Training RMSE:", train_rmse2, "\n")

#Calculate SSE and SST
SSE2 <- sum(residuals_test2^2)
SST2 <- sum((log(test$SalePrice) - mean(log(test$SalePrice)))^2)

# Calculate R^2
LR2 <- 1 - (SSE2 / SST2)
LAR2 <- 1-((1-LR2) * (435-1)/ (435-81-1))

# Print R^2
cat("R^2:", LR2, "\n")
cat("Adjusted R^2:", LAR2, "\n")

summary(Lmodel)


Finalmodel <- lm(log(SalePrice) ~ TotalSqFeet + OverallQual  +
                  GarageCars  +  KitchenQual +  FireplaceQu + 
                  GarageFinish + LotArea + OverallCond  +
                  BsmtQual + ExterQual + GrLivArea  
                + MSZoning + BsmtUnfSF  + TotalPorchSF + YearBuilt 
                , data = dat)
summary(Finalmodel)






##Predictions

print(dat[1,])


attach(dat)
# Shows prediction of house 1 
LSP = 5.406 + (1.917e-04) * TotalSqFeet[1] + (6.058e-02) * OverallQual[1] + (5.992e-02) * GarageCars[1] + (2.310e-03) * YearBuilt[1] + (3.584e-02) * KitchenQual[1] + (1.358e-02) * FireplaceQu[1] + (1.567e-02) * GarageFinish[1] + (2.542e-06) * LotArea[1] + (5.243e-02) * OverallCond[1] + (1.888e-02) * BsmtQual[1] + (2.862e-02) * ExterQual[1] + (6.743e-05) * GrLivArea[1] + +(3.901e-01) * 1 - (8.755e-05) * BsmtUnfSF[1] + (1.501e-04) * TotalPorchSF[1]
exp(LSP)

# shows prediction of house but changes the total square feet variable while holding all other constant
LSP = 5.406 + (1.917e-04) * TotalSqFeet[2] + (6.058e-02) * OverallQual[1] + (5.992e-02) * GarageCars[1] + (2.310e-03) * YearBuilt[1] + (3.584e-02) * KitchenQual[1] + (1.358e-02) * FireplaceQu[1] + (1.567e-02) * GarageFinish[1] + (2.542e-06) * LotArea[1] + (5.243e-02) * OverallCond[1] + (1.888e-02) * BsmtQual[1] + (2.862e-02) * ExterQual[1] + (6.743e-05) * GrLivArea[1] + +(3.901e-01) * 1 - (8.755e-05) * BsmtUnfSF[1] + (1.501e-04) * TotalPorchSF[1]
exp(LSP2)
exp(predict(Finalmodel, dat[2,]))

dat$TotalSqFeet[1:2]

x=exp(LSP)-exp(LSP2)
y=TotalSqFeet[1]-TotalSqFeet[2]
x/y
