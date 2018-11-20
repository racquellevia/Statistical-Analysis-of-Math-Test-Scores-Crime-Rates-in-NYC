
-----------------
#load in school crime data set
library(readr)
library(dplyr)
library(ggplot2)
X2010_2016_School_Safety_Report <- read_csv("2010_-_2016_School_Safety_Report.csv")
X2013_2015_New_York_State_Mathematics_Exam_by_School <- read_csv("2013_-_2015_New_York_State_Mathematics_Exam_by_School.csv")
#save variables as usable
safety <- X2010_2016_School_Safety_Report
math <- X2013_2015_New_York_State_Mathematics_Exam_by_School
#***CLEAN SAFETY DATA SET***

#make it so that school year 13-14 correlates with year 2014, etc (because tests are taken at the end of the year)
safety$`School Year`[safety$`School Year` == "2013-14"] <- "2014"
safety$`School Year`[safety$`School Year` == "2014-15"] <- "2015"
#only take years 2014, 2015, because 2016 isn't available in the safety data we're combining
safety <- dplyr::filter(safety, `School Year`=="2014" | `School Year`=="2015")
#get rid of unneeded columns
safety$`Building Code` <- NULL
colnames(safety)[17] <- "GroupNameofPop"
colnames(safety)[18] <- "BuildingPopulation"

safetyMerge <- safety
safetyMerge$`Location Code` <- NULL
safetyMerge$Address <- NULL
safetyMerge$Borough <- NULL
safetyMerge$`Geographical District Code` <- NULL
colnames(safetyMerge)[4] <- "NumberStudents"

#----------------------------------
# use the above "safety" df to find out insights within the safety df alone
# make another saftey df for the purpose of merging with the math df , rbind
#----------------------------------


#***CLEAN MATH DATA SET***
#get rid of all entries but the aggregate student scores per school
mathMerge <- dplyr::filter(math, `Grade` == "All Grades")
mathMerge <- dplyr::filter(mathMerge, Category=="All Students")
mathMerge <- dplyr::filter(mathMerge, Year == "2014" | Year == "2015")
mathMerge$Category <-NULL
mathMerge$Grade <-NULL

consolidated <- dplyr::filter(safetyMerge, is.na(DBN))
safetyMergeNC <- dplyr::filter(safetyMerge, !is.na(DBN))
consolidated$DBN <- NULL

#before joining with consolidated, separate into 2014, 2015 for both dfs
consolidated14 <- dplyr::filter(consolidated, `School Year` == "2014")
consolidated15 <- dplyr::filter(consolidated, `School Year` == "2015")
safetyMergeNC14 <- dplyr::filter(safetyMergeNC, `School Year` == "2014")
safetyMergeNC15 <- dplyr::filter(safetyMergeNC, `School Year` == "2015")




#for 2014
safetyMergeNC14 <- dplyr::left_join(safetyMergeNC14, consolidated14, by="Building Name")
for(n in 1:nrow(safetyMergeNC14))
{
  if(safetyMergeNC14[n,"Major N.x"] == "N/A" | safetyMergeNC14[n,"Major N.x"] == "#N/A")
  {
    safetyMergeNC14[n,"Major N.x"] <- safetyMergeNC14[n,"Major N.y"]
  }
}

for(n in 1:nrow(safetyMergeNC14))
{
  if(safetyMergeNC14[n,"Oth N.x"] == "N/A" | safetyMergeNC14[n,"Oth N.x"] == "#N/A")
  {
    safetyMergeNC14[n,"Oth N.x"] <- safetyMergeNC14[n,"Oth N.y"]
  }
}

for(n in 1:nrow(safetyMergeNC14))
{
  if(safetyMergeNC14[n,"NoCrim N.x"] == "N/A" | safetyMergeNC14[n,"NoCrim N.x"] == "#N/A")
  {
      safetyMergeNC14[n,"NoCrim N.x"] <- safetyMergeNC14[n,"NoCrim N.y"]
  }
}

for(n in 1:nrow(safetyMergeNC14))
{
  if(safetyMergeNC14[n,"Prop N.x"] == "N/A" | safetyMergeNC14[n,"Prop N.x"] == "#N/A")
  {
    safetyMergeNC14[n,"Prop N.x"] <- safetyMergeNC14[n,"Prop N.y"]
  }
}

for(n in 1:nrow(safetyMergeNC14))
{
  if(safetyMergeNC14[n,"Vio N.x"] == "N/A" | safetyMergeNC14[n,"Vio N.x"] == "#N/A")
  {
    safetyMergeNC14[n,"Vio N.x"] <- safetyMergeNC14[n,"Vio N.y"]
  }
}

for(n in 1:nrow(safetyMergeNC14))
{
   if(safetyMergeNC14[n,"AvgOfMajor N.x"] == "N/A" | safetyMergeNC14[n,"AvgOfMajor N.x"] == "#N/A")
   {
      safetyMergeNC14[n,"AvgOfMajor N.x"] <- safetyMergeNC14[n,"AvgOfMajor N.y"]
   }
}

for(n in 1:nrow(safetyMergeNC14))
{
  if(safetyMergeNC14[n,"AvgOfOth N.x"] == "N/A" | safetyMergeNC14[n,"AvgOfOth N.x"] == "#N/A")
  {
    safetyMergeNC14[n,"AvgOfOth N.x"] <- safetyMergeNC14[n,"AvgOfOth N.y"]
  }
}

for(n in 1:nrow(safetyMergeNC14))
{
  if(safetyMergeNC14[n,"AvgOfNoCrim N.x"] == "N/A" | safetyMergeNC14[n,"AvgOfNoCrim N.x"] == "#N/A")
  {
    safetyMergeNC14[n,"AvgOfNoCrim N.x"] <- safetyMergeNC14[n,"AvgOfNoCrim N.y"]
  }
}

for(n in 1:nrow(safetyMergeNC14))
{
  if(safetyMergeNC14[n,"AvgOfProp N.x"] == "N/A" | safetyMergeNC14[n,"AvgOfProp N.x"] == "#N/A")
  {
    safetyMergeNC14[n,"AvgOfProp N.x"] <- safetyMergeNC14[n,"AvgOfProp N.y"]
  }
}

for(n in 1:nrow(safetyMergeNC14))
{
  if(safetyMergeNC14[n,"AvgOfVio N.x"] == "N/A" | safetyMergeNC14[n,"AvgOfVio N.x"] == "#N/A")
  {
    safetyMergeNC14[n,"AvgOfVio N.x"] <- safetyMergeNC14[n,"AvgOfVio N.y"]
  }
}

safetyMergeNC14$`Building Name` <- NULL
safetyMergeNC14$`# Schools.y`<- NULL
safetyMergeNC14$`School Year.y`<- NULL
safetyMergeNC14$`Location Name.y`<- NULL
safetyMergeNC14$NumberStudents.y<- NULL
safetyMergeNC14$`Schools in Building.y`<- NULL
safetyMergeNC14$`Major N.y`<- NULL
safetyMergeNC14$`Oth N.y`<- NULL
safetyMergeNC14$`NoCrim N.y`<- NULL
safetyMergeNC14$`Prop N.y`<- NULL
safetyMergeNC14$`Vio N.y`<- NULL
safetyMergeNC14$`AvgOfMajor N.y`<- NULL
safetyMergeNC14$`AvgOfOth N.y`<- NULL
safetyMergeNC14$`AvgOfNoCrim N.y`<- NULL
safetyMergeNC14$`AvgOfProp N.y`<- NULL
safetyMergeNC14$`AvgOfVio N.y`<- NULL
safetyMergeNC14$`Borough Name.y`<- NULL
safetyMergeNC14$Postcode.y<- NULL
safetyMergeNC14$Latitude.y<- NULL
safetyMergeNC14$Longitude.y<- NULL
safetyMergeNC14$`Community Board.y`<- NULL
safetyMergeNC14$`Council District.y`<- NULL
safetyMergeNC14$`Census Tract.y`<- NULL
safetyMergeNC14$NTA.y<- NULL

#for 2015
safetyMergeNC15 <- dplyr::left_join(safetyMergeNC15, consolidated15, by="Building Name")

for(n in 1:nrow(safetyMergeNC15))
{
  if(safetyMergeNC15[n,"Major N.x"] == "N/A" | safetyMergeNC15[n,"Major N.x"] == "#N/A")
  {
    safetyMergeNC15[n,"Major N.x"] <- safetyMergeNC15[n,"Major N.y"]
  }
}

for(n in 1:nrow(safetyMergeNC15))
{
  if(safetyMergeNC15[n,"Oth N.x"] == "N/A" | safetyMergeNC15[n,"Oth N.x"] == "#N/A")
  {
    safetyMergeNC15[n,"Oth N.x"] <- safetyMergeNC15[n,"Oth N.y"]
  }
}

for(n in 1:nrow(safetyMergeNC15))
{
  if(safetyMergeNC15[n,"NoCrim N.x"] == "N/A" | safetyMergeNC15[n,"NoCrim N.x"] == "#N/A")
  {
    safetyMergeNC15[n,"NoCrim N.x"] <- safetyMergeNC15[n,"NoCrim N.y"]
  }
}

for(n in 1:nrow(safetyMergeNC15))
{
  if(safetyMergeNC15[n,"Prop N.x"] == "N/A" | safetyMergeNC15[n,"Prop N.x"] == "#N/A")
  {
    safetyMergeNC15[n,"Prop N.x"] <- safetyMergeNC15[n,"Prop N.y"]
  }
}

for(n in 1:nrow(safetyMergeNC15))
{
  if(safetyMergeNC15[n,"Vio N.x"] == "N/A" | safetyMergeNC15[n,"Vio N.x"] == "#N/A")
  {
    safetyMergeNC15[n,"Vio N.x"] <- safetyMergeNC15[n,"Vio N.y"]
  }
}

for(n in 1:nrow(safetyMergeNC15))
{
  if(safetyMergeNC15[n,"AvgOfMajor N.x"] == "N/A" | safetyMergeNC15[n,"AvgOfMajor N.x"] == "#N/A")
  {
    safetyMergeNC15[n,"AvgOfMajor N.x"] <- safetyMergeNC15[n,"AvgOfMajor N.y"]
  }
}

for(n in 1:nrow(safetyMergeNC15))
{
  if(safetyMergeNC15[n,"AvgOfOth N.x"] == "N/A" | safetyMergeNC15[n,"AvgOfOth N.x"] == "#N/A")
  {
    safetyMergeNC15[n,"AvgOfOth N.x"] <- safetyMergeNC15[n,"AvgOfOth N.y"]
  }
}

for(n in 1:nrow(safetyMergeNC15))
{
  if(safetyMergeNC15[n,"AvgOfNoCrim N.x"] == "N/A" | safetyMergeNC15[n,"AvgOfNoCrim N.x"] == "#N/A")
  {
    safetyMergeNC15[n,"AvgOfNoCrim N.x"] <- safetyMergeNC15[n,"AvgOfNoCrim N.y"]
  }
}

for(n in 1:nrow(safetyMergeNC15))
{
  if(safetyMergeNC15[n,"AvgOfProp N.x"] == "N/A" | safetyMergeNC15[n,"AvgOfProp N.x"] == "#N/A")
  {
    safetyMergeNC15[n,"AvgOfProp N.x"] <- safetyMergeNC15[n,"AvgOfProp N.y"]
  }
}

for(n in 1:nrow(safetyMergeNC15))
{
  if(safetyMergeNC15[n,"AvgOfVio N.x"] == "N/A" | safetyMergeNC15[n,"AvgOfVio N.x"] == "#N/A")
  {
    safetyMergeNC15[n,"AvgOfVio N.x"] <- safetyMergeNC15[n,"AvgOfVio N.y"]
  }
}

safetyMergeNC15$`Building Name` <- NULL
safetyMergeNC15$`# Schools.y`<- NULL
safetyMergeNC15$`School Year.y`<- NULL
safetyMergeNC15$`Location Name.y`<- NULL
safetyMergeNC15$NumberStudents.y<- NULL
safetyMergeNC15$`Schools in Building.y`<- NULL
safetyMergeNC15$`Major N.y`<- NULL
safetyMergeNC15$`Oth N.y`<- NULL
safetyMergeNC15$`NoCrim N.y`<- NULL
safetyMergeNC15$`Prop N.y`<- NULL
safetyMergeNC15$`Vio N.y`<- NULL
safetyMergeNC15$`AvgOfMajor N.y`<- NULL
safetyMergeNC15$`AvgOfOth N.y`<- NULL
safetyMergeNC15$`AvgOfNoCrim N.y`<- NULL
safetyMergeNC15$`AvgOfProp N.y`<- NULL
safetyMergeNC15$`AvgOfVio N.y`<- NULL
safetyMergeNC15$`Borough Name.y`<- NULL
safetyMergeNC15$Postcode.y<- NULL
safetyMergeNC15$Latitude.y<- NULL
safetyMergeNC15$Longitude.y<- NULL
safetyMergeNC15$`Community Board.y`<- NULL
safetyMergeNC15$`Council District.y`<- NULL
safetyMergeNC15$`Census Tract.y`<- NULL
safetyMergeNC15$NTA.y<- NULL

#--------------------
#split the mathMerge df into 2014 and 2015
#--------------------
mathMerge$`# Level 1` <- NULL
mathMerge$`# Level 2` <- NULL
mathMerge$`# Level 3` <- NULL
mathMerge$`# Level 4` <- NULL
mathMerge$`# Level 3+4` <- NULL


mathMerge14 <- filter(mathMerge, Year=="2014")
mathMerge15 <- filter(mathMerge, Year=="2015")


#--------------------
#join MathMerge14 with SafetyMergeNC14, join MathMerge15 with SafetyMergeNC15 by DBN
#--------------------

#use these df for analysing w/i EITHER year 2014 or year 2015
safetyMath14 <- dplyr::left_join(mathMerge14, safetyMergeNC14, by="DBN")
safetyMath15 <- dplyr::left_join(mathMerge15, safetyMergeNC15, by="DBN")

#use this df for analysing w/i BOTH year 2014 and year 2015
safetyMathBOTH <- rbind(safetyMath14, safetyMath15)

#convert desired variables into recognized ints/nums from characters
safetyMath14$`Mean Scale Score` <- as.numeric(safetyMath14$`Mean Scale Score`)
safetyMath14$`NoCrim N.x` <- as.numeric(safetyMath14$`NoCrim N.x`)
safetyMath15$`# Schools.x` <- as.numeric(safetyMath15$`# Schools.x`)

safetyMath15$`Mean Scale Score` <- as.numeric(safetyMath15$`Mean Scale Score`)
safetyMath15$`NoCrim N.x` <- as.numeric(safetyMath15$`NoCrim N.x`)

safetyMathBOTH$`Mean Scale Score` <- as.numeric(safetyMathBOTH$`Mean Scale Score`)
safetyMathBOTH$`NoCrim N.x` <- as.numeric(safetyMathBOTH$`NoCrim N.x`)


safetyMathBOTH <- rename(safetyMathBOTH, NumTested = `Number Tested`, 
                         MeanScaleScore = `Mean Scale Score`, Level1p = `% Level 1`, Level2p = `% Level 2`,
                         Level3p = `% Level 3`, Level4p = `% Level 4`, Level34p = `% Level 3+4`,
                         SchoolYear = `School Year.x`, LocationName = `Location Name.x`, NumStudents = `NumberStudents.x`,
                         NumSchools = `# Schools.x`, SchoolsInBuilding = `Schools in Building.x`, MajorN = `Major N.x`,
                         OthN = `Oth N.x`, NoCrimN = `NoCrim N.x`, PropN = `Prop N.x`, VioN = `Vio N.x`,
                         GroupNameOfPop = `GroupNameofPop.x`, BuildingPop = `BuildingPopulation.x`, AvgMajor = `AvgOfMajor N.x`,
                         AvgOth = `AvgOfOth N.x`, AvgNoCrim = `AvgOfNoCrim N.x`, AvgPop = `AvgOfProp N.x`, AvgVio = `AvgOfVio N.x`,
                         BoroughName = `Borough Name.x`, Postcode = `Postcode.x`, Latitude = `Latitude.x`, Longitude = `Longitude.x`,
                         CommunityBoard = `Community Board.x`, CouncilDistrict = `Council District.x`,
                         NTA=`NTA.x`)

safetyMath14 <- rename(safetyMath14, NumTested = `Number Tested`, 
                         MeanScaleScore = `Mean Scale Score`, Level1p = `% Level 1`, Level2p = `% Level 2`,
                         Level3p = `% Level 3`, Level4p = `% Level 4`, Level34p = `% Level 3+4`,
                         SchoolYear = `School Year.x`, LocationName = `Location Name.x`, NumStudents = `NumberStudents.x`,
                         NumSchools = `# Schools.x`, SchoolsInBuilding = `Schools in Building.x`, MajorN = `Major N.x`,
                         OthN = `Oth N.x`, NoCrimN = `NoCrim N.x`, PropN = `Prop N.x`, VioN = `Vio N.x`,
                         GroupNameOfPop = `GroupNameofPop.x`, BuildingPop = `BuildingPopulation.x`, AvgMajor = `AvgOfMajor N.x`,
                         AvgOth = `AvgOfOth N.x`, AvgNoCrim = `AvgOfNoCrim N.x`, AvgPop = `AvgOfProp N.x`, AvgVio = `AvgOfVio N.x`,
                         BoroughName = `Borough Name.x`, Postcode = `Postcode.x`, Latitude = `Latitude.x`, Longitude = `Longitude.x`,
                         CommunityBoard = `Community Board.x`, CouncilDistrict = `Council District.x`,
                         NTA=`NTA.x`)

safetyMath15 <- rename(safetyMath15, NumTested = `Number Tested`, 
                       MeanScaleScore = `Mean Scale Score`, Level1p = `% Level 1`, Level2p = `% Level 2`,
                       Level3p = `% Level 3`, Level4p = `% Level 4`, Level34p = `% Level 3+4`,
                       SchoolYear = `School Year.x`, LocationName = `Location Name.x`, NumStudents = `NumberStudents.x`,
                       NumSchools = `# Schools.x`, SchoolsInBuilding = `Schools in Building.x`, MajorN = `Major N.x`,
                       OthN = `Oth N.x`, NoCrimN = `NoCrim N.x`, PropN = `Prop N.x`, VioN = `Vio N.x`,
                       GroupNameOfPop = `GroupNameofPop.x`, BuildingPop = `BuildingPopulation.x`, AvgMajor = `AvgOfMajor N.x`,
                       AvgOth = `AvgOfOth N.x`, AvgNoCrim = `AvgOfNoCrim N.x`, AvgPop = `AvgOfProp N.x`, AvgVio = `AvgOfVio N.x`,
                       BoroughName = `Borough Name.x`, Postcode = `Postcode.x`, Latitude = `Latitude.x`, Longitude = `Longitude.x`,
                       CommunityBoard = `Community Board.x`, CouncilDistrict = `Council District.x`,
                       NTA=`NTA.x`)

safetyMath14$MajorN <- as.numeric(safetyMath14$MajorN)
safetyMath14$OthN <- as.numeric(safetyMath14$OthN)
safetyMath14$NoCrimN <- as.numeric(safetyMath14$NoCrimN)
safetyMath14$PropN <- as.numeric(safetyMath14$PropN)
safetyMath14$VioN <- as.numeric(safetyMath14$VioN)
safetyMath14$NumSchools <- as.numeric(safetyMath14$NumSchools)
safetyMath14 <- dplyr::mutate(safetyMath14, totCrime = MajorN + OthN + NoCrimN + PropN + VioN)


safetyMath15$MajorN <- as.numeric(safetyMath15$MajorN)
safetyMath15$OthN <- as.numeric(safetyMath15$OthN)
safetyMath15$NoCrimN <- as.numeric(safetyMath15$NoCrimN)
safetyMath15$PropN <- as.numeric(safetyMath15$PropN)
safetyMath15$VioN <- as.numeric(safetyMath15$VioN)
safetyMath15$NumSchools <- as.numeric(safetyMath15$NumSchools)
safetyMath15 <- dplyr::mutate(safetyMath15, totCrime = MajorN + OthN + NoCrimN + PropN + VioN)


safetyMathBOTH$MajorN <- as.numeric(safetyMathBOTH$MajorN)
safetyMathBOTH$OthN <- as.numeric(safetyMathBOTH$OthN)
safetyMathBOTH$NoCrimN <- as.numeric(safetyMathBOTH$NoCrimN)
safetyMathBOTH$PropN <- as.numeric(safetyMathBOTH$PropN)
safetyMathBOTH$VioN <- as.numeric(safetyMathBOTH$VioN)
safetyMathBOTH$NumSchools <- as.numeric(safetyMathBOTH$NumSchools)
safetyMathBOTH <- dplyr::mutate(safetyMathBOTH, totCrime = MajorN + OthN + NoCrimN + PropN + VioN)


#------------------
# PLOTS

# create low crime data
lowCrime <- crimeScore %>% filter(`NoCrim N.x` < 3)
# histogram of mean scale score for low crime
ggplot(lowCrime, aes(`Mean Scale Score`)) + geom_histogram() + xlim(250,375)
# histogram of mean scale score for all crime
ggplot(safetyMathBOTH, aes(`Mean Scale Score`)) + geom_histogram()+xlim(250,375)

# histogram of number of crime
ggplot(safetyMathBOTH, aes(`NoCrim N.x`)) + geom_histogram(binwidth = 1)

#--------------------
#exploratory graphing code
#--------------------

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
library(grid)
library(gridExtra)
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


#graph of number of schools vs number of crimes for 2014, 2015
NumSchoolTotCrime14 <- ggplot(safetyMath14, aes(NumSchools, totCrime)) + xlim(c(0, 8))+ geom_point(color="blue", alpha = 0.3) + geom_smooth(method="lm") + ylab("Total number of Crimes") + xlab("Number of Schools/Location") + labs(title="2014")
NumSchoolTotCrime15 <- ggplot(safetyMath15, aes(NumSchools, totCrime)) +  xlim(c(0, 8))+ geom_point(color="red", alpha = 0.3)+ geom_smooth(method="lm", color="red")+ ylab("Total number of Crimes") + xlab("Number of Schools/Location") + labs(title="2015")
multiplot(NumSchoolTotCrime14, NumSchoolTotCrime15,cols=2)

#box plots for summary statistics

boxplot(totCrime~Year,data=safetyMathBOTH, main="Crime Distribution", 
        xlab="Year", ylab="Number of Crimes")
boxplot(MeanScaleScore~Year,data=safetyMathBOTH, main="Math Test Results", 
        xlab="Year", ylab="Mean Scores")

par(mfrow=c(1,1))
boxplot(safetyMathBOTH$MajorN, main="Major Crimes", ylab="#")
boxplot(safetyMathBOTH$PropN, main="Property Crimes", ylab="#")
boxplot(safetyMathBOTH$VioN, main="Violent Crimes", ylab="#")
boxplot(safetyMathBOTH$NoCrimN, main="NonCriminal Crimes", ylab="#")
boxplot(safetyMathBOTH$OthN, main="Other Crimes", ylab="#")


safeBase <- ggplot(safetyMathBOTH)
major <-geom_boxplot(aes(x = "", y=MajorN),color="black", fill="red", alpha=0.2)
property <- geom_boxplot(aes(x = "", y=PropN),color="black", fill="orange", alpha=0.2)
violent <- geom_boxplot(aes(x = "", y=VioN),color="black", fill="yellow", alpha=0.2) 
noncrim <- geom_boxplot(aes(x = "", y=NoCrimN),color="black", fill="green", alpha=0.2)
other <- geom_boxplot(aes(x = "", y=OthN), color="black", fill="blue", alpha=0.2)
multiplot(safeBase + major+ xlab("Major Crimes") + ylab("Number of Crimes")+coord_cartesian(ylim = c(0, 45)), safeBase + property+ xlab("Property Crimes")+ ylab("Number of Crimes")+coord_cartesian(ylim = c(0, 45)), safeBase + violent+xlab("Violent Crimes")+ ylab("Number of Crimes")+coord_cartesian(ylim = c(0, 45)), safeBase + noncrim+ xlab("Non-Criminal")+ ylab("Number of Crimes")+coord_cartesian(ylim = c(0, 45)), safeBase + other + xlab("Other Crimes")+ ylab("Number of Crimes")+coord_cartesian(ylim = c(0, 45)), cols=5)

#the boxplots below show that up to a certain point, the number of crimes
#that occur in a school are strongly correlated to the math scores that school
#receives. After around 6 or 7 crimes, however, the data becomes rather noisey
#and flattens out to show that one more crime in a "higher crime"
#school is trivial in its effect on math scores.

boxplot(MeanScaleScore~totCrime,data=safetyMath14, main="Math Test & Crime 2014", 
        xlab="Number of Crimes", ylab="Mean Scores")
boxplot(MeanScaleScore~totCrime,data=safetyMath15, main="Math Test & Crime 2015", 
        xlab="Number of Crimes", ylab="Mean Scores")

#bagplots for summary statistics
library(aplpack)
attach(safetyMathBOTH)
p <- bagplot(totCrime,MeanScaleScore, xlab="Number of Crimes", ylab="Mean Scores",
        main="Math Test & Crime")


#summary table for the data frames
library(qwraps2)
summary14 <-
  list("Mean Scores" =
         list("min" = ~ min(MeanScaleScore),
              "max" = ~ max(MeanScaleScore),
              "mean (sd)" = ~ qwraps2::mean_sd(MeanScaleScore)),
       "Number of Crimes" =
         list("min" = ~ min(totCrime),
              "max" = ~ max(totCrime),
              "mean (sd)" = ~ qwraps2::mean_sd(totCrime)),
       "Number of Schools/Location" =
         list("min" = ~ min(NumSchools),
              "max" = ~ max(NumSchools),
              "mean (sd)" = ~ qwraps2::mean_sd(NumSchools)),
       "Borough" =
         list("Bronx" = ~ qwraps2::n_perc0(BoroughName =="BRONX"),
              "Brooklyn"  = ~ qwraps2::n_perc0(BoroughName == "BROOKLYN"),
              "Manhattan"  = ~ qwraps2::n_perc0(BoroughName == "MANHATTAN"),
              "Queens"  = ~ qwraps2::n_perc0(BoroughName == "QUEENS"),
              "Staten Island"  = ~ qwraps2::n_perc0(BoroughName == "STATEN IS"))
  )
summary1 <-
  list("Mean Scores" =
         list("min" = ~ min(MeanScaleScore),
              "max" = ~ max(MeanScaleScore),
              "mean (sd)" = ~ qwraps2::mean_sd(MeanScaleScore))
  )
summary_table(safetyMath14, summary1)

#graph of mean scale math score vs total crime, with size showing the number of schools in the same building
weight15 <- ggplot(safetyMath15, aes(totCrime, MeanScaleScore, size=(1/NumSchools))) + geom_point(aes(alpha=0.1), pch=21, fill="red") + geom_smooth(mapping = aes(weight = (1/NumSchools)), col="white") + geom_vline(xintercept=6, col="green", linetype="dashed") + xlab("Number of Crimes") + ylab("Scaled Math Scores")+ labs(title="Weighted 2015")+theme(legend.position="none")
unweight15 <- ggplot(safetyMath15, aes(totCrime, MeanScaleScore)) + geom_point(aes(alpha=0.1), pch=21, fill="red") + geom_smooth(col="white") + geom_vline(xintercept=6, col="green", linetype="dashed") + xlab("Number of Crimes") + ylab("Scaled Math Scores")+ labs(title="Unweighted 2015")+theme(legend.position="none")

weight14 <- ggplot(safetyMath14, aes(totCrime, MeanScaleScore, size=(1/NumSchools))) + geom_point(aes(alpha=0.1), pch=21, fill="blue") + geom_smooth(mapping = aes(weight = (1/NumSchools)), col="white") + geom_vline(xintercept=6, col="green", linetype="dashed") + xlab("Number of Crimes") + ylab("Scaled Math Scores")+ labs(title="Weighted 2014")+theme(legend.position="none")
unweight14 <- ggplot(safetyMath14, aes(totCrime, MeanScaleScore)) + geom_point(aes(alpha=0.1), pch=21, fill="blue") + geom_smooth(col="white") + geom_vline(xintercept=6, col="green", linetype="dashed")+ xlab("Number of Crimes") + ylab("Scaled Math Scores")+ labs(title="Unweighted 2014")+ theme(legend.position="none")
multiplot(unweight14, weight14, unweight15, weight15, cols=2)


crimeScore <- select(safetyMathBOTH, `NoCrim N.x`,`Mean Scale Score`, Year)
View(crimeScore)
crimeScore$`NoCrim N.x`<-as.numeric(crimeScore$`NoCrim N.x`)
crimeScore$`Mean Scale Score`<-as.numeric(crimeScore$`Mean Scale Score`)


#R squared mean error:
lm <- lm(MeanScaleScore~totCrime,safetyMathBOTH)
sm<-summary(lm)
sm
sum(sm$residuals^2)
# the sum of squared error is: 825922.3

mean(sm$residuals^2)
#the mean squared error is : 368.88
  