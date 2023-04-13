####################### Ohio Poll ReportR ##############################

### check for the installation of packages 
### check for packages 
pkg <- c("stringr","stringi","dplyr","foreign","gtools","haven",
         "ggplot2","ggpubr","gridExtra","pollster","survey",
         "rstudioapi","rgeos","scales","splitstackshape",
         "sjlabelled")

for (i in pkg){
  print(i)
  if(require(i, character.only=TRUE)){
    print(paste(i, "is loaded correctly"))
  } else{
    print(paste("trying to install", i))
    install.packages(i)
    if(require(i, character.only=TRUE)){
      print(paste(i, "installed and loaded"))
    } else{
      stop(paste("could not install", i))
    }
  }
}

# load in packages 
library(foreign)
library(rstudioapi)
library(haven)
library(dplyr)
library(survey)
library(pollster)
library(ggplot2)
library(gridExtra)
library(stringi)
library(stringr)
library(sjlabelled)
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

### set the working directory to local file 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

### read in the survey data 
final_data <-haven::read_spss("raw_spss10162022.sav")

## now let's drop the data 
final_data2 <- subset(final_data, is.na(register1)==F)
final_data2 <- subset(final_data2, DistributionChannel != "test")

final_data2lbl <- sapply(final_data2, attr,"label")

## also read in the text data, which will make things easier to label 
text_svydata <- read.csv("raw_csv10162022text.csv")

## now drop 
text_svydata <- subset(text_svydata, register1 != "") #good, this now matches 
text_svydata <- subset(text_svydata, DistributionChannel != "test") #good, this now matches 

### next, we will want to get the relevant target data by party, race, sex 
table(text_svydata$pid_think)
table(text_svydata$pid)/nrow(text_svydata)

table(text_svydata$ind_lean)

#### let's drop data, and then merge on the data 
text_svydata <- text_svydata[,-c(1:8,10:18)]

### modify names of cols 
colnames(text_svydata) <- paste0(colnames(text_svydata),sep="_","txt")

final_data2all <- merge(final_data2,text_svydata, by.x="ResponseId",by.y="ResponseId_txt" )

## with this, we have values and the numeric data 

## assign leaners to party 
final_data2all$party_simp <- "no lean"
final_data2all$party_simp[final_data2all$pid_think_txt=="Democrat" | final_data2all$ind_lean_txt=="Democratic"] <-
  "Democrat/lean Democrat"
final_data2all$party_simp[final_data2all$pid_think_txt=="Republican" | final_data2all$ind_lean_txt=="Republican"] <-
  "Republican/lean Republican"
### now let's look at dist 
table(final_data2all$party_simp)/nrow(final_data2all) # 52% dem, 46% GOP ; defn tilts dem 

## now let's do simp race ; zero sum 
table(final_data2all$race_txt)

## race simplified, following wru BISG format   
final_data2all$race_simp <- ""
final_data2all$race_simp[final_data2all$race_1==1] <- "white"
final_data2all$race_simp[final_data2all$race_2==1] <- "black"
final_data2all$race_simp[final_data2all$race_4==1] <- "asian/pi"
final_data2all$race_simp[final_data2all$race_5==1] <- "asian/pi"
final_data2all$race_simp[final_data2all$hispanic==1] <- "latino"
table(final_data2all$race_simp) #assign other 
## let's assign other 
final_data2all$race_simp[final_data2all$race_simp==""] <- "other"

## read in race census data 
race_cens <- read.csv("R13210196_SL040.csv")
### label the relevant fields for weighting 
### race 
colnames(race_cens)[colnames(race_cens)=="SE_T011_003"] <- "white"
colnames(race_cens)[colnames(race_cens)=="SE_T011_004"] <- "black"
colnames(race_cens)[colnames(race_cens)=="SE_T011_010"] <- "latino"
race_cens$asian_pi <- race_cens$SE_T011_006 + race_cens$SE_T011_007
race_cens$other <- race_cens$SE_T011_005+race_cens$SE_T011_008+race_cens$SE_T011_009

## now subset 
race_cens <- subset(race_cens, select=c(white,black,latino,asian_pi,other))
race_censT <- t(race_cens)
race_censT <- as.data.frame(race_censT)
race_censT$race_simp <- rownames(race_censT)
race_censT$race_simp[race_censT$race_simp=="asian_pi"] <- "asian/pi"
## now props 
race_censT$race_pct <- (race_censT$`1`/sum(race_censT$`1`))
## now drop first col 
race_censT <- race_censT[,-1]

## drop those who did not finish 
final_data2all <- subset(final_data2all, Finished==1)
final_data2all <- subset(final_data2all, is.na(age)==F)
final_data2all <- subset(final_data2all, is.na(educ)==F)

### now let's look at sex 
table(final_data2all$age_txt)/nrow(final_data2all)
table(final_data2all$educ_txt)

## test to see if the one missing ed is also missing age 
temp_sub <- subset(final_data2all, is.na(age)==T)
temp_sub$educ_txt

### read in the census data 
sexed_demos <- read.csv("sex_ed_dict.csv")

### we have the data 18-24, 25-34, 35-44,45-54,55-64,65 + 

sexed_demos$age18_24 <- sexed_demos$SE_A02002_007+sexed_demos$SE_A02002_020
sexed_demos$age25_34 <- sexed_demos$SE_A02002_008+sexed_demos$SE_A02002_021
sexed_demos$age35_44 <- sexed_demos$SE_A02002_009+sexed_demos$SE_A02002_022
sexed_demos$age45_54 <- sexed_demos$SE_A02002_010+sexed_demos$SE_A02002_023
sexed_demos$age55_64 <- sexed_demos$SE_A02002_011+sexed_demos$SE_A02002_024
sexed_demos$age65over <- sexed_demos$SE_A02002_012+sexed_demos$SE_A02002_013+
  sexed_demos$SE_A02002_014+sexed_demos$SE_A02002_025+sexed_demos$SE_A02002_026+sexed_demos$SE_A02002_027
sexed_demosT <- subset(sexed_demos, select=c(age18_24,age25_34,age35_44,age45_54,age55_64,age65over))
sexed_demosT <- t(sexed_demosT)
sexed_demosT <- as.data.frame(sexed_demosT)
sexed_demosT$age_pct <- (sexed_demosT$`1`/sum(sexed_demosT$`1`))
sexed_demosT$age_txt <- "18-24 years old"
sexed_demosT$age_txt[2] <- "25-34 years old"
sexed_demosT$age_txt[3] <- "35-44 years old"
sexed_demosT$age_txt[4] <- "45-54 years old"
sexed_demosT$age_txt[5] <- "55-64 years old"
sexed_demosT$age_txt[6] <- "65+ years old"
### now drop the one 
sexed_demosT <- sexed_demosT[,-1]

## now that we have age, let's do educ 
table(final_data2all$educ_txt)

### create age X ed vars 
sexed_demos$educ_hs_less <- sexed_demos$ACS20_5yr_B15001004+sexed_demos$ACS20_5yr_B15001005+
  sexed_demos$ACS20_5yr_B15001012+sexed_demos$ACS20_5yr_B15001013+sexed_demos$ACS20_5yr_B15001020+
  sexed_demos$ACS20_5yr_B15001021+sexed_demos$ACS20_5yr_B15001028+sexed_demos$ACS20_5yr_B15001029+
  sexed_demos$ACS20_5yr_B15001036+sexed_demos$ACS20_5yr_B15001037+sexed_demos$ACS20_5yr_B15001045+
  sexed_demos$ACS20_5yr_B15001046+sexed_demos$ACS20_5yr_B15001053+sexed_demos$ACS20_5yr_B15001054+
  sexed_demos$ACS20_5yr_B15001061+sexed_demos$ACS20_5yr_B15001062+sexed_demos$ACS20_5yr_B15001069+
  sexed_demos$ACS20_5yr_B15001070+sexed_demos$ACS20_5yr_B15001077+sexed_demos$ACS20_5yr_B15001078
sexed_demos$educ_highschool <- sexed_demos$ACS20_5yr_B15001006+sexed_demos$ACS20_5yr_B15001014+
  sexed_demos$ACS20_5yr_B15001022+sexed_demos$ACS20_5yr_B15001030+sexed_demos$ACS20_5yr_B15001038+
  sexed_demos$ACS20_5yr_B15001047+sexed_demos$ACS20_5yr_B15001055+sexed_demos$ACS20_5yr_B15001063+
  sexed_demos$ACS20_5yr_B15001071+sexed_demos$ACS20_5yr_B15001079
sexed_demos$educ_somecollege <- sexed_demos$ACS20_5yr_B15001007+sexed_demos$ACS20_5yr_B15001015+
  sexed_demos$ACS20_5yr_B15001023+sexed_demos$ACS20_5yr_B15001031+sexed_demos$ACS20_5yr_B15001039+
  sexed_demos$ACS20_5yr_B15001048+sexed_demos$ACS20_5yr_B15001056+sexed_demos$ACS20_5yr_B15001064+
  sexed_demos$ACS20_5yr_B15001072+sexed_demos$ACS20_5yr_B15001080
sexed_demos$educ_associates <- sexed_demos$ACS20_5yr_B15001008+sexed_demos$ACS20_5yr_B15001016+
  sexed_demos$ACS20_5yr_B15001024+sexed_demos$ACS20_5yr_B15001032+sexed_demos$ACS20_5yr_B15001040+
  sexed_demos$ACS20_5yr_B15001049+sexed_demos$ACS20_5yr_B15001057+sexed_demos$ACS20_5yr_B15001065+
  sexed_demos$ACS20_5yr_B15001073+sexed_demos$ACS20_5yr_B15001081
sexed_demos$educ_bachelor <- sexed_demos$ACS20_5yr_B15001009+sexed_demos$ACS20_5yr_B15001017+
  sexed_demos$ACS20_5yr_B15001025+sexed_demos$ACS20_5yr_B15001033+sexed_demos$ACS20_5yr_B15001041+
  sexed_demos$ACS20_5yr_B15001050+sexed_demos$ACS20_5yr_B15001058+sexed_demos$ACS20_5yr_B15001066+
  sexed_demos$ACS20_5yr_B15001074+sexed_demos$ACS20_5yr_B15001082
sexed_demos$educ_gradpro <- sexed_demos$ACS20_5yr_B15001010+sexed_demos$ACS20_5yr_B15001018+
  sexed_demos$ACS20_5yr_B15001026+sexed_demos$ACS20_5yr_B15001034+sexed_demos$ACS20_5yr_B15001042+
  sexed_demos$ACS20_5yr_B15001051+sexed_demos$ACS20_5yr_B15001059+sexed_demos$ACS20_5yr_B15001067+
  sexed_demos$ACS20_5yr_B15001075+sexed_demos$ACS20_5yr_B15001083
### now get the data subsetted as well 
educ_tab <- subset(sexed_demos, select=c(educ_hs_less,educ_highschool,educ_somecollege,educ_associates,
                                         educ_bachelor,educ_gradpro))
## now transpose 
educ_tabT <- t(educ_tab)
educ_tabT <- as.data.frame(educ_tabT)
educ_tabT$edu_pct <- (educ_tabT$`1`/sum(educ_tabT$`1`))
## make mergeable 
educ_tabT$educ_code <- 1
educ_tabT$educ_code[2] <- 2
educ_tabT$educ_code[3] <- 3
educ_tabT$educ_code[4] <- 4
educ_tabT$educ_code[5] <- 5
educ_tabT$educ_code[6] <- 6
educ_tabT <- educ_tabT[,-1]
### good, ready to go 

## party matrix ; going with the potus vote share 
party_mat <- as.data.frame(cbind(party=c("Republican/lean Republican","Democrat/lean Democrat","no lean"), 
                                 pct=c(53.3,45.2,1.5)))
### ok, now lets start to weight by: party, educ, age, race 
final_data2allw <- merge(final_data2all, race_censT, by="race_simp")
final_data2allw <- merge(final_data2allw, party_mat, by.x="party_simp", by.y="party")
final_data2allw <- merge(final_data2allw, educ_tabT, by.x="educ", by.y="educ_code") # 2 obs dropped 
final_data2allw <- merge(final_data2allw, sexed_demosT, by.x="age_txt", by.y="age_txt") # 2 obs dropped 

### let's get samples now 
party_sample <- final_data2allw %>% 
  group_by(party_simp) %>% 
  tally()
party_sample <- as.data.frame(party_sample)
colnames(party_sample)[2] <- "sample_party_n"
party_sample$party_prop <- party_sample$sample_party_n/sum(party_sample$sample_party_n)

## now the age
age_sample <- final_data2allw %>% 
  group_by(age_txt) %>% 
  tally()
age_sample <- as.data.frame(age_sample)
colnames(age_sample)[2] <- "sample_age_n"
age_sample$age_prop <- age_sample$sample_age_n/sum(age_sample$sample_age_n)

## now let's do race 
race_sample <- final_data2allw %>% 
  group_by(race_simp) %>% 
  tally()
race_sample <- as.data.frame(race_sample)
colnames(race_sample)[2] <- "sample_race_n"
race_sample$race_prop <- race_sample$sample_race_n/sum(race_sample$sample_race_n)

### now let's do educ 
educ_sample <- final_data2allw %>% 
  group_by(educ) %>% 
  tally()
educ_sample <- as.data.frame(educ_sample)
colnames(educ_sample)[2] <- "sample_educ_n"
educ_sample$educ_prop <- educ_sample$sample_educ_n/sum(educ_sample$sample_educ_n)

### now let's merge on 
final_data2allw <- merge(final_data2allw,party_sample,by="party_simp" )
final_data2allw <- merge(final_data2allw,age_sample,by="age_txt" )
final_data2allw <- merge(final_data2allw,race_sample,by="race_simp" )
final_data2allw <- merge(final_data2allw,educ_sample,by="educ" )
final_data2allw$pct <- as.numeric(final_data2allw$pct)
final_data2allw$pct <- final_data2allw$pct/100
## check names 
names(final_data2allw)
## create the weights 
final_data2allw$weight_party <- (final_data2allw$pct/final_data2allw$party_prop)
final_data2allw$weight_age <- (final_data2allw$age_pct/final_data2allw$age_prop)
final_data2allw$weight_race <- (final_data2allw$race_pct/final_data2allw$race_prop)
final_data2allw$weight_educ <- (final_data2allw$edu_pct/final_data2allw$educ_prop)

### final weight 
final_data2allw$final_weight <- final_data2allw$weight_age*final_data2allw$weight_educ*
  final_data2allw$weight_party*final_data2allw$weight_race
summary(final_data2allw$final_weight) # ok, so now we have a max weight of 5; still high, but not nearly as bad.
#additionally, the mean is 1.0050 and median 1.27 ; let's look at quantile 
## 
quantile(final_data2allw$final_weight, seq(0,1,by=0.05)) #good; even the 95th pct is 1.75; look more fine grained
## 
quantile(final_data2allw$final_weight, seq(0.95,1,by=0.001))
length(which(final_data2allw$final_weight>2)) ## 33 obs for which greater than 2; 13 more than 3. Should be workable
## save draft here 
saveRDS(final_data2allw, "cleaned_poll_data.rds")
final_data2allw <- readRDS("cleaned_poll_data.rds")
test_attr <- sapply(final_data2allw, attr,"label")

# trump fav 
test_attr_sub <- test_attr["favorability_trump"]

### now implement the loops and such 
testtab <- moe_topline(df = final_data2allw, variable  = favorability_trump,weight = final_weight,
                       zscore=1.56)
summary_table(df = final_data2allw, variable = favorability_trump, weight = final_weight)
testtab
testtab <- as.data.frame(testtab)
testtab <- testtab[,c(1,3,5)]
testtab[,2] <- round(testtab[,2],2)
testtab[,3] <- round(testtab[,3],2)

## look at obs above 50 weight 
temp_poll <- subset(final_data2allw, final_weight>50)
nrow(temp_poll) # nothing; good 

## code to extract 
# strsplit(str1, "[?]")[[1]][1]

### get the file creation command here 
if (!dir.exists("reports")){
  dir.create("reports")
  print("created reports folder")
}else{
  print("reports folder exists")
}


### Let's do a loop now of all the topline results  
for (i in 36:143) {
  final_data2allw$temp_var <- final_data2allw[,i]
  ### get title 
  temp_name <- colnames(final_data2allw)[i]
  title = final_data2lbl[temp_name]
  title <-  strwrap(title, width = 60, simplify = FALSE)
  title <- sapply(title, paste, collapse = "\n")
  title <- str_replace(title, "Waley", "Whaley")
  title <- str_replace(title,"right direction", "right track" )
  ###
  testtab <- moe_topline(df = final_data2allw, variable  =temp_var ,weight = final_weight)
  testtab <- as.data.frame(testtab)
  testtab <- testtab[,c(1,3,5)]
  testtab[,2] <- round(testtab[,2],2)
  testtab[,3] <- round(testtab[,3],2)
  ### now get the plot 
  temp_barplot <-moe_topline(df = final_data2allw, variable = temp_var, weight = final_weight) %>%
    ggplot(aes(Response, Percent)) +
    geom_bar(stat = "identity", position = "dodge") + 
    geom_errorbar(aes(ymin=Percent-MOE, ymax=Percent+MOE), width=.2,
                  position=position_dodge(.9)) + 
    theme_minimal() + ggtitle(title) + 
    scale_x_discrete(labels = function(x) 
      stringr::str_wrap(x, width = 15)) +
    theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=0.25))
  ### now save to pdf 
  temp_pdf <- paste0("reports",sep="/",temp_name,sep=".pdf")
  pdf(temp_pdf)       # Export PDF
  grid::grid.text(title,x = (0.5), y = (0.8))
  grid.table(testtab, rows=NULL)
  print(temp_barplot)
  dev.off()
}


### now, create the script for cross tabs 
### test cross tab  https://cran.r-project.org/web/packages/pollster/readme/README.html
moe_crosstab(df = final_data2allw, x = favorability_trump, y = party_simp, weight = final_weight, 
             format="wide")
moe_crosstab(df = final_data2allw, x = favorability_trump, y = gender, weight = final_weight, format = "wide")

###create folder for party cross tabs 
if (!dir.exists("reports/party_cross")){
  dir.create("reports/party_cross")
  print("party_cross sub folder created")
}else{
  print("party_cross sub folder exists")
}
### now for gender cross tabs 
if (!dir.exists("reports/gender_cross")){
  dir.create("reports/gender_cross")
  print("gender_cross sub folder created")
}else{
  print("gender_cross sub folder exists")
}

### party and gender loop; might as well do it in one go  
for (i in 36:122) {
  print(i)
  final_data2allw$temp_var <- final_data2allw[,i]
  ### get title 
  temp_name <- colnames(final_data2allw)[i]
  title = final_data2lbl[temp_name]
  title <-  strwrap(title, width = 60, simplify = FALSE)
  title <- sapply(title, paste, collapse = "\n")
  ### Have a str replace here in case for waley error 
  title <- str_replace(title, "Waley", "Whaley")
  title <- str_replace(title,"right direction", "right track" )
  last4letters <- substrRight(title,4)
  ### have if then statement here to skip all of the titles that end in text
  if(last4letters=="Text"){
    print("Skipping question item")
  }else{
    testtab_party <-   moe_crosstab(df = subset(final_data2allw,party_simp!="no lean"),
                                    x = party_simp, y = temp_var, weight = final_weight, format = "long")
    testtab_party <- as.data.frame(testtab_party)
    testtab_party <- testtab_party[,c(1,2,3,4)]
    testtab_party[,3] <- round(testtab_party[,3],2)
    testtab_party[,4] <- round(testtab_party[,4],2)
    ### replace column names 
    colnames(testtab_party)[colnames(testtab_party)=="party_simp"] <- "Party" 
    colnames(testtab_party)[colnames(testtab_party)=="temp_var"] <- "Response" 
    colnames(testtab_party)[colnames(testtab_party)=="pct"] <- "%" 
    colnames(testtab_party)[colnames(testtab_party)=="moe"] <- "MOE" 
    
    ### now get the gender results 
    testtab_gender <-   moe_crosstab(df = subset(final_data2allw, gender_txt== "Male" | gender_txt=="Female"),
                                     x = gender, y = temp_var, weight = final_weight, format = "long")
    testtab_gender <- as.data.frame(testtab_gender)
    testtab_gender <- testtab_gender[,c(1,2,3,4)]
    testtab_gender[,3] <- round(testtab_gender[,3],2)
    testtab_gender[,4] <- round(testtab_gender[,4],2)
    ### fix names here too 
    colnames(testtab_gender)[colnames(testtab_gender)=="gender_simp"] <- "gender" 
    colnames(testtab_gender)[colnames(testtab_gender)=="temp_var"] <- "Response" 
    colnames(testtab_gender)[colnames(testtab_gender)=="pct"] <- "%" 
    colnames(testtab_gender)[colnames(testtab_gender)=="moe"] <- "MOE" 
    ### party plot 
    temp_party_plot<-moe_crosstab(df = subset(final_data2allw,party_simp!="no lean"),
                                  x = party_simp, y = temp_var, weight = final_weight, format = "long") %>%
      ggplot(aes(x = pct, y = temp_var, xmin = (pct - moe), xmax = (pct + moe), color = party_simp)) +
      geom_pointrange(position = position_dodge(width = 0.2)) + xlim(0,100) +
      scale_y_discrete(labels = function(x) 
        stringr::str_wrap(x, width = 15)) + scale_color_manual(name=NULL,values=c("blue","red")) +
      theme_minimal() + theme(axis.title = element_blank())
    
    ### now create the plot for gender 
    temp_gender_plot<-moe_crosstab(df = subset(final_data2allw,gender_txt== "Male" | gender_txt=="Female"),
                                   x = gender, y = temp_var, weight = final_weight, format = "long") %>%
      ggplot(aes(x = pct, y = temp_var, xmin = (pct - moe), xmax = (pct + moe), color = gender)) +
      geom_pointrange(position = position_dodge(width = 0.2)) + xlim(0,100) +
      scale_y_discrete(labels = function(x) 
        stringr::str_wrap(x, width = 15)) + scale_color_manual(name=NULL,values=c("green","purple")) +
      theme_minimal() + theme(axis.title = element_blank()) 
    # I think this looks good 
    
    
    ### now save to pdf ; party
    temp_pdf_party <- paste0("reports/party_cross",sep="/",temp_name,sep=".pdf")
    pdf(temp_pdf_party)       # Export PDF
    grid::grid.text(title,x = (0.5), y = (0.8))
    grid.table(testtab_party, rows=NULL)
    print(temp_party_plot)
    dev.off()
    ### now gender 
    temp_pdf_gender <- paste0("reports/gender_cross",sep="/",temp_name,sep=".pdf")
    pdf(temp_pdf_gender)       # Export PDF
    grid::grid.text(title,x = (0.5), y = (0.8))
    grid.table(testtab_gender, rows=NULL)
    print(temp_gender_plot)
    dev.off()
    
  }
}

### create a plot for the sampling error; let's startify by party 
party_weight_violin<-ggplot(final_data2allw, aes(x=party_simp, y=final_weight, fill=party_simp)) + 
  geom_violin(trim=FALSE) +   scale_fill_manual(name=NULL,values=c("blue","red")) + 
  theme_minimal()+ylab("Weights") + 
  theme(axis.title.x = element_blank()) +
  scale_x_discrete(labels = function(x) 
    stringr::str_wrap(x, width = 15))
## save here 
ggsave(party_weight_violin, filename = "party_violin_weight.png", width=6, height = 4, dpi=400,bg="White")


dem_sub <- subset(final_data2allw, party_simp=="Democrat/lean Democrat")
gop_sub <- subset(final_data2allw, party_simp=="Republican/lean Republican")

## now do quantile here 
quantile(dem_sub$final_weight, seq(0,1,0.01))
quantile(gop_sub$final_weight, seq(0,1,0.01))

### create a vector of all the items in order according to the survey 
mt_vec <- c()
for (i in 36:122) {
  print(i)
  final_data2allw$temp_var <- final_data2allw[,i]
  ### get title 
  temp_name <- colnames(final_data2allw)[i]
  mt_vec <- c(mt_vec, temp_name)

}

mt_vec <- mt_vec[!grepl('_TEXT',mt_vec)]
### save 
saveRDS(mt_vec, "question_order_vec.rds")

### test getting files 
file_names_pt <- paste0(mt_vec, sep=".","pdf")
file_names_pt <- paste0("final_topline",sep="/",mt_vec, sep=".","pdf") ## looks good 

#### Attempt to re run loops, but export as a png 
### folder creation command for png versions 
if (!dir.exists("reports_png")){
  dir.create("reports_png")
  print("reports_png folder created")
}else{
  print("reports_png folder exists")
}

###gender png 
if (!dir.exists("gender_png")){
  dir.create("gender_png")
  print("gender_png folder created")
}else{
  print("gender_png folder exists")
}
## party png 
###party png 
if (!dir.exists("party_png")){
  dir.create("party_png")
  print("party_png folder created")
}else{
  print("party_png folder exists")
}

### Let's do a loop now of all the topline 
for (i in 36:143) {
  final_data2allw$temp_var <- final_data2allw[,i]
  ### get title 
  temp_name <- colnames(final_data2allw)[i]
  title = final_data2lbl[temp_name]
  title <-  strwrap(title, width = 60, simplify = FALSE)
  title <- sapply(title, paste, collapse = "\n")
  title <- str_replace(title, "Waley", "Whaley")
  title <- str_replace(title,"right direction", "right track" )
  ###
  testtab <- moe_topline(df = final_data2allw, variable  =temp_var ,weight = final_weight)
  testtab <- as.data.frame(testtab)
  testtab <- testtab[,c(1,3,5)]
  testtab[,2] <- round(testtab[,2],2)
  testtab[,3] <- round(testtab[,3],2)
  ### now get the plot 
  temp_barplot <-moe_topline(df = final_data2allw, variable = temp_var, weight = final_weight) %>%
    ggplot(aes(Response, Percent)) +
    geom_bar(stat = "identity", position = "dodge") + 
    geom_errorbar(aes(ymin=Percent-MOE, ymax=Percent+MOE), width=.2,
                  position=position_dodge(.9)) + 
    theme_minimal() + ggtitle(title) + 
    scale_x_discrete(labels = function(x) 
      stringr::str_wrap(x, width = 15)) +
    theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=0.25)) +
    ggtitle(title)
  ### now save to pdf 
  ## create names for plots and tables 
  table_temp <- paste0(temp_name,sep="_","table")
  bar_temp <- paste0(temp_name,sep="_","barplot")
  
  ### command 
  temp_png_tab <- paste0("reports_png",sep="/",table_temp,sep=".png")
  temp_png_bar <- paste0("reports_png",sep="/",bar_temp,sep=".png")
  
  png(temp_png_tab, width=7,height=10,units="in", res=400)       # Export PNG as table
  p<-tableGrob(testtab,rows=NULL)
  grid.arrange(temp_barplot,p)
  #grid::grid.text(title,x = (0.5), y = (0.8))
  #grid.table(testtab, rows=NULL)
  dev.off()
}

#### Section here for the party and gender cross tabs 
for (i in 36:122) {
  print(i)
  final_data2allw$temp_var <- final_data2allw[,i]
  ### get title 
  temp_name <- colnames(final_data2allw)[i]
  title = final_data2lbl[temp_name]
  title <-  strwrap(title, width = 60, simplify = FALSE)
  title <- sapply(title, paste, collapse = "\n")
  ### Have a str replace here in case for waley error 
  title <- str_replace(title, "Waley", "Whaley")
  title <- str_replace(title,"right direction", "right track" )
  last4letters <- substrRight(title,4)
  ### have if then statement here to skip all of the titles that end in text
  if(last4letters=="Text"){
    print("Skipping question item")
  }else{
    testtab_party <-   moe_crosstab(df = subset(final_data2allw,party_simp!="no lean"),
                                    x = party_simp, y = temp_var, weight = final_weight, format = "long")
    testtab_party <- as.data.frame(testtab_party)
    testtab_party <- testtab_party[,c(1,2,3,4)]
    testtab_party[,3] <- round(testtab_party[,3],2)
    testtab_party[,4] <- round(testtab_party[,4],2)
    ### replace column names 
    colnames(testtab_party)[colnames(testtab_party)=="party_simp"] <- "Party" 
    colnames(testtab_party)[colnames(testtab_party)=="temp_var"] <- "Response" 
    colnames(testtab_party)[colnames(testtab_party)=="pct"] <- "%" 
    colnames(testtab_party)[colnames(testtab_party)=="moe"] <- "MOE" 
    
    ### now get the gender results 
    testtab_gender <-   moe_crosstab(df = subset(final_data2allw, gender_txt== "Male" | gender_txt=="Female"),
                                     x = gender, y = temp_var, weight = final_weight, format = "long")
    testtab_gender <- as.data.frame(testtab_gender)
    testtab_gender <- testtab_gender[,c(1,2,3,4)]
    testtab_gender[,3] <- round(testtab_gender[,3],2)
    testtab_gender[,4] <- round(testtab_gender[,4],2)
    ### fix names here too 
    colnames(testtab_gender)[colnames(testtab_gender)=="gender_simp"] <- "gender" 
    colnames(testtab_gender)[colnames(testtab_gender)=="temp_var"] <- "Response" 
    colnames(testtab_gender)[colnames(testtab_gender)=="pct"] <- "%" 
    colnames(testtab_gender)[colnames(testtab_gender)=="moe"] <- "MOE" 
    ### party plot 
    temp_party_plot<-moe_crosstab(df = subset(final_data2allw,party_simp!="no lean"),
                                  x = party_simp, y = temp_var, weight = final_weight, format = "long") %>%
      ggplot(aes(x = pct, y = temp_var, xmin = (pct - moe), xmax = (pct + moe), color = party_simp)) +
      geom_pointrange(position = position_dodge(width = 0.2)) + xlim(0,100) +
      scale_y_discrete(labels = function(x) 
        stringr::str_wrap(x, width = 15)) + scale_color_manual(name=NULL,values=c("blue","red")) +
      theme_minimal() + theme(axis.title = element_blank()) + ggtitle(title)
    
    ### now create the plot for gender 
    temp_gender_plot<-moe_crosstab(df = subset(final_data2allw,gender_txt== "Male" | gender_txt=="Female"),
                                   x = gender, y = temp_var, weight = final_weight, format = "long") %>%
      ggplot(aes(x = pct, y = temp_var, xmin = (pct - moe), xmax = (pct + moe), color = gender)) +
      geom_pointrange(position = position_dodge(width = 0.2)) + xlim(0,100) +
      scale_y_discrete(labels = function(x) 
        stringr::str_wrap(x, width = 15)) + scale_color_manual(name=NULL,values=c("green","purple")) +
      theme_minimal() + theme(axis.title = element_blank()) +ggtitle(title)
    # I think this looks good 
    
    
    ### now save to pdf ; party
    temp_pdf_party <- paste0("party_png",sep="/",temp_name,sep=".png")
    png(temp_pdf_party,width=7,height=10,units="in", res=400)       # Export PDF
    p<-tableGrob(testtab_party,rows=NULL)
    grid.arrange(temp_party_plot,p)
    dev.off()
    
    ### now gender 
    temp_pdf_gender <- paste0("gender_png",sep="/",temp_name,sep=".png")
    png(temp_pdf_gender,width=7,height=10,units="in", res=400)       # Export PDF
    p2<-tableGrob(testtab_gender,rows=NULL)
    grid.arrange(temp_gender_plot,p2)
    dev.off()
    
  }
}
### should be ready to go now 


#file_names_pt <- paste0("reports_png",sep="/",mt_vec,sep="_", "table", sep=".","png") ## looks good
#details = file.info(list.files("reports_png", full.names = T))
#details <- details[with(details, order(as.POSIXct(mtime))), ]
#files = rownames(details)
