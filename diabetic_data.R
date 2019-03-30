#uploading library
library(ggplot2)
library(swirl)
library(titanic)
library(tidyr)
library(dplyr)
library(sqldf)
library(stringr)
library(stats)
library(scales)
library(gridExtra)
library(Amelia)
library(devtools)
library(lasagnar)  
library(tabplot)
library(DataExplorer)
library(plyr)
library(dplyr)
library(car)
library(MASS)
library(e1071)
library(caret)
library(MASS)
library(car)
library(e1071)
library(caret)
library(cowplot)
library(caTools)
library(mice)
library(purrr)
library(sqldf)
library(contrast)

#uploading file
diab_data<-read.csv("diabetic_data.csv", stringsAsFactors = F)
str(diab_data)

#--------start of data cleaning-------------
sapply(diab_data, function(x) sum(is.na(x))) # no NAs found

#from the file we can see that there are some redundant values in weight column
distinct_weight<-sqldf("select count(*) as cnt, weight from diab_data group by weight  ")
distinct_weight
# majority weight has value as '?' removing weight
diab_data1<-diab_data[,-c(6)]

#from the file we can see that there are some redundant values in columns of race
distinct_race<-sqldf("select count(*) as cnt, race from diab_data1 group by race  ")
distinct_race
#replacing '?' with Other
diab_data1$race [(which(diab_data1$race == '?'))] <-'Other'


#from the file we can see that there are some redundant values in gender
# Female has the majoriry values, replacing 'Unknown with' with  Female
distinct_gender<-sqldf("select count(*) as cnt, gender from diab_data1 group by gender  ")
distinct_gender
diab_data1$gender [(which(diab_data1$gender == 'Unknown/Invalid'))] <-'Female'

#40% of value has '?' , removing payer_code column
distinct_payercode<-sqldf("select count(*) as cnt, payer_code from diab_data group by payer_code  ")
distinct_payercode
diab_data1<-diab_data1[,-c(10)]

#approximately 50% of value have redundant value as '?' , removing this column.
distinct_med_spec<-sqldf("select count(*) as cnt, medical_specialty from diab_data group by medical_specialty  ")
distinct_med_spec
diab_data1<-diab_data1[-c(10)]


#no duplicate found
sum(duplicated(diab_data1))

#removing encounterid and patient_nbr as these are id and will not be useful in modelling
diab_data1<-diab_data1[,-c(1:2)]

#changing readmitted to binary type :
#from the obersvation there are 3 values ,'NO',<30 ,>30 , 
#if there are values <30 , >30 , then we can covert to 'YES' 
distinct_readmitted<-sqldf("select count(*) as cnt, readmitted from diab_data1 group by readmitted  ")
distinct_readmitted
diab_data1$readmitted [(which(diab_data1$readmitted == '<30'))] <-'YES'
diab_data1$readmitted [(which(diab_data1$readmitted == '>30'))] <-'YES'

#Create the derived metric 'comorbidity' 
#converting diag_1,diag_2 and diag_3 into numeric attribute
diab_data1$diag_1<-as.numeric(diab_data1$diag_1)
diab_data1$diag_1<-as.numeric(diab_data1$diag_1)
diab_data1$diag_2<-as.numeric(diab_data1$diag_2)
diab_data1$diag_3<-as.numeric(diab_data1$diag_3)

#deriving diabetic or not values
diab_data1$combordity1  <-  ifelse((diab_data1$diag_1 >= 250.00 & diab_data1$diag_1 <251.00) | 
                                   (diab_data1$diag_2 >= 250.00 & diab_data1$diag_2 <251.00) |
                                   (diab_data1$diag_3 >= 250.00 & diab_data1$diag_3 <251.00) , 'DIB','OTH' )


#deriving circulatory or not values
diab_data1$combordity2  <-  ifelse((diab_data1$diag_1 >= 390 & diab_data1$diag_1 <=459) | 
                                     (diab_data1$diag_2 >= 390 & diab_data1$diag_2 <459) |
                                     (diab_data1$diag_3 >= 390 & diab_data1$diag_3 <459) , 'CIRC','OTH' )


#replacing NA with 'OTH' which is neither diabetic nor ciculatory
diab_data1$combordity1[is.na(diab_data1$combordity1)] = 'OTH'
diab_data1$combordity2[is.na(diab_data1$combordity2)] = 'OTH'
diab_data1$combordity  <-  NA

#deriving combordity values as per the definition
diab_data1$combordity <-ifelse(diab_data1$combordity1=='OTH' & diab_data1$combordity2 =='OTH',0,
                        ifelse(diab_data1$combordity1=='DIB' & diab_data1$combordity2 =='OTH',1,
                        ifelse(diab_data1$combordity1=='OTH' & diab_data1$combordity2 =='CIRC',2,
                        ifelse(diab_data1$combordity1=='DIB' & diab_data1$combordity2 =='CIRC',3,0)
                                                                            )))


#checking for NA
sum(is.na(diab_data1$combordity))
#0 record found

#removing drugs as these are important parameters
diab_data1<-diab_data1[,-c(20:36)]
diab_data1<-diab_data1[,-c(18)]
diab_data1<-diab_data1[,-c(20:24)]

diab_data1$race<-as.factor(diab_data1$race)
diab_data1$gender<-as.factor(diab_data1$gender)
diab_data1$age<-as.factor(diab_data1$age)
diab_data1$A1Cresult<-as.factor(diab_data1$A1Cresult)
diab_data1$insulin<-as.factor(diab_data1$insulin)
diab_data1$change<-as.factor(diab_data1$change)
diab_data1$diabetesMed<-as.factor(diab_data1$diabetesMed)
diab_data1$admission_type_id<-as.factor(diab_data1$admission_type_id)
diab_data1$discharge_disposition_id<-as.factor(diab_data1$discharge_disposition_id)
diab_data1$admission_source_id<-as.factor(diab_data1$admission_source_id)
diab_data1$combordity<-as.factor(diab_data1$combordity)

#removing diag_1,diag_2,diag_3 as these have been used to derive new metrics
diab_data1<-diab_data1[,-c(14:16)]

#removing combordity 1 and combordity 2
diab_data1<-diab_data1[,-c(20:21)]

#----------------------------------------------------------------------------
#              Start of exploratory data analysis
#-----------------------------------------------------------------------------

bar_theme1<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                   legend.position="none")

#plotting combordity against various factors
plot_grid(ggplot(diab_data1, aes(x=diab_data1$gender,fill=diab_data1$combordity))+ geom_bar(),
          ggplot(diab_data1, aes(x=diab_data1$age,fill=diab_data1$combordity))+ geom_bar()+bar_theme1,
          ggplot(diab_data1, aes(x=diab_data1$time_in_hospital,fill=diab_data1$combordity))+ geom_bar()+bar_theme1,
          ggplot(diab_data1, aes(x=diab_data1$num_lab_procedures,fill=diab_data1$combordity))+ geom_bar()+bar_theme1,
          ggplot(diab_data1, aes(x=diab_data1$A1Cresult,fill=diab_data1$combordity))+ geom_bar()+bar_theme1,
          align = "h")  


#All the bar graphs shows that combordity 2 is the most prevalent in all the plots
#female count is slightly more in the population
#most patient are in the age group of 70-80 
#no. of lab procedure shows the normalised distribution


#plotting readmitted against various factors
plot_grid(ggplot(diab_data1, aes(x=diab_data1$gender,fill=diab_data1$readmitted))+ geom_bar(),
          ggplot(diab_data1, aes(x=diab_data1$age,fill=diab_data1$readmitted))+ geom_bar()+bar_theme1,
          ggplot(diab_data1, aes(x=diab_data1$time_in_hospital,fill=diab_data1$readmitted))+ geom_bar()+bar_theme1,
          ggplot(diab_data1, aes(x=diab_data1$num_lab_procedures,fill=diab_data1$readmitted))+ geom_bar()+bar_theme1,
          ggplot(diab_data1, aes(x=diab_data1$A1Cresult,fill=diab_data1$readmitted))+ geom_bar()+bar_theme1,
          align = "h")  

#female count is more then male and so the rate of readmission
#age group 70-80 has max readmission followed by 60-70 which is intuitive
#time in hospital 3 days is the maximum.
#50 lab procedure is the most commmon 


#plotting boxplot for combordity against  numeric variables
plot_grid(ggplot(diab_data1, aes(x=diab_data1$insulin,fill=diab_data1$combordity))+ geom_bar(),
          ggplot(diab_data1, aes(x=diab_data1$A1Cresult,fill=diab_data1$combordity))+ geom_bar()+bar_theme1,
          ggplot(diab_data1, aes(x=diab_data1$change,fill=diab_data1$combordity))+ geom_bar()+bar_theme1,
          align = "h")  

#no insulin patient count is max followed by steady insulin
#no A1C result patient is more
#no Change in medication is having more no. of patient 

plot_grid(ggplot(diab_data1, aes(x=diab_data1$insulin,fill=diab_data1$combordity))+ geom_bar(),
          ggplot(diab_data1, aes(x=diab_data1$A1Cresult,fill=diab_data1$combordity))+ geom_bar()+bar_theme1,
          ggplot(diab_data1, aes(x=diab_data1$change,fill=diab_data1$combordity))+ geom_bar()+bar_theme1,
          align = "h")  



#admission type 1 has the most count
#discharge id 1 has the most count
#admission source id 7 has the most count

#plotting boxplot for combordity against numeric attributes
plot_grid(ggplot(diab_data1, aes(x=diab_data1$combordity))+ geom_bar()+bar_theme1, align = "h")  
#from this plot combordity 2 is the most frequent

ggplot(diab_data1, mapping = aes(y = diab_data1$time_in_hospital, x =diab_data1$combordity))+geom_boxplot()+labs(x="readmitted", y="time in hospital")
#time spent in hospital for comb 2 is high on mean , howere there are lots of outliers in comb 1 and comb3

ggplot(diab_data1, mapping = aes(y = diab_data1$num_lab_procedures, x =diab_data1$combordity))+geom_boxplot()+labs(x="readmitted", y="num of lab procedure")
#num of lab procedure median is almost same for all comb values.

ggplot(diab_data1, mapping = aes(y = diab_data1$num_medications, x =diab_data1$combordity))+
  geom_boxplot()+labs(x="readmitted", y="num_medications")
#num_medications median is highest for comb 2.

#------------------------------------------------------------
#                        End of EDA 
#------------------------------------------------------------


#---------------------------------------------------------------------
#        start of data preparation for modelling 
#---------------------------------------------------------------------
diab_data_rf<-diab_data1;

#converting factor variables of 2 levels into number
diab_data1$change <- ifelse(diab_data1$change =="No",1,0)
diab_data1$diabetesMed <- ifelse(diab_data1$diabetesMed =="No",1,0)
diab_data1$readmitted <- ifelse(diab_data1$readmitted =="YES",1,0)

#scaling the numeric variables

diab_data1$time_in_hospital <- scale(diab_data1$time_in_hospital,center = TRUE, scale = TRUE)
diab_data1$num_lab_procedures <- scale(diab_data1$num_lab_procedures,center = TRUE, scale = TRUE)
diab_data1$num_procedures <- scale(diab_data1$num_procedures,center = TRUE, scale = TRUE)
diab_data1$num_medications <- scale(diab_data1$num_medications,center = TRUE, scale = TRUE)
diab_data1$number_outpatient <- scale(diab_data1$number_outpatient,center = TRUE, scale = TRUE)
diab_data1$number_emergency <- scale(diab_data1$number_emergency,center = TRUE, scale = TRUE)
diab_data1$number_inpatient <- scale(diab_data1$number_inpatient,center = TRUE, scale = TRUE)
diab_data1$number_diagnoses <- scale(diab_data1$number_diagnoses,center = TRUE, scale = TRUE)

#removing admission_source_id as this is not the important variable and it will improve the model performance

diab_data1 <- diab_data1[,c(-6)]

#creating Dummy Variables
library(dummies)
#Conversion of factor to variables to dummy variables
# creating dummy variables for factor attributes
diab_data_fin <- dummy.data.frame(diab_data1)

set.seed(100)

indices = sample.split(diab_data_fin$readmitted, SplitRatio = 0.7)
train = diab_data_fin[indices,]
test = diab_data_fin[!(indices),]



#-------------------------------------------------------------------------------------------------------------
#                           Logistic Regression: modelling starts 
#-------------------------------------------------------------------------------------------------------------

#Initial model
model_1 = glm(readmitted ~ ., data = train, family = "binomial")
summary(model_1) #AIC 90397


# Stepwise selection
library("MASS")

model_2<- stepAIC(model_1, direction="both")
summary(model_2)--90375
sort((vif(model_2)), decreasing = TRUE) #sorting and checking for the highest VIF valued variable


model_3 <-glm(formula = readmitted ~  discharge_disposition_id28 
              + change                      
              +discharge_disposition_id20  
              +admission_type_id3          
              +`age[0-10)`                 
              +`age[40-50)`                
              +`A1Cresult>8`               
              +discharge_disposition_id4   
              +admission_type_id5          
              +discharge_disposition_id27  
              +discharge_disposition_id19  
              +genderFemale                
              +combordity1                
              +admission_type_id2          
              +admission_type_id1          
              +`age[50-60)`                
              +num_lab_procedures          
              +insulinNo                   
              +discharge_disposition_id2   
              +admission_type_id7          
              +raceHispanic                
              +combordity2                 
              +discharge_disposition_id25  
              +A1CresultNone               
              +discharge_disposition_id1   
              +time_in_hospital            
              +discharge_disposition_id23  
              +admission_type_id6         
              +`age[60-70)`                
              +insulinSteady               
              +discharge_disposition_id18  
              +discharge_disposition_id3   
              +`age[80-90)`                
              +`age[70-80)`                
              +num_procedures              
              +raceAfricanAmerican         
              +combordity0                 
              +diabetesMed                 
              +raceCaucasian               
              +number_outpatient           
              +discharge_disposition_id13  
              +discharge_disposition_id14  
              +number_emergency            
              +number_diagnoses            
              +discharge_disposition_id11  
              +number_inpatient ,
              family = "binomial", data = train)


summary(model_3)#--90375
sort((vif(model_3)), decreasing = TRUE)
 
#removing admission_type_id3 , admission_type_id2,admission_type_id1 as VIF is high and is in-significant



model_4 <-glm(formula = readmitted ~  discharge_disposition_id28 
              + change                      
              +discharge_disposition_id20  
              +`age[0-10)`                 
              +`age[40-50)`                
              +`A1Cresult>8`               
              +discharge_disposition_id4   
              +admission_type_id5          
              +discharge_disposition_id27  
              +discharge_disposition_id19  
              +genderFemale                
              +combordity1                
              +`age[50-60)`                
              +num_lab_procedures          
              +insulinNo                   
              +discharge_disposition_id2   
              +admission_type_id7          
              +raceHispanic                
              +combordity2                 
              +discharge_disposition_id25  
              +A1CresultNone               
              +discharge_disposition_id1   
              +time_in_hospital            
              +discharge_disposition_id23  
              +admission_type_id6         
              +`age[60-70)`                
              +insulinSteady               
              +discharge_disposition_id18  
              +discharge_disposition_id3   
              +`age[80-90)`                
              +`age[70-80)`                
              +num_procedures              
              +raceAfricanAmerican         
              +combordity0                 
              +diabetesMed                 
              +raceCaucasian               
              +number_outpatient           
              +discharge_disposition_id13  
              +discharge_disposition_id14  
              +number_emergency            
              +number_diagnoses            
              +discharge_disposition_id11  
              +number_inpatient ,
              family = "binomial", data = train)


summary(model_4)#--90419
sort((vif(model_4)), decreasing = TRUE)

#removing discharge_disposition_id28 , change ,discharge_disposition_id20,discharge_disposition_id4,
#discharge_disposition_id27,discharge_disposition_id19 , #discharge_disposition_id11 as these are insignificant





model_5 <-glm(formula = readmitted ~  
              + change                      
              +`age[0-10)`                 
              +`age[40-50)`                
              +`A1Cresult>8`               
              +admission_type_id5          
              +genderFemale                
              +combordity1                
              +`age[50-60)`                
              +num_lab_procedures          
              +insulinNo                   
              +discharge_disposition_id2   
              +admission_type_id7          
              +raceHispanic                
              +combordity2                 
              +discharge_disposition_id25  
              +A1CresultNone               
              +discharge_disposition_id1   
              +time_in_hospital            
              +discharge_disposition_id23  
              +admission_type_id6         
              +`age[60-70)`                
              +insulinSteady               
              +discharge_disposition_id18  
              +discharge_disposition_id3   
              +`age[80-90)`                
              +`age[70-80)`                
              +num_procedures              
              +raceAfricanAmerican         
              +combordity0                 
              +diabetesMed                 
              +raceCaucasian               
              +number_outpatient           
              +discharge_disposition_id13  
              +discharge_disposition_id14  
              +number_emergency            
              +number_diagnoses            
              +number_inpatient ,
              family = "binomial", data = train)


summary(model_5)#--92197
sort((vif(model_5)), decreasing = TRUE)

#removing discharge_disposition_id2 , discharge_disposition_id3 ,discharge_disposition_id18 , discharge_disposition_id2




model_6 <-glm(formula = readmitted ~  
                + change                      
              +`age[0-10)`                 
              +`age[40-50)`                
              +`A1Cresult>8`               
              +admission_type_id5          
              +genderFemale                
              +combordity1                
              +`age[50-60)`                
              +num_lab_procedures          
              +insulinNo                   
               +admission_type_id7          
              +raceHispanic                
              +combordity2                 
              +discharge_disposition_id25  
              +A1CresultNone               
              +discharge_disposition_id1   
              +time_in_hospital            
              +discharge_disposition_id23  
              +admission_type_id6         
              +`age[60-70)`                
              +insulinSteady               
              +`age[80-90)`                
              +`age[70-80)`                
              +num_procedures              
              +raceAfricanAmerican         
              +combordity0                 
              +diabetesMed                 
              +raceCaucasian               
              +number_outpatient           
              +discharge_disposition_id13  
              +discharge_disposition_id14  
              +number_emergency            
              +number_diagnoses            
              +number_inpatient ,
              family = "binomial", data = train)


summary(model_6)#--92196
sort((vif(model_6)), decreasing = TRUE)

#removing change , admission_type_id5 , admission_type_id7 



model_7 <-glm(formula = readmitted ~  
                    +`age[0-10)`                 
              +`age[40-50)`                
              +`A1Cresult>8`               
               +genderFemale                
              +combordity1                
              +`age[50-60)`                
              +num_lab_procedures          
              +insulinNo                   
              +raceHispanic                
              +combordity2                 
              +discharge_disposition_id25  
              +A1CresultNone               
              +discharge_disposition_id1   
              +time_in_hospital            
              +discharge_disposition_id23  
              +admission_type_id6         
              +`age[60-70)`                
              +insulinSteady               
              +`age[80-90)`                
              +`age[70-80)`                
              +num_procedures              
              +raceAfricanAmerican         
              +combordity0                 
              +diabetesMed                 
              +raceCaucasian               
              +number_outpatient           
              +discharge_disposition_id13  
              +discharge_disposition_id14  
              +number_emergency            
              +number_diagnoses            
              +number_inpatient ,
              family = "binomial", data = train)


summary(model_7)#--92207
sort((vif(model_7)), decreasing = TRUE)

#removing `age[0-10)`  , `A1Cresult>8`  , num_lab_procedures , discharge_disposition_id25




model_8 <-glm(formula = readmitted ~  
                   +`age[40-50)`                
                +genderFemale                
              +combordity1                
              +`age[50-60)`                
                   +insulinNo                   
              +raceHispanic                
              +combordity2                 
                +A1CresultNone               
              +discharge_disposition_id1   
              +time_in_hospital            
              +discharge_disposition_id23  
              +admission_type_id6         
              +`age[60-70)`                
              +insulinSteady               
              +`age[80-90)`                
              +`age[70-80)`                
              +num_procedures              
              +raceAfricanAmerican         
              +combordity0                 
              +diabetesMed                 
              +raceCaucasian               
              +number_outpatient           
              +discharge_disposition_id13  
              +discharge_disposition_id14  
              +number_emergency            
              +number_diagnoses            
              +number_inpatient ,
              family = "binomial", data = train)


summary(model_8)#--92218
sort((vif(model_8)), decreasing = TRUE)


#removing `age[40-50)` , genderFemale ,combordity1 , A1CresultNone


model_9 <-glm(formula = readmitted ~  
               +`age[50-60)`                
              +insulinNo                   
              +raceHispanic                
              +combordity2                 
              +discharge_disposition_id1   
              +time_in_hospital            
              +discharge_disposition_id23  
              +admission_type_id6         
              +`age[60-70)`                
              +insulinSteady               
              +`age[80-90)`                
              +`age[70-80)`                
              +num_procedures              
              +raceAfricanAmerican         
              +combordity0                 
              +diabetesMed                 
              +raceCaucasian               
              +number_outpatient           
              +discharge_disposition_id13  
              +discharge_disposition_id14  
              +number_emergency            
              +number_diagnoses            
              +number_inpatient ,
              family = "binomial", data = train)


summary(model_9)#--92218
sort((vif(model_9)), decreasing = TRUE)


#removing raceCaucasian and raceAfricanAmerican as it has high VIF 




model_10 <-glm(formula = readmitted ~  
                +`age[50-60)`                
              +insulinNo                   
              +raceHispanic                
              +combordity2                 
              +discharge_disposition_id1   
              +time_in_hospital            
              +discharge_disposition_id23  
              +admission_type_id6         
              +`age[60-70)`                
              +insulinSteady               
              +`age[80-90)`                
              +`age[70-80)`                
              +num_procedures              
                 +combordity0                 
              +diabetesMed                 
               +number_outpatient           
              +discharge_disposition_id13  
              +discharge_disposition_id14  
              +number_emergency            
              +number_diagnoses            
              +number_inpatient ,
              family = "binomial", data = train)


summary(model_10)#--92341
sort((vif(model_10)), decreasing = TRUE)


#removing raceHispanic , age[50-60)` , insulinNo 


model_11 <-glm(formula = readmitted ~  
                combordity2                 
               +discharge_disposition_id1   
               +time_in_hospital            
               +discharge_disposition_id23  
               +admission_type_id6         
               +`age[60-70)`                
               +insulinSteady               
               +`age[80-90)`                
               +`age[70-80)`                
               +num_procedures              
               +combordity0                 
               +diabetesMed                 
               +number_outpatient           
               +discharge_disposition_id13  
               +discharge_disposition_id14  
               +number_emergency            
               +number_diagnoses            
               +number_inpatient ,
               family = "binomial", data = train)


summary(model_11)#--92355
sort((vif(model_11)), decreasing = TRUE)


#at this point all the variables are significant and VIF is below 2

#Estimate Std. Error z value Pr(>|z|)    
#(Intercept)                -0.088007   0.022040  -3.993 6.52e-05 ***
# combordity2                -0.068515   0.019538  -3.507 0.000453 ***
# discharge_disposition_id1   0.058239   0.017285   3.369 0.000754 ***
# time_in_hospital            0.068194   0.008500   8.023 1.04e-15 ***
# discharge_disposition_id23 -0.423452   0.126266  -3.354 0.000798 ***
# admission_type_id6          0.396381   0.035136  11.281  < 2e-16 ***
# `age[60-70)`                0.110759   0.021533   5.144 2.69e-07 ***
# insulinSteady              -0.133691   0.018257  -7.323 2.43e-13 ***
# `age[80-90)`                0.170725   0.024288   7.029 2.08e-12 ***
# `age[70-80)`                0.182950   0.020968   8.725  < 2e-16 ***
# num_procedures             -0.092129   0.008168 -11.280  < 2e-16 ***
# combordity0                -0.240825   0.022294 -10.802  < 2e-16 ***
# diabetesMed                -0.298106   0.020223 -14.741  < 2e-16 ***
# number_outpatient           0.114396   0.009215  12.414  < 2e-16 ***
# discharge_disposition_id13 -2.006736   0.182098 -11.020  < 2e-16 ***
# discharge_disposition_id14 -2.536171   0.224088 -11.318  < 2e-16 ***
# number_emergency            0.220039   0.013884  15.848  < 2e-16 ***
# number_diagnoses            0.192988   0.008982  21.485  < 2e-16 ***
# number_inpatient            0.469258   0.010478  44.787  < 2e-16 ***
  

final_model<- model_11



#----------------------------------------------------------------------------------------------
#                         Start of Model Evaluation
#-----------------------------------------------------------------------------------------------

#predicted probabilities of readmitted for test data
test1<-test[,-70]
test_pred = predict(final_model, type = "response", newdata = test[,-70])

# Let's see the summary 
summary(test_pred)

test$prob <- test_pred

# Let's use the probability cutoff of 50%.
test_pred_readmitted <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_readmitted <- factor(ifelse(test$readmitted==1,"Yes","No"))

table(test_actual_readmitted,test_pred_readmitted)


test_conf1 <- confusionMatrix(test_pred_readmitted, test_actual_readmitted, positive = "Yes")
test_conf1
#Sensitivity :0.4116           
#Specificity : 0.7982  
#accuracy : 0.62


#Checking for other levels of cut off

#At 0.40 
test_pred_readmitted_1 <- factor(ifelse(test_pred >= 0.40, "Yes", "No")) 
test_conf2 <- confusionMatrix(test_pred_readmitted_1, test_actual_readmitted, positive = "Yes")
test_conf2

#Sensitivity : 0.7531        
#Specificity :  0.4601   

# finding the optimal probalility cutoff value

perform_fn <- function(cutoff) 
{
  predicted_readmitted <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_readmitted, test_actual_readmitted, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Summary of test probability

summary(test_pred)
s = seq(.01,.80,length=100)
OUT = matrix(0,100,3)

for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.05)]
cutoff #0.4409091

# Let's choose a cutoff value of 0.4409091
test_cutoff_readmitted <- factor(ifelse(test_pred >=0.4409091, "Yes", "No"))

conf_final <- confusionMatrix(test_cutoff_readmitted, test_actual_readmitted, positive = "Yes")

acc <- conf_final$overall[1]
acc #0.6152637 

sens <- conf_final$byClass[1]
sens # 0.6057849 

spec <- conf_final$byClass[2]
spec #0.6233672 

#final accuracy is 0.6152637



### ----------------KS -statistic - Test Data ######

library(ROCR)

test_cutoff_readmitted <- ifelse(test_cutoff_readmitted=="Yes",1,0)
test_actual_readmitted <- ifelse(test_actual_readmitted=="Yes",1,0)

#on testing  data
pred_object_test<- prediction(test_cutoff_readmitted, test_actual_readmitted)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

#Plot Receiver Operating Characteristics (ROC) Curve: AUC calculation 


plot(performance_measures_test, type = "b", col = "red", lwd=1.5,
     main = "ROC Curve",
     ylab = "Sensitivity:TPR", 
     xlab = "(1 - Specificity):FPR")
abline(0,1, lty = 8, col = "grey", untf = T)
auc<-performance(pred_object_test,"auc")
auc.value <- unlist(auc@y.values)
text(0.8, 0.23, labels=sprintf("AUC: %0.3f", auc.value))

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test) # 0.2291521

#-----------------------------------------------------------------------------------------------
                          # Lift & Gain Chart 
                          # plotting the lift chart
#-----------------------------------------------------------------------------------------------

lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}
readmitted_decile = lift(test_actual_readmitted, test_pred, groups = 10)

#majority of resp is in top 4 deciles
#plot the lift chart 

plot(readmitted_decile$Cumlift, type="l", lwd=2, col="red",
     xlim = c(0,10),
     ylim = c(0,4),
     main = "Lift Chart",
     xlab = "Decile",
     ylab = "Lift")
abline(h=1, col="brown")
axis(1, 1:10)
abline(h=0:10, v=0:10, lty=3)


#Plot Gain Chart 
install.packages("InformationValue")
library(InformationValue)
ks_plot(test_actual_readmitted, test_cutoff_readmitted) # Gain chart plot


#--------------------end of model evaluation for logistic regression 



#--------------------start of Random forest ------------------

# Build the random forest
library(randomForest)
set.seed(71)
str(diab_data_rf)
diab_data_rf$readmitted<-as.factor(diab_data_rf$readmitted)



set.seed(100)
indices = sample.split(diab_data_rf$readmitted, SplitRatio = 0.7)
trainrf = diab_data_rf[indices,]
testtf = diab_data_rf[!(indices),]
data.rf <- randomForest(readmitted ~ ., data=trainrf, proximity=FALSE,
                        ntree=1000, mtry=5, do.trace=TRUE, na.action=na.omit)
final_model1<-data.rf
testPred <- predict(data.rf, newdata=testtf)
table(testPred, testtf$readmitted)

testtf$predicted_readmitted<-testPred


#from this we can conclude that accuracy is 

#accuracy = (11478+7949) / ( 11478+7949 +4981 + 6122 ) = .6363
#Specificity = 11477 / (11477 +4981 ) = 0.6973085
#Sensitivity = 7949 / (6122 +7949) = 0.5649208



#-------------end of random forest ------------------



#-------------- comparing the Logistic regression and random forest 

# there is not much drastic improvement in random forest , the accuracy is 0.6363 while GLM has 0.6152637 
# however the performance of Random forest is very good which took only 15 min ; GLM took 3.5 hrs
# since accuracy is high for random forest we can take random forest as the final model

#-------------------end of comparasion------------------------



#----stratifying into risk buckets----------

#using random forest

diab_data_rf$predicted_readmitt_YESNO<-predict(data.rf, newdata=diab_data_rf) #calculating prob YES/NO on entire dataset
diab_data_rf$predicted_readmitt_prob<-predict(data.rf, diab_data_rf, type="prob")#calculating numeric probability on entire dataset
diab_data_rf$predicted_readmitt_prob<-1-diab_data_rf$predicted_readmitt_prob # reversing the probability 

diab_data_rf$patient_nbr <-diab_data$patient_nbr


diab_data_rf$RISK_CATEGORY <-ifelse(diab_data_rf$predicted_readmitt_prob>=0.7 ,'High Risk',
                               ifelse(diab_data_rf$predicted_readmitt_prob<0.7 & diab_data_rf$predicted_readmitt_prob>=0.3,'Medium Risk',
                                      ifelse(diab_data_rf$predicted_readmitt_prob <0.3 ,'Low Risk','OTH')))

diab_data_rf$RISK_CATEGORY1<-as.factor(diab_data_rf$RISK_CATEGORY[,1])


#plotting risk stratification against various attributes
plot_grid(ggplot(diab_data_rf, aes(x=diab_data_rf$gender,fill=diab_data_rf$RISK_CATEGORY1))+ geom_bar(),
          ggplot(diab_data_rf, aes(x=diab_data_rf$age,fill=diab_data_rf$RISK_CATEGORY1))+ geom_bar()+bar_theme1,
          ggplot(diab_data_rf, aes(x=diab_data_rf$time_in_hospital,fill=diab_data_rf$RISK_CATEGORY1))+ geom_bar()+bar_theme1,
          ggplot(diab_data_rf, aes(x=diab_data_rf$num_lab_procedures,fill=diab_data_rf$RISK_CATEGORY1))+ geom_bar()+bar_theme1,
          ggplot(diab_data_rf, aes(x=diab_data_rf$A1Cresult,fill=diab_data_rf$RISK_CATEGORY1))+ geom_bar()+bar_theme1,
          align = "h") 


#----end of risk stratification -----------------------

