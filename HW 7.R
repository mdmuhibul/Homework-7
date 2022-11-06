Md Muhibul Islam
Homework 7
Introduction to Econometrics
ECO B2000
November 05, 2022

Question 01:
Lab Group: Zakaria Sule , Liam, Victoria K , Amira Elmakawy, Md Muhibul Islam

Question 02:
Lab 6

attach(Household_Pulse_data)
summary of the people who received the vaccine in the household pulse data first:

summary(Household_Pulse_data$RECVDVACC)
create the data to use:

Household_Pulse_data$vaxx <- (Household_Pulse_data$RECVDVACC == "yes got vaxx")
is.na(Household_Pulse_data$vaxx) <- which(Household_Pulse_data$RECVDVACC == "NA")

generic format for logit model :
model_logit1 <- glm(RECVDVACC ~ EEDUC, family = binomial, data = Household_Pulse_data)
summary(model_logit1)
summary(as.numeric(Household_Pulse_data$RECVDVACC))
table(Household_Pulse_data$vaxx,Household_Pulse_data$EEDUC)

#since we got that RECVACC has 3 levels and we're doing only 2 levels ,
so we remove the "NA's" by :
  
Household_Pulse_data$vaxx <- (Household_Pulse_data$RECVDVACC == "yes got vaxx")
summary(Household_Pulse_data$vaxx)  

now we did it as two levels.
We want to subset :

pick_use1 <- (Household_Pulse_data$TBIRTH_YEAR <2000)
dat_use1 <- subset(Household_Pulse_data, pick_use1)
dat_use1$RECVDVACC <- droplevels(dat_use1$RECVDVACC)

So start from this baseline model and launch ahead
model_logit2 <- glm(vaxx ~ EEDUC + MS + RRACE +  GENID_DESCRIBE,
                    family = binomial, data = dat_use1)
summary(model_logit2)


to_be_predicted2 <- data.frame(EEDUC = "some coll", MS = "divorced", RRACE = "White", GENID_DESCRIBE = "female", data = dat_use1)
to_be_predicted$yhat <- predict(model_logit2, to_be_predicted, type = "response)

summary(to_be_predicted$yhat)

The predicted probability of the divorced white females with some college who got vaxxed is 0.828.
Almost all of the variables are statistically significant with the vaccination status.
 
 new_data_to_be_predicted <- data.frame(TBIRTH_YEAR = 1990,
                                       EEDUC = factor("bach deg", levels = levels(dat_use1$EEDUC)),
                                       MS = factor("never",levels = levels(dat_use1$MS)),
                                       RRACE = factor("Black",levels = levels(dat_use1$RRACE)),
                                       RHISPANIC = factor("Hispanic",levels = levels(dat_use1$RHISPANIC)),
                                       GENID_DESCRIBE = factor("male", levels = levels(dat_use1$GENID_DESCRIBE)

predict(model_logit1,new_data_to_be_predicted)
new_data_to_be_predicted <- data.frame(TBIRTH_YEAR = 1996,
                                       EEDUC = factor("some coll", levels = levels(dat_use1$EEDUC)),
                                       MS = factor("married",levels = levels(dat_use1$MS)),
                                       RRACE = factor("white",levels = levels(dat_use1$RRACE)),
                                       RHISPANIC = factor("Not Hispanic",levels = levels(dat_use1$RHISPANIC)),
                                       GENID_DESCRIBE = factor("Female", levels = levels(dat_use1$GENID_DESCRIBE))                                       
predict(model_logit1,new_data_to_be_predicted)

# Probit model : 

model_probit1 <- glm(vaxx ~ EEDUC + MS + RRACE  + GENID_DESCRIBE + ANYWORK*INCOME,
                     family = binomial (link = 'probit'), data = dat_use1)

summary (model_probit1)

summary(model_probit1)
to_be_predicted3<- data.frame(EEDUC = "some coll", MS = "separated", RRACE = "Asian", GENID_DESCRIBE = "female",
                              ANYWORK = "no employment in last 7 days", INCOME= "HH income $150 - 199
                                
                                ",  data = dat_use1)
to_be_predicted3$yhat<-predict(model_probit1, to_be_predicted3, type="response")
summary(to_be_predicted3$yhat)


Question 03: Final project 

Project group: Project group : Md Muhibul Islam, Amira Elmakawy, Zakaria Sule

Article 1: Influence of covid-19 on labor market in USA. 

In January 2022, Brookings Metro published a report that assessed the impact of long Covid on the labor market. 
Data on the condition’s prevalence was limited, so the eport used various studies to make a conservative estimate: 

1.6 million full-time equivalent workers 
could be out of work due to long Covid. With 10.6 million unfilled jobs at the time, long Covid potentially accounted for 15% of the labor shortage. 
This June, the Census Bureau finally added four questions about long Covid to its Household Pulse Survey (HPS), giving researchers a better 
understanding of the condition’s prevalence. This report uses the new data to assess the labor market impact and economic burden of long Covid, 
and finds that: 

Around 16 million working-age Americans (those aged 18 to 65) have long Covid today.
Of those, 2 to 4 million are out of work due to long Covid.
The annual cost of those lost wages alone is around $170 billion a year (and potentially as high as $230 billion). 

https://www.brookings.edu/research/new-data-shows-long-covid-is-keeping-as-many-as-4-million-people-out-of-work/ 


Article 2: Global inflation due to covid

One of the major concern is worldwide inflation is increasing day by day where different products prices
are rising. In all locations but Europe and Greater China, inflation is the most-cited threat to
respondents’ economies over the next 12 months . In Europe, volatile energy
prices and inflation are the growth risks cited most often, with geopolitical instability
or conflicts a more distant third. In Greater China,2 the COVID-19 pandemic remains
the most reported risk, cited by nearly half of respondents for the second quarter in a row.

The COVID-19 pandemic has caused an unconventional recession, and we
do not expect the recovery will be typical either. While the paramount
policy goals are to control the virus, get to full employment, and make the
necessary investments for a more resilient and inclusive recovery,
economic uncertainties and risks demand careful attention going forward.


https://www.mckinsey.com/capabilities/strategy-and-corporate-finance/our-insights/the-coronavirus-effect-on-global-economic-sentiment



