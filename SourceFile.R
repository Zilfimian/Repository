#setwd("C:/Users/Lusine Zilfimian/BIG BLACK HOLE/GitHub Desktop/DataMotus/2020.6 Covid 19/Final Report")

labName <- function(x){
  
  set_label(x,
  
  # ================== Demographic characteristics ==================
  Q1.Marz = "Where are you living/residence area?",
  Q2.Gender = "Sex",
  Q3.Age_group = "How old are you?",
  Q4.Mar.Stat = "What is your marital status?",
  Q5.Educ = "What is the highest level of education that you have completed?", 
  Q6.Ethnic = "Current nationality or ethnic group",
  Q7.Home = "How many people live with you?", 
  Q7.1.Home0_17 = "Number of people living with you by age groups: Number of children 0-17", 
  Q7.1.Home18_64 = "Number of people living with you by age groups: Number of adults 18-64", 
  Q7.1.Home65 = "Number of people living with you by age groups:	Number of elderly 65+", # Q7.1
  # =============== Main source of information  ====================
  Q8.CovidSource = "What is your main source of information regarding COVID19 (risks, recommended preventive action, coping strategies)?",
  Q8.1.SourceEval = "How would you rate the information you received?",
  # ============== Employment and livelihood resources =============
  Q9.ActivityBefore = "How would you best describe your employment status during a typical week prior to the spread of Covid-19?", 
  Q9.1.JobTimeChange = "Since the spread of COVID19, has the number of hours devoted to paid work changed?", 
  Q9.2.Leave = "Since the spread of COVID19, have you been imposed to take a leave?",
  Q9.3.Pension = "Does your employer pay contributions toward pension on your behalf?",
  Q9.4.JobLocationChange = "Since the spread of Covid-19, are there any changes in your typical place of work?",
  Q9.5.WithoutWork = "If you could not work for at least two weeks because of the coronavirus what would most likely happen to your earnings?",
  Q9.6.Business = "Is your business formally registered?", 
  Q9.7.OnBusiness = "How is your business affected after the spread of COVID-19?",
  Q10.HealthInsh = "Are you currently covered by any form of health insurance or health plan?",
  Q11.AidGov = "Do you receive any benefits and/or any financial support from the Government, local municipalities since the spread of COVID19?",
  Q12.AidGovFood = "Do you receive any in-kind support from the Government and /or local municipalities since the spread of COVID19? - Yes, food",
  Q12.AidGovProtect = "Do you receive any in-kind support from the Government and /or local municipalities since the spread of COVID19? - Yes, supplies for prevention (gloves, masks, sanitizer, etc.)", 
  Q12.AidGovPersHyg = "Do you receive any in-kind support from the Government and /or local municipalities since the spread of COVID19? - Yes, personal hygiene supplies (menstrual supplies, baby diapers, etc.)",
  Q12.AidGovNot = "Do you receive any in-kind support from the Government and /or local municipalities since the spread of COVID19? - No",
  Q12.AidGovDNK = "Do you receive any in-kind support from the Government and /or local municipalities since the spread of COVID19? - I don’t know", # can be skipped
  
  Q12.1.AidNGOFood = "Do you receive any in-kind support from Non-Governmental/civil society organization or other non-profit organizations? - Yes, food",
  Q12.1.AidNGOProtect = "Do you receive any in-kind support from Non-Governmental/civil society organization or other non-profit organizations? - Yes, supplies for prevention (gloves, masks, sanitizer, etc.)",
  Q12.1.AidNGOPersHyg= "Do you receive any in-kind support from Non-Governmental/civil society organization or other non-profit organizations? - Yes, personal hygiene supplies (menstrual supplies, baby diapers, etc.)",
  Q12.1.AidNGONot = "Do you receive any in-kind support from Non-Governmental/civil society organization or other non-profit organizations? - No",
  Q12.1.AidNGODNK = "Do you receive any in-kind support from Non-Governmental/civil society organization or other non-profit organizations? - I don’t know", # can be skipped
  
  Q13.IncAgro = "As a result of COVID19, how have the following personal resources been affected? - Income/earnings from farming",
  Q13.IncPB = "As a result of COVID19, how have the following personal resources been affected? - Income/earnings from own business/family business, freelancer activity",
  Q13.IncWage = "As a result of COVID19, how have the following personal resources been affected? - Income/earnings from a paid job",
  Q13.IncSavings = "As a result of COVID19, how have the following personal resources been affected? - Income from properties, investments or savings", 
  Q13.IncSoc = "As a result of COVID19, how have the following personal resources been affected? - Pensions, other social payments", 
  Q13.ArgoSpec = "As a result of COVID19, how have the following personal resources been affected? - Food from farming, raising animals or fishing", 
  Q13.IncIntRemit = "As a result of COVID19, how have the following personal resources been affected? - Money or goods received from people living abroad", 
  Q13.IncDomRemit = "As a result of COVID19, how have the following personal resources been affected? -  Support from family/friends in the country (money. food, etc.)",
  Q13.AidGov = "As a result of COVID19, how have the following personal resources been affected? - Government support", # same as Q11.AidGov should be skipped
  Q13.AidNGO = "As a result of COVID19, how have the following personal resources been affected? -  Support/Charity from NGOs or other organizations", # same as Q12.1NGO  should be skipped
  Q13.WageInt = "As a result of COVID19, how have the following personal resources been affected? - Income from working abroad",
  
  # ================ Distribution of Household Chores ================
  Q14.TimeHWFood = "As a result of COVID19, has the number of hours devoted to the following activities changed? - Cooking and serving meals", 
  Q14.TimeHWClean = "As a result of COVID19, has the number of hours devoted to the following activities changed? - Cleaning and maintaining own dwelling and surroundings (e.g. clothes, household)",
  Q14.TimeHWHHmanag = "As a result of COVID19, has the number of hours devoted to the following activities changed? - Household management (e.g. paying bills)", 
  Q14.TimeHWShop = "As a result of COVID19, has the number of hours devoted to the following activities changed? - Shopping for my family/household member", 
  Q14.TimeHWRow = "As a result of COVID19, has the number of hours devoted to the following activities changed? - Collecting water/firewood/fuel" , 
  Q14.TimeHWGame = "As a result of COVID19, has the number of hours devoted to the following activities changed? - Playing with, talking to and reading to children", 
  Q14.TimeHWHW = "As a result of COVID19, has the number of hours devoted to the following activities changed? - Instructing, teaching, training children", 
  Q14.TimeHWCareChild = "As a result of COVID19, has the number of hours devoted to the following activities changed? - Caring for children, including feeding, cleaning, physical care", 
  Q14.TimeHWCareOld = "As a result of COVID19, has the number of hours devoted to the following activities changed? - Assisting older/sick/disabled adults with medical care, feeding, cleaning, physical care", 
  Q14.TimeHWCareOldSup = "As a result of COVID19, has the number of hours devoted to the following activities changed? - Affective/emotional support for adult family members",
  
  Q14.TimeHWPet = "As a result of COVID19, has the number of hours devoted to the following activities changed? - Pet care",
  Q15.MostTime = "Since the spread of COVID19, in which activity do you spend the most time?", 
  
  Q16.RolesPart = "Since the spread of COVID19 have roles and responsibilities within the household been affected? - My partner helps me more with household chores and/or caring for family", 
  Q16.RolesDaugh = "Since the spread of COVID19 have roles and responsibilities within the household been affected? - My daughter(s) helps me more with household chores and/or caring for family", 
  Q16.RolesSon = "Since the spread of COVID19 have roles and responsibilities within the household been affected? - My son(s) helps me more with household chores and/or caring for family", 
  Q16.RolesOther = "Since the spread of COVID19 have roles and responsibilities within the household been affected?- Other family/household members help me more with household chores and/or caring for family", 
  Q16.RolesWorker = "Since the spread of COVID19 have roles and responsibilities within the household been affected? - Hired a domestic worker/babysitter/nurse", 
  Q16.RolesWorkerL = "Since the spread of COVID19 have roles and responsibilities within the household been affected? - Domestic worker/babysitter/nurse works longer hours with us",  
  Q16.RolesWorkerN = "Since the spread of COVID19 have roles and responsibilities within the household been affected?- Domestic worker/babysitter/nurse no longer works with us",  
  Q16.RolesOwn = "Since the spread of COVID19 have roles and responsibilities within the household been affected? - I am on my own, no one can longer help me with household chores and caring for family",
  
  # ============ Access to basic services and safety ==========
  
  Q17.Ill = "As a result of COVID19, did you (personally) experience any of the following: Physical illness",  
  Q17.IllFam = "As a result of COVID19, did you (personally) experience any of the following: Illness of a family/household member", 
  Q17.Death = "As a result of COVID19, did you (personally) experience any of the following: Death of a family/household member", 
  Q17.Mental = "As a result of COVID19, did you (personally) experience any of the following: Psychological/Mental/Emotional health was affected (e.g. stress, anxiety, etc.)",  
  Q17.Migrate = "As a result of COVID19, did you (personally) experience any of the following: Migrated/moved to different geographical area within the same country",  
  Q17.FromAbroad = "As a result of COVID19, did you (personally) experience any of the following: Recently returned from abroad",  
  Q17.School = "As a result of COVID19, did you (personally) experience any of the following: Children’s school was cancelled or reduced",
  
  
  Q18.DifFood = "As a result of COVID19, did you (personally) experience difficulties in accessing basic services: Food products/supply", 
  Q18.DifMed = "As a result of COVID19, did you (personally) experience difficulties in accessing basic services: Medical supplies for personal protection (masks, gloves, etc.)", 
  Q18.DifHealth = "As a result of COVID19, did you (personally) experience difficulties in accessing basic services: Health services/assistance for myself and/or my family member" , 
  Q18.DifHyg = "As a result of COVID19, did you (personally) experience difficulties in accessing basic services: Hygiene and sanitary products (soap, water treatment tabs, menstrual products)", 
  Q18.DifTrans = "As a result of COVID19, did you (personally) experience difficulties in accessing basic services: Public transport", 
  Q18.DifWater = "As a result of COVID19, did you (personally) experience difficulties in accessing basic services: Water supply", 
  Q18.DifSocial = "As a result of COVID19, did you (personally) experience difficulties in accessing basic services: Social services/assistance for myself and/or family member", 
  Q18.DifElec = "As a result of COVID19, did you (personally) experience difficulties in accessing basic services: Electricity supply", 
  Q18.DifGas = "As a result of COVID19, did you (personally) experience difficulties in accessing basic services: Gas supply",
  
  
  Q19.RestrBasic = "If restrictive measures related to spread of COVID-19 continue, what would most likely to happen to your financial situation? - Would be difficult to keep up with basic expenses",    
  Q19.RestrRent = "If restrictive measures related to spread of COVID-19 continue, what would most likely to happen to your financial situation? - Would be difficult to pay for renting",  
  Q19.RestrUtil = "If restrictive measures related to spread of COVID-19 continue, what would most likely to happen to your financial situation? - Would be difficult to pay for utilities",
  Q19.RestrHealth = "If restrictive measures related to spread of COVID-19 continue, what would most likely to happen to your financial situation? - Will have to stop seeking health services/assistance" ,    
  Q19.RestrFriend = "If restrictive measures related to spread of COVID-19 continue, what would most likely to happen to your financial situation? - Will have to ask help from relatives and friends",    
  Q19.RestrAuth = "If restrictive measures related to spread of COVID-19 continue, what would most likely to happen to your financial situation? - Will have to ask help from the local authorities", 
  Q19.RestrLoan = "If restrictive measures related to spread of COVID-19 continue, what would most likely to happen to your financial situation? - Will have to take a loan",    
  Q19.RestrAltern = "If restrictive measures related to spread of COVID-19 continue, what would most likely to happen to your financial situation? - Will have to transform to alternative heating means",   
  
  
  Q20.Discrim = "Have you felt increase of any form of discrimination, prejudice in the country/area you live after the spread of COVID-19?",  
  Q21.Violence = "Have you felt/heard about increase of domestic violence since the spread of COVID-19?", 
  Q21.1.Help.Support  = "Do you know where to seek help and support in case of someone experiencing domestic violence such as hotlines, psychological and police support?",
  
  
  # To women only
  Q22.Gynecol = "Since the spread of COVID19, did you personally experience difficulties in accessing the gynecological and obstetric care services?",  
  Q22.1.Contr = "Since the spread of COVID19, did you personally experience difficulties in accessing contraceptives?", 
  Q23.Education = "How the spread of COVID19 has influenced on your education?", 
  Q24.CivilHelpCom = "How is your civil activity is in terms of COVID19 spread? - I actively help my community, by sewing and distributing masks, collecting money for helping medical workers and hospitals",  
  Q24.CivilHelpEld = "How is your civil activity is in terms of COVID19 spread? - I am helping elderly people in my community",  
  Q24.CivilMember = "How is your civil activity is in terms of COVID19 spread? - I am an member of civil organization, which helps people in need due to COVID19",  
  Q24.CivilNews = "How is your civil activity is in terms of COVID19 spread? - I actively discuss and disseminate news about COVID19 on social media and other websites",
  Q24.CivilActNone = "How is your civil activity is in terms of COVID19 spread? - None of the above" , 
  Q25.CommunityServices  = "Would you like to answer to a few questions about community services?", # should be skipped
  
  Q26.CSPreEduc = "Your opinion about community services for these months - Preschool education",  
  Q26.CSSchool = "Your opinion about community services for these months - Schools / professional, art, general education, music, etc.",  
  Q26.CSWaste = "Your opinion about community services for these months - Waste collection",  
  Q26.CSStreet = "Your opinion about community services for these months - Street care and lighting",  
  Q26.CSPubTran = "Your opinion about community services for these months - Public transport",  
  Q26.CSMainten = "Your opinion about community services for these months - Maintenance of residential buildings",  
  Q26.CSLand = "Your opinion about community services for these months - Landscaping",  
  Q26.CSSanit = "Your opinion about community services for these months - Sanitary services",  
  Q26.CSComMed = "Your opinion about community services for these months - Community Medical Service",  
  Q26.CSTax = "Your opinion about community services for these months - Local tax administration",  
  Q26.CSInfo = "Your opinion about community services for these months - Providing information",  
  Q26.CSDecis = "Your opinion about community services for these months - Administrative decision making",  
  Q26.CSMArket = "Your opinion about community services for these months - Community market",  
  Q26.CSMES = "Your opinion about community services for these months - Emergency services - MES",  
  Q26.CSLaw = "Your opinion about community services for these months - Law enforcement services",  
  Q26.CSGas = "Your opinion about community services for these months - Gas supply",  
  Q26.CSPower = "Your opinion about community services for these months - Power supply", 
  Q26.CSWater = "Your opinion about community services for these months - Water and wastewater", 
  Q26.CSTV = "Your opinion about community services for these months - Communication and television broadcasting",
  Q27.MobileOwn = "Are you the registered owner of this mobile phone?")
  
  
}




#=================================================


map = function(x){
  
  load("Armenia1.rda")  
  Armenia1_UTM<-spTransform(Armenia1, CRS("+init=epsg:32737"))
  NAME_1<-c(Armenia1_UTM@data$NAME_1)
  
  x$Q1.Marz <- as.character(x$Q1.Marz)
  x$Q1.Marz[x$Q1.Marz=="Yerevan"] = "Erevan"
  
  
  
  x$Q1.Marz[x$Q1.Marz=="Kotayq"] = "Kotayk"
People <- plyr::join(data.frame("Q1.Marz"=NAME_1), as.data.frame(x %>%
    dplyr::select(Q1.Marz,Percent)), by = "Q1.Marz"
)%>%dplyr::pull(.)
df<-data.frame(NAME_1, People)
colnames(df)[2] <- "Percent"
Armenia1_UTM@data$id <- rownames(Armenia1_UTM@data)
Armenia1_UTM@data <- plyr::join(Armenia1_UTM@data, df, by="NAME_1")
Armenia1_df <- fortify(Armenia1_UTM)
Armenia1_UTM@data$id <- as.character(as.numeric(Armenia1_UTM@data$id) +1)
Armenia1_df <- plyr::join(Armenia1_df,Armenia1_UTM@data, by="id")


#============================================ Gegharkunik
rem <- Armenia1_df%>% filter(NAME_1 == "Gegharkunik", long >1042857.14 , long < 1057142.86, lat >14515789) %>% dplyr::select(long, lat)

Armenia1_df <- Armenia1_df[!Armenia1_df$long %in% rem$long & !Armenia1_df$lat %in% rem$lat,]

#============================================ 

#=================================== Tavush
# remtavush1 <- Armenia1_df%>% filter(long > 1021004, long < 1025959, lat < 14563430)%>% dplyr::select(long, lat) 
# remtavush2 <- Armenia1_df%>% filter(long > 1004310, long < 1007952,  lat<14567723, lat>14563384) %>% dplyr::select(long, lat) 
# Armenia1_df <- Armenia1_df[!Armenia1_df$long %in% remtavush1$long & !Armenia1_df$lat %in% remtavush1$lat,]
# 
# Armenia1_df <- Armenia1_df[!Armenia1_df$long %in% remtavush2$long & !Armenia1_df$lat %in% remtavush2$lat,]
# ==================================
#Region <- Armenia1_df$NAME_1

##########################################

centroid <- aggregate(cbind(long,lat) ~ Percent+NAME_1, data=Armenia1_df, FUN=mean)

ggplot() +
  geom_polygon(data = Armenia1_df , aes(x = long, y = lat, group = group, fill =
      Percent), color = "black", size = 0.25) +
  geom_text(data = centroid,size = 3, mapping = aes(x=long, y=lat, label= Percent))+
    theme(#aspect.ratio=1,
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank()
  )+
  labs(title=" ") + 
  scale_fill_distiller(palette = "PuBu", breaks = pretty_breaks(n = 5))


}







