

# Typhoon Haiyan
getGDELTdisaster(disaster.name="Haiyan",
              time.start="2013-10-28",
              disaster.start="2013-11-08",
              disaster.end="2013-11-08",
              time.end="2013-11-18",
              local.folder="C:/Users/TLScherer/Documents/GDELT",
              data.destination="H:/Work/SideProjects/GDELT/OCHA/testfolder/",
              targetcountry="Philippines")

load("H:/Work/SideProjects/GDELT/OCHA/testfolder/Haiyanloc")
load("H:/Work/SideProjects/GDELT/OCHA/testfolder/Haiyanact1")
load("H:/Work/SideProjects/GDELT/OCHA/testfolder/Haiyanact2")

CameoBreakdown(gdelt_df_name="Haiyanloc",
  disaster.name="Haiyan",
  time.start="2013-10-28",
  disaster.start="2013-11-08",
  disaster.end="2013-11-08",
  time.end="2013-11-18",
  data.destination="H:/Work/SideProjects/GDELT/OCHA/testfolder/")

DailyCountbyCode(gdelt_df_name="Haiyanloc",
                 subcodes=c("103", "023", "07", "012", "013"),
                 disaster.name="Haiyan",
                 time.start="2013-10-28",
                 disaster.start="2013-11-08",
                 disaster.end="2013-11-08",
                 time.end="2013-11-18",
                 data.destination="H:/Work/SideProjects/GDELT/OCHA/testfolder/",
                 col=c("red","orange","blue","pink","green"),
                 subnames=c("Demand Aid", "Aid Appeal", "Provide Aid", "Pessimistic", "Optimistic"))

CameoBreakdown(gdelt_df_name="Haiyanact1",
               disaster.name="Haiyan",
               time.start="2013-10-28",
               disaster.start="2013-11-08",
               disaster.end="2013-11-08",
               time.end="2013-11-18",
               data.destination="H:/Work/SideProjects/GDELT/OCHA/testfolder/")

DailyCountbyCode(gdelt_df_name="Haiyanact1",
                 subcodes=c("103", "023", "07", "012", "013"),
                 disaster.name="Haiyan",
                 time.start="2013-10-28",
                 disaster.start="2013-11-08",
                 disaster.end="2013-11-08",
                 time.end="2013-11-18",
                 data.destination="H:/Work/SideProjects/GDELT/OCHA/testfolder/",
                 col=c("red","orange","blue","pink","green"),
                 subnames=c("Demand Aid", "Aid Appeal", "Provide Aid", "Pessimistic", "Optimistic"))


### Cameo breakdown of: 
# Philippine actors towards UN
# Philippine gov towards UN
# Philippine nongov towards UN
# Philippine towards aid groups

Haiyan_PHLtoUN<-Haiyanact1[ which(Haiyanact1$Actor2KnownGroupCode=="UNO"),]
Haiyan_GOVtoUN<-Haiyanact1[ which(Haiyanact1$Actor1Type1Code=="GOV" & Haiyanact1$Actor2KnownGroupCode=="UNO"),]
Haiyan_NOTGOVtoUN<-Haiyanact1[ which(Haiyanact1$Actor1Type1Code!="GOV" & Haiyanact1$Actor2KnownGroupCode=="UNO"),]
Haiyan_PHLtoNGO<-Haiyanact1[ which(Haiyanact1$Actor2Type1Code=="NGO"),]

CameoBreakdown(gdelt_df_name="Haiyan_PHLtoUN",
               disaster.name="Haiyan",
               time.start="2013-10-28",
               disaster.start="2013-11-08",
               time.end="2013-11-18",
               data.destination="H:/Work/SideProjects/GDELT/OCHA/testfolder/")

DailyCountbyCode(gdelt_df_name="Haiyan_PHLtoUN",
                 subcodes=c("01", "03", "04", "07", "11"),
                 disaster.name="Haiyan",
                 time.start="2013-10-28",
                 disaster.start="2013-11-08",
                 disaster.end="2013-11-08",
                 time.end="2013-11-18",
                 data.destination="H:/Work/SideProjects/GDELT/OCHA/testfolder/",
                 col=c("purple","green","orange","blue", "red"),
                 subnames=c("Statement", "Cooperate Intent", "Consult", "Provide Aid", "Disapprove"))


CameoBreakdown(gdelt_df_name="Haiyan_GOVtoUN",
               disaster.name="Haiyan",
               time.start="2013-10-28",
               disaster.start="2013-11-08",
               time.end="2013-11-18",
               data.destination="H:/Work/SideProjects/GDELT/OCHA/testfolder/")


DailyCountbyCode(gdelt_df_name="Haiyan_GOVtoUN",
                 subcodes=c("03", "04", "06", "11"),
                 disaster.name="Haiyan",
                 time.start="2013-10-28",
                 disaster.start="2013-11-08",
                 disaster.end="2013-11-08",
                 time.end="2013-11-18",
                 data.destination="H:/Work/SideProjects/GDELT/OCHA/testfolder/",
                 col=c("green","orange","blue", "red"),
                 subnames=c("Cooperate Intent", "Consult", "Material Cooperation", "Disapprove"))


CameoBreakdown(gdelt_df_name="Haiyan_NOTGOVtoUN",
               disaster.name="Haiyan",
               time.start="2013-10-28",
               disaster.start="2013-11-08",
               time.end="2013-11-18",
               data.destination="H:/Work/SideProjects/GDELT/OCHA/testfolder/")

DailyCountbyCode(gdelt_df_name="Haiyan_NOTGOVtoUN",
                 subcodes=c("02", "03", "04", "07", "11"),
                 disaster.name="Haiyan",
                 time.start="2013-10-28",
                 disaster.start="2013-11-08",
                 disaster.end="2013-11-08",
                 time.end="2013-11-18",
                 data.destination="H:/Work/SideProjects/GDELT/OCHA/testfolder/",
                 col=c("orange","blue", "green", "Orange", "red"),
                 subnames=c("Appeal", "Cooperate Intent", "Consult", "Provide Aid", "Disapprove"))

CameoBreakdown(gdelt_df_name="Haiyan_PHLtoNGO",
               disaster.name="Haiyan",
               time.start="2013-10-28",
               disaster.start="2013-11-08",
               time.end="2013-11-18",
               data.destination="H:/Work/SideProjects/GDELT/OCHA/testfolder/")

DailyCountbyCode(gdelt_df_name="Haiyan_PHLtoNGO",
                 subcodes=c("01", "04", "07", "11"),
                 disaster.name="Haiyan",
                 time.start="2013-10-28",
                 disaster.start="2013-11-08",
                 disaster.end="2013-11-08",
                 time.end="2013-11-18",
                 data.destination="H:/Work/SideProjects/GDELT/OCHA/testfolder/",
                 col=c("orange","green", "blue", "red"),
                 subnames=c("Statement", "Consult", "Provide Aid", "Disapprove"))


