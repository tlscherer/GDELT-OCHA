

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

DailyCountbyCode(gdelt_df_name="Haiyanloc",
                subcodes=c("01", "07"),
                disaster.name="Haiyan",
                time.start="2013-10-28",
                disaster.start="2013-11-08",
                disaster.end="2013-11-08",
                time.end="2013-11-18",
                data.destination="H:/Work/SideProjects/GDELT/OCHA/testfolder/")


CameoBreakdown(gdelt_df_name="Haiyanloc",
  disaster.name="Haiyan",
  time.start="2013-10-28",
  disaster.start="2013-11-08",
  disaster.end="2013-11-08",
  time.end="2013-11-18",
  data.destination="H:/Work/SideProjects/GDELT/OCHA/testfolder/")


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

CameoBreakdown(gdelt_df_name="Haiyan_GOVtoUN",
               disaster.name="Haiyan",
               time.start="2013-10-28",
               disaster.start="2013-11-08",
               time.end="2013-11-18",
               data.destination="H:/Work/SideProjects/GDELT/OCHA/testfolder/")

CameoBreakdown(gdelt_df_name="Haiyan_NOTGOVtoUN",
               disaster.name="Haiyan",
               time.start="2013-10-28",
               disaster.start="2013-11-08",
               time.end="2013-11-18",
               data.destination="H:/Work/SideProjects/GDELT/OCHA/testfolder/")

CameoBreakdown(gdelt_df_name="Haiyan_PHLtoNGO",
               disaster.name="Haiyan",
               time.start="2013-10-28",
               disaster.start="2013-11-08",
               time.end="2013-11-18",
               data.destination="H:/Work/SideProjects/GDELT/OCHA/testfolder/")

