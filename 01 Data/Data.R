install.packages('devtools')
devtools::install_github('rstudio/rsconnect')
library(rsconnect)
require("jsonlite")
require("RCurl")
require("dplyr")
require("tidyr")
# Change the USER and PASS below to be your UTEid
df_mainData <- data.frame(fromJSON(getURL(URLencode('skipper.cs.utexas.edu:5001/rest/native/?query="Select DATE_TIME, GLOBALACTIVEPOWER, GLOBALREACTIVEPOWER, VOLTAGE, GLOBALINTENSITY, SUBMETERING1, SUBMETERING2, SUBMETERING3, GLOBALACTIVEPOWER * 1000/60 - SUBMETERING1 - SUBMETERING2 - SUBMETERING3 as ActiveEnergyConsumedPerMinute from HOUSEHOLD_POWER_CONSUMPTION"'),httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_jzp78', PASS='orcl_jzp78', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))

df_Data2 <- data.frame(fromJSON(getURL(URLencode('skipper.cs.utexas.edu:5001/rest/native/?query="Select * from HOUSEHOLD_POWER_CONSUMPTION"'),httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_jzp78', PASS='orcl_jzp78', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))
df_Data2 <- df_Data2 %>% mutate(DayMonthYear = strsplit(as.character(DateFull), "/")) 
df_Data2 <- df_Data2 %>% separate(DayMonthYear, c("c", "DAY", "MONTH", "YEAR"))
df_Data2 <- df_Data2 %>% mutate(HourMinuteSecond = strsplit(as.character(df_Data2$TimeFull), ";")) 
df_Data2 <- df_Data2 %>% separate(HourMinuteSecond, c("d", "Hour", "Minute", "Second"))
df_Data2 <- df_Data2 %>% mutate(ActiveEnergyConsumedPerMinute = GLOBALACTIVEPOWER * 1000/60 - SUBMETERING1 - SUBMETERING2 - SUBMETERING3)
df_Data2 <- df_Data2 %>% group_by(Hour, MONTH) %>% summarise(AvgActiveEnergyConsumedPerMinute = mean(ActiveEnergyConsumedPerMinute))
df_Data2 <- df_Data2 %>% mutate(KPI = 'High')
ggplot() + 
  coord_cartesian() + 
  scale_x_discrete() +
  scale_y_discrete() +
  #facet_grid(PCLASS~SURVIVED, labeller=label_both) +
  labs(x="Month", y="Hour") +
  layer(data=df_Data2, 
        mapping=aes(x=MONTH, y=Hour, label=AvgActiveEnergyConsumedPerMinute), 
        stat="identity", 
        stat_params=list(), 
        geom="text",
        geom_params=list(colour="black"), 
        #position=position_identity()
        position=position_identity()
  ) +
  layer(data=df_Data2, 
        mapping=aes(x=MONTH, y=Hour, fill=KPI), 
        stat="identity", 
        stat_params=list(), 
        geom="tile",
        position=position_identity()
  )
