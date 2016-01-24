### テスト運転中
setwd("~/GitHub/BeeLabR/Environmental/EnvLogs/")
files <- dir(pattern = "xlsx")[1]
###



ExptInfo <-
  read.xlsx(files, 1, startRow = 2, stringsAsFactors = FALSE) %>%
  na.omit

Hist_ls <- as.list(numeric(nrow(ExptInfo)))
Hourly_ls <- as.list(numeric(nrow(ExptInfo)))
Daily_ls <- as.list(numeric(nrow(ExptInfo)))


histor <- function(df, value) 
  df %>%
  mutate(DayNight = as.factor(DayNight)) %>%
  ggplot(aes_string(x = value, fill = "DayNight")) +
  geom_histogram(alpha = .25)

lapply(1:nrow(ExptInfo), function(X) {
  ExptInfo %>%
    slice(X) %>%
    {
      temps <-
        readGL(ID = .$GL_id, ch = .$GL_ch,
               StartDay = .$StartDay, StartTime = .$StartTime,
               EndDay = .$EndDay, EndTime = .$EndTime,
               ONtime = .$LightOn, OFFtime = .$LightOff) 
      
      RH_CO2s <-    
        readMCH(ID = .$MCH_id,
                StartDay = .$StartDay, StartTime = .$StartTime,
                EndDay = .$EndDay, EndTime = .$EndTime,
                ONtime = .$LightOn, OFFtime = .$LightOff)
      
      Hist_ls[[X]] <<-
        list(histor(temps$Raw, "Temp"), histor(RH_CO2s$Raw, "RH"), histor(RH_CO2s$Raw, "CO2"))
      
      
      temps$Hourly %<>%
        rename(GL_ID = ID, GL_CH = ch)
      RH_CO2s$Hourly %<>%
        rename(MCH_ID = ID)
      Hourly_ls[[X]] <<-
        full_join(temps$Hourly, RH_CO2s$Hourly, by = c("DayNight", "Day", "Hour", "DayHour", "Time"))

      temps$Daily %<>%
        rename(GL_ID = ID, GL_CH = ch)
      RH_CO2s$Daily %<>%
        rename(MCH_ID = ID)
      Daily_ls[[X]] <<-
        full_join(temps$Daily, RH_CO2s$Daily, by = c("DayNight", "Day", "Time"))
    }
})

#### input fin #####

#### data handling ####

# 先に場所情報を乗っける？
chamset <- function(GL_ID, GL_CH) {
  ExptInfo %>%
    filter(GL_id == GL_ID, GL_ch == GL_CH) %>%
    .$ChamberID
}


Hourly_ls %>%
  rbind_all %>%
  mutate(cham = Vectorize(chamset)(GL_ID, GL_CH)) %>%
  melt2(id.vars = )
  ggplot(aes(x = Time, y = MeanTemp.x, col = ID_CH)) +
  geom_ribbon(aes(ymin = MeanTemp.x - SDTemp.x, ymax = MeanTemp.x + SDTemp.x, fill = ID_CH), alpha = .1) +
  geom_line()