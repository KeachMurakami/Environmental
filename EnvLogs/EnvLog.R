EnvLog <-
  function(files, by_cham_facet = F){
    
    if(missing(files)) files <- file.choose()
    
    ExptInfo <-
      read.xlsx(files, 1, startRow = 2, stringsAsFactors = FALSE) %>%
      na.omit
    
    #### data input  #####
    Hist_ls <- as.list(numeric(nrow(ExptInfo)))
    Hourly_ls <- as.list(numeric(nrow(ExptInfo)))
    Daily_ls <- as.list(numeric(nrow(ExptInfo)))
    Span_ls <- as.list(numeric(nrow(ExptInfo)))
    
    histor <- function(df, value) {
      df %>%
        mutate(DayNight = as.factor(DayNight)) %>%
        ggplot(aes_string(x = value, fill = "DayNight")) +
        geom_histogram(alpha = .25) +
        ggtitle(label = unique(df$cham))
    }
    
    
    lapply(1:nrow(ExptInfo), function(X) {
      
      get_cham <- function(df){
        if(is.data.frame(df)) {
          mutate(df, cham = ExptInfo$ChamberID[X])
        } else {
          df
        }
      }
      
      ExptInfo %>%
        slice(X) %>%
        {
          temps <-
            readGL(ID = .$GL_id, ch = .$GL_ch,
                   StartDay = .$StartDay, StartTime = .$StartTime,
                   EndDay = .$EndDay, EndTime = .$EndTime,
                   ONtime = .$LightOn, OFFtime = .$LightOff) %>%
            llply(get_cham)
          
          RH_CO2s <-    
            readMCH(ID = .$MCH_id,
                    StartDay = .$StartDay, StartTime = .$StartTime,
                    EndDay = .$EndDay, EndTime = .$EndTime,
                    ONtime = .$LightOn, OFFtime = .$LightOff) %>%
            llply(get_cham)
          
          Hist_ls[[X]] <<-
            list(histor(temps$Raw, "Temp"), histor(RH_CO2s$Raw, "RH"), histor(RH_CO2s$Raw, "CO2"))
          
          
          temps$Hourly %<>%
            rename(GL_ID = ID, GL_CH = ch)
          RH_CO2s$Hourly %<>%
            rename(MCH_ID = ID, MeanTempMCH = MeanTemp, SDTempMCH = SDTemp)
          Hourly_ls[[X]] <<-
            full_join(temps$Hourly, RH_CO2s$Hourly, by = c("DayNight", "Day", "Hour", "DayHour", "Time", "cham"))
          
          temps$Daily %<>%
            rename(GL_ID = ID, GL_CH = ch)
          RH_CO2s$Daily %<>%
            rename(MCH_ID = ID, MeanTempMCH = MeanTemp, SDTempMCH = SDTemp)
          Daily_ls[[X]] <<-
            full_join(temps$Daily, RH_CO2s$Daily, by = c("DayNight", "Day", "Time", "cham"))
          
          temps$Span %<>%
            rename(GL_ID = ID, GL_CH = ch)
          RH_CO2s$Span %<>%
            rename(MCH_ID = ID, MeanTempMCH = MeanTemp, SDTempMCH = SDTemp)
          Span_ls[[X]] <<-
            full_join(temps$Span, RH_CO2s$Span, by = c("DayNight", "cham"))
          
        }
    })
    
    #### data input fin #####
    
    #### data visualize ####

    PlotHourly <-
      function(by_cham = by_cham_facet){
        lapply(c("Temp", "RH", "CO2"), function(variables){
          fig1 <-
            Hourly_ls %>%
            rbind_all2 %>%
            select(cham, Time, DayNight, GL_ID, GL_CH, MCH_ID, v = ends_with(variables)) %>%
            ggplot(aes(x = Time, y = v1, fill = cham)) +
            geom_ribbon(aes(ymin = v1 - v2, ymax = v1 + v2), alpha = .1) +
            geom_point(aes(col = cham), alpha = .5) +
            ggtitle(variables)
          
          if(by_cham) {
            fig1 + facet_grid(cham ~ .)
          } else {
            fig1
          }
        })
      }
    
    PlotDaily <-
      function(by_cham = by_cham_facet){
        lapply(c("Temp", "RH", "CO2"), function(variables){
          fig2 <-
            Daily_ls %>%
            rbind_all2 %>%
            select(cham, Time, DayNight, GL_ID, GL_CH, MCH_ID, v = ends_with(variables)) %>%
            ggplot(aes(x = Time, y = v1, col = cham)) +
            geom_errorbar(aes(ymin = v1 - v2, ymax = v1 + v2), alpha = .1) +
            geom_point(alpha = .5) +
            ggtitle(variables)
          
          if(by_cham) {
            fig2 + facet_grid(cham ~ .)
          } else {
            fig2
          }
        })
      }
    
    StatSpan <-
      Span_ls %>%
      rbind_all2
    
    list(HourlyTimeCourse = PlotHourly(),
         DailyTimeCourse = PlotDaily(),
         SpanHistograms = Hist_ls,
         SpanData = StatSpan,
         Info = ExptInfo) %>%
      return
  }

