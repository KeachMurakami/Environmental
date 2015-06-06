library(shiny)
library(ggplot2)
library(plyr)
library(dplyr)
library(data.table)
library(magrittr)
library(tidyr)
library(data.table)

shinyServer(function(input, output) {
  output$contents <- renderTable({
    
    inFile <- input$file1
    read.xlsx(inFile$datapath, sheetIndex = 1, stringsAsFactors = FALSE) %>%
      select(-(1:2)) %>%
      na.omit %>%
      return
  })
  
  output$distPlot <- renderPlot({

    inFile <- input$file1  
    Memo.dat <- 
      read.xlsx(inFile$datapath, sheetIndex = 1, stringsAsFactors = FALSE)
    
    GL_id <- input$GL_id 
    Plant <- Memo.dat[2, 2]
    Room <- Memo.dat[3, 2]
    Memo <- Memo.dat[4, 2]

    Memo.dat1 <-
      Memo.dat %>%
      select(-(1:2)) %>%
      filter(is.used. > 0)
    
    ch.names <- Memo.dat1$Channel
    ch.types <- Memo.dat1$What.
    ch.num <- length(ch.names)
    chamID <- Memo.dat1$Where.
    LightSources <- Memo.dat1$LightSources
    Remarks <- Memo.dat1$Remarks
    
    type.num <- 
      ch.types %>%
      table %>%
      length
    
    types <-
      ch.types %>%
      unique
  
    Memo.dat2 <-
      Memo.dat1 %>%
      mutate(Names = paste0(Where., ":", What.)) %>%
      select(-is.used., -Where., -What., -LightSources, -Remarks)

    DataDirs <-
      dir(paste0("~/GitHub/BeeLabR/Environmental/RawData/GL_logs/", GL_id, "/"))
    Start_End <- 
      input$FromTo %>% str_split(pattern = "-")
    Start <-
      Start_End[[1]] %>% paste(collapse = "") %>% str_sub(start = 3)
    End <-
      Start_End[[2]] %>% paste(collapse = "") %>% str_sub(start = 3)
    
    DataDirsUsed <-
      DataDirs[(as.numeric(DataDirs) > Start) & (as.numeric(DataDirs) <= End)]
    DataFiles <-
      dir(paste0("~/GitHub/BeeLabR/Environmental/RawData/GL_logs/", GL_id, "/", DataDirsUsed), pattern = "CSV", full.names = T)
  
    skip_rows <- ifelse(GL_id == "D", yes = 33, no = 23)
    data <-
      lapply(1:length(DataFiles), function(x){
          read.csv(DataFiles[x], sep = ",", skip = skip_rows, header = TRUE) %>%
          select(2, (3 + ch.names)) %>%
          setnames(c("Time", paste0(chamID, ":", ch.types))) %>%
          return
      }) %>%
      rbind_all
    rm(skip_rows)
    
    DNdet <- function(Hour, ONtime = 7, OFFtime = 23){
      Hour <- as.numeric(Hour)
      ifelse(test = (Hour >= ONtime && Hour < OFFtime) , yes = "Day", no = "Night")
    }
        
    data1 <-
      data %>%
      separate(col = Time, into = c("Day", "time"), sep = " ", remove = F) %>%  
      separate(col = time, into = c("Hour", "Min", "Sec"), sep = ":") %>%
      select(-Day, -Min, -Sec)
    
    
    data2 <-
      lapply(1:ch.num, function(x){
        temp <-
          Memo.dat2 %>%
          filter(Channel == ch.names[x])
        data1 %>%
          select(Time, Hour, value = (x + 2)) %>%
          mutate(LightOn = temp$LightOn, LightOff = temp$LightOff,
                 DayNight = Vectorize(DNdet)(Hour, LightOn, LightOff),
                 value = temp$slope * value + temp$intercept,
                 variable = temp$Names) %>%
          select(Time, DayNight, value, variable) %>%
          return
      }) %>%
      rbind_all
  
  if(1 == 0){
    daily_summary <-
      data2 %>%
      separate(col = Time, into = c("Day", "Time"), sep = " ", remove = T) %>%
      select(-Time) %>%
      group_by(Day, DayNight, variable) %>%
      summarise_each(funs(mean2, sd2)) %>%
      mutate(ave = sprintf("%.1f", mean2),
             sd = sprintf("%.2f", sd2),
             ave.sd = paste0(ave, "+-", sd, "SD")) %>%
      select(Day, DayNight, variable, ave, sd, ave.sd) %>%
      separate(col = variable, into = c("chamID", "log"), sep = ":", remove = T)
    
    span_summary <-
      data2 %>%
      select(-Time) %>%
      group_by(DayNight, variable) %>%
      summarise_each(funs(mean2, sd2)) %>%
      mutate(ave = sprintf("%.1f", mean2),
             sd = sprintf("%.2f", sd2),
             ave.sd = paste0(ave, "+-", sd, "SD")) %>%
      select(DayNight, variable, ave, sd, ave.sd) %>%
      separate(col = variable, into = c("chamID", "log"), sep = ":", remove = T)
  }
  # Graphics <-
  #   lapply(1:type.num, function(x){
  #   data3 <-
        data2 %>%
        mutate(Time = as.POSIXct(strptime(Time, format = "%Y/%m/%d %H:%M:%S"))) %>%
#         separate(col = variable, into = c("chamID", "log"), sep = ":", remove = T) %>%
#         filter(log == types[x])
#       
  #     hists <- 
  #       data3 %>%
  #       ggplot(data = ., aes(x = value, y = ..density.., fill = DayNight)) +
  #       theme_bw(20) +
  #       geom_histogram(binwidth = 0.1, color = "grey", fill = "cornsilk", alpha = 0.5) +
  #       geom_density(color = "grey", alpha = 0.3) +
  #       xlab(NULL) +
  #       facet_wrap( ~ chamID)
      
      ggplot(aes(x = Time, y = value, color = DayNight)) +
      theme_bw(20) +
      geom_line(color = "grey") +
      geom_point(alpha = 0.5) +
      facet_wrap(~ variable) %>%
    return
  })
})