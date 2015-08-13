---
title: "MCH_grapher"
author: "KeachMurakami"
date: "August 13, 2015"
output: html_document
---

---
output:
  html_document:
    css: /Users/keach/Dropbox/R/Sources/markdown.css
---

making graphs (Temp Hum, and Volt) from Graphtec Logger data
========================================================

```r
opts_chunk$set(eval = TRUE,
               error = FALSE,
               prompt = TRUE,
               message = FALSE,
               fig.hight = 20,
               fig.width = 12,
               warning = FALSE)

library(tidyr)
library(data.table)
```



Information
-----------
* calculated at 2015-08-13 18:15:00  
* File name: ../RawData/GL_logs/B/150503//150503-145616_UG.CSV, ../RawData/GL_logs/B/150511//150511-152501_UG.CSV  
* chamber ID:  cham chamM, chamN  
* for Cucumber in 420 with W, WFr.  
*  ,    


```r
> DNdet <- function(Hour, ONtime, OFFtime){
+   Hour <- as.numeric(Hour)
+     ifelse(test = (Hour >= ONtime && Hour < OFFtime) , yes = "Day", no = "Night")
+ }
> 
> data1 <-
+   data %>%
+   separate(col = Time, into = c("Hour", "Min", "Sec"), sep = ":", remove = F) %>%
+   mutate(DayNight = Vectorize(DNdet)(Hour, LightOn, LightOff))
>   
> daily_summary <-
+   data1 %>%
+   select(Date, DayNight, RH:CO2, chamID) %>%
+   melt(id.vars = c("Date", "DayNight", "chamID")) %>%
+   group_by(Date, DayNight, chamID, variable) %>%
+   summarise_each(funs(mean2, sd2)) %>%
+   mutate(ave = sprintf("%.1f", mean2),
+          sd = sprintf("%.2f", sd2),
+          ave.sd = paste0(ave, "+-", sd, "SD")) %>%
+   select(Date, DayNight, chamID, variable, ave, sd, ave.sd)
> 
> span_summary <-
+   data1 %>%
+   select(DayNight, RH:CO2, chamID) %>%
+   melt(id.vars = c("DayNight", "chamID")) %>%
+   group_by(DayNight, chamID, variable) %>%
+   summarise_each(funs(mean2, sd2)) %>%
+   mutate(ave = sprintf("%.1f", mean2),
+          sd = sprintf("%.2f", sd2),
+          ave.sd = paste0(ave, "+-", sd, "SD")) %>%
+   select(DayNight, variable, ave, sd, ave.sd)
```



```r
>     data2 <-
+         data1 %>%
+         mutate(Time = as.POSIXct(strptime(paste0(Date, ":", Time), format = "%Y/%m/%d:%H:%M:%S"))) %>%
+     select(Time, DayNight, chamID, MCHid, RH:CO2) %>%
+     melt(id.vars = c("Time", "DayNight", "chamID", "MCHid")) %>%
+     data.table
> 
> Graphics <-
+   lapply(1:type.num, function(x){
+     data3 <-
+       data2 %>%
+       filter(variable == types[x])
+     
+       hists <- 
+         data3 %>%
+         ggplot(data = ., aes(x = value, y = ..density.., fill = DayNight)) +
+         theme_bw(20) +
+         geom_histogram(binwidth = 0.1, color = "grey", fill = "cornsilk", alpha = 0.5) +
+         geom_density(color = "grey", alpha = 0.3) +
+         xlab(NULL) +
+         facet_grid(chamID ~ .)
+     
+       timecourse <-
+         data3 %>%
+         ggplot(aes(x = Time, y = value, color = DayNight)) +
+         theme_bw(20) +
+         geom_line(color = "grey") +
+         geom_point(alpha = 0.3) +
+         facet_grid(chamID ~ .)
+       
+       list(Histgram = hists, TimeCourse = timecourse) %>%
+         return
+       })
> 
> names(Graphics) <- types
```


```r
> # file.info
> file.info(DataFiles) %>% kable
```



|                                                  |    size|isdir | mode|mtime               |ctime               |atime               | uid| gid|uname |grname |
|:-------------------------------------------------|-------:|:-----|----:|:-------------------|:-------------------|:-------------------|---:|---:|:-----|:------|
|../RawData/GL_logs/B/150503//150503-145616_UG.CSV | 2946316|FALSE |  644|2015-06-03 20:51:19 |2015-06-03 21:04:39 |2015-08-13 18:10:27 | 501|  20|keach |staff  |
|../RawData/GL_logs/B/150511//150511-152501_UG.CSV |  353292|FALSE |  644|2015-06-03 20:51:19 |2015-06-03 21:04:39 |2015-08-13 18:10:27 | 501|  20|keach |staff  |

```r
> file.info(Memo.file) %>% kable
```



|             |  size|isdir | mode|mtime               |ctime               |atime               | uid| gid|uname |grname |
|:------------|-----:|:-----|----:|:-------------------|:-------------------|:-------------------|---:|---:|:-----|:------|
|example.xlsx | 34771|FALSE |  644|2015-08-13 17:05:58 |2015-08-13 17:05:58 |2015-08-13 18:15:00 | 501|  20|keach |staff  |

```r
> daily_summary %>% kable
```



|Date       |DayNight |chamID |variable |ave   |sd     |ave.sd          |
|:----------|:--------|:------|:--------|:-----|:------|:---------------|
|2015/06/01 |Day      |chamM  |RH       |57.2  |7.50   |57.2+-7.50SD    |
|2015/06/01 |Day      |chamM  |temp     |24.7  |0.42   |24.7+-0.42SD    |
|2015/06/01 |Day      |chamM  |CO2      |739.0 |102.52 |739.0+-102.52SD |
|2015/06/01 |Day      |chamN  |RH       |47.4  |5.72   |47.4+-5.72SD    |
|2015/06/01 |Day      |chamN  |temp     |24.4  |0.20   |24.4+-0.20SD    |
|2015/06/01 |Day      |chamN  |CO2      |727.2 |103.08 |727.2+-103.08SD |
|2015/06/01 |Night    |chamM  |RH       |61.6  |12.81  |61.6+-12.81SD   |
|2015/06/01 |Night    |chamM  |temp     |21.0  |1.11   |21.0+-1.11SD    |
|2015/06/01 |Night    |chamM  |CO2      |557.4 |12.98  |557.4+-12.98SD  |
|2015/06/01 |Night    |chamN  |RH       |50.9  |12.88  |50.9+-12.88SD   |
|2015/06/01 |Night    |chamN  |temp     |20.6  |1.58   |20.6+-1.58SD    |
|2015/06/01 |Night    |chamN  |CO2      |543.9 |15.48  |543.9+-15.48SD  |
|2015/06/02 |Day      |chamM  |RH       |63.8  |9.02   |63.8+-9.02SD    |
|2015/06/02 |Day      |chamM  |temp     |24.6  |0.70   |24.6+-0.70SD    |
|2015/06/02 |Day      |chamM  |CO2      |596.5 |102.29 |596.5+-102.29SD |
|2015/06/02 |Day      |chamN  |RH       |52.2  |8.07   |52.2+-8.07SD    |
|2015/06/02 |Day      |chamN  |temp     |24.2  |0.55   |24.2+-0.55SD    |
|2015/06/02 |Day      |chamN  |CO2      |606.4 |107.55 |606.4+-107.55SD |
|2015/06/02 |Night    |chamM  |RH       |64.3  |8.34   |64.3+-8.34SD    |
|2015/06/02 |Night    |chamM  |temp     |20.8  |0.66   |20.8+-0.66SD    |
|2015/06/02 |Night    |chamM  |CO2      |478.1 |31.20  |478.1+-31.20SD  |
|2015/06/02 |Night    |chamN  |RH       |56.1  |7.84   |56.1+-7.84SD    |
|2015/06/02 |Night    |chamN  |temp     |19.7  |0.69   |19.7+-0.69SD    |
|2015/06/02 |Night    |chamN  |CO2      |465.5 |29.62  |465.5+-29.62SD  |
|2015/06/03 |Day      |chamM  |RH       |64.2  |8.59   |64.2+-8.59SD    |
|2015/06/03 |Day      |chamM  |temp     |24.6  |0.77   |24.6+-0.77SD    |
|2015/06/03 |Day      |chamM  |CO2      |568.3 |111.07 |568.3+-111.07SD |
|2015/06/03 |Day      |chamN  |RH       |50.3  |6.77   |50.3+-6.77SD    |
|2015/06/03 |Day      |chamN  |temp     |24.2  |0.54   |24.2+-0.54SD    |
|2015/06/03 |Day      |chamN  |CO2      |575.2 |103.34 |575.2+-103.34SD |
|2015/06/03 |Night    |chamM  |RH       |68.0  |9.20   |68.0+-9.20SD    |
|2015/06/03 |Night    |chamM  |temp     |20.8  |0.68   |20.8+-0.68SD    |
|2015/06/03 |Night    |chamM  |CO2      |462.3 |27.37  |462.3+-27.37SD  |
|2015/06/03 |Night    |chamN  |RH       |58.3  |8.64   |58.3+-8.64SD    |
|2015/06/03 |Night    |chamN  |temp     |19.6  |0.74   |19.6+-0.74SD    |
|2015/06/03 |Night    |chamN  |CO2      |451.3 |28.18  |451.3+-28.18SD  |
|2015/06/04 |Day      |chamM  |RH       |65.3  |8.90   |65.3+-8.90SD    |
|2015/06/04 |Day      |chamM  |temp     |24.6  |0.72   |24.6+-0.72SD    |
|2015/06/04 |Day      |chamM  |CO2      |518.5 |56.91  |518.5+-56.91SD  |
|2015/06/04 |Day      |chamN  |RH       |47.7  |5.97   |47.7+-5.97SD    |
|2015/06/04 |Day      |chamN  |temp     |24.2  |0.58   |24.2+-0.58SD    |
|2015/06/04 |Day      |chamN  |CO2      |535.4 |64.95  |535.4+-64.95SD  |
|2015/06/04 |Night    |chamM  |RH       |67.6  |9.22   |67.6+-9.22SD    |
|2015/06/04 |Night    |chamM  |temp     |20.7  |0.62   |20.7+-0.62SD    |
|2015/06/04 |Night    |chamM  |CO2      |471.1 |23.14  |471.1+-23.14SD  |
|2015/06/04 |Night    |chamN  |RH       |55.8  |8.14   |55.8+-8.14SD    |
|2015/06/04 |Night    |chamN  |temp     |19.6  |0.74   |19.6+-0.74SD    |
|2015/06/04 |Night    |chamN  |CO2      |458.6 |23.21  |458.6+-23.21SD  |
|2015/06/05 |Day      |chamM  |RH       |71.4  |11.42  |71.4+-11.42SD   |
|2015/06/05 |Day      |chamM  |temp     |24.6  |0.77   |24.6+-0.77SD    |
|2015/06/05 |Day      |chamM  |CO2      |487.2 |88.51  |487.2+-88.51SD  |
|2015/06/05 |Day      |chamN  |RH       |49.4  |6.46   |49.4+-6.46SD    |
|2015/06/05 |Day      |chamN  |temp     |24.1  |0.59   |24.1+-0.59SD    |
|2015/06/05 |Day      |chamN  |CO2      |506.0 |81.51  |506.0+-81.51SD  |
|2015/06/05 |Night    |chamM  |RH       |70.6  |10.38  |70.6+-10.38SD   |
|2015/06/05 |Night    |chamM  |temp     |20.7  |0.65   |20.7+-0.65SD    |
|2015/06/05 |Night    |chamM  |CO2      |444.4 |22.59  |444.4+-22.59SD  |
|2015/06/05 |Night    |chamN  |RH       |55.5  |8.09   |55.5+-8.09SD    |
|2015/06/05 |Night    |chamN  |temp     |19.6  |0.69   |19.6+-0.69SD    |
|2015/06/05 |Night    |chamN  |CO2      |432.4 |25.04  |432.4+-25.04SD  |
|2015/06/06 |Day      |chamM  |RH       |73.3  |10.41  |73.3+-10.41SD   |
|2015/06/06 |Day      |chamM  |temp     |24.7  |0.66   |24.7+-0.66SD    |
|2015/06/06 |Day      |chamM  |CO2      |460.8 |91.93  |460.8+-91.93SD  |
|2015/06/06 |Day      |chamN  |RH       |54.5  |7.92   |54.5+-7.92SD    |
|2015/06/06 |Day      |chamN  |temp     |24.2  |0.57   |24.2+-0.57SD    |
|2015/06/06 |Day      |chamN  |CO2      |487.6 |101.76 |487.6+-101.76SD |
|2015/06/06 |Night    |chamM  |RH       |80.9  |11.43  |80.9+-11.43SD   |
|2015/06/06 |Night    |chamM  |temp     |20.7  |0.55   |20.7+-0.55SD    |
|2015/06/06 |Night    |chamM  |CO2      |457.9 |29.58  |457.9+-29.58SD  |
|2015/06/06 |Night    |chamN  |RH       |58.8  |8.48   |58.8+-8.48SD    |
|2015/06/06 |Night    |chamN  |temp     |19.6  |0.71   |19.6+-0.71SD    |
|2015/06/06 |Night    |chamN  |CO2      |445.2 |34.74  |445.2+-34.74SD  |
|2015/06/07 |Day      |chamM  |RH       |71.4  |10.43  |71.4+-10.43SD   |
|2015/06/07 |Day      |chamM  |temp     |24.6  |0.62   |24.6+-0.62SD    |
|2015/06/07 |Day      |chamM  |CO2      |447.9 |55.21  |447.9+-55.21SD  |
|2015/06/07 |Day      |chamN  |RH       |55.0  |7.48   |55.0+-7.48SD    |
|2015/06/07 |Day      |chamN  |temp     |24.2  |0.59   |24.2+-0.59SD    |
|2015/06/07 |Day      |chamN  |CO2      |469.3 |54.04  |469.3+-54.04SD  |
|2015/06/07 |Night    |chamM  |RH       |76.3  |10.64  |76.3+-10.64SD   |
|2015/06/07 |Night    |chamM  |temp     |20.7  |0.58   |20.7+-0.58SD    |
|2015/06/07 |Night    |chamM  |CO2      |478.3 |15.51  |478.3+-15.51SD  |
|2015/06/07 |Night    |chamN  |RH       |60.8  |8.68   |60.8+-8.68SD    |
|2015/06/07 |Night    |chamN  |temp     |19.6  |0.73   |19.6+-0.73SD    |
|2015/06/07 |Night    |chamN  |CO2      |464.8 |15.85  |464.8+-15.85SD  |
|2015/06/08 |Day      |chamM  |RH       |69.8  |9.38   |69.8+-9.38SD    |
|2015/06/08 |Day      |chamM  |temp     |24.4  |0.87   |24.4+-0.87SD    |
|2015/06/08 |Day      |chamM  |CO2      |416.8 |11.94  |416.8+-11.94SD  |
|2015/06/08 |Day      |chamN  |RH       |54.7  |6.78   |54.7+-6.78SD    |
|2015/06/08 |Day      |chamN  |temp     |24.0  |1.06   |24.0+-1.06SD    |
|2015/06/08 |Day      |chamN  |CO2      |435.8 |16.94  |435.8+-16.94SD  |
|2015/06/08 |Night    |chamM  |RH       |74.0  |8.91   |74.0+-8.91SD    |
|2015/06/08 |Night    |chamM  |temp     |20.6  |0.41   |20.6+-0.41SD    |
|2015/06/08 |Night    |chamM  |CO2      |447.3 |12.80  |447.3+-12.80SD  |
|2015/06/08 |Night    |chamN  |RH       |61.1  |7.11   |61.1+-7.11SD    |
|2015/06/08 |Night    |chamN  |temp     |19.5  |0.19   |19.5+-0.19SD    |
|2015/06/08 |Night    |chamN  |CO2      |435.8 |11.80  |435.8+-11.80SD  |
|2015/06/09 |Day      |chamM  |RH       |56.5  |7.55   |56.5+-7.55SD    |
|2015/06/09 |Day      |chamM  |temp     |24.7  |0.35   |24.7+-0.35SD    |
|2015/06/09 |Day      |chamM  |CO2      |786.5 |87.84  |786.5+-87.84SD  |
|2015/06/09 |Day      |chamN  |RH       |46.3  |5.31   |46.3+-5.31SD    |
|2015/06/09 |Day      |chamN  |temp     |24.2  |0.40   |24.2+-0.40SD    |
|2015/06/09 |Day      |chamN  |CO2      |779.7 |98.95  |779.7+-98.95SD  |
|2015/06/09 |Night    |chamM  |RH       |62.2  |15.83  |62.2+-15.83SD   |
|2015/06/09 |Night    |chamM  |temp     |21.1  |1.38   |21.1+-1.38SD    |
|2015/06/09 |Night    |chamM  |CO2      |596.6 |12.94  |596.6+-12.94SD  |
|2015/06/09 |Night    |chamN  |RH       |52.8  |13.16  |52.8+-13.16SD   |
|2015/06/09 |Night    |chamN  |temp     |20.3  |1.86   |20.3+-1.86SD    |
|2015/06/09 |Night    |chamN  |CO2      |581.1 |12.71  |581.1+-12.71SD  |
|2015/06/10 |Day      |chamM  |RH       |65.8  |10.58  |65.8+-10.58SD   |
|2015/06/10 |Day      |chamM  |temp     |24.6  |0.54   |24.6+-0.54SD    |
|2015/06/10 |Day      |chamM  |CO2      |532.6 |58.39  |532.6+-58.39SD  |
|2015/06/10 |Day      |chamN  |RH       |48.0  |5.66   |48.0+-5.66SD    |
|2015/06/10 |Day      |chamN  |temp     |24.2  |0.65   |24.2+-0.65SD    |
|2015/06/10 |Day      |chamN  |CO2      |541.5 |64.35  |541.5+-64.35SD  |
|2015/06/10 |Night    |chamM  |RH       |65.9  |9.30   |65.9+-9.30SD    |
|2015/06/10 |Night    |chamM  |temp     |20.7  |0.56   |20.7+-0.56SD    |
|2015/06/10 |Night    |chamM  |CO2      |527.7 |22.60  |527.7+-22.60SD  |
|2015/06/10 |Night    |chamN  |RH       |57.1  |7.69   |57.1+-7.69SD    |
|2015/06/10 |Night    |chamN  |temp     |19.6  |0.83   |19.6+-0.83SD    |
|2015/06/10 |Night    |chamN  |CO2      |518.1 |21.48  |518.1+-21.48SD  |
|2015/06/11 |Day      |chamM  |RH       |67.9  |9.28   |67.9+-9.28SD    |
|2015/06/11 |Day      |chamM  |temp     |24.8  |0.53   |24.8+-0.53SD    |
|2015/06/11 |Day      |chamM  |CO2      |719.7 |329.99 |719.7+-329.99SD |
|2015/06/11 |Day      |chamN  |RH       |54.8  |9.00   |54.8+-9.00SD    |
|2015/06/11 |Day      |chamN  |temp     |24.1  |0.64   |24.1+-0.64SD    |
|2015/06/11 |Day      |chamN  |CO2      |731.5 |370.18 |731.5+-370.18SD |
|2015/06/11 |Night    |chamM  |RH       |72.4  |9.27   |72.4+-9.27SD    |
|2015/06/11 |Night    |chamM  |temp     |20.9  |0.55   |20.9+-0.55SD    |
|2015/06/11 |Night    |chamM  |CO2      |467.5 |12.27  |467.5+-12.27SD  |
|2015/06/11 |Night    |chamN  |RH       |58.0  |8.54   |58.0+-8.54SD    |
|2015/06/11 |Night    |chamN  |temp     |19.6  |0.84   |19.6+-0.84SD    |
|2015/06/11 |Night    |chamN  |CO2      |459.1 |11.64  |459.1+-11.64SD  |
|2015/06/12 |Day      |chamM  |RH       |68.8  |10.11  |68.8+-10.11SD   |
|2015/06/12 |Day      |chamM  |temp     |24.8  |0.63   |24.8+-0.63SD    |
|2015/06/12 |Day      |chamM  |CO2      |680.3 |189.85 |680.3+-189.85SD |
|2015/06/12 |Day      |chamN  |RH       |56.5  |9.01   |56.5+-9.01SD    |
|2015/06/12 |Day      |chamN  |temp     |24.1  |0.70   |24.1+-0.70SD    |
|2015/06/12 |Day      |chamN  |CO2      |697.6 |193.05 |697.6+-193.05SD |
|2015/06/12 |Night    |chamM  |RH       |79.7  |11.47  |79.7+-11.47SD   |
|2015/06/12 |Night    |chamM  |temp     |20.8  |0.48   |20.8+-0.48SD    |
|2015/06/12 |Night    |chamM  |CO2      |452.8 |70.64  |452.8+-70.64SD  |
|2015/06/12 |Night    |chamN  |RH       |70.6  |11.50  |70.6+-11.50SD   |
|2015/06/12 |Night    |chamN  |temp     |19.5  |0.82   |19.5+-0.82SD    |
|2015/06/12 |Night    |chamN  |CO2      |439.6 |67.35  |439.6+-67.35SD  |
|2015/06/13 |Day      |chamM  |RH       |68.8  |10.73  |68.8+-10.73SD   |
|2015/06/13 |Day      |chamM  |temp     |24.8  |0.64   |24.8+-0.64SD    |
|2015/06/13 |Day      |chamM  |CO2      |449.4 |18.13  |449.4+-18.13SD  |
|2015/06/13 |Day      |chamN  |RH       |50.9  |6.52   |50.9+-6.52SD    |
|2015/06/13 |Day      |chamN  |temp     |24.1  |0.67   |24.1+-0.67SD    |
|2015/06/13 |Day      |chamN  |CO2      |458.8 |18.46  |458.8+-18.46SD  |
|2015/06/13 |Night    |chamM  |RH       |70.1  |9.45   |70.1+-9.45SD    |
|2015/06/13 |Night    |chamM  |temp     |20.8  |0.52   |20.8+-0.52SD    |
|2015/06/13 |Night    |chamM  |CO2      |513.2 |44.26  |513.2+-44.26SD  |
|2015/06/13 |Night    |chamN  |RH       |59.9  |8.48   |59.9+-8.48SD    |
|2015/06/13 |Night    |chamN  |temp     |19.5  |0.83   |19.5+-0.83SD    |
|2015/06/13 |Night    |chamN  |CO2      |498.8 |38.02  |498.8+-38.02SD  |
|2015/06/14 |Day      |chamM  |RH       |71.9  |9.64   |71.9+-9.64SD    |
|2015/06/14 |Day      |chamM  |temp     |24.8  |0.60   |24.8+-0.60SD    |
|2015/06/14 |Day      |chamM  |CO2      |427.6 |23.14  |427.6+-23.14SD  |
|2015/06/14 |Day      |chamN  |RH       |50.7  |6.32   |50.7+-6.32SD    |
|2015/06/14 |Day      |chamN  |temp     |24.1  |0.69   |24.1+-0.69SD    |
|2015/06/14 |Day      |chamN  |CO2      |444.8 |21.88  |444.8+-21.88SD  |
|2015/06/14 |Night    |chamM  |RH       |77.8  |10.60  |77.8+-10.60SD   |
|2015/06/14 |Night    |chamM  |temp     |20.9  |0.52   |20.9+-0.52SD    |
|2015/06/14 |Night    |chamM  |CO2      |448.3 |14.44  |448.3+-14.44SD  |
|2015/06/14 |Night    |chamN  |RH       |58.3  |8.05   |58.3+-8.05SD    |
|2015/06/14 |Night    |chamN  |temp     |19.6  |0.87   |19.6+-0.87SD    |
|2015/06/14 |Night    |chamN  |CO2      |441.8 |8.57   |441.8+-8.57SD   |
|2015/06/15 |Day      |chamM  |RH       |72.4  |9.92   |72.4+-9.92SD    |
|2015/06/15 |Day      |chamM  |temp     |24.8  |0.58   |24.8+-0.58SD    |
|2015/06/15 |Day      |chamM  |CO2      |536.7 |76.37  |536.7+-76.37SD  |
|2015/06/15 |Day      |chamN  |RH       |54.7  |7.46   |54.7+-7.46SD    |
|2015/06/15 |Day      |chamN  |temp     |24.1  |0.74   |24.1+-0.74SD    |
|2015/06/15 |Day      |chamN  |CO2      |570.5 |84.17  |570.5+-84.17SD  |
|2015/06/15 |Night    |chamM  |RH       |77.0  |10.17  |77.0+-10.17SD   |
|2015/06/15 |Night    |chamM  |temp     |20.8  |0.48   |20.8+-0.48SD    |
|2015/06/15 |Night    |chamM  |CO2      |431.3 |22.45  |431.3+-22.45SD  |
|2015/06/15 |Night    |chamN  |RH       |59.8  |8.19   |59.8+-8.19SD    |
|2015/06/15 |Night    |chamN  |temp     |19.6  |0.90   |19.6+-0.90SD    |
|2015/06/15 |Night    |chamN  |CO2      |424.1 |27.35  |424.1+-27.35SD  |
|2015/06/16 |Day      |chamM  |RH       |60.0  |9.98   |60.0+-9.98SD    |
|2015/06/16 |Day      |chamM  |temp     |25.1  |0.87   |25.1+-0.87SD    |
|2015/06/16 |Day      |chamM  |CO2      |528.5 |78.17  |528.5+-78.17SD  |
|2015/06/16 |Day      |chamN  |RH       |48.2  |6.32   |48.2+-6.32SD    |
|2015/06/16 |Day      |chamN  |temp     |24.3  |1.04   |24.3+-1.04SD    |
|2015/06/16 |Day      |chamN  |CO2      |532.9 |73.76  |532.9+-73.76SD  |
|2015/06/16 |Night    |chamM  |RH       |77.1  |8.67   |77.1+-8.67SD    |
|2015/06/16 |Night    |chamM  |temp     |20.8  |0.28   |20.8+-0.28SD    |
|2015/06/16 |Night    |chamM  |CO2      |467.5 |13.91  |467.5+-13.91SD  |
|2015/06/16 |Night    |chamN  |RH       |59.8  |6.92   |59.8+-6.92SD    |
|2015/06/16 |Night    |chamN  |temp     |19.4  |0.35   |19.4+-0.35SD    |
|2015/06/16 |Night    |chamN  |CO2      |455.3 |13.99  |455.3+-13.99SD  |
|2015/06/20 |Day      |chamM  |RH       |56.1  |7.37   |56.1+-7.37SD    |
|2015/06/20 |Day      |chamM  |temp     |24.9  |0.30   |24.9+-0.30SD    |
|2015/06/20 |Day      |chamM  |CO2      |577.1 |108.84 |577.1+-108.84SD |
|2015/06/20 |Day      |chamN  |RH       |48.6  |6.06   |48.6+-6.06SD    |
|2015/06/20 |Day      |chamN  |temp     |24.2  |0.35   |24.2+-0.35SD    |
|2015/06/20 |Day      |chamN  |CO2      |577.6 |106.60 |577.6+-106.60SD |
|2015/06/20 |Night    |chamM  |RH       |57.5  |11.37  |57.5+-11.37SD   |
|2015/06/20 |Night    |chamM  |temp     |21.4  |1.09   |21.4+-1.09SD    |
|2015/06/20 |Night    |chamM  |CO2      |462.8 |8.89   |462.8+-8.89SD   |
|2015/06/20 |Night    |chamN  |RH       |51.4  |13.19  |51.4+-13.19SD   |
|2015/06/20 |Night    |chamN  |temp     |20.9  |2.18   |20.9+-2.18SD    |
|2015/06/20 |Night    |chamN  |CO2      |458.8 |10.15  |458.8+-10.15SD  |
|2015/06/21 |Day      |chamM  |RH       |52.8  |4.92   |52.8+-4.92SD    |
|2015/06/21 |Day      |chamM  |temp     |24.8  |0.55   |24.8+-0.55SD    |
|2015/06/21 |Day      |chamM  |CO2      |419.1 |8.22   |419.1+-8.22SD   |
|2015/06/21 |Day      |chamN  |RH       |48.3  |5.67   |48.3+-5.67SD    |
|2015/06/21 |Day      |chamN  |temp     |24.2  |0.74   |24.2+-0.74SD    |
|2015/06/21 |Day      |chamN  |CO2      |417.2 |7.56   |417.2+-7.56SD   |
|2015/06/21 |Night    |chamM  |RH       |59.8  |6.60   |59.8+-6.60SD    |
|2015/06/21 |Night    |chamM  |temp     |20.9  |0.53   |20.9+-0.53SD    |
|2015/06/21 |Night    |chamM  |CO2      |438.6 |11.25  |438.6+-11.25SD  |
|2015/06/21 |Night    |chamN  |RH       |56.7  |7.72   |56.7+-7.72SD    |
|2015/06/21 |Night    |chamN  |temp     |19.6  |0.95   |19.6+-0.95SD    |
|2015/06/21 |Night    |chamN  |CO2      |432.5 |11.48  |432.5+-11.48SD  |
|2015/06/22 |Day      |chamM  |RH       |55.2  |6.17   |55.2+-6.17SD    |
|2015/06/22 |Day      |chamM  |temp     |24.8  |0.54   |24.8+-0.54SD    |
|2015/06/22 |Day      |chamM  |CO2      |587.8 |151.66 |587.8+-151.66SD |
|2015/06/22 |Day      |chamN  |RH       |48.5  |5.77   |48.5+-5.77SD    |
|2015/06/22 |Day      |chamN  |temp     |24.2  |0.74   |24.2+-0.74SD    |
|2015/06/22 |Day      |chamN  |CO2      |591.5 |158.79 |591.5+-158.79SD |
|2015/06/22 |Night    |chamM  |RH       |59.8  |6.48   |59.8+-6.48SD    |
|2015/06/22 |Night    |chamM  |temp     |20.9  |0.50   |20.9+-0.50SD    |
|2015/06/22 |Night    |chamM  |CO2      |434.0 |42.70  |434.0+-42.70SD  |
|2015/06/22 |Night    |chamN  |RH       |56.8  |7.70   |56.8+-7.70SD    |
|2015/06/22 |Night    |chamN  |temp     |19.7  |0.96   |19.7+-0.96SD    |
|2015/06/22 |Night    |chamN  |CO2      |427.7 |43.89  |427.7+-43.89SD  |
|2015/06/23 |Day      |chamM  |RH       |54.9  |5.27   |54.9+-5.27SD    |
|2015/06/23 |Day      |chamM  |temp     |24.8  |0.58   |24.8+-0.58SD    |
|2015/06/23 |Day      |chamM  |CO2      |500.0 |49.14  |500.0+-49.14SD  |
|2015/06/23 |Day      |chamN  |RH       |49.5  |6.01   |49.5+-6.01SD    |
|2015/06/23 |Day      |chamN  |temp     |24.2  |0.77   |24.2+-0.77SD    |
|2015/06/23 |Day      |chamN  |CO2      |499.3 |49.21  |499.3+-49.21SD  |
|2015/06/23 |Night    |chamM  |RH       |60.5  |6.53   |60.5+-6.53SD    |
|2015/06/23 |Night    |chamM  |temp     |21.0  |0.53   |21.0+-0.53SD    |
|2015/06/23 |Night    |chamM  |CO2      |473.1 |23.68  |473.1+-23.68SD  |
|2015/06/23 |Night    |chamN  |RH       |57.2  |7.81   |57.2+-7.81SD    |
|2015/06/23 |Night    |chamN  |temp     |19.7  |0.98   |19.7+-0.98SD    |
|2015/06/23 |Night    |chamN  |CO2      |466.6 |23.08  |466.6+-23.08SD  |
|2015/06/24 |Day      |chamM  |RH       |55.4  |5.14   |55.4+-5.14SD    |
|2015/06/24 |Day      |chamM  |temp     |24.8  |0.53   |24.8+-0.53SD    |
|2015/06/24 |Day      |chamM  |CO2      |523.4 |110.12 |523.4+-110.12SD |
|2015/06/24 |Day      |chamN  |RH       |51.1  |6.51   |51.1+-6.51SD    |
|2015/06/24 |Day      |chamN  |temp     |24.2  |0.77   |24.2+-0.77SD    |
|2015/06/24 |Day      |chamN  |CO2      |522.9 |113.43 |522.9+-113.43SD |
|2015/06/24 |Night    |chamM  |RH       |60.2  |6.32   |60.2+-6.32SD    |
|2015/06/24 |Night    |chamM  |temp     |21.0  |0.50   |21.0+-0.50SD    |
|2015/06/24 |Night    |chamM  |CO2      |436.1 |30.71  |436.1+-30.71SD  |
|2015/06/24 |Night    |chamN  |RH       |56.9  |7.64   |56.9+-7.64SD    |
|2015/06/24 |Night    |chamN  |temp     |19.7  |0.99   |19.7+-0.99SD    |
|2015/06/24 |Night    |chamN  |CO2      |430.0 |32.02  |430.0+-32.02SD  |
|2015/06/25 |Day      |chamM  |RH       |58.8  |7.49   |58.8+-7.49SD    |
|2015/06/25 |Day      |chamM  |temp     |24.7  |0.57   |24.7+-0.57SD    |
|2015/06/25 |Day      |chamM  |CO2      |564.3 |116.25 |564.3+-116.25SD |
|2015/06/25 |Day      |chamN  |RH       |52.7  |6.58   |52.7+-6.58SD    |
|2015/06/25 |Day      |chamN  |temp     |24.2  |0.80   |24.2+-0.80SD    |
|2015/06/25 |Day      |chamN  |CO2      |569.1 |119.50 |569.1+-119.50SD |
|2015/06/25 |Night    |chamM  |RH       |60.8  |7.07   |60.8+-7.07SD    |
|2015/06/25 |Night    |chamM  |temp     |20.9  |0.52   |20.9+-0.52SD    |
|2015/06/25 |Night    |chamM  |CO2      |456.2 |18.96  |456.2+-18.96SD  |
|2015/06/25 |Night    |chamN  |RH       |57.3  |7.65   |57.3+-7.65SD    |
|2015/06/25 |Night    |chamN  |temp     |19.8  |1.03   |19.8+-1.03SD    |
|2015/06/25 |Night    |chamN  |CO2      |449.4 |18.06  |449.4+-18.06SD  |
|2015/06/26 |Day      |chamM  |RH       |60.7  |8.94   |60.7+-8.94SD    |
|2015/06/26 |Day      |chamM  |temp     |24.6  |0.70   |24.6+-0.70SD    |
|2015/06/26 |Day      |chamM  |CO2      |620.4 |160.01 |620.4+-160.01SD |
|2015/06/26 |Day      |chamN  |RH       |53.7  |7.38   |53.7+-7.38SD    |
|2015/06/26 |Day      |chamN  |temp     |24.3  |0.77   |24.3+-0.77SD    |
|2015/06/26 |Day      |chamN  |CO2      |632.5 |164.09 |632.5+-164.09SD |
|2015/06/26 |Night    |chamM  |RH       |63.4  |8.61   |63.4+-8.61SD    |
|2015/06/26 |Night    |chamM  |temp     |20.6  |0.57   |20.6+-0.57SD    |
|2015/06/26 |Night    |chamM  |CO2      |435.4 |44.06  |435.4+-44.06SD  |
|2015/06/26 |Night    |chamN  |RH       |58.6  |8.11   |58.6+-8.11SD    |
|2015/06/26 |Night    |chamN  |temp     |19.9  |1.00   |19.9+-1.00SD    |
|2015/06/26 |Night    |chamN  |CO2      |431.0 |46.88  |431.0+-46.88SD  |
|2015/06/27 |Day      |chamM  |RH       |56.4  |7.72   |56.4+-7.72SD    |
|2015/06/27 |Day      |chamM  |temp     |24.2  |1.32   |24.2+-1.32SD    |
|2015/06/27 |Day      |chamM  |CO2      |414.5 |7.10   |414.5+-7.10SD   |
|2015/06/27 |Day      |chamN  |RH       |50.5  |6.30   |50.5+-6.30SD    |
|2015/06/27 |Day      |chamN  |temp     |23.9  |1.42   |23.9+-1.42SD    |
|2015/06/27 |Day      |chamN  |CO2      |419.0 |4.65   |419.0+-4.65SD   |
|2015/06/27 |Night    |chamM  |RH       |63.2  |6.94   |63.2+-6.94SD    |
|2015/06/27 |Night    |chamM  |temp     |20.6  |0.38   |20.6+-0.38SD    |
|2015/06/27 |Night    |chamM  |CO2      |468.6 |29.52  |468.6+-29.52SD  |
|2015/06/27 |Night    |chamN  |RH       |58.1  |6.64   |58.1+-6.64SD    |
|2015/06/27 |Night    |chamN  |temp     |19.7  |0.34   |19.7+-0.34SD    |
|2015/06/27 |Night    |chamN  |CO2      |464.6 |29.26  |464.6+-29.26SD  |
|2015/07/02 |Day      |chamN  |RH       |73.3  |NA     |73.3+-NASD      |
|2015/07/02 |Day      |chamN  |temp     |24.3  |NA     |24.3+-NASD      |
|2015/07/02 |Day      |chamN  |CO2      |504.1 |NA     |504.1+-NASD     |

```r
> span_summary %>% kable
```



|chamID |DayNight |variable |ave   |sd     |ave.sd          |
|:------|:--------|:--------|:-----|:------|:---------------|
|chamM  |Day      |RH       |63.9  |11.08  |63.9+-11.08SD   |
|chamM  |Day      |temp     |24.7  |0.65   |24.7+-0.65SD    |
|chamM  |Day      |CO2      |544.9 |152.33 |544.9+-152.33SD |
|chamN  |Day      |RH       |51.2  |7.44   |51.2+-7.44SD    |
|chamN  |Day      |temp     |24.2  |0.69   |24.2+-0.69SD    |
|chamN  |Day      |CO2      |555.4 |156.96 |555.4+-156.96SD |
|chamM  |Night    |RH       |68.8  |11.53  |68.8+-11.53SD   |
|chamM  |Night    |temp     |20.8  |0.57   |20.8+-0.57SD    |
|chamM  |Night    |CO2      |462.8 |40.65  |462.8+-40.65SD  |
|chamN  |Night    |RH       |58.5  |8.86   |58.5+-8.86SD    |
|chamN  |Night    |temp     |19.6  |0.86   |19.6+-0.86SD    |
|chamN  |Night    |CO2      |453.4 |39.62  |453.4+-39.62SD  |

```r
> Graphics
```

```
## $temp
## $temp$Histgram
```

![plot of chunk Output](figure/Output-1.png) 

```
## 
## $temp$TimeCourse
```

![plot of chunk Output](figure/Output-2.png) 

```
## 
## 
## $RH
## $RH$Histgram
```

![plot of chunk Output](figure/Output-3.png) 

```
## 
## $RH$TimeCourse
```

![plot of chunk Output](figure/Output-4.png) 

```
## 
## 
## $CO2
## $CO2$Histgram
```

![plot of chunk Output](figure/Output-5.png) 

```
## 
## $CO2$TimeCourse
```

![plot of chunk Output](figure/Output-6.png) 

```r
> sessionInfo()
```

```
## R version 3.1.2 (2014-10-31)
## Platform: x86_64-apple-darwin13.4.0 (64-bit)
## 
## locale:
## [1] C
## 
## attached base packages:
## [1] datasets  grid      utils     stats     graphics  grDevices methods  
## [8] base     
## 
## other attached packages:
##  [1] googlesheets_0.1.0        shinyga_0.1.2.9001       
##  [3] shinydashboard_0.5.0.9000 googleVis_0.5.8          
##  [5] rCharts_0.4.5             tidyr_0.2.0              
##  [7] data.table_1.9.4          shiny_0.11.1             
##  [9] RCurl_1.95-4.5            bitops_1.0-6             
## [11] stringr_0.6.2             agricolae_1.2-1          
## [13] GGally_0.5.0              magrittr_1.5             
## [15] gridExtra_0.9.1           foreach_1.4.2            
## [17] gtable_0.1.2              knitr_1.9                
## [19] xlsx_0.5.7                xlsxjars_0.6.1           
## [21] rJava_0.9-6               reshape2_1.4.1           
## [23] dplyr_0.4.1               plyr_1.8.1               
## [25] mvtnorm_1.0-2             RColorBrewer_1.1-2       
## [27] gcookbook_1.0             ggplot2_1.0.0            
## [29] MASS_7.3-39              
## 
## loaded via a namespace (and not attached):
##  [1] DBI_0.3.1         LearnBayes_2.15   Matrix_1.1-5     
##  [4] R6_2.0.1          RJSONIO_1.3-0     Rcpp_0.11.6      
##  [7] assertthat_0.1    boot_1.3-15       cellranger_1.0.0 
## [10] chron_2.3-45      cluster_2.0.1     coda_0.16-1      
## [13] codetools_0.2-10  colorspace_1.2-4  combinat_0.0-8   
## [16] deldir_0.1-7      digest_0.6.8      evaluate_0.5.5   
## [19] formatR_1.0       htmltools_0.2.6   httpuv_1.3.2     
## [22] httr_0.6.1        iterators_1.0.7   jsonlite_0.9.16  
## [25] klaR_0.6-12       labeling_0.3      lattice_0.20-29  
## [28] lazyeval_0.1.10   markdown_0.7.4    mime_0.2         
## [31] munsell_0.4.2     nlme_3.1-120      parallel_3.1.2   
## [34] proto_0.3-10      reshape_0.8.5     rmarkdown_0.5.1  
## [37] rstudio_0.98.1091 rstudioapi_0.2    scales_0.2.4     
## [40] sp_1.0-17         spdep_0.5-83      splines_3.1.2    
## [43] stringi_0.4-1     tools_3.1.2       whisker_0.3-2    
## [46] xml2_0.1.1        xtable_1.7-4      yaml_2.1.13      
## [49] zoo_1.7-11
```


