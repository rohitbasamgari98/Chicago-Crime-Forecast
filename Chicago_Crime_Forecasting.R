---
  title: "Project"
output: html_document
date: "2022-12-07"
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}
library(tibble)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(ggplot2)

# tsibble: tidy temporal data frames and tools
library(tsibble)

# fable (forecast table)
library(fable)

# fabletools - provides tools for building modelling packages, with a focus on time series forecasting
library(fabletools)

# Feature Extraction and Statistics for Time Series in tsibble format
library(feasts)

# tsibbledata: used datasets for example global_economy
library(tsibbledata)
```


```{r}
library(tidyverse)
library(lubridate)
df<-read.csv("R:\\UB all downloads\\SEM 2\\SDM2\\project\\archive\\Chicago_Crimes_2012_to_2017.csv")


names(df)<-str_to_lower(names(df)) %>%
  str_replace_all(" ","_") %>%
  str_replace_all("__","_") %>%
  str_replace_all("#","_num")
#df<-na.omit(df)
df<-tibble(df)
head(df)
#sort(df$date)
```

```{r}
df<-df%>%mutate(primarydescription=as.factor(primarydescription))
df<-df%>% mutate(date=mdy_hms(date),date = floor_date(date, unit = "hours"))
df<-df%>%arrange(date)
df1<-data.frame(df$date,df$primarydescription)
head(df1)
```

```{r}
library(scales)
df1 %>% 
  group_by(df.primarydescription) %>%
  summarise(count=n()) %>%
  ggplot(aes(x = reorder(df.primarydescription,count), y = count)) +
  geom_bar(stat = "identity",fill="violet") +
  labs(x ="Crime Type", y = "Number of crimes", title = "Crimes in Chicago") + 
  scale_y_continuous(label = comma) +
  coord_flip()
```

```{r}
data <- df1%>% 
  filter(df.primarydescription == 'THEFT') %>% group_by(df.date)%>%summarise(Theft = n())
data
```

```{r}
data$df.date<- format(as.POSIXct(data$df.date,format='%Y-%m-%d %H:%M:%S'),format='%Y-%m-%d')
head(data)
```

```{r}
library(feasts)

data<-data%>% mutate(df.date=ymd(df.date))
data<-data%>%as_tsibble(index=df.date)
head(data)
```

```{r}
data%>%autoplot(Theft,color="purple")+labs(y = "Theft",x="Month")
```

```{r}
data<-tsibble::fill_gaps(data)
data %>% gg_season(Theft,period="month",labels = "both") +labs(y = "Theft",x="week", title = "Seasonal plot: Theft")
```


```{r}
data<-tsibble::fill_gaps(data)
data %>% gg_season(Theft,period="week",labels = "both") +labs(y = "Theft",x="day", title = "Seasonal plot: Theft")
```

```{r}
df<-data
df<-df %>% filter(df.date < ymd("2020-12-01"))
```


```{r}
fit <- df %>%
  model(
    Mean = MEAN(Theft),
    Naive = NAIVE(Theft),
    Seasonal_Naive = SNAIVE(Theft),
    Drift = RW(Theft ~ drift())
  )
accuracy(fit)
```

```{r}
fc <- fit %>% forecast(h = 31)
fc %>% autoplot(df,level=NULL)+labs(y = "Theft",x="Month")
```

```{r}
accuracy(fc,data)
```

```{r}
fit <- df %>%
  model(
    arima_auto = ARIMA(log(Theft)~0+pdq(2,2,4)+PDQ(1,1,0)),
  )
accuracy(fit)
report(fit)

fc <- fit %>% forecast(h =31)
```

```{r}
accuracy(fc,data)
```

```{r}
fc %>% autoplot(df,level=95)+labs(y = "Theft",x="Month")
```

