install.packages("openxlsx")
library(openxlsx)
install.packages("tidyverse")
library(tidyverse)
library(lubridate) 
library(zoo) # as.yearmon
install.packages("magrittr") # %>%
library(magrittr)
install.packages("ggplot2")
library(ggplot2)
install.packages("ggpubr") # ggarrange
library(ggpubr)


sessionCnt.df <- read.csv("DataAnalyst_Ecom_data_sessionCounts.csv")
addCart.df <- read.csv("DataAnalyst_Ecom_data_addsToCart.csv")

# check type of variables, na
sapply(sessionCnt.df, class) # browser, device, date 
sapply(addCart.df, class) # okay
sessionCnt.df$dim_browser <- as.factor(sessionCnt.df$dim_browser)
sessionCnt.df$dim_deviceCategory <- as.factor(sessionCnt.df$dim_deviceCategory)
sessionCnt.df$dim_date <- as.Date(sessionCnt.df$dim_date,"%m/%d/%y")
sum(is.na(sessionCnt.df))
sum(is.na(addCart.df))

colnames(sessionCnt.df)[c(1,2,3)] <- c("Browser", "Device", "Date")
colnames(addCart.df)[c(1,2,3)] <- c("Year","Month", "AddToCart")


#########################
# Explore the datasets
#########################
# How many kinds of browsers, devices, respectively
levels(sessionCnt.df$Browser)
levels(sessionCnt.df$Device)

# ----- Look at daily ----- #
sessionCnt.df %>%
  group_by(Date) %>%
  summarise(Transactions = sum(transactions),
            Sessions = sum(sessions),
            QTY.dagg = sum(QTY),
            ECR = sum(transactions)/sum(sessions)) -> daily.agg.df

daily.agg.df %>% 
  ggplot() +
  aes(x = Date, y = Transactions) +
  geom_line(colour = "#2a9d8f") +
  geom_point(colour = "#2a9d8f") +
  labs(title = "Daily Transactions", y = "Number of Transactions") +
  scale_x_date(date_breaks = "1 month", date_labels = "%m")


daily.agg.df %>% 
  ggplot() +
  aes(x = Date, y = Sessions) +
  geom_line(colour = "#e9c46a") +
  geom_point(colour = "#e9c46a") +
  labs(title = "Daily Sessions", y = "Number of Sessions") +
  scale_x_date(date_breaks = "1 month", date_labels = "%m") 
  

daily.agg.df %>% 
  ggplot() +
  aes(x = Date, y = QTY.dagg) +
  geom_line(colour = "#f4a261") +
  geom_point(colour = "#f4a261") +
  labs(title = "Daily Qyantity", y = "Quantity") +
  scale_x_date(date_breaks = "1 month", date_labels = "%m")


daily.agg.df %>% 
  ggplot() +
  aes(x = Date, y = ECR) +
  geom_line(colour = "#457b9d") +
  geom_point(colour = "#457b9d") +
  labs(title = "Daily ECR", y = "ECR") +
  scale_x_date(date_breaks = "1 month", date_labels = "%m")
scale_y_continuous(labels = scales::comma)

# No obvious pattern


# ----- Monthly merics by Devices ----- #
sessionCnt.df %>%
  mutate(Month = month(Date), Year = year(Date)) %>%
  group_by(Month, Year, Device) %>%
  summarise(Transactions = sum(transactions),
            Sessions = sum(sessions),
            QTY.agg = sum(QTY),
            ECR = sum(transactions)/sum(sessions)) %>%
  arrange(Year, Month) -> sheetOne.df

sheetOne.df$YearMonth =  as.Date(make_datetime(sheetOne.df$Year, sheetOne.df$Month, tz = "EST"), "%y/%m/%d")
addCart.df$YearMonth =  as.Date(make_datetime(addCart.df$Year, addCart.df$Month, tz = "EST"), "%y/%m/%d")

x.font.size <- 8

sheetOne.df %>% 
  ggplot() +
  aes(x = YearMonth, y = Sessions, color = Device) +
  geom_line(aes(group = Device)) +
  geom_point() +
  labs(title = "Monthly Sessions by Device",
       y = "Number of Sessions") +
  scale_x_date(date_breaks = "1 months",
               date_labels = "%b\n%Y",
               expand = c(0.02,0.02)) +
  scale_y_continuous(labels = scales::comma) +
  theme(axis.text.x=element_text(size = x.font.size)) -> MS.byDiv.p # stop axis labels being abbreviated



sheetOne.df %>% 
  ggplot() +
  aes(x = YearMonth, y = Transactions, color = Device) +
  geom_line(aes(group = Device)) +
  geom_point() +
  labs(title = "Monthly Transactions by Device",
       y = "Number of Transactions") +
  scale_x_date(date_breaks = "1 months",
               date_labels = "%b\n%Y",
               expand = c(0.02,0.02)) +
  scale_y_continuous(labels = scales::comma) +
  theme(axis.text.x=element_text(size = x.font.size)) -> MT.byDiv.p

sheetOne.df %>% 
  ggplot() +
  aes(x = YearMonth, y = QTY.agg, color = Device) +
  geom_line(aes(group = Device)) +
  geom_point() +
  labs(title = "Monthly Quantity by Device", y = "Quantity") +
  scale_x_date(date_breaks = "1 months",
               date_labels = "%b\n%Y",
               expand = c(0.02,0.02)) +
  scale_y_continuous(labels = scales::comma) +
  theme(axis.text.x=element_text(size = x.font.size)) -> MQ.byDiv.p

sheetOne.df %>% 
  ggplot() +
  aes(x = YearMonth, y = ECR, color = Device) +
  geom_line(aes(group = Device)) +
  geom_point() +
  labs(title = "Monthly ECR by Device", y = "ECR") +
  scale_x_date(date_breaks = "1 months",
               date_labels = "%b\n%Y",
               expand = c(0.02,0.02)) +
  scale_y_continuous(labels = scales::comma) +
  theme(axis.text.x=element_text(size = x.font.size)) -> ME.byDiv.p



ggarrange(MS.byDiv.p, MT.byDiv.p, MQ.byDiv.p, ME.byDiv.p)

# ----- Pie charts for Browsers ----- #
sessionCnt.df %>%
  group_by(Browser) %>%
  summarise(Transactions = sum(transactions),
            Sessions = sum(sessions),
            QTY.agg = sum(QTY)) -> browser.df

browser.df %>%
  mutate(share = Transactions/sum(Transactions)) %>%
  filter(share > 0.01) -> browser.df # can ask the client


palette.rnd <- distinctColorPalette(nrow(browser.df))

browser.df %>%
  ggplot(aes(x = "", y = Transactions, fill = Browser))+
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(round(Transactions/sum(Transactions), 2)*100, "%")), 
            position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL, 
       title = "Browsers share") +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5))



#########################
# Performing the tasks
#########################

# The first sheet should contain a Month * Device aggregation of the data with the following metrics:
# Sessions, Transactions, QTY, and ECR (= Transactions / Sessions)

sheetOne.df <- sheetOne.df[,-which(names(sheetOne.df) == "YearMonth")]
wb <- createWorkbook()
addWorksheet(wb = wb, sheetName = "Sheet 1", gridLines = FALSE)
writeDataTable(wb = wb, sheet = 1, x = sheetOne.df)


# The second sheet should contain a Month over Month comparison (for the most recent two months
# in the data) for all available metrics (including Adds to Cart), showing: the most recent month's
# valuem, the prior month's value, and both the absolute and relative differences between them
sheetOne.df %>%
  group_by(Month, Year) %>%
  summarise(Transactions.all = sum(Transactions),
            Sessions.all = sum(Sessions),
            QTY.all = sum(QTY.agg),
            ECR.all = sum(Transactions)/sum(Sessions)) %>%
  arrange(Year, Month) -> Mcomp.df

Mcomp.df %>%
  left_join(addCart.df, by = "Month") -> Mcomp.df
colnames(Mcomp.df)[which(names(Mcomp.df) == "Year.x")] <- "Year"
Mcomp.df <- Mcomp.df[-which(names(Mcomp.df) == "Year.y")] # remove dim_year

# Create month over month comparisons
Mcomp.df$CartToBuyRate <- Mcomp.df$QTY.all/Mcomp.df$AddToCart  
Mcomp.df$SesDiff <- c(NA, diff(Mcomp.df$Sessions.all))
Mcomp.df$SesGrowthRate <- Mcomp.df$SesDiff/lag(Mcomp.df$Sessions.all)
Mcomp.df$TransDiff <- c(NA, diff(Mcomp.df$Transactions.all))
Mcomp.df$TransGrowthRate <- Mcomp.df$TransDiff/lag(Mcomp.df$Transactions.all)
Mcomp.df$QTY.allDiff <- c(NA, diff(Mcomp.df$QTY.all))
Mcomp.df$QTY.allGrowthRate <- Mcomp.df$QTY.allDiff/lag(Mcomp.df$QTY.all)
Mcomp.df$ECR.allDiff <- c(NA, diff(Mcomp.df$ECR.all))
Mcomp.df$AddToCartDiff <- c(NA, diff(Mcomp.df$AddToCart))
Mcomp.df$CartToBuyRateDiff <- c(NA, diff(Mcomp.df$CartToBuyRate))

# Rearrange the order of columns
Mcomp.df %>%
  relocate(Sessions.all, .before = SesDiff) %>%
  relocate(Transactions.all, .before = TransDiff) %>%
  relocate(QTY.all, .before = QTY.allDiff) %>%
  relocate(ECR.all, .before = ECR.allDiff) %>%
  relocate(AddToCart, .before = AddToCartDiff) %>%
  relocate(CartToBuyRate, .before = CartToBuyRateDiff) -> Mcomp.df


# Visualize AddTo Cart and CartToBuyRate
Mcomp.df$YearMonth <- as.Date(make_datetime(Mcomp.df$Year, Mcomp.df$Month, tz = "EST"), "%y/%m/%d")

addCart.df %>% 
  ggplot() +
  aes(x = YearMonth, y = AddToCart) +
  geom_line(colour = "#87986a") +
  geom_point(colour = "#87986a") +
  labs(title = "Monthly Adds to Cart", y = "Number of Items") +
  scale_x_date(date_breaks = "1 months",
               date_labels = "%b\n%Y",
               expand = c(0.02,0.02)) +
  scale_y_continuous(labels = scales::comma) -> MAtC.p

Mcomp.df %>% 
  ggplot() +
  aes(x = YearMonth, y = CartToBuyRate) +
  geom_line(colour = "#87986a") +
  geom_point(colour = "#87986a") +
  labs(title = "Monthly Cart-to-Buy Rate", y = "Rate") +
  scale_x_date(date_breaks = "1 months",
               date_labels = "%b\n%Y",
               expand = c(0.02,0.02)) +
  scale_y_continuous(labels = scales::comma) -> MCtBR.p

ggarrange(MAtC.p, MCtBR.p, nrow = 2)

Mcomp.df <- Mcomp.df[,-which(names(Mcomp.df) == "YearMonth")]


# Wirte to Excel file
addWorksheet(wb = wb, sheetName = "Sheet 2", gridLines = FALSE)
writeDataTable(wb = wb, sheet = 2, x = Mcomp.df)
saveWorkbook(wb, "Client Project_Joy Chang.xlsx", overwrite = TRUE)


############
# Extra
############
# Separate by devices
MCompare.sbyDev <- function(dev){
  sheetOne.df %>%
    filter(Device == dev) -> dev.df
  dev.df$SesDiff <- c(NA, diff(dev.df$Sessions))
  dev.df$SesGrowthRate <- dev.df$SesDiff/lag(dev.df$Sessions)
  dev.df$TransDiff <- c(NA, diff(dev.df$Transactions))
  dev.df$TransGrowthRate <- dev.df$TransDiff/lag(dev.df$Transactions)
  dev.df$QTY.aggDiff <- c(NA, diff(dev.df$QTY.agg))
  dev.df$QTY.aggGrowthRate <- dev.df$QTY.aggDiff/lag(dev.df$QTY.agg)
  dev.df$ECRDiff <- c(NA, diff(dev.df$ECR))
  return(dev.df)
}

df.list <- lapply(levels(sheetOne.df$Device), MCompare.sbyDev)
MComp.sepByDev.df <- bind_rows(df.list[[1]], df.list[[2]], df.list[[3]])