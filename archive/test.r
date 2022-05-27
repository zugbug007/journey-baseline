library(adobeanalyticsr)
df <- adobeanalyticsr::aw_anomaly_report(rsid = Sys.getenv("AW_REPORTSUITE_ID"),
                                   date_range = date_range,
                                   metrics = "event179",
                                   segmentId = "s1957_61f29c94872031367baf6ed9",
                                   granularity = "day",
                                   quickView = FALSE,
                                   anomalyDetection = TRUE,
                                   countRepeatInstances = TRUE,
                                   debug = FALSE)


#get_anomaly_data <- function(segment_ids, metrics, data_range)
# install.packages("CGPfunctions")
library(CGPfunctions)
library(ggplot2)
install.packages("ggthemes")
df <- newgdp[16:30, ]

newggslopegraph(newcancer,Year,Survival,Type)



newggslopegraph(df, Year, GDP, Country,
                Title = "GDP evolution",
                SubTitle = "1970-1979",
                Caption = "By R CHARTS",
                DataLabelPadding = 0.2,
                DataLabelLineSize = 0.5,
                DataLabelFillColor = "lightblue")

newggslopegraph(df, Year, GDP, Country,
                Title = "GDP evolution",
                SubTitle = "1970-1979",
                Caption = "By R CHARTS",
                ThemeChoice = "gdocs")
newggslopegraph(df, Year, GDP, Country,
                Title = "GDP evolution",
                SubTitle = "1970-1979",
                Caption = "By R CHARTS") +
  theme_gray() +
  theme(legend.position = "none")

newggslopegraph(df, Year, GDP, Country,
                Title = "GDP evolution",
                SubTitle = "1970-1979",
                Caption = "By R CHARTS")

newggslopegraph(df, Year, GDP, Country,
                Title = "GDP evolution",
                SubTitle = "1970-1979",
                Caption = "By R CHARTS",
                ReverseYAxis = TRUE,
                ReverseXAxis = FALSE)  

newggslopegraph(df, Year, GDP, Country,
                Title = "GDP evolution",
                SubTitle = "1970-1979",
                Caption = "By R CHARTS",
                ThemeChoice = "econ")

newggslopegraph(df, Year, GDP, Country,
                Title = "GDP evolution",
                SubTitle = "1970-1979",
                Caption = "By R CHARTS",
                ThemeChoice = "wsj")
newggslopegraph(df, Year, GDP, Country,
                Title = "GDP evolution",
                SubTitle = "1970-1979",
                Caption = "By R CHARTS",
                ThemeChoice = "gdocs")
newggslopegraph(df, Year, GDP, Country,
                Title = "GDP evolution",
                SubTitle = "1970-1979",
                Caption = "By R CHARTS",
                ThemeChoice = "tufte")
newggslopegraph(dataframe = newcancer,
                Times = Year,
                Measurement = Survival,
                Grouping = Type,
                Title = "Estimates of Percent Survival Rates",
                SubTitle = "Based on: Edward Tufte, Beautiful Evidence, 174, 176.",
                Caption = NULL
)
newggslopegraph(newgdp, 
                Year, 
                GDP, 
                Country, 
                Title = "Gross GDP", 
                SubTitle = NULL, 
                Caption = NULL,
                LineThickness = .5,
                YTextSize = 4,
                LineColor = c(rep("gray",3), "red", rep("gray",3), "red", rep("gray",10))
)


newggslopegraph(baseline, 
                journey_type, 
                Visits_mean, 
                journey_name, 
                Title = "Visits Baseline by Journey", 
                SubTitle = NULL, 
                Caption = NULL,
                LineThickness = 1.2,
                YTextSize = 6,
                LineColor = c(rep("gray",3), "red", rep("gray",3), "red", rep("gray",10))
)

