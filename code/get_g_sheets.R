##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                          Import Google Sheet Data                        ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

gsheet = "https://docs.google.com/spreadsheets/d/18yWHyyWGSxSYc35lIAYvWPBFxZNB04WHnDG0_MmyHEo/edit#gid=0"

journey_segments_googlesheet <- read_sheet(gsheet, range = "journey")
journey_metrics_googlesheet <- read_sheet(gsheet, range = "metrics")
voice_of_the_supporter_googlesheet <- read_sheet(gsheet, range = "VoS")

journey_segments <- journey_segments_googlesheet# %>% slice(173:179)
# slice only includes the data and not the header rows so row number - 1 in GSheet
# 39:40 for shop checkout funnel
