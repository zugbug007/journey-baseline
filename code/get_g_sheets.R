##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                          Import Google Sheet Data                        ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

gsheet = "https://docs.google.com/spreadsheets/d/18yWHyyWGSxSYc35lIAYvWPBFxZNB04WHnDG0_MmyHEo/edit#gid=0"

journey_segments_googlesheet <- read_sheet(gsheet, range = "journey")
journey_metrics_googlesheet <- read_sheet(gsheet, range = "metrics")

journey_segments <- journey_segments_googlesheet #%>% slice(35:45)
