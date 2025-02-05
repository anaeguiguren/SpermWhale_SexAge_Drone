library(shiny)
library(dplyr)
library(lubridate)

# Define the folder path where flight logs are stored
AIRDATfolderpath <- "D:/Gal2023_Drone/Airdata_UAV_logs/20230201_flight_logs/"

# Read all CSV files and merge into a single dataframe
tmp.files <- list.files(path = AIRDATfolderpath, pattern = "*.csv", full.names = TRUE)
df_list <- lapply(tmp.files, read.csv)
d <- bind_rows(df_list)

# Convert and arrange data
d$datetime_utc <- ymd_hms(d$datetime.utc., tz = "UTC")
d$datetime_utc6 <- with_tz(d$datetime_utc, "Etc/GMT+6")

d <- d %>%
  arrange(datetime_utc6, time.millisecond.) %>%
  select(time.millisecond., datetime_utc6, height_above_takeoff.feet., gimbal_pitch.degrees., gimbal_roll.degrees.)

# Define the Shiny UI
ui <- fluidPage(
  titlePanel("Gimbal Angle Finder"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("video_file", "Enter Video File Name:", "Gal2023_DJIMini2_20230201_091905.MP4"),
      numericInput("snapshot_time", "Enter Snapshot Time (seconds):", value = 220.854, min = 0, step = 0.1),
      actionButton("submit", "Find Gimbal Angle")
    ),
    
    mainPanel(
      tableOutput("result_table")
    )
  )
)

# Define the Shiny Server
server <- function(input, output) {
  
  result <- eventReactive(input$submit, {
    
    # Extract date and time from the video file name
    video_file <- input$video_file
    snapshot_time <- input$snapshot_time
    
    date_part <- unlist(strsplit(video_file, "_"))[3]  
    time_part <- gsub(".MP4", "", unlist(strsplit(video_file, "_"))[4])
    
    # Compute snapshot local datetime
    snapshot_datetime <- ymd_hms(paste0(date_part, time_part), tz = "Etc/GMT+6") + round(snapshot_time)
    
    # Filter the data for the closest timestamp match
    out <- d %>%
      filter(datetime_utc6 == snapshot_datetime) %>%
      select(datetime_utc6, gimbal_pitch.degrees., height_above_takeoff.feet.)
    
    # Return the first matching row (if exists)
    if (nrow(out) > 0) {
      out$datetime_utc6 <- format(out$datetime_utc6, "%Y-%m-%d %H:%M:%S")
      return(out[1, ])
    } else {
      return(data.frame(Message = "No matching data found"))
    }
  })
  
  # Render the output table
  output$result_table <- renderTable({
    result()
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
