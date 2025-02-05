library(shiny)
library(dplyr)
library(lubridate)

# Define the folder path where flight logs are stored
FLIGHTREADfolderpath <- "D:/Gal2023_Drone/FlightReader_logs/"

# Read all CSV files and merge into a single dataframe
tmp.files <- list.files(path = FLIGHTREADfolderpath, pattern = "*.csv", full.names = TRUE, recursive = T)
df_list <- lapply(tmp.files, read.csv)
df_list <- lapply(df_list, function(df) {
  df %>% mutate(across(everything(), as.character))
})




d <- bind_rows(df_list)

# Convert datetime to proper format
d$datetime_utc6 <- mdy_hms(d$CUSTOM.updateDateTime24, tz = "Etc/GMT+6")

# Arrange data by timestamp
d <- d %>%
  arrange(datetime_utc6, OSD.flyTime..s.) %>%
  select(OSD.flyTime..s., datetime_utc6, OSD.height..m., GIMBAL.pitch)



d <- d %>%
  distinct(datetime_utc6, .keep_all = TRUE)

d$GIMBAL.pitch <- as.numeric(d$GIMBAL.pitch)


# Define UI
ui <- fluidPage(
  titlePanel("Gimbal Pitch < -87° Time Finder"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("video_file", "Enter Video File Name:", "Gal2023_DJIMini2_20230201_091905.MP4"),
      actionButton("submit", "Find Time Ranges")
    ),
    
    mainPanel(
      tableOutput("result_table")
    )
  )
)

# Define Server Logic
server <- function(input, output) {
  
  result <- eventReactive(input$submit, {
    
    # Extract date and time from the video file name
    video_file <- input$video_file
    date_part <- unlist(strsplit(video_file, "_"))[3]  
    time_part <- gsub(".MP4", "", unlist(strsplit(video_file, "_"))[4])
    
    # Compute video start datetime (local time)
    video_start_time <- ymd_hms(paste0(date_part, time_part), tz = "Etc/GMT+6")
    
    # Filter rows where gimbal pitch < -87°
    filtered_data <- d %>%
      filter(GIMBAL.pitch < -87)
    
    if (nrow(filtered_data) == 0) {
      return(data.frame(Message = "No gimbal pitch < -87° found"))
    }
    
    # Compute time in seconds relative to video start
    filtered_data <- filtered_data %>%
      mutate(time_seconds = as.numeric(difftime(datetime_utc6, video_start_time, units = "secs")))
    
    # Identify continuous segments
    filtered_data <- filtered_data %>%
      mutate(group = cumsum(c(1, diff(time_seconds) > 1))) # New group if time gap > 1 sec
    
    # Summarize start and end times for each segment
    result_table <- filtered_data %>%
      group_by(group) %>%
      summarize(start_time_sec = min(time_seconds), end_time_sec = max(time_seconds), .groups = "drop") %>%
      select(start_time_sec, end_time_sec)
    
    # Filter only time intervals within 0 - 720 seconds
    result_table <- result_table %>%
      filter(start_time_sec >= 0, end_time_sec <= 720)
    
    # If no valid intervals, return a message
    if (nrow(result_table) == 0) {
      return(data.frame(Message = "No intervals within 0-720s found"))
    }
    
    return(result_table)
  })
  
  # Render result table
  output$result_table <- renderTable({
    result()
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
