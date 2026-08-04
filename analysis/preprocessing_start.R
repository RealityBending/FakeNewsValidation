library(jsonlite)
library(progress)


options(warn = 2) # Stop on warnings

path <- "C:/Users/asf25/Box/FakeNewsValidation/"

files <- list.files(path, full.names = TRUE, pattern = "*.csv")

# Progress bar
progbar <- progress_bar$new(total = length(files))

alldata <- list()

for (file in files) {
  progbar$tick()
  rawdata <- read.csv(file)
  
  files = "01aa6u0q69.csv"
  
  participant <- gsub(".csv", "", rev(strsplit(file, "/")[[1]])[1]) # Filename without extension
  

  # Initialize participant-level data
  dat <- rawdata[rawdata$screen == "browser_info", ]
  

  data_ppt <- data.frame(
    Participant = participant,
    Experiment_StartDate = as.POSIXct(paste(dat$date, dat$time), format = "%d/%m/%Y %H:%M:%S"),
    Experiment_Duration = rawdata[rawdata$screen == "demographics_debrief", "time_elapsed"] / 1000 / 60,
    Browser_Version = paste(dat$browser, dat$browser_version),
    Mobile = dat$mobile,
    Platform = dat$os,
    Screen_Width = dat$screen_width,
    Screen_Height = dat$screen_height
    )
    
    if("prolific_id" %in% colnames(dat)){
    data_ppt$Prolific_ID <- dat$prolific_id
    }
  
  # Demographics
  demog1 <- jsonlite::fromJSON(rawdata[rawdata$screen == "demographics_1", ]$response)
  demog2 <- jsonlite::fromJSON(rawdata[rawdata$screen == "demographics_2", ]$response)
  
  
  data_ppt$Gender <- ifelse(!is.null(demog1$gender), demog1$gender, NA)
  data_ppt$Age <- ifelse(!is.null(demog2$age), demog2$age, NA)
  data_ppt$Ethnicity <- ifelse(!is.null(demog2$ethnicity), demog2$ethnicity, NA)
  data_ppt$Nationality <- ifelse(!is.null(demog2$nationality), demog2$nationality, NA)
  data_ppt$Years_Singapore <- ifelse(!is.null(demog2$years_singapore), demog2$years_singapore, NA)
  
  data_ppt$Education <- ifelse(!is.null(demog1$education), demog1$education, NA)
  data_ppt$Education <- ifelse(demog1$education %in% c("University (bachelor) <sub><sup>or equivalent</sup></sub>"), "Bachelor or equivalent", data_ppt$education)
  data_ppt$Education <- ifelse(demog1$education == "other", demog1$`education-Comment`,demog1$education)
  
  
  data_ppt$Student <- ifelse(!is.null(demog1$student), demog1$student, NA)
  data_ppt$Discipline <- ifelse(!is.null(demog2$education), demog2$education, NA)
  
  data_ppt$English <- ifelse(!is.null(demog1$english), demog1$english, NA)
  
  data_ppt$AI_expertise <- ifelse(!is.null(demog1$ai_expertise), demog1$ai_expertise, NA)
  
  ## QUESTIONNAIRES
  
  # Bait
  bait <- as.data.frame(jsonlite::fromJSON(rawdata[
    rawdata$screen == "questionnaire_bait",
    "response"
  ]))
  data_ppt <- cbind(data_ppt, bait)
  data_ppt$Duration_BAIT <- as.numeric(rawdata[rawdata$screen == "questionnaire_bait","rt"]) / 1000 / 60
  
  # PID5
  pid5 <- as.data.frame(jsonlite::fromJSON(rawdata[ rawdata$screen == "questionnaire_pid5","response"]))
  data_ppt <- cbind(data_ppt, setNames(pid5, paste0("PID5_", names(pid5))))  data_ppt$Duration_PID5 <- as.numeric(rawdata[rawdata$screen == "questionnaire_pid5","rt"]) / 1000 / 60
  
  # IPIP6 
  ipip6 <- as.data.frame(jsonlite::fromJSON(rawdata[ rawdata$screen == "questionnaire_ipip6","response"]))
  data_ppt <- cbind(data_ppt, setNames(ipip6, paste0("IPIP6_", names(pid5))))  data_ppt$Duration_IPIP6 <- as.numeric(rawdata[rawdata$screen == "questionnaire_ipip6","rt"]) / 1000 / 60
  data_ppt$Duration_PID5 <- as.numeric(rawdata[rawdata$screen == "questionnaire_ipip6","rt"]) / 1000 / 60
  
  # Save all
  alldata[[file]] <- data_ppt
}
