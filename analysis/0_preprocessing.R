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
  if (grepl("axpezsac1r", file)) next  # skip this participant
  
  rawdata <- read.csv(file)
  
  #resp <- rawdata[rawdata$screen == "demographics_1", "response"]
  #if (length(resp) == 0 || is.na(resp)) print(file)
  

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
  data_ppt$Education <- ifelse(data_ppt$Education %in% "University (bachelor) <sub><sup>or equivalent</sup></sub>",
                               "Bachelor or equivalent", data_ppt$Education)
  if (!is.null(demog1$education) && demog1$education == "other" && !is.null(demog1$`education-Comment`)) {
    data_ppt$Education <- demog1$`education-Comment`
  }  
  
  data_ppt$Student <- ifelse(!is.null(demog1$student), demog1$student, NA)
  data_ppt$Discipline <- ifelse(!is.null(demog2$discipline), demog2$discipline, NA)
  
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
  data_ppt <- cbind(data_ppt, setNames(pid5, paste0("PID5_", names(pid5))))  
  data_ppt$Duration_PID5 <- as.numeric(rawdata[rawdata$screen == "questionnaire_pid5","rt"]) / 1000 / 60
  
  # IPIP6 
  ipip6 <- as.data.frame(jsonlite::fromJSON(rawdata[ rawdata$screen == "questionnaire_ipip6","response"]))
  data_ppt <- cbind(data_ppt, setNames(ipip6, paste0("IPIP6_", names(ipip6))))  
  data_ppt$Duration_IPIP6 <- as.numeric(rawdata[rawdata$screen == "questionnaire_ipip6","rt"]) / 1000 / 60

  # Media Frequency
  media_frequency<- as.data.frame(jsonlite::fromJSON(rawdata[ rawdata$screen == "questionnaire_media_frequency","response"]))
  data_ppt <- cbind(data_ppt, setNames(media_frequency, paste0("MediaFrequency_", names(media_frequency))))  
  data_ppt$Duration_MediaFrequency <- as.numeric(rawdata[rawdata$screen == "questionnaire_media_frequency","rt"]) / 1000 / 60
  
  
  
  # IAS
  # ias <- as.data.frame(jsonlite::fromJSON(rawdata[ rawdata$screen == "questionnaire_ias","response"]))
  # data_ppt <- cbind(data_ppt, setNames(ias, paste0("IAS_", names(ias))))  
  # data_ppt$Duration_IAS <- as.numeric(rawdata[rawdata$screen == "questionnaire_ias","rt"]) / 1000 / 60
  # 
  # PHQ4
  # phq4 <- as.data.frame(jsonlite::fromJSON(rawdata[ rawdata$screen == "questionnaire_phq4","response"]))
  # data_ppt <- cbind(data_ppt, setNames(phq4, paste0("PHQ4_", names(phq4))))  
  # data_ppt$Duration_PHQ4 <- as.numeric(rawdata[rawdata$screen == "questionnaire_phq4","rt"]) / 1000 / 60
  # 
  # GCB 
  # gcb <- as.data.frame(jsonlite::fromJSON(rawdata[ rawdata$screen == "questionnaire_gcb","response"]))
  # data_ppt <- cbind(data_ppt, setNames(gcb, paste0("GCB_", names(gcb))))  
  # data_ppt$Duration_GCB <- as.numeric(rawdata[rawdata$screen == "questionnaire_gcb","rt"]) / 1000 / 60
  # 

  # questionnaire_restingstate
  # rest <- as.data.frame(jsonlite::fromJSON(rawdata[ rawdata$screen == "questionnaire_restingstate","response"]))
  # data_ppt <- cbind(data_ppt, setNames(rest, paste0("RS_", names(rest))))  
  # data_ppt$Duration_RestingState <- as.numeric(rawdata[rawdata$screen == "questionnaire_restingstate","rt"]) / 1000 / 60
  # 
  # New types 
  # news_options <- c(
  #   "Local news", "International news", "Business and financial news",
  #   "World politics", "Local politics", "News about the economy",
  #   "Fun/weird news", "Health and education news", "Lifestyle news",
  #   "Arts and culture news", "Sports news", "Science and technology news",
  #   "Crime/sensational news", "Celebrity-related news"
  # )
  
  # news_type <- jsonlite::fromJSON(rawdata[rawdata$screen == "questionnaire_news_types", "response"])
  # selected <- news_type$type  
  # 
  # for (opt in news_options) {
  #   colname <- paste0("NewsType_", gsub("[ /-]+", "_", opt))
  #   data_ppt[[colname]] <- opt %in% selected
  # }
  
  #data_ppt$Duration_NewsType <- as.numeric(rawdata[rawdata$screen == "questionnaire_news_types", "rt"]) / 1000 / 60
  
  # Demographic debrief 
  debrief<- as.data.frame(jsonlite::fromJSON(rawdata[ rawdata$screen == "demographics_debrief","response"]))
  data_ppt <- cbind(data_ppt, setNames(debrief, paste0("Debrief_", names(debrief))))  
  data_ppt$Duration_Debrief <- as.numeric(rawdata[rawdata$screen == "demographics_debrief","rt"]) / 1000 / 60
  
  
  # Save all
  alldata[[participant]] <- data_ppt
  
  
}

demo_data <- dplyr::bind_rows(alldata)


# Anonymize ---------------------------------------------------------------
# Generate IDs
ids <- paste0("S", format(sprintf("%03d", 1:nrow(demo_data))))
# Sort Participant according to date and assign new IDs
names(ids) <- demo_data$Participant[order(demo_data$Experiment_StartDate)]
# Replace IDs
demo_data$Participant <- ids[demo_data$Participant]

write.csv(demo_data, "../data/rawdata_participant.csv", row.names = FALSE)


