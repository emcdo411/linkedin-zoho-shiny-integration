# zoho_pull_script.R
library(jsonlite)
library(readr)
library(dplyr)
library(lubridate)

# Load mock API token config
zoho_token <- jsonlite::fromJSON("../config/zoho_keys.json")$access_token

# Simulate pulling new leads from an API (here, hardcoded)
new_leads <- data.frame(
  FirstName = c("Brian", "Fatima"),
  LastName = c("Douglas", "Khan"),
  Email = c("bdouglas@example.com", "fkhan@example.com"),
  Title = c("UX Designer", "Security Analyst"),
  Company = c("OrbitWorks", "DataForge"),
  Source = c("LinkedIn", "Referral"),
  Status = c("Warm", "Hot"),
  Timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  stringsAsFactors = FALSE
)

# File path to the local sync CSV
file_path <- "../data/leads_google_sync.csv"

# Read current leads if file exists
if (file.exists(file_path)) {
  existing_leads <- read_csv(file_path, show_col_types = FALSE)
} else {
  existing_leads <- data.frame()
}

# Combine and remove duplicates based on email
all_leads <- bind_rows(existing_leads, new_leads) %>%
  distinct(Email, .keep_all = TRUE)

# Save back to CSV
write_csv(all_leads, file_path)

# Log action
cat(paste(Sys.time(), "- Synced", nrow(new_leads), "new leads\n"))
