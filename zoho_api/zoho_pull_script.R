library(httr)
library(jsonlite)
library(dplyr)
library(purrr)

# Load saved tokens
tokens <- fromJSON("config/zoho_keys.json")

# Refresh access token
refresh_response <- POST(
  url = "https://accounts.zoho.com/oauth/v2/token",
  body = list(
    refresh_token = tokens$refresh_token,
    client_id = "1000.JLNWR2TV697Q1O7GHAT0AMWZBG16MF",
    client_secret = "c7ac8a64641df6f6fd44361ee80a762dba3914c91c",
    grant_type = "refresh_token"
  ),
  encode = "form"
)

new_tokens <- content(refresh_response, as = "parsed", type = "application/json")
access_token <- new_tokens$access_token

# Pull leads
res <- GET(
  url = "https://www.zohoapis.com/crm/v2/Leads",
  add_headers(Authorization = paste("Zoho-oauthtoken", access_token))
)

leads <- content(res, as = "parsed", type = "application/json")$data

# Handle empty response
if (length(leads) == 0) {
  cat("⚠️ No leads found in Zoho CRM.\n")
} else {
  # Safely extract each field with NA fallback
  leads_df <- map_df(leads, function(x) {
    tibble(
      Full_Name     = x$Full_Name %||% NA,
      Email         = x$Email %||% NA,
      Company       = x$Company %||% NA,
      Phone         = x$Phone %||% NA,
      Lead_Status   = x$Lead_Status %||% NA,
      Created_Time  = x$Created_Time %||% NA
    )
  })
  
  dir.create("data", showWarnings = FALSE)
  write.csv(leads_df, file = "data/leads_google_sync.csv", row.names = FALSE)
  cat("✅ Leads pulled and written to data/leads_google_sync.csv\n")
}

