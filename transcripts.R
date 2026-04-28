library(tidyverse)
library(officer)

# ---- Parse VTT transcript ----

parse_vtt <- function(path) {
  lines <- readLines(path, encoding = "UTF-8")
  
  # Remove WEBVTT header
  lines <- lines[lines != "WEBVTT" & lines != ""]
  
  # Identify line types
  is_sequence <- str_detect(lines, "^\\d+ \"")
  is_timestamp <- str_detect(lines, "-->")
  is_text <- !is_sequence & !is_timestamp
  
  # Build a tibble by iterating through blocks
  result <- tibble(
    sequence = integer(),
    speaker = character(),
    start = character(),
    end = character(),
    text = character()
  )
  
  i <- 1
  while (i <= length(lines)) {
    if (is_sequence[i]) {
      seq_num <- as.integer(str_extract(lines[i], "^\\d+"))
      speaker <- str_extract(lines[i], '(?<=")([^"(]+)(?=\\s*\\()') |> str_trim()
      
      
      i <- i + 1
      if (i <= length(lines) && is_timestamp[i]) {
        times <- str_split(lines[i], " --> ")[[1]]
        start <- str_trim(times[1])
        end <- str_trim(times[2])
        
        i <- i + 1
        text_lines <- character()
        while (i <= length(lines) && is_text[i] && lines[i] != "") {
          text_lines <- c(text_lines, lines[i])
          i <- i + 1
        }
        
        result <- result |>
          add_row(
            sequence = seq_num,
            speaker = speaker,
            start = start,
            end = end,
            text = paste(text_lines, collapse = " ")
          )
      }
    } else {
      i <- i + 1
    }
  }
  
  result
}

# ---- Parse chat Word doc ----

parse_chat <- function(path) {
  doc <- read_docx(path)
  content <- docx_summary(doc) |>
    filter(content_type == "paragraph") |>
    pull(text)
  
  # Remove empty lines
  content <- content[content != "" & content != " "]
  
  # Chat structure repeats: name, "Unverified", timestamp, message(s)
  result <- tibble(
    name = character(),
    timestamp = character(),
    message = character()
  )
  
  i <- 1
  while (i <= length(content)) {
    # Check if this looks like a name line (followed by Unverified)
    if (i + 2 <= length(content) && content[i + 1] == "Unverified") {
      name <- content[i]
      timestamp <- content[i + 2]
      
      i <- i + 3
      msg_lines <- character()
      while (i <= length(content) && content[i] != "Unverified") {
        # Stop if next line looks like a new name block
        if (i + 1 <= length(content) && content[i + 1] == "Unverified") break
        msg_lines <- c(msg_lines, content[i])
        i <- i + 1
      }
      
      result <- result |>
        add_row(
          name = name,
          timestamp = timestamp,
          message = paste(msg_lines, collapse = " ")
        )
    } else {
      i <- i + 1
    }
  }
  
  result
}

# ---- Load both ----

transcript <- parse_vtt(here::here("docs","Transcript.vtt"))
chat <- parse_chat(here::here("docs","How scarcity affects you and your clients chat.docx"))

# Quick check
glimpse(transcript)
glimpse(chat)