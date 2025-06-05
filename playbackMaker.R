library(tuneR)
library(seewave)
library(dplyr)

# ==== USER-DEFINED VARIABLES ====
species <- "CrestedEagle"
note_dir <- "C:\\Users\\Jawor\\Desktop\\Playbacks\\Notes\\CrestedEagle_Notes"
output_dir <- "C:\\Users\\Jawor\\Desktop\\Playbacks\\CrestedEagle_Playbacks"
n_playbacks <- 100  # Number of playbacks to generate
silence_rate <- 44100  # Sample rate (adjust if different)

# Create output folder if needed
if (!dir.exists(output_dir)) dir.create(output_dir)

# ==== LOAD AND STANDARDIZE NOTES ====
note_files <- list.files(note_dir, pattern = "\\.wav$", full.names = TRUE)

# Read, convert to mono if needed, and resample
notes <- lapply(note_files, function(file) {
  wav <- readWave(file)
  
  # Convert stereo to mono
  if (wav@stereo) {
    wav <- mono(wav, which = "left")
  }
  
  # Resample if needed
  if (wav@samp.rate != silence_rate) {
    wav <- seewave::resamp(wav, f = wav@samp.rate, g = silence_rate, output = "Wave")
  }
  
  return(wav)
})

names(notes) <- basename(note_files)  # Use full filenames as keys

# ==== SILENCE GENERATORS ====
make_silence <- function(duration, samp.rate = silence_rate) {
  silence <- rep(0, duration * samp.rate)
  Wave(left = silence, samp.rate = samp.rate, bit = 16)
}
silence3s <- make_silence(3)
silence1s <- make_silence(1)

# ==== TRACKING TABLE ====
playback_log <- data.frame(
  PlaybackName = character(),
  Species = character(),
  Note1 = character(),
  Note2 = character(),
  Note3 = character(),
  Note4 = character(),
  AllNotes = character(),
  stringsAsFactors = FALSE
)

# ==== GENERATE UNIQUE PLAYBACKS ====
set.seed(123)  # For reproducibility
used_combinations <- list()

for (i in 1:n_playbacks) {
  repeat {
    # Randomly select 4 unique note names
    selected_names <- sample(names(notes), 4, replace = FALSE)
    
    # Sort to check uniqueness (order matters in playback, but for avoiding duplicates, sort helps)
    combo_key <- paste(sort(selected_names), collapse = "_")
    if (!(combo_key %in% used_combinations)) {
      used_combinations <- c(used_combinations, combo_key)
      break
    }
  }
  
  # Combine audio
  playback <- bind(
    silence3s,
    notes[[selected_names[1]]], notes[[selected_names[2]]],
    silence1s,
    notes[[selected_names[3]]], notes[[selected_names[4]]]
  )
  
  # Define playback file name
  playback_name <- paste0(species, i)
  playback_filename <- file.path(output_dir, paste0(playback_name, ".wav"))
  
  # Save WAV file
  writeWave(playback, filename = playback_filename)
  
  # Add record to log
  playback_log <- playback_log |> 
    add_row(
      PlaybackName = playback_name,
      Species = species,
      Note1 = selected_names[1],
      Note2 = selected_names[2],
      Note3 = selected_names[3],
      Note4 = selected_names[4],
      AllNotes = paste(Note1, Note2, Note3, Note4, sep = ", ")
    )
}

# ==== EXPORT LOG ====
log_filename <- file.path(output_dir, paste0(species, "_playback_log.csv"))
write.csv(playback_log, log_filename, row.names = FALSE)

# Preview
print(playback_log)
