#Code for generating playbacks
library(tuneR)
library(seewave)
library(tidyverse)


# Path to folder with note files
note_dir <- "C:\\Users\\Jawor\\Desktop\\Playbacks\\Notes\\XC436991_HarpyEagle"

# List all WAV files in the folder
note_files <- list.files(note_dir, pattern = "\\.wav$", full.names = TRUE)

# Optionally sort them numerically if named Note1.wav, Note2.wav, etc.
note_files <- note_files[order(as.numeric(gsub("\\D", "", basename(note_files))))]

# Read all notes into a list
notes <- lapply(note_files, readWave)

notes <- lapply(note_files, function(file) {
  wav <- readWave(file)
  if (wav@stereo) {
    wav <- mono(wav, which = "left")  # Or "both" to average L/R channels
  }
  return(wav)
})

names(notes) <- paste0("Note", seq_along(notes))

make_silence <- function(duration, samp.rate = 44100) {
  silence <- rep(0, duration * samp.rate)
  Wave(left = silence, samp.rate = samp.rate, bit = 16)
}
silence3s <- make_silence(3)
silence1s <- make_silence(1)

output <- "C:\\Users\\Jawor\\Desktop\\Playbacks"
set.seed(123)
for (i in 1:10) {
  selected <- sample(1:69, 4)
  pb <- bind(
    silence3s,
    notes[[selected[1]]], notes[[selected[2]]],
    silence1s,
    notes[[selected[3]]], notes[[selected[4]]]
  )
  writeWave(pb, file.path(output, sprintf("XC436991_HarpyEagle_%02d.wav", i)))
}
getwd()
