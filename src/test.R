tar_load_globals()
tar_load(participants)
tar_load(stimuli)
tar_load(vocabulary)
tar_load(aoi_coords)

files <- list.files(
  "data-raw/eyetracking-oxf",
  pattern = ".csv$",
  full.names = TRUE
)

gaze <- get_gaze_oxf(files_oxf, participants, stimuli)

attrition_trials <- get_attrition_trials(gaze, participants, stimuli, vocabulary,
  vocabulary_by = "none",
  aoi_coords = aoi_coords,
  min_looking = c(
    prime = 0.75,
    test = 1.00,
    test_each = 0.10,
    test_any = 0.00
  )
)

attrition_participants <- get_attrition_participants(attrition_trials,
  vocabulary,
  min_trials = c(
    cognate = 2,
    noncognate = 2,
    unrelated = 2
  ),
  min_l1_vocab = 0.1
)
