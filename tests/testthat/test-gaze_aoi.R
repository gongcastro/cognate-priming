test_gaze_aoi <- function(gaze_aoi){
	
	test_that("gaze_aoi has the right columns", {
		expect_equal(
			colnames(gaze_aoi),
			c("id",
			  "age_group",
			  "trial",
			  "trial_id",
			  "phase",
			  "time",
			  "x",
			  "y",
			  "valid_sample",
			  "aoi_prime",
			  "aoi_target",
			  "aoi_distractor",
			  "trial_type",
			  "filename"))
	})
	
	test_that("gaze_aoi has the variable classes", {
		expect_type(gaze_aoi$id, "character")
		expect_equal(class(gaze_aoi$age_group), "character")
		expect_equal(class(gaze_aoi$trial), "integer")
		expect_type(gaze_aoi$phase, "character")
		expect_type(gaze_aoi$time, "double")
		expect_type(gaze_aoi$x, "double")
		expect_type(gaze_aoi$y, "double")
		expect_type(gaze_aoi$valid_sample, "logical")
		expect_type(gaze_aoi$aoi_prime, "logical")
		expect_type(gaze_aoi$aoi_target, "logical")
		expect_type(gaze_aoi$aoi_distractor, "logical")
		expect_type(gaze_aoi$trial_type, "character")
		expect_type(gaze_aoi$filename, "character")
		
	})
	
	test_that("gaze_aoi missing data is dealt with", {
		expect_false(any(is.na(gaze_aoi$id)))
		expect_false(any(is.na(gaze_aoi$age_group)))
		expect_false(any(is.na(gaze_aoi$trial)))
		expect_false(any(is.na(gaze_aoi$phase)))
		expect_false(any(is.na(unique(gaze_aoi$time))))
		expect_false(any(is.na(gaze_aoi$valid_sample)))
		expect_false(any(is.na(gaze_aoi$aoi_prime)))
		expect_false(any(is.na(gaze_aoi$aoi_target)))
		expect_false(any(is.na(gaze_aoi$aoi_distractor)))
		expect_false(any(is.na(gaze_aoi$trial_type)))
		expect_false(any(is.na(gaze_aoi$filename)))
	})
	
	test_that("gaze_aoi variables have the right values", {
		expect_true(all(unique(gaze_aoi$age_group) %in% paste0(c(21, 25, 30), " months")))
		expect_true(all(between(unique(gaze_aoi$trial), 0, 32)))
		expect_true(all(unique(gaze_aoi$phase) %in% c("Prime", "Target-Distractor")))
		expect_true(all(between(unique(gaze_aoi$time[gaze_aoi$phase=="Prime"]), 0, 1.5)))
		expect_true(all(between(unique(gaze_aoi$time[gaze_aoi$phase=="Target-Distractor"]), 0, 2)))
		expect_true(all(unique(gaze_aoi$valid_sample %in% c(TRUE, FALSE))))
		expect_true(all(unique(gaze_aoi$aoi_prime) %in% c(TRUE, FALSE)))
		expect_true(all(unique(gaze_aoi$aoi_target) %in% c(TRUE, FALSE)))
		expect_true(all(unique(gaze_aoi$aoi_distractor) %in% c(TRUE, FALSE)))
		expect_true(all(unique(gaze_aoi$trial_type) %in% c("Cognate", "Non-cognate", "Unrelated")))
	})
	
}