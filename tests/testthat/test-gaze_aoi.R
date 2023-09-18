test_gaze_aoi <- function(gaze_aoi){
	
	test_that("gaze_aoi has the right columns", {
		expect_equal(
			colnames(gaze_aoi),
			c("filename",
			  "trial",
			  "phase",
			  "timestamp",
			  "x",
			  "y",
			  "is_gaze_prime",
			  "is_gaze_target",
			  "is_gaze_distractor",
			  "is_valid_gaze",
			  "is_imputed",
			  "trial_type",
			  "prime_cdi",
			  "target_cdi",
			  "distractor_cdi")
		)
	})
	
	test_that("gaze_aoi has the variable classes", {
		expect_type(gaze_aoi$filename, "character")
		expect_equal(class(gaze_aoi$trial), "integer")
		expect_type(gaze_aoi$phase, "character")
		expect_type(gaze_aoi$timestamp, "double")
		expect_type(gaze_aoi$x, "double")
		expect_type(gaze_aoi$y, "double")
		expect_type(gaze_aoi$is_gaze_prime, "logical")
		expect_type(gaze_aoi$is_gaze_target, "logical")
		expect_type(gaze_aoi$is_gaze_distractor, "logical")
		expect_type(gaze_aoi$is_valid_gaze, "logical")
		expect_type(gaze_aoi$is_imputed, "logical")
		expect_type(gaze_aoi$trial_type, "character")

		
	})
	
	test_that("gaze_aoi missing data is dealt with", {
		expect_false(any(is.na(gaze_aoi$filename)))
		expect_false(any(is.na(gaze_aoi$trial)))
		expect_false(any(is.na(gaze_aoi$phase)))
		expect_false(any(is.na(unique(gaze_aoi$timestamp))))
		expect_false(any(is.na(gaze_aoi$is_valid_gaze)))
		expect_false(any(is.na(gaze_aoi$trial_type)))
		expect_false(any(is.na(gaze_aoi$prime_cdi)))
		expect_false(any(is.na(gaze_aoi$target_cdi)))
		expect_false(any(is.na(gaze_aoi$distractor_cdi)))
		
	})
	
	test_that("gaze_aoi variables have the right values", {
		expect_true(all(between(unique(gaze_aoi$trial), 0, 32)))
		expect_true(all(unique(gaze_aoi$phase) %in% c("Prime", "Target-Distractor")))
		expect_true(all(between(unique(gaze_aoi$timestamp[gaze_aoi$phase=="Prime"]), 0, 1.5)))
		expect_true(all(between(unique(gaze_aoi$timestamp[gaze_aoi$phase=="Target-Distractor"]), 0, 2)))
		expect_true(all(unique(gaze_aoi$trial_type) %in% c("Cognate", "Non-cognate", "Unrelated")))
	})
	
}
