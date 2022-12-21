test_gaze_processed <- function(gaze_processed){
	
	test_that("gaze_processed has the right columns", {
		expect_equal(
			colnames(gaze_processed),
			c("id",
			  "age_group",
			  "trial",
			  "trial_id",
			  "phase",
			  "time",
			  "x",
			  "y",
			  "valid_sample",
			  "filename"))
	})
	
	test_that("gaze_processed has the variable classes", {
		expect_type(gaze_processed$id, "character")
		expect_equal(class(gaze_processed$age_group), "character")
		expect_equal(class(gaze_processed$trial), "integer")
		expect_equal(class(gaze_processed$trial_id), "integer")
		expect_type(gaze_processed$phase, "character")
		expect_type(gaze_processed$time, "double")
		expect_type(gaze_processed$x, "double")
		expect_type(gaze_processed$y, "double")
		expect_type(gaze_processed$valid_sample, "logical")
		expect_type(gaze_processed$filename, "character")
		
	})
	
	test_that("gaze_processed missing data is dealt with", {
		expect_false(any(is.na(gaze_processed$id)))
		expect_false(any(is.na(gaze_processed$age_group)))
		expect_false(any(is.na(gaze_processed$trial)))
		expect_false(any(is.na(gaze_processed$trial_id)))
		expect_false(any(is.na(gaze_processed$valid_sample)))

	})
	
	test_that("gaze_processed variables have the right values", {
		expect_true(all(unique(gaze_processed$age_group) %in% paste0(c(21, 25, 30), " months")))
		expect_true(all(between(unique(gaze_processed$trial), 0, 32)))
		expect_true(all(between(unique(gaze_processed$trial_id), 0, 32)))
		expect_true(all(unique(gaze_processed$phase) %in% c("Prime", "Target-Distractor")))
		expect_true(all(between(unique(gaze_processed$time[gaze_processed$phase=="Prime"]), 0, 1.5)))
		expect_true(all(between(unique(gaze_processed$time[gaze_processed$phase=="Target-Distractor"]), 0, 2)))
		expect_true(all(unique(gaze_processed$valid_sample %in% c(TRUE, FALSE))))
	})
	
}