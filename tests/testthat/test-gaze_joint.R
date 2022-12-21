test_gaze_joint <- function(gaze_joint){
	
	test_that("gaze_joint has the right columns", {
		expect_equal(
			colnames(gaze_joint),
			c("id",
			  "trial",
			  "trial_id",
			  "phase",
			  "time",
			  "x",
			  "y",
			  "valid_sample",
			  "filename"))
	})
	
	test_that("gaze_joint has the variable classes", {
		expect_type(gaze_joint$id, "character")
		expect_equal(class(gaze_joint$trial), "integer")
		expect_type(gaze_joint$phase, "character")
		expect_type(gaze_joint$time, "double")
		expect_type(gaze_joint$x, "double")
		expect_type(gaze_joint$y, "double")
		expect_type(gaze_joint$valid_sample, "logical")
		expect_type(gaze_joint$filename, "character")
		
	})
	
	test_that("gaze_joint missing data is dealt with", {
		expect_false(any(is.na(gaze_joint$id)))
		expect_false(any(is.na(gaze_joint$trial)))
		expect_false(any(is.na(gaze_joint$valid_sample)))
		
	})
	
	test_that("gaze_joint variables have the right values", {
		expect_true(all(between(unique(gaze_joint$trial), 0, 32)))
		expect_true(all(unique(gaze_joint$phase) %in% c("Prime", "Target-Distractor")))
		expect_true(all(between(unique(gaze_joint$time[gaze_joint$phase=="Prime"]), 0, 1.5)))
		expect_true(all(between(unique(gaze_joint$time[gaze_joint$phase=="Target-Distractor"]), 0, 2)))
		expect_true(all(unique(gaze_joint$valid_sample %in% c(TRUE, FALSE))))
	})
	
}