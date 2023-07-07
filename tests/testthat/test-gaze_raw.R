test_gaze_raw <- function(gaze_raw){
	
	test_that("gaze_raw has the right columns", {
		expect_equal(
			colnames(gaze_raw),
			c("id",
			  "trial",
			  "trial_id",
			  "phase",
			  "timestamp",
			  "x",
			  "y",
			  "is_valid_gaze",
			  "filename"))
	})
	
	test_that("gaze_raw has the variable classes", {
		expect_type(gaze_raw$id, "integer")
		expect_equal(class(gaze_raw$trial), "integer")
		expect_type(gaze_raw$phase, "character")
		expect_type(gaze_raw$timestamp, "double")
		expect_type(gaze_raw$x, "double")
		expect_type(gaze_raw$y, "double")
		expect_type(gaze_raw$is_valid_gaze, "logical")
		expect_type(gaze_raw$filename, "character")
		
	})
	
	test_that("gaze_raw missing data is dealt with", {
		expect_false(any(is.na(gaze_raw$id)))
		expect_false(any(is.na(gaze_raw$trial)))
		expect_false(any(is.na(gaze_raw$is_valid_gaze)))
		
	})
	
	test_that("gaze_raw variables have the right values", {
		expect_true(all(between(unique(gaze_raw$trial), 0, 32)))
		expect_true(all(unique(gaze_raw$phase) %in% c("Prime", "Target-Distractor")))
		expect_true(all(between(unique(gaze_raw$timestamp[gaze_raw$phase=="Prime"]), 0, 1.5)))
		expect_true(all(between(unique(gaze_raw$timestamp[gaze_raw$phase=="Target-Distractor"]), 0, 2)))
	})
	
}