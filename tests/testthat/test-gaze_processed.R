test_gaze_processed <- function(gaze_processed){
	
	test_that("gaze_processed has the right columns", {
		expect_equal(
			colnames(gaze_processed),
			c("filename",
			  "trial",
			  "phase",
			  "timestamp",
			  "x",
			  "y",
			  "is_valid_gaze",
			  "is_imputed")
		)
	})
	
	test_that("gaze_processed has the variable classes", {
		expect_type(gaze_processed$filename, "character")
		expect_equal(class(gaze_processed$trial), "integer")
		expect_type(gaze_processed$phase, "character")
		expect_type(gaze_processed$timestamp, "double")
		expect_type(gaze_processed$x, "double")
		expect_type(gaze_processed$y, "double")
		expect_type(gaze_processed$is_valid_gaze, "logical")
		expect_type(gaze_processed$is_imputed, "logical")
	})
	
	test_that("gaze_processed missing data is dealt with", {
		expect_false(any(is.na(gaze_processed$filename)))
		expect_false(any(is.na(gaze_processed$trial)))
		expect_false(any(is.na(gaze_processed$is_valid_gaze)))
		expect_false(any(is.na(gaze_processed$is_imputed)))
	})
	
	test_that("gaze_processed variables have the right values", {
		expect_true(all(between(unique(gaze_processed$trial), 0, 32)))
		expect_true(all(unique(gaze_processed$phase) %in% c("Prime", "Target-Distractor")))
		expect_true(all(between(unique(gaze_processed$timestamp[gaze_processed$phase=="Prime"]), 0, 1.5)))
		expect_true(all(between(unique(gaze_processed$timestamp[gaze_processed$phase=="Target-Distractor"]), 0, 2)))
	})
	
}