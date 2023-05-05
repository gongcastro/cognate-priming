test_gaze_imputed <- function(gaze_imputed){
	
	test_that("gaze_imputed has the right columns", {
		expect_equal(
			colnames(gaze_imputed),
			c("id",
			  "age_group",
			  "trial",
			  "trial_id",
			  "phase",
			  "time",
			  "valid_sample",
			  "x",
			  "y",
			  "is_imputed",
			  "filename"))
	})
	
	test_that("gaze_imputed has the variable classes", {
		expect_type(gaze_imputed$id, "character")
		expect_equal(class(gaze_imputed$age_group), "character")
		expect_equal(class(gaze_imputed$trial), "integer")
		expect_type(gaze_imputed$phase, "character")
		expect_type(gaze_imputed$time, "double")
		expect_type(gaze_imputed$x, "double")
		expect_type(gaze_imputed$y, "double")
		expect_type(gaze_imputed$valid_sample, "logical")
		expect_type(gaze_imputed$is_imputed, "logical")
		expect_type(gaze_imputed$filename, "character")
		
	})
	
	test_that("gaze_imputed missing data is dealt with", {
		expect_false(any(is.na(gaze_imputed$id)))
		expect_false(any(is.na(gaze_imputed$age_group)))
		expect_false(any(is.na(gaze_imputed$trial)))
		expect_false(any(is.na(gaze_imputed$valid_sample)))
		expect_false(any(is.na(gaze_imputed$is_imputed)))
		
	})
	
	test_that("gaze_imputed variables have the right values", {
		expect_true(all(unique(gaze_imputed$age_group) %in% paste0(c(21, 25, 30), " months")))
		expect_true(all(between(unique(gaze_imputed$trial), 0, 32)))
		expect_true(all(unique(gaze_imputed$phase) %in% c("Prime", "Target-Distractor")))
		expect_true(all(between(unique(gaze_imputed$time[gaze_imputed$phase=="Prime"]), 0, 1.5)))
		expect_true(all(between(unique(gaze_imputed$time[gaze_imputed$phase=="Target-Distractor"]), 0, 2)))
		expect_true(all(unique(gaze_imputed$valid_sample %in% c(TRUE, FALSE))))
		expect_true(all(unique(gaze_imputed$is_imputed) %in% c(TRUE, FALSE)))
	})
	
}