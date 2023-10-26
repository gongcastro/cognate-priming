test_gaze <- function(x){
	
	test_that("x has the right columns", {
		expect_equal(
			colnames(x),
			c("child_id",
			  "session_id",
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
	
	test_that("x has the variable classes", {
		expect_type(x$child_id, "character")
		expect_type(x$session_id, "character")
		expect_equal(class(x$trial), "integer")
		expect_type(x$phase, "character")
		expect_type(x$timestamp, "double")
		expect_type(x$x, "double")
		expect_type(x$y, "double")
		expect_type(x$is_gaze_prime, "logical")
		expect_type(x$is_gaze_target, "logical")
		expect_type(x$is_gaze_distractor, "logical")
		expect_type(x$is_valid_gaze, "logical")
		expect_type(x$is_imputed, "logical")
		expect_type(x$trial_type, "character")
		
		
	})
	
	test_that("x missing data is dealt with", {
		expect_false(any(is.na(x$child_id)))
		expect_false(any(is.na(x$session_id)))
		expect_false(any(is.na(x$trial)))
		expect_false(any(is.na(x$phase)))
		expect_false(any(is.na(unique(x$timestamp))))
		expect_false(any(is.na(x$is_valid_gaze)))
		expect_false(any(is.na(x$trial_type)))
		expect_false(any(is.na(x$prime_cdi)))
		expect_false(any(is.na(x$target_cdi)))
		expect_false(any(is.na(x$distractor_cdi)))
		
	})
	
	test_that("x variables have the right values", {
		expect_true(all(between(unique(x$trial), 0, 32)))
		expect_true(all(unique(x$phase) %in% c("Prime", "Target-Distractor")))
		expect_true(all(between(unique(x$timestamp[x$phase=="Prime"]), 0, 1.5)))
		expect_true(all(between(unique(x$timestamp[x$phase=="Target-Distractor"]), 0, 2)))
		expect_true(all(unique(x$trial_type) %in% c("Cognate", "Non-cognate", "Unrelated")))
	})
	
}

test_gaze_raw <- function(x){
	
	test_that("x has the right columns", {
		expect_equal(
			colnames(x),
			c("filename",
			  "trial",
			  "phase",
			  "x",
			  "y",
			  "is_valid_gaze")
		)
	})
	
	test_that("x has the variable classes", {
		expect_type(x$filename, "character")
		expect_equal(class(x$trial), "integer")
		expect_type(x$phase, "character")
		expect_type(x$x, "double")
		expect_type(x$y, "double")
		expect_type(x$is_valid_gaze, "logical")
	})
	
	test_that("x missing data is dealt with", {
		expect_false(any(is.na(x$filename)))
		expect_false(any(is.na(x$trial)))
		expect_false(any(is.na(x$phase)))
		expect_false(any(is.na(x$is_valid_gaze)))
	})
	
	test_that("x variables have the right values", {
		phases <- c("Audio", "Blank", "Getter", "Prime", "Target-Distractor")
		expect_true(all(between(unique(x$trial), 0, 32)))
		expect_true(all(unique(x$phase) %in% phases))
	})
	
}
