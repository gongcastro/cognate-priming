test_attrition_trials <- function(x){
	
	test_that("x has the right columns", {
		expect_equal(
			colnames(x),
			c("session_id",
			  "trial",
			  "trial_type",
			  "looking_time",
			  "is_valid_trial",
			  "is_valid_vocab",
			  "is_valid_vocab_all",
			  "is_valid_gaze",
			  "is_valid_gaze_all")
		)
	})
	
	test_that("x has no duplicated combinations of participant and age group", {
		expect_true(
			all(count(x, session_id, trial)$n == 1)
		)
	})
	
	test_that("attrition has the variable classes", {
		expect_equal(class(x$session_id), "character")
		expect_equal(class(x$trial), "integer")
		expect_type(x$trial_type, "character")
		expect_type(x$is_valid_gaze, "logical")
		expect_type(x$is_valid_gaze_all, "list")
		expect_type(x$is_valid_vocab, "logical")
		expect_type(x$is_valid_vocab_all, "list")
		expect_type(x$is_valid_trial, "logical")
	})
	
	test_that("attrition missing data is dealt with", {
		expect_false(any(is.na(x$session_id)))
		expect_false(any(is.na(x$trial)))
		expect_false(any(is.na(x$trial_type)))
		expect_false(any(is.na(x$is_valid_gaze)))
		expect_false(any(is.na(x$is_valid_gaze_all)))
		expect_false(any(is.na(x$is_valid_vocab)))
		expect_false(any(is.na(x$is_valid_vocab_all)))
		expect_false(any(is.na(x$is_valid_trial)))
	})
	
	test_that("attrition variables have the right values", {
		expect_true(all(between(unique(x$trial), 0, 32)))
		expect_true(all(unique(x$trial_type) %in% c("Cognate", "Non-cognate", "Unrelated")))
	})
	
}

test_attrition_participants <- function(x){
	
	test_that("x has the right columns", {
		expect_equal(
			colnames(x),
			c("session_id",
			  ".ntrials",
			  "is_valid_participant"))
	})
	
	test_that("x has no duplicated combinations of participant and age group", {
		expect_true(
			all(count(x, session_id)$n == 1) 
		)
	})
	
	test_that("x has the variable classes", {
		expect_equal(class(x$session_id), "character")
		expect_type(x$.ntrials, "list")
		expect_type(unlist(x$.ntrials), "integer")
		expect_type(x$is_valid_participant, "logical")
	})
	
	test_that("attrition missing data is dealt with", {
		expect_false(any(is.na(x$session_id)))
		expect_false(any(is.na(x$.ntrials)))
		expect_false(any(is.na(x$is_valid_participant)))
		
		
	})
	
	test_that("attrition variables have the right values", {
		expect_true(all(between(unlist(map(x$.ntrials, "cognate")), 0, 8)))
		expect_true(all(between(unlist(map(x$.ntrials, "noncognate")), 0, 8)))
		expect_true(all(between(unlist(map(x$.ntrials, "unrelated")), 0, 16)))
	})
	
}
