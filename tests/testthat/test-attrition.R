test_attrition_trials <- function(attrition_trials){
	
	test_that("attrition_trials has the right columns", {
		expect_equal(
			colnames(attrition_trials),
			c("id",
			  "age_group",
			  "trial_type",
			  "trial",
			  "is_valid_gaze_prime",
			  "is_valid_gaze_test",
			  "is_valid_gaze_test_each",
			  "is_valid_trial"))
	})
	
	test_that("attrition_trials has no duplicated combinations of participant and age group", {
		expect_true(
			all(count(attrition_trials, id, age_group, trial)$n == 1)
		)
	})
	
	test_that("attrition has the variable classes", {
		expect_type(attrition_trials$id, "integer")
		expect_equal(class(attrition_trials$age_group), "character")
		expect_equal(class(attrition_trials$trial), "integer")
		expect_type(attrition_trials$trial_type, "character")
		expect_type(attrition_trials$is_valid_gaze_prime, "logical")
		expect_type(attrition_trials$is_valid_gaze_test, "logical")
		expect_type(attrition_trials$is_valid_gaze_test_each, "logical")
		expect_type(attrition_trials$is_valid_trial, "logical")
	})
	
	test_that("attrition missing data is dealt with", {
		expect_false(any(is.na(attrition_trials$id)))
		expect_false(any(is.na(attrition_trials$age_group)))
		expect_false(any(is.na(attrition_trials$trial)))
		expect_false(any(is.na(attrition_trials$trial_type)))
		expect_false(any(is.na(attrition_trials$is_valid_gaze_prime)))
		expect_false(any(is.na(attrition_trials$is_valid_gaze_test)))
		expect_false(any(is.na(attrition_trials$is_valid_gaze_test_each)))
		expect_false(any(is.na(attrition_trials$is_valid_trial)))
		
	})
	
	test_that("attrition variables have the right values", {
		expect_true(all(unique(attrition_trials$age_group) %in% paste0(c(21, 25, 30), " months")))
		expect_true(all(between(unique(attrition_trials$trial), 0, 32)))
		expect_true(all(unique(attrition_trials$trial_type) %in% c("Cognate", "Non-cognate", "Unrelated")))
		
	})
	
}

test_attrition_participants <- function(attrition_participants){
	
	test_that("attrition_participants has the right columns", {
		expect_equal(
			colnames(attrition_participants),
			c("id",
			  "age_group",
			  "cognate",
			  "noncognate",
			  "unrelated",
			  "is_valid_cognate",
			  "is_valid_noncognate",
			  "is_valid_unrelated",
			  "is_valid_participant"))
	})
	
	test_that("attrition_participants has no duplicated combinations of participant and age group", {
		expect_true(
			all(count(attrition_participants, id, age_group)$n == 1) 
		)
	})
	
	test_that("attrition_participants has the variable classes", {
		expect_type(attrition_participants$id, "integer")
		expect_equal(class(attrition_participants$age_group), "character")
		expect_type(attrition_participants$cognate, "integer")
		expect_type(attrition_participants$noncognate, "integer")
		expect_type(attrition_participants$unrelated, "integer")
		expect_type(attrition_participants$is_valid_cognate, "logical")
		expect_type(attrition_participants$is_valid_noncognate, "logical")
		expect_type(attrition_participants$is_valid_unrelated, "logical")
		expect_type(attrition_participants$is_valid_participant, "logical")
	})
	
	test_that("attrition missing data is dealt with", {
		expect_false(any(is.na(attrition_participants$id)))
		expect_false(any(is.na(attrition_participants$age_group)))
		expect_false(any(is.na(attrition_participants$cognate)))
		expect_false(any(is.na(attrition_participants$noncognate)))
		expect_false(any(is.na(attrition_participants$unrelated)))
		expect_false(any(is.na(attrition_participants$is_valid_cognate)))
		expect_false(any(is.na(attrition_participants$is_valid_noncognate)))
		expect_false(any(is.na(attrition_participants$is_valid_unrelated)))
		expect_false(any(is.na(attrition_participants$is_valid_participant)))
		
		
	})
	
	test_that("attrition variables have the right values", {
		expect_true(all(unique(attrition_participants$age_group) %in% paste0(c(21, 25, 30), " months")))
		expect_true(all(between(unique(attrition_participants$cognate), 0, 8)))
		expect_true(all(between(unique(attrition_participants$noncognate), 0, 8)))
		expect_true(all(between(unique(attrition_participants$unrelated), 0, 16)))
	})
	
}
