test_attrition <- function(
		attrition,
		missing_trials_threshold = c(
			cognate = 2,
			non_cognate = 2, 
			unrelated = 2
		)
){
	
	test_that("attrition has the right columns", {
		expect_equal(
			colnames(attrition),
			c(
				"participant",
				"age_group",
				"trial",
				"trial_type",
				"valid_gaze_prime",
				"valid_gaze_target",
				"valid_gaze_distractor",
				"valid_trial",
				"cognate",
				"non_cognate",
				"unrelated",
				"valid_participant"
			)
		)
	})
	
	test_that("attrition has no duplicated combinations of participant and age group", {
		expect_true(
			attrition %>% 
				count(participant, age_group, trial) %>% 
				pull(n) %>% 
				all(. == 1) 
		)
	})
	
	test_that("attrition has the variable classes", {
		expect_type(attrition$participant, "character")
		expect_equal(class(attrition$age_group), "factor")
		expect_equal(class(attrition$trial), "integer")
		expect_type(attrition$trial_type, "character")
		expect_type(attrition$valid_gaze_prime, "logical")
		expect_type(attrition$valid_gaze_target, "logical")
		expect_type(attrition$valid_gaze_distractor, "logical")
		expect_type(attrition$valid_trial, "logical")
		expect_type(attrition$cognate, "integer")
		expect_type(attrition$non_cognate, "integer")
		expect_type(attrition$unrelated, "integer")
		expect_type(attrition$valid_participant, "logical")
		
		
	})
	
	test_that("attrition missing data is dealt with", {
		expect_false(any(is.na(attrition$participant)))
		expect_false(any(is.na(attrition$age_group)))
		expect_false(any(is.na(attrition$trial)))
		expect_false(any(is.na(attrition$trial_type)))
		expect_false(any(is.na(attrition$valid_gaze_prime)))
		expect_false(any(is.na(attrition$valid_gaze_target)))
		expect_false(any(is.na(attrition$valid_gaze_distractor)))
		expect_false(any(is.na(attrition$cognate)))
		expect_false(any(is.na(attrition$non_cognate)))
		expect_false(any(is.na(attrition$valid_participant)))
		
	})
	
	test_that("attrition variables have the right values", {
		expect_equal(levels(attrition$age_group), paste0(c(21, 25, 30), " months"))
		expect_true(all(between(unique(attrition$trial), 0, 32)))
		expect_true(all(unique(attrition$trial_type) %in% c("Cognate", "Non-cognate", "Unrelated")))
		expect_true(all(unique(attrition$valid_gaze_prime) %in% c(TRUE, FALSE)))
		expect_true(all(unique(attrition$valid_gaze_target) %in% c(TRUE, FALSE)))
		expect_true(all(unique(attrition$valid_gaze_distractor) %in% c(TRUE, FALSE)))
		expect_true(all(unique(attrition$valid_trial) %in% c(TRUE, FALSE)))
		expect_true(all(between(unique(attrition$cognate), 0, 8)))
		expect_true(all(between(unique(attrition$non_cognate), 0, 8)))
		expect_true(all(between(unique(attrition$unrelated), 0, 16)))
		expect_true(all(unique(attrition$valid_participant %in% c(TRUE, FALSE))))
	})
	
	# test_that("attrition valid participants only when they should", {
	# 	expect_true(all(attrition$valid_gaze_prime[attrition$valid_trial]))
	# 	expect_true(all(attrition$valid_gaze_target[attrition$valid_trial]))
	# 	expect_true(all(attrition$valid_gaze_distractor[attrition$valid_trial]))
	# 	expect_true(all(between(attrition$cognate[attrition$valid_participant], missing_trials_threshold["cognate"], 8)))
	# 	expect_true(all(between(attrition$non_cognate[attrition$valid_participant], missing_trials_threshold["non_cognate"], 8)))
	# 	expect_true(all(between(attrition$unrelated[attrition$valid_participant], missing_trials_threshold["unrelated"], 16)))
	# })
	
}
