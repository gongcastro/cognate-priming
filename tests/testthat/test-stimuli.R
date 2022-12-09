test_stimuli <- function(stimuli){
	
	test_that("stimuli has the right columns", {
		expect_equal(
			colnames(stimuli),
			c(
				"trial",
				"test_language",
				"version",
				"list",
				"trial_type",
				"prime",
				"target",
				"distractor",
				"audio",
				"target_location",
				"prime_cdi",
				"target_cdi",
				"distractor_cdi",
				"valid_trial",
				"familiarity_prime",
				"familiarity_target",
				"familiarity_se_prime",
				"familiarity_se_target",
				"frequency_prime_childes",
				"frequency_target_childes",
				"category_prime",
				"category_target",
				"is_animate_prime",
				"is_animate_target"
			)
		)
	})
	
	test_that("stimuli has the variable classes", {
		expect_type(stimuli$trial, "integer")
		expect_type(stimuli$test_language, "character")
		expect_type(stimuli$version, "character")
		expect_type(stimuli$list, "integer")
		expect_type(stimuli$trial_type, "character")
		expect_type(stimuli$prime, "character")
		expect_type(stimuli$target, "character")
		expect_type(stimuli$distractor, "character")
		expect_type(stimuli$prime_cdi, "character")
		expect_type(stimuli$target_cdi, "character")
		expect_type(stimuli$distractor_cdi, "character")
		expect_type(stimuli$audio, "character")
		expect_type(stimuli$target_location, "character")
		expect_type(stimuli$valid_trial, "logical")
		expect_type(stimuli$familiarity_prime, "double")
		expect_type(stimuli$familiarity_target, "double")
		expect_type(stimuli$familiarity_se_prime, "double")
		expect_type(stimuli$familiarity_se_target, "double")
		expect_type(stimuli$frequency_prime_childes, "double")
		expect_type(stimuli$frequency_target_childes, "double")
		expect_type(stimuli$category_prime, "character")
		expect_type(stimuli$category_target, "character")
		expect_type(stimuli$is_animate_prime, "logical")
		expect_type(stimuli$is_animate_target, "logical")
	})
	
	
	test_that("stimuli missing data is dealt with", {
		expect_false(any(is.na(stimuli$trial)))
		expect_false(any(is.na(stimuli$test_language)))
		expect_false(any(is.na(stimuli$version)))
		expect_false(any(is.na(stimuli$list)))
		expect_false(any(is.na(stimuli$trial_type)))
		expect_false(any(is.na(stimuli$prime)))
		expect_false(any(is.na(stimuli$target)))
		expect_false(any(is.na(stimuli$distractor)))
		expect_false(any(is.na(stimuli$prime_cdi)))
		expect_false(any(is.na(stimuli$target_cdi)))
		expect_false(any(is.na(stimuli$distractor_cdi)))
		expect_false(any(is.na(stimuli$audio)))
		expect_false(any(is.na(stimuli$target_location)))
		expect_false(any(is.na(stimuli$valid_trial)))
		expect_false(any(is.na(stimuli$familiarity_prime)))
		expect_lt(length(stimuli$familiarity_prime[is.na(stimuli$familiarity_prime)])/nrow(stimuli), 0.10) 
		expect_lt(length(stimuli$familiarity_target[is.na(stimuli$familiarity_target)])/nrow(stimuli), 0.10) 
		expect_lt(length(stimuli$familiarity_se_prime[is.na(stimuli$familiarity_se_prime)])/nrow(stimuli), 0.10) 
		expect_lt(length(stimuli$familiarity_se_target[is.na(stimuli$familiarity_se_target)])/nrow(stimuli), 0.10) 
		expect_lt(length(stimuli$frequency_prime_childes[is.na(stimuli$frequency_prime_childes)])/nrow(stimuli), 0.10) 
		expect_lt(length(stimuli$frequency_target_childes[is.na(stimuli$frequency_target_childes)])/nrow(stimuli), 0.10) 
		expect_false(any(is.na(stimuli$category_prime)))
		expect_false(any(is.na(stimuli$category_target)))
		expect_false(any(is.na(stimuli$is_animate_prime)))
		expect_false(any(is.na(stimuli$is_animate_target)))
	})
	
	test_that("stimuli variables have the right values", {
		expect_true(all(stimuli$trial %in% 1:32))
		expect_true(all(stimuli$test_language %in% c("Catalan", "Spanish")))
		expect_true(all(stimuli$trial_type %in% c("Cognate", "Non-cognate", "Unrelated")))
		expect_true(all(
			case_when(
				stimuli$test_language=="Catalan" ~ stimuli$version %in% c("ll", "i"),
				stimuli$test_language=="Spanish" ~ stimuli$version %in% c("euro", "latin"))
		))
		expect_true(all(stimuli$list %in% 1:3))
		expect_true(all(stimuli$target_location %in% c("r", "l")))
		expect_true(all(stimuli$valid_trial %in% c(TRUE, FALSE)))
		expect_true(all(between(stimuli$familiarity_prime[!is.na(stimuli$familiarity_prime)], 0, 1)))
		expect_true(all(between(stimuli$familiarity_target[!is.na(stimuli$familiarity_prime)], 0, 1)))
		expect_true(all(between(stimuli$familiarity_se_prime[!is.na(stimuli$familiarity_se_prime)], 0, 1)))
		expect_true(all(between(stimuli$familiarity_se_target[!is.na(stimuli$familiarity_se_prime)], 0, 1)))
		expect_true(all(unique(stimuli$frequency_prime_childes) >= 0))
		expect_true(all(unique(stimuli$frequency_target_childes) >= 0))
		expect_true(all(stimuli$is_animate_prime %in% c(TRUE, FALSE)))
		expect_true(all(stimuli$is_animate_target %in% c(TRUE, FALSE)))
		expect_true(all(stimuli$category_prime %in% c("Animals", "Body parts", "Clothes", "Food and drink", "Games and rutines", "Household Items", "Outside", "Toys", "Vehicles")))
		expect_true(all(stimuli$category_target %in% c("Animals", "Body parts", "Clothes", "Food and drink", "Games and rutines", "Household Items", "Outside", "Toys", "Vehicles")))
	})
}

