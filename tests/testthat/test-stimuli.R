test_stimuli <- function(x){
	
	test_that("x has the right columns", {
		expect_setequal(
			colnames(x),
			c("trial",
			  "test_language",
			  "version",
			  "list",
			  "trial_type",
			  "prime",
			  "target",
			  "distractor",
			  "audio",
			  "duration",
			  "target_location",
			  "prime_cdi",
			  "target_cdi",
			  "distractor_cdi",
			  "valid_trial",
			  "familiarity_prime",
			  "familiarity_target",
			  "familiarity_se_prime",
			  "familiarity_se_target",
			  "freq_prime",
			  "freq_target",
			  "semantic_category_prime",
			  "semantic_category_target",
			  "is_animate_prime",
			  "is_animate_target"))
	})
	
	test_that("x has the variable classes", {
		expect_type(x$trial, "integer")
		expect_type(x$test_language, "character")
		expect_type(x$version, "character")
		expect_type(x$list, "integer")
		expect_type(x$trial_type, "character")
		expect_type(x$prime, "character")
		expect_type(x$target, "character")
		expect_type(x$distractor, "character")
		expect_type(x$prime_cdi, "character")
		expect_type(x$target_cdi, "character")
		expect_type(x$distractor_cdi, "character")
		expect_type(x$audio, "character")
		expect_type(x$target_location, "character")
		expect_type(x$valid_trial, "logical")
		expect_type(x$familiarity_prime, "double")
		expect_type(x$familiarity_target, "double")
		expect_type(x$familiarity_se_prime, "double")
		expect_type(x$familiarity_se_target, "double")
		expect_type(x$freq_prime, "double")
		expect_type(x$freq_target, "double")
		expect_type(x$semantic_category_prime, "character")
		expect_type(x$semantic_category_target, "character")
		expect_type(x$is_animate_prime, "logical")
		expect_type(x$is_animate_target, "logical")
	})
	
	
	test_that("x missing data is dealt with", {
		expect_false(any(is.na(x$trial)))
		expect_false(any(is.na(x$test_language)))
		expect_false(any(is.na(x$version)))
		expect_false(any(is.na(x$list)))
		expect_false(any(is.na(x$trial_type)))
		expect_false(any(is.na(x$prime)))
		expect_false(any(is.na(x$target)))
		expect_false(any(is.na(x$distractor)))
		expect_false(any(is.na(x$prime_cdi)))
		expect_false(any(is.na(x$target_cdi)))
		expect_false(any(is.na(x$distractor_cdi)))
		expect_false(any(is.na(x$audio)))
		expect_false(any(is.na(x$target_location)))
		expect_false(any(is.na(x$valid_trial)))
		expect_false(any(is.na(x$familiarity_prime)))
		expect_lt(length(x$familiarity_prime[is.na(x$familiarity_prime)])/nrow(x), 0.10) 
		expect_lt(length(x$familiarity_target[is.na(x$familiarity_target)])/nrow(x), 0.10) 
		expect_lt(length(x$familiarity_se_prime[is.na(x$familiarity_se_prime)])/nrow(x), 0.10) 
		expect_lt(length(x$familiarity_se_target[is.na(x$familiarity_se_target)])/nrow(x), 0.10) 
		expect_lt(length(x$freq_prime[is.na(x$freq_prime)])/nrow(x), 0.10) 
		expect_lt(length(x$freq_target[is.na(x$freq_target)])/nrow(x), 0.10) 
		expect_false(any(is.na(x$semantic_category_prime)))
		expect_false(any(is.na(x$semantic_category_target)))
		expect_false(any(is.na(x$is_animate_prime)))
		expect_false(any(is.na(x$is_animate_target)))
	})
	
	test_that("x variables have the right values", {
		expect_true(all(x$trial %in% 1:32))
		expect_true(all(x$test_language %in% c("Catalan", "Spanish")))
		expect_true(all(x$trial_type %in% c("Cognate", "Non-cognate", "Unrelated")))
		expect_true(all(
			case_when(
				x$test_language=="Catalan" ~ x$version %in% c("ll", "i"),
				x$test_language=="Spanish" ~ x$version %in% c("euro", "latin"))
		))
		expect_true(all(x$list %in% 1:3))
		expect_true(all(x$target_location %in% c("r", "l")))
		expect_true(all(x$valid_trial %in% c(TRUE, FALSE)))
		expect_true(all(between(x$familiarity_prime[!is.na(x$familiarity_prime)], 0, 1)))
		expect_true(all(between(x$familiarity_target[!is.na(x$familiarity_prime)], 0, 1)))
		expect_true(all(between(x$familiarity_se_prime[!is.na(x$familiarity_se_prime)], 0, 1)))
		expect_true(all(between(x$familiarity_se_target[!is.na(x$familiarity_se_prime)], 0, 1)))
		expect_true(all(unique(x$freq_prime) >= 0))
		expect_true(all(unique(x$freq_target) >= 0))
		expect_true(all(x$is_animate_prime %in% c(TRUE, FALSE)))
		expect_true(all(x$is_animate_target %in% c(TRUE, FALSE)))
		expect_true(all(x$semantic_category_prime %in% 
							c("Animals",
							  "Body parts",
							  "Clothes",
							  "Food and drink", 
							  "Games and rutines", 
							  "Household items", 
							  "Outside", 
							  "Toys", 
							  "Vehicles")
		))
		expect_true(all(x$semantic_category_target %in% 
							c("Animals", 
							  "Body parts", 
							  "Clothes", 
							  "Food and drink",
							  "Games and rutines",
							  "Household items",
							  "Outside", 
							  "Toys", 
							  "Vehicles")))
	})
}

