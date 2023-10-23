test_stimuli <- function(stimuli){
	
	test_that("stimuli has the right columns", {
		expect_setequal(
			colnames(stimuli),
			c("trial",
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
			  "distractor_cdi"))
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
	})
	
	test_that("stimuli variables have the right values", {
		expect_true(all(stimuli$trial %in% 1:32))
		expect_true(all(stimuli$test_language %in% c("Catalan", "Spanish", "English")))
		expect_true(all(stimuli$trial_type %in% c("Cognate", "Non-cognate", "Unrelated")))
		expect_true(all(
			case_when(
				stimuli$test_language=="Catalan" ~ stimuli$version %in% c("ll", "i"),
				stimuli$test_language=="Spanish" ~ stimuli$version %in% c("euro", "latin"),
				stimuli$test_language=="English" ~ stimuli$version %in% c("british"))
			
		))
		expect_true(all(stimuli$list %in% 1:4))
		expect_true(all(stimuli$target_location %in% c("r", "l")))
	})
	
}