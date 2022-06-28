test_gaze <- function(gaze){
	
	test_that("gaze has the right columns", {
		expect_equal(
			colnames(gaze),
			c(
				"participant",
				"age_group",
				"trial_type",
				"time_bin",
				"time_bin_center",
				"sum_target",
				"sum_distractor",
				"n",
				"prop",
				"logit",
				"lp",
				"n_trials",
				"vocab_size_total",
				"vocab_size_l1",
				"vocab_size_conceptual",
				"vocab_size_total_center",
				"vocab_size_l1_center",
				"vocab_size_conceptual_center"
			)
		)
	})
	
	test_that("gaze has the variable classes", {
		expect_type(gaze$participant, "character")
		expect_equal(class(gaze$age_group), "factor")
		expect_equal(class(gaze$trial_type), "factor")
		expect_type(gaze$time_bin, "integer")
		expect_type(gaze$time_bin_center, "double")
		expect_type(gaze$sum_target, "integer")
		expect_type(gaze$sum_distractor, "integer")
		expect_type(gaze$n, "integer")
		expect_type(gaze$prop, "double")
		expect_type(gaze$logit, "double")
		expect_equal(class(gaze$lp), "factor")
		expect_type(gaze$n_trials, "integer")
		expect_type(gaze$vocab_size_total, "double")
		expect_type(gaze$vocab_size_l1, "double")
		expect_type(gaze$vocab_size_conceptual, "double")
		expect_type(gaze$vocab_size_total_center, "double")
		expect_type(gaze$vocab_size_l1_center, "double")
		expect_type(gaze$vocab_size_conceptual_center, "double")		
	})
	
	test_that("gaze missing data is dealt with", {
		expect_false(any(is.na(gaze$participant)))
		expect_false(any(is.na(gaze$age_group)))
		expect_false(any(is.na(gaze$trial_type)))
		expect_false(any(is.na(gaze$time_bin)))
		expect_false(any(is.na(gaze$time_bin_center)))
		expect_false(any(is.na(gaze$sum_target)))
		expect_false(any(is.na(gaze$sum_distractor)))
		expect_false(any(is.na(gaze$n)))
		expect_false(any(is.na(gaze$prop)))
		expect_false(any(is.na(gaze$logit)))
		expect_false(any(is.na(gaze$n_trials)))
		expect_false(any(is.na(gaze$lp)))
		expect_false(any(is.na(gaze$vocab_size_total)))
		expect_false(any(is.na(gaze$vocab_size_l1)))
		expect_false(any(is.na(gaze$vocab_size_conceptual)))
		expect_false(any(is.na(gaze$vocab_size_total_center)))
		expect_false(any(is.na(gaze$vocab_size_l1_center)))
		expect_false(any(is.na(gaze$vocab_size_conceptual_center)))
	})
	
	test_that("gaze variables have the right values", {
		expect_equal(levels(gaze$age_group), paste0(c(21, 25, 30), " months"))
		expect_equal(levels(gaze$trial_type), c("Cognate", "Non-cognate", "Unrelated"))
		expect_true(all(unique(gaze$time_bin) %in% 1:20))
		expect_equal(length(unique(gaze$time_bin_center)), 20)
		expect_true(all(unique(gaze$sum_target) >= 0))
		expect_true(all(unique(gaze$sum_distractor) >= 0))
		expect_true(all(unique(gaze$n) > 0))
		expect_true(all(gaze$sum_target <= gaze$n))
		expect_true(all(gaze$sum_distractor <= gaze$n))
		expect_true(all(between(unique(gaze$prop), 0, 1)))
		expect_false(any(is.nan(unique(gaze$logit))))
		expect_equal(levels(gaze$lp), c("Monolingual", "Bilingual"))
		expect_true(all(unique(gaze$n_trials) > 0))
		expect_true(all(between(unique(gaze$vocab_size_total), 0, 1)))
		expect_true(all(between(unique(gaze$vocab_size_l1), 0, 1)))
		expect_true(all(between(unique(gaze$vocab_size_conceptual), 0, 1)))
	})
	
}
