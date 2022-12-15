test_df <- function(df){
	
	test_that("df has the right columns", {
		expect_equal(
			colnames(df),
			c("id",
			  "age_group",
			  "time_bin",
			  "ot1",
			  "ot2",
			  "ot3",
			  "sum_target",
			  "n",
			  "prop",
			  "logit",
			  "trial_type",
			  "lp",
			  "n_trials",
			  "vocab",
			  "vocab_std"))
	})
	
	test_that("df has the variable classes", {
		expect_type(df$id, "character")
		expect_equal(class(df$age_group), "factor")
		expect_type(df$ot1, "double")
		expect_type(df$ot2, "double")
		expect_type(df$ot3, "double")
		expect_type(df$sum_target, "integer")
		expect_type(df$n, "integer")
		expect_type(df$prop, "double")
		expect_type(df$logit, "double")
		expect_equal(class(df$trial_type), "factor")
		expect_equal(class(df$lp), "factor")
		expect_type(df$n_trials, "integer")
		expect_type(df$vocab, "double")
		expect_type(df$vocab_std, "double")		
	})
	
	test_that("df missing data is dealt with", {
		expect_false(any(is.na(df$id)))
		expect_false(any(is.na(df$age_group)))
		expect_false(any(is.na(df$time_bin)))
		expect_false(any(is.na(df$ot1)))
		expect_false(any(is.na(df$ot2)))
		expect_false(any(is.na(df$ot3)))
		expect_false(any(is.na(df$sum_target)))
		expect_false(any(is.na(df$n)))
		expect_false(any(is.na(df$prop)))
		expect_false(any(is.na(df$logit)))
		expect_false(any(is.na(df$n_trials)))
		expect_false(any(is.na(df$trial_type)))
		expect_false(any(is.na(df$lp)))
		expect_false(any(is.na(df$vocab)))
		expect_false(any(is.na(df$vocab_std)))
	})
	
	test_that("gaze variables have the right values", {
		expect_equal(levels(df$age_group), paste0(c(21, 25, 30), " months"))
		expect_equal(levels(df$trial_type), c("Cognate", "Non-cognate", "Unrelated"))
		expect_true(all(unique(df$sum_target) >= 0))
		expect_true(all(unique(df$n) > 0))
		expect_true(all(df$sum_target <= df$n))
		expect_true(all(between(unique(df$prop), 0, 1)))
		expect_false(any(is.nan(unique(df$logit))))
		expect_equal(levels(df$lp), c("Monolingual", "Bilingual"))
		expect_true(all(unique(df$n_trials) > 0))
		expect_true(all(between(unique(df$vocab), 0, 1)))
	})
	
}
