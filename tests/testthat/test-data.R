test_data_time <- function(data_time){
	
	test_that("data_time has the right columns", {
		expect_equal(
			colnames(data_time),
			c("id",
			  "age_group",
			  "lp",
			  "trial_type",
			  "timebin",
			  ".prop",
			  ".logit",
			  ".ntrials",
			  ".n",
			  "vocab",
			  "ot1",
			  "ot2",
			  "ot3"))
	})
	
	test_that("data_time has the variable classes", {
		expect_type(data_time$id, "integer")
		expect_equal(class(data_time$age_group), "factor")
		expect_equal(class(data_time$lp), "factor")
		expect_equal(class(data_time$trial_type), "factor")
		expect_type(data_time$timebin, "integer")
		expect_type(data_time$.ntrials, "integer")
		expect_type(data_time$.n, "integer")
		expect_type(data_time$.prop, "double")
		expect_type(data_time$.logit, "double")
		expect_type(data_time$vocab, "double")
		expect_type(data_time$ot1, "double")
		expect_type(data_time$ot2, "double")
		expect_type(data_time$ot3, "double")
	})
	
	test_that("data_time missing data is dealt with", {
		expect_false(any(is.na(data_time$id)))
		expect_false(any(is.na(data_time$age_group)))
		expect_false(any(is.na(data_time$lp)))
		expect_false(any(is.na(data_time$trial_type)))
		expect_false(any(is.na(data_time$timebin)))
		expect_false(any(is.na(data_time$.ntrials)))
		expect_false(any(is.na(data_time$.n)))
		expect_false(any(is.na(data_time$.prop)))
		expect_false(any(is.na(data_time$.logit)))
		expect_false(any(is.na(data_time$vocab)))
		expect_false(any(is.na(data_time$ot1)))
		expect_false(any(is.na(data_time$ot2)))
		expect_false(any(is.na(data_time$ot3)))
	})
	
	test_that("data_time variables have the right values", {
		expect_equal(levels(data_time$age_group), paste0(c(21, 25, 30), " months"))
		expect_equal(levels(data_time$lp), c("Monolingual", "Bilingual"))
		expect_equal(levels(data_time$trial_type), c("Cognate", "Non-cognate", "Unrelated"))
		expect_true(all(unique(data_time$.ntrials) > 0))
		expect_true(all(unique(data_time$.n) > 0))
		expect_true(all(between(unique(data_time$.prop), 0, 1)))
		expect_false(any(is.nan(unique(data_time$.logit))))
		expect_true(all(between(unique(data_time$vocab), 0, 1)))
	})
	
}

test_data_summary <- function(data_summary){
	
	test_that("data_summary has the right columns", {
		expect_equal(
			colnames(data_summary),
			c("id",
			  "age_group",
			  "lp",
			  "trial_type",
			  "vocab",
			  ".prop",
			  ".logit",
			  ".ntrials"))
	})
	
	test_that("data_summary has the variable classes", {
		expect_type(data_summary$id, "integer")
		expect_equal(class(data_summary$age_group), "factor")
		expect_equal(class(data_summary$lp), "factor")
		expect_equal(class(data_summary$trial_type), "factor")
		expect_type(data_summary$.ntrials, "integer")
		expect_type(data_summary$.prop, "double")
		expect_type(data_summary$.logit, "double")
		expect_type(data_summary$vocab, "double")
	})
	
	test_that("data_summary missing data is dealt with", {
		expect_false(any(is.na(data_summary$id)))
		expect_false(any(is.na(data_summary$age_group)))
		expect_false(any(is.na(data_summary$lp)))
		expect_false(any(is.na(data_summary$trial_type)))
		expect_false(any(is.na(data_summary$.ntrials)))
		expect_false(any(is.na(data_summary$.prop)))
		expect_false(any(is.na(data_summary$.logit)))
		expect_false(any(is.na(data_summary$vocab)))
		
	})
	
	test_that("data_summary variables have the right values", {
		expect_equal(levels(data_summary$age_group), paste0(c(21, 25, 30), " months"))
		expect_equal(levels(data_summary$lp), c("Monolingual", "Bilingual"))
		expect_equal(levels(data_summary$trial_type), c("Cognate", "Non-cognate", "Unrelated"))
		expect_true(all(unique(data_summary$.ntrials) > 0))
		expect_true(all(between(unique(data_summary$.prop), 0, 1)))
		expect_false(any(is.nan(unique(data_summary$.logit))))
		expect_true(all(between(unique(data_summary$vocab), 0, 1)))
	})
	
}
