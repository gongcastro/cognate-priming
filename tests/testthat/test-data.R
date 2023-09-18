test_data_time <- function(data_time){
	
	test_that("data_time has the right columns", {
		expect_equal(
			colnames(data_time),
			c("id",
			  "age_group",
			  "age",
			  "lp",
			  "trial_type",
			  "timebin",
			  ".prop_target",
			  ".prop_distractor",
			  ".logit_target",
			  ".logit_distractor",
			  ".n",
			  "voc_total",
			  "voc_l1",
			  "voc_l2",
			  "voc_concept",
			  "voc_te")
		)
	})
	
	test_that("data_time has the variable classes", {
		expect_type(data_time$id, "integer")
		expect_equal(class(data_time$age_group), "factor")
		expect_type(data_time$age, "double")
		expect_equal(class(data_time$lp), "factor")
		expect_equal(class(data_time$trial_type), "factor")
		expect_type(data_time$timebin, "integer")
		expect_type(data_time$.n, "integer")
		expect_type(data_time$.prop_target, "double")
		expect_type(data_time$.logit_target, "double")
		expect_type(data_time$.prop_distractor, "double")
		expect_type(data_time$.logit_distractor, "double")
		expect_type(data_time$voc_total, "double")
		expect_type(data_time$voc_l1, "double")
		expect_type(data_time$voc_l2, "double")
		expect_type(data_time$voc_concept, "double")
		expect_type(data_time$voc_te, "double")
	})
	
	test_that("data_time missing data is dealt with", {
		expect_false(any(is.na(data_time$id)))
		expect_false(any(is.na(data_time$age_group)))
		expect_false(any(is.na(data_time$age)))
		expect_false(any(is.na(data_time$lp)))
		expect_false(any(is.na(data_time$trial_type)))
		expect_false(any(is.na(data_time$timebin)))
		expect_false(any(is.na(data_time$.n)))
		expect_false(any(is.na(data_time$.prop_target)))
		expect_false(any(is.na(data_time$.prop_distractor)))
		expect_false(any(is.na(data_time$.logit_target)))
		expect_false(any(is.na(data_time$.logit_distractor)))
		expect_false(any(is.na(data_time$voc_total)))
		expect_false(any(is.na(data_time$voc_l1)))
		expect_false(any(is.na(data_time$voc_l2)))
		expect_false(any(is.na(data_time$voc_concept)))
		expect_false(any(is.na(data_time$voc_te)))
		
	})
	
	test_that("data_time variables have the right values", {
		expect_equal(levels(data_time$age_group), paste0(c(21, 25, 30), " months"))
		expect_equal(levels(data_time$lp), c("Monolingual", "Bilingual"))
		expect_equal(levels(data_time$trial_type), c("Cognate", "Non-cognate", "Unrelated"))
		expect_true(all(unique(data_time$.n) > 0))
		expect_true(all(unique(data_time$age) > 0))
		expect_true(all(between(unique(data_time$.prop_target), 0, 1)))
		expect_false(any(is.nan(unique(data_time$.logit_target))))
		expect_true(all(between(unique(data_time$.prop_distractor), 0, 1)))
		expect_false(any(is.nan(unique(data_time$.logit_distractor))))
		expect_true(all(between(unique(data_time$voc_total), 0, 1)))
		expect_true(all(between(unique(data_time$voc_l1), 0, 1)))
		expect_true(all(between(unique(data_time$voc_l2), 0, 1)))
		expect_true(all(between(unique(data_time$voc_concept), 0, 1)))
		expect_true(all(between(unique(data_time$voc_te), 0, 1)))
		
	})
	
}

test_data_summary <- function(data_summary){
	
	test_that("data_summary has the right columns", {
		expect_equal(
			colnames(data_summary),
			c("id",
			  "age_group",
			  "age",
			  "lp",
			  "trial_type",
			  ".prop_target",
			  ".prop_distractor",
			  ".logit_target",
			  ".logit_distractor",
			  "voc_total",
			  "voc_l1",
			  "voc_l2",
			  "voc_concept",
			  "voc_te"))
	})
	
	test_that("data_summary has the variable classes", {
		expect_type(data_summary$id, "integer")
		expect_equal(class(data_summary$age_group), "factor")
		expect_type(data_summary$age, "double")
		expect_equal(class(data_summary$lp), "factor")
		expect_equal(class(data_summary$trial_type), "factor")
		expect_type(data_summary$.prop_target, "double")
		expect_type(data_summary$.logit_target, "double")
		expect_type(data_summary$.prop_distractor, "double")
		expect_type(data_summary$.logit_distractor, "double")
		expect_type(data_summary$voc_total, "double")
		expect_type(data_summary$voc_l1, "double")
		expect_type(data_summary$voc_l2, "double")
		expect_type(data_summary$voc_concept, "double")
		expect_type(data_summary$voc_te, "double")
	})
	
	test_that("data_summary missing data is dealt with", {
		expect_false(any(is.na(data_summary$id)))
		expect_false(any(is.na(data_summary$age_group)))
		expect_false(any(is.na(data_summary$age)))
		expect_false(any(is.na(data_summary$lp)))
		expect_false(any(is.na(data_summary$trial_type)))
		expect_false(any(is.na(data_summary$.prop_target)))
		expect_false(any(is.na(data_summary$.logit_target)))
		expect_false(any(is.na(data_summary$.prop_distractor)))
		expect_false(any(is.na(data_summary$.logit_distractor)))
		expect_false(any(is.na(data_summary$voc_total)))
		expect_false(any(is.na(data_summary$voc_l1)))
		expect_false(any(is.na(data_summary$voc_l2)))
		expect_false(any(is.na(data_summary$voc_concept)))
		expect_false(any(is.na(data_summary$voc_te)))
	})
	
	test_that("data_summary variables have the right values", {
		expect_equal(levels(data_summary$age_group), paste0(c(21, 25, 30), " months"))
		expect_equal(levels(data_summary$lp), c("Monolingual", "Bilingual"))
		expect_equal(levels(data_summary$trial_type), c("Cognate", "Non-cognate", "Unrelated"))
		expect_true(all(unique(data_summary$age) > 0))
		expect_true(all(between(unique(data_summary$.prop_target), 0, 1)))
		expect_false(any(is.nan(unique(data_summary$.logit_target))))
		expect_true(all(between(unique(data_summary$.prop_distractor), 0, 1)))
		expect_false(any(is.nan(unique(data_summary$.logit_distractor))))
		expect_true(all(between(unique(data_summary$voc_total), 0, 1)))
		expect_true(all(between(unique(data_summary$voc_l1), 0, 1)))
		expect_true(all(between(unique(data_summary$voc_l2), 0, 1)))
		expect_true(all(between(unique(data_summary$voc_concept), 0, 1)))
		expect_true(all(between(unique(data_summary$voc_te), 0, 1)))
	})
	
}
