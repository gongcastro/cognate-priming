test_data_time <- function(x) {
	
	test_that("x has the right columns", {
		expect_contains(
			colnames(x),
			c("child_id",
			  "session_id",
			  "age",
			  "lp",
			  "condition",
			  "timebin",
			  ".sum",
			  ".prop",
			  ".elog",
			  ".nsamples")
		)
	})
	
	test_that("x has the variable classes", {
		expect_type(x$child_id, "integer")
		expect_type(x$session_id, "integer")
		expect_type(x$age, "double")
		expect_equal(class(x$condition), "factor")
		expect_type(x$timebin, "integer")
		expect_type(x$.prop, "double")
		expect_type(x$.elog, "double")
		expect_type(x$.sum, "integer")
		expect_type(x$.nsamples, "integer")
	})
	
	test_that("x missing data is dealt with", {
		expect_false(any(is.na(x$child_id)))
		expect_false(any(is.na(x$session_id)))
		expect_false(any(is.na(x$age)))
		expect_false(any(is.na(x$lp)))
		expect_false(any(is.na(x$condition)))
		expect_false(any(is.na(x$timebin)))
		expect_false(any(is.na(x$.nsamples)))
		expect_false(any(is.na(x$.prop)))
		expect_false(any(is.na(x$.elog)))
		expect_false(any(is.na(x$.sum)))
	})
	
	test_that("x variables have the right values", {
		expect_equal(levels(x$lp),
					 c("Monolingual (English)", "Monolingual", "Bilingual"))
		expect_in(levels(x$condition), c("Cognate", "Non-cognate",
										 "Related" ,"Unrelated"))
		expect_gte(min(x$.nsamples), 0)
		expect_gte(min(x$age), 0)
		expect_gte(min(x$.prop), 0)
		expect_lte(max(x$.prop), 1)
		expect_gte(min(x$.sum), 0)
		expect_false(any(is.nan(unique(x$.elog))))
	})
	
}

