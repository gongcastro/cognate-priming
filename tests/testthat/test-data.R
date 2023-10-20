test_data_time <- function(data_time){
	
	test_that("data_time has the right columns", {
		expect_setequal(
			colnames(data_time),
			c("id",
			  "age",
			  "lp",
			  "condition",
			  "timebin",
			  ".sum",
			  ".prop",
			  ".elog",
			  ".nsamples",
			  ".ntrials")
		)
	})
	
	test_that("data_time has the variable classes", {
		expect_type(data_time$id, "integer")
		expect_type(data_time$age, "double")
		expect_equal(class(data_time$condition), "factor")
		expect_type(data_time$timebin, "integer")
		expect_type(data_time$.ntrials, "integer")
		expect_type(data_time$.prop, "double")
		expect_type(data_time$.elog, "double")
		expect_type(data_time$.sum, "integer")
		expect_type(data_time$.nsamples, "integer")
	})
	
	test_that("data_time missing data is dealt with", {
		expect_false(any(is.na(data_time$id)))
		expect_false(any(is.na(data_time$age)))
		expect_false(any(is.na(data_time$lp)))
		expect_false(any(is.na(data_time$condition)))
		expect_false(any(is.na(data_time$timebin)))
		expect_false(any(is.na(data_time$.ntrials)))
		expect_false(any(is.na(data_time$.nsamples)))
		expect_false(any(is.na(data_time$.prop)))
		expect_false(any(is.na(data_time$.elog)))
		expect_false(any(is.na(data_time$.sum)))
		
	})
	
	test_that("data_time variables have the right values", {
		expect_equal(levels(data_time$lp), c("Monolingual", "Bilingual"))
		expect_in(levels(data_time$condition), c("Cognate", "Non-cognate",
												 "Related" ,"Unrelated"))
		expect_gte(min(data_time$.nsamples), 0)
		expect_gte(min(data_time$.ntrials), 0)
		expect_gte(min(data_time$age), 0)
		expect_gte(min(data_time$.prop), 0)
		expect_lte(max(data_time$.prop), 1)
		expect_gte(min(data_time$.sum), 0)
		expect_false(any(is.nan(unique(data_time$.elog))))
	})
	
}

