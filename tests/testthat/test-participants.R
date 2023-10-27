test_participants <- function(x){
	
	test_that("x has the right columns", {
		expect_setequal(
			colnames(x),
			c("child_id",
			  "location",
			  "date_test",
			  "session_id",
			  "session_n",
			  "age_group",
			  "age",
			  "sex",
			  "lp",
			  "doe",
			  "test_language",
			  "list",
			  "version",
			  "vocab_id",
			  "list",
			  "vocab_id_response",
			  "filename"))
	})
	
	test_that("x variables are the right types", {
		expect_type(x$child_id, "character")
		expect_type(x$vocab_id, "character")
		expect_type(x$sex, "integer")
		expect_equal(class(x$date_test), "Date")
		expect_type(x$age_group, "character")
		expect_type(x$lp, "integer")
		expect_type(x$age, "double")
		expect_type(x$test_language, "integer")
		expect_type(x$list, "integer")
		expect_type(x$version, "character")
		expect_type(x$filename, "character")
		expect_type(x$doe, "list")
	})
	
	test_that("x has no missing data", {
		expect_false(any(is.na(x$child_id)))
		expect_false(any(is.na(x$date_test)))
		expect_false(any(is.na(x$lp)))
		expect_false(any(is.na(x$age_group)))
		expect_false(any(is.na(x$age)))
		expect_false(any(is.na(x$test_language)))
		expect_false(any(is.na(x$list)))
		expect_false(any(is.na(x$version)))
		expect_false(any(is.na(x$filename[
			x$location=="Barcelona"
		])))
	})
	
	
}

