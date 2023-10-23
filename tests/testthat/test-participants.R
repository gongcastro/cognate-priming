test_participants <- function(participants){
	
	test_that("participants has the right columns", {
		expect_setequal(
			colnames(participants),
			c("child_id",
			  "location",
			  "date_test",
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
	
	test_that("participants variables are the right types", {
		expect_type(participants$child_id, "character")
		expect_type(participants$vocab_id, "character")
		expect_type(participants$sex, "integer")
		expect_equal(class(participants$date_test), "Date")
		expect_type(participants$lp, "integer")
		expect_type(participants$age, "double")
		expect_type(participants$test_language, "integer")
		expect_type(participants$list, "integer")
		expect_type(participants$version, "character")
		expect_type(participants$filename, "character")
		expect_type(participants$doe, "list")
	})
	
	test_that("participants has no missing data", {
		expect_false(any(is.na(participants$child_id)))
		expect_false(any(is.na(participants$date_test)))
		expect_false(any(is.na(participants$lp)))
		expect_false(any(is.na(participants$age)))
		expect_false(any(is.na(participants$test_language)))
		expect_false(any(is.na(participants$list)))
		expect_false(any(is.na(participants$version)))
		expect_false(any(is.na(participants$filename[
			participants$location=="Barcelona"
		])))
	})
	

}

