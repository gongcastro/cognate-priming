test_participants <- function(participants){
	
	test_that("participants has the right columns", {
		expect_equal(
			colnames(participants),
			c("id",
			  "id_db",
			  "date_test",
			  "lp",
			  "doe_catalan",
			  "doe_spanish",
			  "doe_others",
			  "age_group",
			  "age",
			  "test_language",
			  "list",
			  "version",
			  "filename"))
	})
	
	test_that("participants variables are the right types", {
		expect_type(participants$id, "integer")
		expect_type(participants$id_db, "character")
		expect_equal(class(participants$date_test), "Date")
		expect_type(participants$lp, "integer")
		expect_type(participants$age_group, "integer")
		expect_type(participants$age, "double")
		expect_type(participants$test_language, "integer")
		expect_type(participants$list, "integer")
		expect_type(participants$version, "character")
		expect_type(participants$filename, "character")
	})
	
	test_that("participants has no missing data", {
		expect_false(any(is.na(participants$id)))
		expect_false(any(is.na(participants$id_db)))
		expect_false(any(is.na(participants$date_test)))
		expect_false(any(is.na(participants$lp)))
		expect_false(any(is.na(participants$age_group)))
		expect_false(any(is.na(participants$age)))
		expect_false(any(is.na(participants$test_language)))
		expect_false(any(is.na(participants$list)))
		expect_false(any(is.na(participants$version)))
		expect_false(any(is.na(participants$filename)))
	})
	
	test_that("id and age_group combinations are not duplicated", {
		expect_true(
			participants |>
				janitor::get_dupes(id, age_group) |> 
				nrow() < 1
		)
	})
	
	test_that("id_db and age_group combinations are not duplicated", {
		expect_true(
			participants |>
				janitor::get_dupes(id_db, age_group) |> 
				nrow() < 1
		)
	})
	
	test_that("filenames are not duplicated", {
		expect_true(!any(duplicated(participants$filename)))
	})
	
	test_that("all filenames correspond to a file in data/gaze/00_raw", {
		gaze_files <- list.files("data-raw/barcelona/eyetracking", 
								 pattern = ".csv$")
		expect_true(all(participants$filename %in% gaze_files))
	})
}

