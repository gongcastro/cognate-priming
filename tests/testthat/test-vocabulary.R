test_vocabulary <- function(x){
	
	test_that("x has the right columns", {
		expect_setequal(
			colnames(x),
			c("filename",
			  "total_prop",
			  "l1_prop",
			  "l2_prop",
			  "concept_prop",
			  "te_prop",
			  "total_contents",
			  "l1_contents",
			  "l2_contents",
			  "concept_contents",
			  "te_contents",
			  "is_imputed"
			))
	})
	
	test_that("x has the variable classes", {
		expect_type(x$filename, "character")
		expect_type(x$total_prop, "double")
		expect_type(x$l1_prop, "double")
		expect_type(x$l2_prop, "double")
		expect_type(x$concept_prop, "double")
		expect_type(x$te_prop, "double")
		expect_type(x$total_contents, "list")
		expect_type(x$l1_contents, "list")
		expect_type(x$l2_contents, "list")
		expect_type(x$concept_contents, "list")
		expect_type(x$te_contents, "list")
		expect_type(x$is_imputed, "logical")
		
		
	})
	
	test_that("x missing data is dealt with", {
		expect_false(any(is.na(x$filename)))
		expect_false(any(is.na(x$total_prop)))
		expect_false(any(is.na(x$l1_prop)))
		expect_false(any(is.na(x$l2_prop)))
		expect_false(any(is.na(x$concept_prop)))
		expect_false(any(is.na(x$te_prop)))
		expect_false(any(is.na(x$is_imputed)))
		
	})
	
	test_that("stimuli variables have the right values", {
		expect_true(all(between(unique(x$total_prop), 0, 1)))
		expect_true(all(between(unique(x$l1_prop), 0, 1)))
		expect_true(all(between(unique(x$l2_prop), 0, 1)))
		expect_true(all(between(unique(x$concept_prop), 0, 1)))
		expect_true(all(between(unique(x$te_prop), 0, 1)))
		expect_true(all(unique(x$is_imputed) %in% c(TRUE, FALSE)))
	})
	
}

