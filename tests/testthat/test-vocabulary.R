test_vocabulary <- function(vocabulary){
	
	test_that("vocabulary has the right columns", {
		expect_equal(
			colnames(vocabulary),
			c("id_db",
			  "age_group",
			  "lp",
			  "total_prop",
			  "l1_prop",
			  "l2_prop",
			  "concept_prop",
			  "te_prop",
			  "is_imputed"
			))
	})
	
	test_that("vocabulary has the variable classes", {
		expect_type(vocabulary$id_db, "character")
		expect_equal(class(vocabulary$age_group), "factor")
		expect_equal(class(vocabulary$lp), "factor")
		expect_type(vocabulary$total_prop, "double")
		expect_type(vocabulary$l1_prop, "double")
		expect_type(vocabulary$l2_prop, "double")
		expect_type(vocabulary$concept_prop, "double")
		expect_type(vocabulary$te_prop, "double")
		expect_type(vocabulary$is_imputed, "logical")
		
		
	})
	
	test_that("vocabulary missing data is dealt with", {
		expect_false(any(is.na(vocabulary$id_db)))
		expect_false(any(is.na(vocabulary$age_group)))
		expect_false(any(is.na(vocabulary$lp)))
		expect_false(any(is.na(vocabulary$total_prop)))
		expect_false(any(is.na(vocabulary$l1_prop)))
		expect_false(any(is.na(vocabulary$l2_prop)))
		expect_false(any(is.na(vocabulary$concept_prop)))
		expect_false(any(is.na(vocabulary$te_prop)))
		expect_false(any(is.na(vocabulary$is_imputed)))
		
	})
	
	test_that("stimuli variables have the right values", {
		expect_equal(levels(vocabulary$age_group), paste0(c(21, 25, 30), " months"))
		expect_equal(levels(vocabulary$lp), c("Monolingual", "Bilingual"))
		expect_true(all(between(unique(vocabulary$total_prop), 0, 1)))
		expect_true(all(between(unique(vocabulary$l1_prop), 0, 1)))
		expect_true(all(between(unique(vocabulary$l2_prop), 0, 1)))
		expect_true(all(between(unique(vocabulary$concept_prop), 0, 1)))
		expect_true(all(between(unique(vocabulary$te_prop), 0, 1)))
		expect_true(all(unique(vocabulary$is_imputed) %in% c(TRUE, FALSE)))
	})
	
}

