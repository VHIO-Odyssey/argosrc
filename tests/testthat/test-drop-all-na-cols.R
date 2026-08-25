# These tests document that issue tables ending up in the Excel report
# produced by argos_write_verification_report() drop columns that are
# entirely NA, regardless of which path generated them: automatic
# verifications (filter_issues(), used by the verif_* functions), ad-hoc
# verifications (argos_add_to_verifications()), or completeness checks
# (argos_add_completeness_results()). All three rely on the shared internal
# helper drop_all_na_cols().

test_that("drop_all_na_cols() drops only columns that are entirely NA", {
  df <- tibble::tibble(
    a = c(1, 2, NA),
    all_na = NA,
    b = c("x", NA, "z")
  )

  result <- drop_all_na_cols(df)

  expect_equal(names(result), c("a", "b"))
  expect_equal(nrow(result), 3)
})

test_that("filter_issues() drops all-NA context columns (auto verifications)", {
  verified_data <- tibble::tibble(
    id = c("1", "2", "3"),
    # Not present for this project/form -> entirely NA
    redcap_event_name = NA_character_,
    redcap_form_name = c("form_a", "form_a", "form_a"),
    redcap_instance_number = NA_character_,
    value = c(10, 20, 5),
    .ok = c(TRUE, TRUE, FALSE)
  )

  issues <- argosrc:::filter_issues(verified_data, "value is <<value>>")

  # Only the failing row is kept
  expect_equal(nrow(issues), 1)
  expect_equal(as.character(issues$id), "3")

  # All-NA columns (redcap_event_name, redcap_instance_number) are dropped,
  # while the id, the non-NA context column, and the issue text remain.
  expect_equal(names(issues), c("id", "redcap_form_name", "issue"))
})

test_that("argos_add_to_verifications() drops all-NA context columns (ad-hoc verifications)", {
  verified_data <- tibble::tibble(
    id = c("1", "2"),
    redcap_event_name = NA_character_,
    redcap_form_name = c("form_a", "form_a"),
    value = c(1, 2),
    .ok = c(FALSE, TRUE)
  )

  result <- argos_add_to_verifications(
    verified_data,
    verification_description = "desc",
    issue_text = "value is {value}"
  )

  issues <- result$issues[[1]]

  expect_equal(nrow(issues), 1)
  expect_equal(as.character(issues$id), "1")
  expect_equal(names(issues), c("id", "redcap_form_name", "issue"))
})

test_that("argos_add_completeness_results() drops all-NA columns independently per form", {
  completeness_results <- tibble::tibble(
    id = c("1", "2", "3"),
    # Never populated in this fixture -> entirely NA across all forms
    redcap_event_name = NA_character_,
    redcap_form_name = c("form_a", "form_b", "form_b"),
    # All-NA for form_a (unique instrument) but populated for form_b
    # (repeating instrument): the drop must be evaluated per form, not
    # globally, or redcap_instance_number would be kept/dropped incorrectly
    # for one of the two forms.
    redcap_instance_number = c(NA_character_, "1", "2"),
    variable = c("var1", "var2", "var2"),
    completeness_issue = c(
      "Regular missing",
      "Regular missing",
      "Regular missing"
    )
  )
  attr(completeness_results, "reviewed_forms") <- c("form_a", "form_b")

  result <- argos_add_completeness_results(
    tibble::tibble(),
    completeness_results,
    verification_text = "Some completeness check"
  )

  form_a_issues <- result$issues[[1]]
  form_b_issues <- result$issues[[2]]

  # redcap_event_name is all-NA for both forms -> dropped in both
  # redcap_instance_number is all-NA only for form_a -> dropped there, but
  # kept for form_b
  expect_equal(names(form_a_issues), c("id", "redcap_form_name", "issue"))
  expect_equal(
    names(form_b_issues),
    c("id", "redcap_form_name", "redcap_instance_number", "issue")
  )
})
