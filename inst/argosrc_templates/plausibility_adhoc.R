# Code in this scripts all ad hoc verifications that are automtically handled by
# argosrc.
#
# All verifications must contain an .ok column to indicate whether each row
# passed the check.
#
# Add at the end of each verification a call to argos_add_to_plausibility()
# indicating a descrption and a glue template for issue text.
#
# The function will filter rows where .ok is FALSE or NA, build the issue text
# with glue, and reshape the output to the structure expected by
# argos_check_plausibility().
