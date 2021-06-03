SELECT cohort_definition_id AS cohort_id,
	COUNT(*) AS cohort_entries,
	COUNT(DISTINCT subject_id) AS cohort_subjects
FROM @resultsSchema.@cohortTable
{@cohortIds != ''} ? {WHERE cohort_definition_id IN (@cohortIds)}
GROUP BY cohort_definition_id;

