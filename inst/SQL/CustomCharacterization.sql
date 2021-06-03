
IF OBJECT_ID('@resultsSchema.@databaseName_characterization', 'U') IS NOT NULL
DROP TABLE @resultsSchema.@databaseName_characterization;

IF OBJECT_ID('@resultsSchema.@databaseName_targetcohort', 'U') IS NOT NULL
DROP TABLE @resultsSchema.@databaseName_targetcohort;

IF OBJECT_ID('@resultsSchema.@databaseName_customcohort', 'U') IS NOT NULL
DROP TABLE @resultsSchema.@databaseName_customcohort;

-- Load target population into targetcohort table
CREATE TABLE @resultsSchema.@databaseName_targetcohort
(
PERSON_ID BIGINT NOT NULL,
INDEX_DATE date NOT NULL,
COHORT_END_DATE date NOT NULL
);

INSERT INTO @resultsSchema.@databaseName_targetcohort (PERSON_ID, INDEX_DATE, COHORT_END_DATE)
SELECT
  c.subject_id,
  -- subject_id is equal to person_id
  c.cohort_start_date,
  -- cohort_start_date is equal to index_date
  c.cohort_end_date -- cohort_end_date is equal to cohort_end_date
FROM @resultsSchema.@cohortTable c
WHERE C.cohort_definition_id = @targetCohortId;

-- Load custom cohorts
CREATE TABLE @resultsSchema.@databaseName_customcohort
(
PERSON_ID BIGINT NOT NULL,
INDEX_DATE date NOT NULL
);

INSERT INTO @resultsSchema.@databaseName_customcohort (PERSON_ID, INDEX_DATE)
SELECT
  c.person_id,
  c.condition_start_date
FROM @cdmDatabaseSchema.condition_occurrence c
WHERE C.condition_concept_id IN (@characterizationConceptSet);

-- Do characterization
CREATE TABLE @resultsSchema.@databaseName_characterization
(
mean REAL
);

INSERT INTO @resultsSchema.@databaseName_characterization (mean)
SELECT round(count(DISTINCT t.person_id) * 1.0 / (SELECT count(DISTINCT person_id) FROM @resultsSchema.@databaseName_targetcohort),4)
FROM @resultsSchema.@databaseName_targetcohort as t
JOIN @resultsSchema.@databaseName_customcohort as c
ON t.person_id = c.person_id
WHERE c.index_date <= t.index_date;


IF OBJECT_ID('@resultsSchema.@databaseName_targetcohort', 'U') IS NOT NULL
DROP TABLE @resultsSchema.@databaseName_targetcohort;

IF OBJECT_ID('@resultsSchema.@databaseName_customcohort', 'U') IS NOT NULL
DROP TABLE @resultsSchema.@databaseName_customcohort;
