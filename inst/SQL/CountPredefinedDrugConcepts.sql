
IF OBJECT_ID('@resultsSchema.drug_concepts', 'U') IS NOT NULL
DROP TABLE @resultsSchema.drug_concepts;

CREATE TABLE @resultsSchema.drug_concepts
(
name varchar(255) NOT NULL,
concept_id int NOT NULL,
concept_name varchar(255)  NOT NULL,
count_total int not NULL,
count_persons int not NULL
);

INSERT INTO @resultsSchema.drug_concepts (name, concept_id, concept_name, count_total, count_persons)
SELECT
    name, concept_id, concept_name,
    count(occurrence.person_id) AS count_total,
    count(DISTINCT occurrence.person_id) AS count_persons
FROM @resultsSchema.all_drug_concepts as concepts
    LEFT JOIN @cdmDatabaseSchema.drug_exposure AS occurrence
    ON occurrence.drug_concept_id = concepts.concept_id
WHERE occurrence.person_id IN (SELECT target_cohort.subject_id
FROM @resultsSchema.@cohortTable as target_cohort
WHERE target_cohort.cohort_definition_id IN (@targetCohortIds))
GROUP BY name, concept_id, concept_name;

IF OBJECT_ID('@resultsSchema.drug_concepts_present', 'U') IS NOT NULL
DROP TABLE @resultsSchema.drug_concepts_present;

CREATE TABLE @resultsSchema.drug_concepts_present
(
name varchar(255) NOT NULL,
concept_id int NOT NULL,
concept_name varchar(255)  NOT NULL,
count_total int not NULL,
count_persons int not NULL
);

INSERT INTO @resultsSchema.drug_concepts_present (name, concept_id, concept_name, count_total, count_persons)
SELECT *
FROM @resultsSchema.drug_concepts
WHERE count_total > 0;