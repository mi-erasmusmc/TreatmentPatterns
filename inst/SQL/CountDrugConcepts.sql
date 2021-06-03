DROP TABLE IF EXISTS @resultsSchema.drug_concepts;

CREATE TABLE @resultsSchema.drug_concepts AS
SELECT
  CONCAT(asthma_drugs.med_group, ' all') AS name,
    descendants.concept_id AS concept_id,
       descendants.concept_name AS concept_name,
    count(person_id) AS count_total,
    count(DISTINCT person_id) AS count_persons
FROM asthma_drugs
  INNER JOIN @resultsSchema.concept_ancestor
  -- look for all descendants
    ON concept_ancestor.ancestor_concept_id = asthma_drugs.concept_id
  LEFT JOIN @resultsSchema.concept AS descendants -- get information descendants
    ON descendants.concept_id = concept_ancestor.descendant_concept_id
  LEFT JOIN @resultsSchema.concept_relationship AS dose_form  -- get dose form
    ON dose_form.concept_id_1 = descendants.concept_id
       AND dose_form.relationship_id = 'RxNorm has dose form'
  LEFT JOIN @resultsSchema.concept AS dose_form_info -- get information dose form (todo: check double dose forms)
    ON dose_form_info.concept_id = dose_form.concept_id_2
  INNER JOIN medgroup_doseform AS selected_dose_forms
    ON asthma_drugs.med_group = selected_dose_forms.med_group AND
       dose_form_info.concept_name IS NOT DISTINCT FROM selected_dose_forms.dose_form
    LEFT JOIN @cdmDatabaseSchema.drug_exposure AS frequency
    ON frequency.drug_concept_id = descendants.concept_id
    WHERE frequency.drug_exposure_start_date > DATEFROMPARTS(2009, 12, 31)
GROUP BY asthma_drugs.med_group, descendants.concept_id, descendants.concept_name;


DROP TABLE IF EXISTS @resultsSchema.drug_concepts_present;
CREATE TABLE @resultsSchema.drug_concepts_present AS
SELECT *
FROM @resultsSchema.drug_concepts
WHERE count_total > 0;


DROP TABLE IF EXISTS @resultsSchema.drug_concepts_agg;

CREATE TABLE @resultsSchema.drug_concepts_agg AS
SELECT name,
       sum(count_persons) as total_persons,
       sum(count_total) as total_total
FROM @resultsSchema.drug_concepts
GROUP BY name;
