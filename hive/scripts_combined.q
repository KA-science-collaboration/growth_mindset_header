-- combines the user_matchup and anonymized_problemlog scripts, so they can easily be run together


-- Summarizes a user's participation inside of an A/B experiment.
--
-- Inputs:
--   EXPERIMENT - the canonical name for the experiment we want to
--       summarize user participations in.
--   EXP_PARTITION - used for the experiment partition name beacuse
--       because S3 does not like underscores etc in bucket names
--   dt - This effectively specifies as as-of date for the analysis. Because
--         GAEBingo prunes the data in GAEBingoIdentityRecord after experiments
--         are archived, if the experiment you are interested in is no longer
--         running you should set this to the last day it was still live.
-- Example Inputs:
-- set hivevar:EXPERIMENT=Accuracy model: Early proficiency;
-- set hivevar:EXP_PARTITION=early-prof;
-- set hivevar:dt=2012-12-05;


-- to run use
-- s3cmd put user_matchup.q s3://ka-mapreduce/tmp/growth_user_matchup.q
-- ssh ka-analytics
-- analytics/src/emr.py 's3://ka-mapreduce/tmp/growth_user_matchup.q'

set hivevar:dt=2013-04-08;

ADD FILE s3://ka-mapreduce/code/py/bingo_alternative_selector.py;

DROP TABLE user_experiment_info;
CREATE EXTERNAL TABLE IF NOT EXISTS user_experiment_info(
  user STRING,
  user_id STRING,
  user_email STRING,
  bingo_identity STRING
  )
COMMENT 'Info about a user\'s participation in an experiment'
PARTITIONED BY (experiment STRING, alternative STRING)
LOCATION 's3://ka-mapreduce/summary_tables/user_experiment_info';
ALTER TABLE user_experiment_info RECOVER PARTITIONS;

alter table bingo_alternative_infop recover partitions;
SET hive.exec.dynamic.partition=true;


set hivevar:EXPERIMENT=growth mindset header;
set hivevar:EXP_PARTITION=growth-mindset;

INSERT OVERWRITE TABLE user_experiment_info
PARTITION (experiment="${EXP_PARTITION}", alternative)
SELECT 
  get_json_object(ud.json, '$.user') AS user,
  get_json_object(ud.json, '$.user_id') AS user_id,
  get_json_object(ud.json, '$.user_email') AS user_email,
  id_alt.identity, 
  id_alt.alternative
FROM
(
  -- Create a map from bingo identities to alternatives for this experiment
  FROM (
    SELECT
        get_json_object(bir.json, '$.identity') AS bingo_identity,
        get_json_object(bir.json, '$.pickled.participating_tests')
            AS participating_tests,
        alt.canonical_name AS canonical_name,
        alt.hashable_name AS hashable_name,
        alt.name AS alternative_name,
        alt.weight AS alternative_weight,
        alt.number AS alternative_number
    FROM bingo_alternative_infop alt
    INNER JOIN GAEBingoIdentityRecord bir
      ON True   -- Simulate a CROSS JOIN (only available on Hive v0.10+)
    WHERE alt.canonical_name = "${EXPERIMENT}" AND alt.dt = "${dt}"
      AND get_json_object(bir.json, '$.pickled.participating_tests') LIKE 
        '%mindset%'
    CLUSTER BY bingo_identity
  ) map_output
  SELECT TRANSFORM(map_output.*)
  USING 'bingo_alternative_selector.py'
  AS identity, experiment, alternative
) id_alt
-- now annotate the bingo id -> alternative map with UserData info
-- TODO(jace): we should include the bingo identity in userdata_info
INNER JOIN UserData ud
ON id_alt.identity = get_json_object(ud.json, '$.gae_bingo_identity')
;




set hivevar:EXPERIMENT=growth mindset header subtest;
set hivevar:EXP_PARTITION=growth-mindset-subtest;

INSERT OVERWRITE TABLE user_experiment_info
PARTITION (experiment="${EXP_PARTITION}", alternative)
SELECT 
  get_json_object(ud.json, '$.user') AS user,
  get_json_object(ud.json, '$.user_id') AS user_id,
  get_json_object(ud.json, '$.user_email') AS user_email,
  id_alt.identity, 
  id_alt.alternative
FROM
(
  -- Create a map from bingo identities to alternatives for this experiment
  FROM (
    SELECT
        get_json_object(bir.json, '$.identity') AS bingo_identity,
        get_json_object(bir.json, '$.pickled.participating_tests')
            AS participating_tests,
        alt.canonical_name AS canonical_name,
        alt.hashable_name AS hashable_name,
        alt.name AS alternative_name,
        alt.weight AS alternative_weight,
        alt.number AS alternative_number
    FROM bingo_alternative_infop alt
    INNER JOIN GAEBingoIdentityRecord bir
      ON True   -- Simulate a CROSS JOIN (only available on Hive v0.10+)
    WHERE alt.canonical_name = "${EXPERIMENT}" AND alt.dt = "${dt}"
      AND get_json_object(bir.json, '$.pickled.participating_tests') LIKE 
        '%mindset%'
    CLUSTER BY bingo_identity
  ) map_output
  SELECT TRANSFORM(map_output.*)
  USING 'bingo_alternative_selector.py'
  AS identity, experiment, alternative
) id_alt
-- now annotate the bingo id -> alternative map with UserData info
-- TODO(jace): we should include the bingo identity in userdata_info
INNER JOIN UserData ud
ON id_alt.identity = get_json_object(ud.json, '$.gae_bingo_identity')
;




INSERT OVERWRITE DIRECTORY 's3://ka-mapreduce/tmp/jascha/gm_ab_perproblem'
SELECT 
  uei.experiment AS experiment,
  uei.alternative AS alternative,
  uei.bingo_identity AS identity,
  IF(ucs.num_coaches IS NULL, 0, ucs.num_coaches) AS num_coaches,
  IF(ucs.max_coach_students IS NULL, 0, ucs.max_coach_students) AS max_coach_students,
  pl.time_done AS time_done,
  pl.dt AS dt,
  pl.exercise AS exercise,
  pl.problem_type AS problem_type,
  pl.seed AS seed,
  pl.problem_number AS problem_number,
  pl.topic_mode AS topic_mode,
  pl.review_mode AS review_mode,
  pl.correct AS correct,
  pl.proficiency AS proficiency,
  pl.hints AS hints,
  pl.time_taken AS time_taken
FROM
  user_experiment_info uei 
    LEFT OUTER JOIN user_coach_summary ucs
      ON (ucs.user = uei.user)
    JOIN (
     SELECT
      get_json_object(pl.json, '$.exercise') AS exercise,
      get_json_object(pl.json, '$.problem_type') AS problem_type,
      get_json_object(pl.json, '$.seed') AS seed,
      get_json_object(pl.json, '$.problem_number') AS problem_number,
      get_json_object(pl.json, '$.time_done') AS time_done,
      cast(get_json_object(pl.json, '$.topic_mode') = 'true' AS INT) AS topic_mode,
      cast(get_json_object(pl.json, '$.review_mode') = 'true' AS INT) AS review_mode,
      cast(get_json_object(pl.json, '$.correct') = 'true' AS INT) AS correct,
      get_json_object(pl.json, '$.user_id') AS user_id,
      cast(get_json_object(pl.json, '$.earned_proficiency') = 'true' AS INT) AS proficiency,
      cast(get_json_object(pl.json, '$.count_hints') AS INT) AS hints,
      cast(get_json_object(pl.json, '$.time_taken') AS INT) AS time_taken,
      dt AS dt
     FROM problemlog pl WHERE 
      -- dt == '2013-02-05' 
      dt > '2012-01-01'
     ) pl
    ON (uei.user_id = pl.user_id)
WHERE
 uei.experiment = 'growth-mindset'
   OR
 uei.experiment = 'growth-mindset-subtest'
ORDER BY
  experiment, 
  alternative, 
  identity, 
  time_done
;
