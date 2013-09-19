-- combines the user_matchup and anonymized_problemlog scripts, so they can easily be run together


-- Summarizes a user's participation inside the growth mindset header and growth mindset header subtest A/B tests.
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
-- set hivevar:EXPERIMENT=growth mindset header;
-- set hivevar:EXP_PARTITION=growth-mindset;
-- set hivevar:dt=2012-08-22;


-- to run use
-- s3cmd put request_log_reducer.py s3://ka-mapreduce/tmp/jascha/request_log_reducer.py
-- s3cmd put user_matchup.q s3://ka-mapreduce/tmp/growth_user_matchup.q
-- ssh ka-analytics
-- analytics/src/emr.py 's3://ka-mapreduce/tmp/growth_user_matchup.q'

-- need to have a very large number of reducers, due to potential
-- memory/cpu issues in request_log_reducer.py

set mapred.reduce.tasks=1000;

set hivevar:dt=2013-09-17;

set hivevar:dt_problemlog_start=2013-01-01;

ADD FILE s3://ka-mapreduce/code/py/bingo_alternative_selector.py;
ADD FILE s3://ka-mapreduce/tmp/jascha/request_log_reducer.py;

DROP TABLE mindset_experiment_info;
CREATE EXTERNAL TABLE IF NOT EXISTS mindset_experiment_info(
  user STRING,
  user_id STRING,
  user_email STRING,
  bingo_identity STRING,
  message_text STRING,
  -- jsonified ProblemLog object
  json STRING,
  dt STRING
  )
COMMENT 'Info about a user\'s participation in an experiment'
PARTITIONED BY (experiment STRING, alternative STRING)
LOCATION 's3://ka-mapreduce/summary_tables/mindset_experiment_info';

alter table bingo_alternative_infop recover partitions;
SET hive.exec.dynamic.partition=true;

set hivevar:EXPERIMENT=growth mindset header;
set hivevar:EXP_PARTITION=growth-mindset;

INSERT OVERWRITE TABLE mindset_experiment_info
PARTITION (experiment="${EXP_PARTITION}", alternative)
SELECT
  get_json_object(ud.json, '$.user') AS user,
  get_json_object(ud.json, '$.user_id') AS user_id,
  get_json_object(ud.json, '$.user_email') AS user_email,
  id_alt.identity,
  alllogs.message_text,
  alllogs.json,
  alllogs.dt,
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
) id_al
-- now annotate the bingo id -> alternative map with UserData info
-- TODO(jace): we should include the bingo identity in userdata_info
INNER JOIN UserData ud
ON id_alt.identity = get_json_object(ud.json, '$.gae_bingo_identity')
-- attach mindset message texts, obtained by unioning problemlogs and kalogs 
-- and matching these entries on total_done = problem_number - 1
INNER JOIN
(
  FROM (
    SELECT
      logs.id as id,
      logs.info as info,
      logs.dt as dt,
      logs.time_stamp as time_stamp
    FROM (
        SELECT
          bingo_id as id,
          kalog as info,
          dt as dt,
          time_stamp
        FROM website_request_logs wrl
        WHERE bingo_id IS NOT NULL and dt > ${dt_problemlog_start}

        UNION ALL

        SELECT
          id, info, dt, 
          get_json_object(plid.info, '$.backup_timestamp') as time_stamp
        FROM (
          SELECT
            udi.bingo_id as id,
            pl.json as info,
            pl.dt as dt
          FROM problemlog pl
          INNER JOIN userdata_info udi
          ON pl.user = udi.user
          WHERE pl.dt > ${dt_problemlog_start} and udi.bingo_id IS NOT NULL
        ) plid
    ) logs DISTRIBUTE BY id
  ) pwrl
  -- why do we need both logs and pwrl?
  SELECT TRANSFORM(pwrl.*)
  USING 'request_log_reducer.py'
  AS bingo_id, json, message_text, dt
) alllogs
ON id_alt.identity = alllogs.bingo_id
;





set hivevar:EXPERIMENT=growth mindset header subtest;
set hivevar:EXP_PARTITION=growth-mindset-subtest;


INSERT OVERWRITE TABLE mindset_experiment_info
PARTITION (experiment="${EXP_PARTITION}", alternative)
SELECT
  get_json_object(ud.json, '$.user') AS user,
  get_json_object(ud.json, '$.user_id') AS user_id,
  get_json_object(ud.json, '$.user_email') AS user_email,
  id_alt.identity,
  alllogs.message_text,
  alllogs.json,
  alllogs.dt,
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
) id_al
-- now annotate the bingo id -> alternative map with UserData info
-- TODO(jace): we should include the bingo identity in userdata_info
INNER JOIN UserData ud
ON id_alt.identity = get_json_object(ud.json, '$.gae_bingo_identity')
-- attach mindset message texts, obtained by unioning problemlogs and kalogs 
-- and matching these entries on total_done = problem_number - 1
INNER JOIN
(
  FROM (
    SELECT
      logs.id as id,
      logs.info as info,
      logs.dt as dt,
      logs.time_stamp as time_stamp
    FROM (
        SELECT
          bingo_id as id,
          kalog as info,
          dt as dt,
          time_stamp
        FROM website_request_logs wrl
        WHERE bingo_id IS NOT NULL and dt > ${dt_problemlog_start}

        UNION ALL

        SELECT
          id, info, dt, 
          get_json_object(plid.info, '$.backup_timestamp') as time_stamp
        FROM (
          SELECT
            udi.bingo_id as id,
            pl.json as info,
            pl.dt as dt
          FROM problemlog pl
          INNER JOIN userdata_info udi
          ON pl.user = udi.user
          WHERE pl.dt > ${dt_problemlog_start} and udi.bingo_id IS NOT NULL
        ) plid
    ) logs DISTRIBUTE BY id
  ) pwrl
  -- why do we need both logs and pwrl?
  SELECT TRANSFORM(pwrl.*)
  USING 'request_log_reducer.py'
  AS bingo_id, json, message_text, dt
) alllogs
ON id_alt.identity = alllogs.bingo_id
;


INSERT OVERWRITE DIRECTORY 's3://ka-mapreduce/tmp/jascha/gm_ab_perproblem'
SELECT
  mei.experiment AS experiment,
  mei.alternative AS alternative,
  mei.bingo_identity AS identity,
  mei.message_text AS message_text,
  IF(ucs.num_coaches IS NULL, 0, ucs.num_coaches) AS num_coaches,
  IF(ucs.max_coach_students IS NULL, 0, ucs.max_coach_students) AS max_coach_students,
  get_json_object(mei.json, '$.exercise') AS exercise,
  get_json_object(mei.json, '$.problem_type') AS problem_type,
  get_json_object(mei.json, '$.seed') AS seed,
  get_json_object(mei.json, '$.problem_number') AS problem_number,
  get_json_object(mei.json, '$.time_done') AS time_done,
  cast(get_json_object(mei.json, '$.topic_mode') = 'true' AS INT) AS topic_mode,
  cast(get_json_object(mei.json, '$.review_mode') = 'true' AS INT) AS review_mode,
  cast(get_json_object(mei.json, '$.correct') = 'true' AS INT) AS correct,
  get_json_object(mei.json, '$.user_id') AS user_id,
  cast(get_json_object(mei.json, '$.earned_proficiency') = 'true' AS INT) AS proficiency,
  cast(get_json_object(mei.json, '$.count_hints') AS INT) AS hints,
  cast(get_json_object(mei.json, '$.time_taken') AS INT) AS time_taken,
  mei.dt AS dt
FROM
  mindset_experiment_info mei
    LEFT OUTER JOIN user_coach_summary ucs
      ON (ucs.user = mei.user)
WHERE
 mei.experiment = 'growth-mindset'
   OR
 mei.experiment = 'growth-mindset-subtest'
ORDER BY
  experiment,
  alternative,
  identity,
  time_done
;
