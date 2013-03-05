-- get rid of last version of this table
-- s3cmd del --recursive s3://ka-mapreduce/tmp/jascha/gm_ab_perproblem

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

-- download the table
-- analytics@ip-10-0-0-108:/ebs/kadata/tmp/jascha$ s3cmd get --recursive s3://ka-mapreduce/tmp/jascha/gm_ab_perproblem
-- cd /ebs/kadata/tmp/jascha/gm_ab_perproblem/
-- cat -v 00* | sed 's/\^A/,/g' > gm_ab_perproblem.csv

