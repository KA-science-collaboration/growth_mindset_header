# on local machine:
# set dt as appropriate in scripts_combined (to a recent day)
s3cmd put scripts_combined.q s3://ka-mapreduce/tmp/scripts_combined.q
ssh ka-analytics

# on ka-analytics
s3cmd del --recursive s3://ka-mapreduce/tmp/jascha/gm_ab_perproblem
analytics/src/emr.py --num_instances 100 's3://ka-mapreduce/tmp/scripts_combined.q'
# elastic-mapreduce --list --active
# ssh -L 2020:[lookup ip from above]:9100 ka-analytics
# connect to http://localhost:2020 to check status

# after query finishes, still on ka-analytics
cd /ebs/kadata/tmp/jascha/
rm -rf gm_ab_perproblem
s3cmd get --recursive s3://ka-mapreduce/tmp/jascha/gm_ab_perproblem
cd gm_ab_perproblem
cat -v 00* | sed 's/\^A/,/g' > gm_ab_perproblem.csv
# if the subconditions are incorrectly sorted by alphabetical rather than calling order, then do something like
#sed -i 's/foo/bar/g' gm_ab_perproblem.csv
gzip gm_ab_perproblem.csv

# on wired local machine
cd ~/Dropbox/growth_mindset/
scp ka-analytics:/ebs/kadata/tmp/jascha/gm_ab_perproblem/gm_ab_perproblem.csv.gz ./gm_ab_perproblem_[DATE].csv.gz
# zcat gm_ab_perproblem_[DATE].csv.gz | python2 ../growth_mindset_header/analysis/gm_process.py > gm_process_output_[DATE].csv
# zcat gm_ab_perproblem_130601.csv.gz | python2 ../growth_mindset_header/analysis/gm_process.py -m date -p true





set mapred.reduce.tasks=100




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
          time_stamp as time_stamp
        FROM website_request_logs wrl
        WHERE bingo_id IS NOT NULL and dt = '2013-08-24'

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
          WHERE pl.dt = '2013-08-24' and udi.bingo_id IS NOT NULL
        ) plid
    ) logs ORDER BY time_stamp
  ) pwrl
  -- why do we need both logs and pwrl?
  SELECT TRANSFORM(pwrl.*)
  USING 'request_log_reducer.py'
  AS bingo_id, json, message_text, dt
limit 1000;


  FROM (
    SELECT
      logs.id as id,
      logs.info as info,
      logs.dt as dt,
      logs.time_stamp
    FROM (
        SELECT
          bingo_id as id,
          kalog as info,
          dt as dt,
          time_stamp
        FROM website_request_logs wrl
        WHERE bingo_id IS NOT NULL and dt = '2013-09-01'

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
          WHERE pl.dt = '2013-09-01' and udi.bingo_id IS NOT NULL
        ) plid
    ) logs
  ) pwrl
  SELECT TRANSFORM(pwrl.*)
  USING 'request_log_reducer.py'
  AS bingo_id, json, message_text, dt
limit 1000;
