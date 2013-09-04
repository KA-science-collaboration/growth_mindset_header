DROP TABLE brain_pageloads;
CREATE EXTERNAL TABLE IF NOT EXISTS brain_pageloads(
    bingo_id STRING,
    timestamp STRING,
    url STRING
    )
COMMENT 'Summary of loads of brain workout page'
LOCATION 's3://ka-mapreduce/tmp/brain_pageloads';

INSERT OVERWRITE TABLE brain_pageloads
SELECT
    wrl.bingo_id as bingo_id,
    wrl.time_stamp as timestamp,
    wrl.url as url
FROM website_request_logs wrl
WHERE url = 'brainworkout_1' OR url = 'brainworkout_2'
ORDER BY timestamp

INSERT OVERWRITE DIRECTORY 's3://ka-mapreduce/tmp/sitan/gm_pageloads'
SELECT bingo_id, timestamp, url
FROM brain_pageloads
ORDER BY timestamp
