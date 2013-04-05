# on local machine:
# set dt as appropriate in scripts_combined (to a recent day)
s3cmd put scripts_combined.q s3://ka-mapreduce/tmp/scripts_combined.q
ssh ka-analytics

# on ka-analytics
s3cmd del --recursive s3://ka-mapreduce/tmp/jascha/gm_ab_perproblem
analytics/src/emr.py 's3://ka-mapreduce/tmp/scripts_combined.q'
# elastic-mapreduce --list --active
# ssh -L 2020:[lookup ip from above]:9100 ka-analytics
# connect to http://localhost:2020 to check status

# after query finishes
cd /ebs/kadata/tmp/jascha/
rm -rf gm_ab_perproblem
s3cmd get --recursive s3://ka-mapreduce/tmp/jascha/gm_ab_perproblem
cat -v 00* | sed 's/\^A/,/g' > gm_ab_perproblem.csv
