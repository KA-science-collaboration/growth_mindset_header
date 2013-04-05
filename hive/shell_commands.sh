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
cd gm_ab_perproblem
cat -v 00* | sed 's/\^A/,/g' > gm_ab_perproblem.csv
# if the subconditions are incorrectly sorted by alphabetical rather than calling order, then do something like
#sed -i 's/foo/bar/g' gm_ab_perproblem.csv
gzip gm_ab_perproblem.csv

# on wired local machine
cd ~/Dropbox/growth_mindset/
scp ka-analytics:/ebs/kadata/tmp/jascha/gm_ab_perproblem.csv.gz ./gm_ab_perproblem_[DATE].csv.gz
