# on local machine:
# set dt as appropriate in scripts_combined (to a recent day)
s3cmd put growth_mindset_scripts_combined.q s3://ka-mapreduce/tmp/growth_mindset_scripts_combined.q
s3cmd put request_log_reducer.py s3://ka-mapreduce/tmp/jascha/request_log_reducer.py
ssh ka-analytics

# on ka-analytics
s3cmd del --recursive s3://ka-mapreduce/tmp/jascha/gm_ab_perproblem
s3cmd del --recursive s3://ka-mapreduce/summary_tables/mindset_experiment_info

analytics/src/emr.py --num_instances 50 's3://ka-mapreduce/tmp/growth_mindset_scripts_combined.q'
# elastic-mapreduce --list --active
# ssh -L 2020:[lookup ip from above]:9100 ka-analytics
# connect to http://localhost:2020 to check status

# after query finishes, still on ka-analytics
#cd /ebs/kadata/tmp/jascha/
cd /ebs/modeling/jascha/header_text
rm -rf gm_ab_perproblem
s3cmd get --recursive s3://ka-mapreduce/tmp/jascha/gm_ab_perproblem
cd gm_ab_perproblem
cat -v 00* | sed 's/,//g' | sed 's/\^A/,/g' > gm_ab_perproblem.csv
gzip gm_ab_perproblem.csv

cd ..
s3cmd get --recursive s3://ka-mapreduce/tmp/jascha/brainworkout
cd brainworkout
cat -v 00* | sed 's/,//g' | sed 's/\^A/,/g' > brainworkout.csv
gzip brainworkout.csv

# cat -v 00* | sed 's/,//g' > gm_ab_perproblem_temp.csv
# cat gm_ab_perproblem_temp.csv | sed 's/\^A/,/g' > gm_ab_perproblem.csv
# rm gm_ab_perproblem_temp.csv
# if the subconditions are incorrectly sorted by alphabetical rather than calling order, then do something like
#sed -i 's/foo/bar/g' gm_ab_perproblem.csv

# on wired local machine

export gmdate=[DATE STRING]
cd /data/growth
mkdir ${gmdate}
cd ${gmdate}
scp ka-analytics:/ebs/modeling/jascha/header_text/brainworkout/brainworkout.csv.gz ./brainworkout_${gmdate}.csv.gz
scp ka-analytics:/ebs/modeling/jascha/header_text/gm_ab_perproblem/gm_ab_perproblem.csv.gz ./gm_ab_perproblem_${gmdate}.csv.gz
#scp ka-analytics:/ebs/kadata/tmp/jascha/gm_ab_perproblem/gm_ab_perproblem.csv.gz ./gm_ab_perproblem.csv.gz
cat gm_ab_perproblem_${gmdate}.csv.gz | openssl enc -aes-256-cbc -e > gm_ab_perproblem_${gmdate}.csv.gz.aes

cd ~/Dropbox/growth_mindset/
mv /data/tmp/gm_ab_perproblem_${gmdate}.csv.gz.aes ./

# and some possible analysis lines
cat gm_ab_perproblem_${gmdate}.csv.gz.aes | openssl aes-256-cbc -d | gunzip | python2 ~/Dropbox/growth_mindset_header/analysis/gm_process.py > gm_process_output_${gmdate}.csv
mkdir figures_${gmdate}
cat gm_ab_perproblem_${gmdate}.csv.gz.aes | openssl aes-256-cbc -d | gunzip | python2 ~/Dropbox/growth_mindset_header/analysis/gm_process.py -m date -p true --base_filename figures_${gmdate}/
cat gm_process_output_${gmdate}.csv | python2 ../growth_mindset_header/analysis/gm_process_alternatives.py 


# analysis only of already active students
zcat gm_ab_perproblem.csv.gz | python2 ~/Dropbox/growth_mindset_header/analysis/gm_process.py --min_pre_problems 100 > gm_process_output_alreadyactive_${gmdate}.csv
mkdir figures_alreadyactive_${gmdate}
zcat gm_ab_perproblem.csv.gz | python2 ~/Dropbox/growth_mindset_header/analysis/gm_process.py -m date -p true --min_pre_problems 100 --base_filename figures_alreadyactive3_${gmdate}/
cat gm_process_output_alreadyactive_${gmdate}.csv | python2 ../growth_mindset_header/analysis/gm_process_alternatives.py 



## random incantations -- scratch space below here
# drop a column from csv

cat gm_process_output_2013-10-10.csv | python gm_post_vs_pre.py

zcat gm_ab_perproblem_userid.csv.gz | cut -d , -f 1-14,16- > gm_ab_perproblem.csv
gzip gm_ab_perproblem.csv

zcat gm_ab_perproblem.csv.gz | head | cut -d , -f 1-14,16-

