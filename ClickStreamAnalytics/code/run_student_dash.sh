#!/bin/bash


# arg[1]: directory where  selected files need to dropped
# arg[2]: time window (in days) of clickstream from  
# arg[3]: directory where clickstreams are located


# Soma S Dhavala: Dec 19th, 2014

# inputs: status, messege, folder 

base_dir="/data/production/analytics/dashboards/"
report_status () {
	
	if [ $1 -ne 0 ]; then
		echo "$2 : failed" > "$3"/error.txt
		echo "$2 : failed" >> "$3"/log.txt	
		exit 1
	else
		echo "$2 : ok" >> "$3"/log.txt	
	fi
}

if [[  $# -ne 2 ]] ; then
    echo "two arguments are required"
    exit 1
fi

if [[ ! -d $1 ]] ; then
    echo "output directory does not exist"
    exit 1
else
	msg="Output dir $1 exists"
	report_status 0 "$msg" "$1" 
fi

newdirname=`date +"%Y%m%d%H%M%S"`


#output_dir="./../data/output/"
output_dir=$1
# input folder that comes from the use (like /data/clickstreams)
input_dir=$base_dir/data/input/
#input_dir="/data/production/logstream/dashboards/"



if [[ ! -d $input_dir ]] ; then
    msg="clickstream directory does not exist"
  	report_status 1 "$msg" "$output_dir"
fi



test $2 -gt 0 2>/dev/null
report_status	$? "check time window positve integer" "$output_dir"
time_window=$2
#time_window="7"


if [[ "$2" -lt 0 ]] ; then
	msg="time window is not a positve integer"
	report_status 1 "$msg" "$output_dir"
fi


report_status 0 "creating project directories" "$output_dir"


#inputdir=$2 
archive_dir=$base_dir/data/archive/

project_dir="$archive_dir"/$newdirname
project_input_dir="$project_dir"/input
project_output_dir="$project_dir"/output
project_log_dir="$project_dir"/logs


echo $input_dir
echo $output_dir
echo $project_dir


# place where  selected clickstreams will be copied
mkdir -p $project_input_dir
chmod 700 $project_input_dir
report_status $? "creating project input directories" "$output_dir"


# place where  processed data will be made available
mkdir -p $project_output_dir
chmod 700 $project_output_dir
report_status $? "creating project output directories" "$output_dir"

# place all logs
mkdir -p $project_log_dir
chmod 700 $project_log_dir
report_status $? "creating project log directories" "$output_dir"

report_status 0  "creating project directories" "$output_dir"

# copy all files that are older than certain time
# copy them to a folder for processing by dashboards engine
find "$input_dir" -type f -mtime -$time_window -exec cp {} "$project_input_dir" \;
report_status $? "copied files" "$output_dir"

# remove all empty files and merge all clickstreams files into one
find "$project_input_dir" -type f -empty -delete
report_status $? "deleted empty files" "$output_dir"


# merge all files 
cat  "$project_input_dir"/* > "$project_input_dir"/input.csv
report_status $? "merging selected clickstream files" "$output_dir"


# and remove the individual files
find "$project_input_dir" -type f -not -name "input.csv" | xargs rm
# sort
sort -k1 "$project_input_dir"/input.csv -o "$project_input_dir"/input.csv
report_status $? "sorted data on session id and timestamp" "$output_dir"


# R program can be called to operate on the input file
# R requires input file, outfile, and any optional parameters
report_status 0 "calling R for processing" $output_dir
Rscript $base_dir/code/drive_student_att_dashboard_data.R "$project_input_dir"/input.csv "$project_output_dir"/output.csv
report_status $? "exited R environment" "$output_dir"

# if successful, copy it to output folder
cp "$project_output_dir"/output.csv "$output_dir"/
