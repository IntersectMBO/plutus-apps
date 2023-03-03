#!/usr/bin/env bash

# For monitoring time to sync marconi-chain-index. Logs cpu, mem and sync %.

# Run marconi-chain-index with "> >(tee marconi-chain-index-log.txt)" and then run this script with marconi-chain-index's log as arg
if [[ $1 ]]; then marconi_chain_index_log=$1; else echo "provide marconi-chain-index log file (arg0)" && exit 1; fi
if [[ $2 ]]; then pid=$2; else echo "provide marconi-chain-index PID (arg1)" && exit 1; fi

startMs=$(date +%s)

if [[ -z $pid ]]
then
	echo -e "pid empty, exiting.\n"
	exit 1
else
	echo -e "marconi-chain-index pid=$pid\n"
fi

exec > >(tee monitor-marconi-chain-index-log.txt)

echo -e "Starting monitoring at $(date)"
echo -e "Process started at $(ps -p "$pid" -o start_time=)\n"
ps -p "$pid" -o etime,%cpu,rss,%mem

while true
do
	last_log=$(tail -n1 "$marconi_chain_index_log")
	sync_percent=$(echo "$last_log" | cut -d '(' -f2 | cut -d ')' -f1)
	echo -e "Synced: $sync_percent"
	ps -p "$pid" -o etime=,%cpu=,rss=,%mem=

	if [[ "$last_log" == *"Synchronised"* ]]
	then
		echo -e "\nFinished at $(date)"
		echo "Time taken: $(($(date +%s) - startMs)) seconds"
		exit 1
	else
		sleep 10
	fi
done
