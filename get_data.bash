#!/bin/bash
addr[0]="https://portal.invinets.com/latest_reading/update/424BL"
addr[1]="https://portal.invinets.com/latest_reading/update/424EU"
addr[2]="https://portal.invinets.com/latest_reading/update/424GK"
addr[3]="https://portal.invinets.com/latest_reading/update/424FY"

if [ -z "$1" ]
then
    echo "First argument not set - provide 0-3"
    exit 1
fi

delay=10
if [ -n "$2" ]
then
    delay=$2
fi

while [ true ]
do
    data=$(curl -s ${addr[$1]})
    stripped_data=$(echo $data | sed -e 's/"[^"]*": //g' | tr -d '{},"')
    echo $stripped_data
    sleep $delay
done
