#!/bin/bash

initialData=$1
finalDir=$3
pathFinalFile=$finalDir/$4
valueTime=$5
# echo $finalDir
grep -E -o 'var .*' $2 > $finalDir/temp && \
sed 's/var \(.*\);/\1/' $finalDir/temp  > $finalDir/temp2 && \
# Identification of the greatest boundary betwwen variables
maxBv=$(awk -v max=0 '{if($2>max){want=$2; max=$2}}END{print want} ' $finalDir/temp2) && \
# echo "The max boundary is " $maxBv"."

# Identification of the number of variables in the influence graph 
nbLine=$(wc -l $finalDir/temp2 | awk '{print $1}') 
# echo $nbLine "variable(s) defined in the influence graph."
rm -f $finalDir/temp*

# Creation of settings file for Gnuplot
settingsFile=$finalDir/settings.gnu
echo -n "# Command file from Gnuplot in order to represent the behavior of variables during time

set terminal pdf
set output \"$pathFinalFile\"
set title \"\"
set xlabel \"Temps (heures)\"
set ylabel \"\"
set xrange [0.0:$valueTime.0]
set yrange [0.0:$(($maxBv+2)).0]
set tics font \"helvetica,20\"
set key right
set key font \"helvetica,20\"
set key samplen 2
set xlabel font \"helvetica,20\"
set style rect fc lt -1 fs solid 0.15 noborder
plot " >> $settingsFile

dayBox=""
curves=""
for ((i=1; i<=$nbLine; i++));
do
	positionVariable=$(($i+1))
	variableName=$(awk -v var=$positionVariable 'NR==1{print $var}' $initialData)
	# echo $variableName", column "$positionVariable
	if [ ! $i -eq $nbLine ]
		then
			if [ $variableName == "day" ]
				then
					dayBox="$dayBox \"$1\" using 1:(\$$positionVariable <=1 ? 1:1/0) notitle with filledcurves x1 ls 5 lt rgb \"#DDDDDD\","
					dayBox="$dayBox \"$1\" using 1:(\$$positionVariable <=1 ? 1:1/0) notitle with filledcurves x2 ls 5 lt rgb \"#DDDDDD\","
				else
					if [ ! $variableName == "night" ]
						then
							curves="$curves\"$1\" using 1:$positionVariable title '$variableName' with lines lw 2 ,"
					fi
			fi
		else
			if [ $variableName == "day" ]
				then
					isDayLastVariable=true
					dayBox="$dayBox \"$1\" using 1:(\$$positionVariable <=1 ? 1:1/0) notitle with filledcurves x1 ls 5 lt rgb \"#DDDDDD\","
					dayBox="$dayBox \"$1\" using 1:(\$$positionVariable <=1 ? 1:1/0) notitle with filledcurves x2 ls 5 lt rgb \"#DDDDDD\","
				else
					if [ ! $variableName == "night" ]
						then
							curves="$curves \"$1\" using 1:$positionVariable title '$variableName' with lines lw 2" >> $settingsFile
					fi
			fi
	fi
done


if [ $isDayLastVariable ]
	then
		echo -n $dayBox >> $settingsFile
		echo ${curves::-2} >> $settingsFile		
	else
		echo -n $dayBox >> $settingsFile
		echo $curves >> $settingsFile
fi
echo "quit" >> $settingsFile


gnuplot $settingsFile
rm -f $settingsFile
