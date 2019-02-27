#!/bin/sh

# Path of Absolute in settings.conf
. ./settings.conf
# echo $pathSolver

# Path of Soft
pathSoft=$(pwd)
# echo $pathSoft

for i in "$@"
do
	case $i in
		-i|--input)
		isDefinedInitialFile=true
		initialFile=$2
		absoluteInitialDir=$(cd $(dirname $initialFile) && pwd)
		initialFilename=$(basename $initialFile)
		shift
		shift
		;;
		-o|--output)
		isDefinedOutputFile=true
		filenameFinal=$(basename $2)
		# echo $filenameFinal && \
		finalDir=$(dirname $2)
		# echo $finalDir
		if [ ! -d "$finalDir" ]
			then
				mkdir $(dirname $2)
		fi
		absoluteFinalDir=$(cd $(dirname $2) && pwd)
		# echo $absoluteFinalDir/$filenameFinal && \
		shift
		shift
		;;
		-t|--time)
		maxTime=$2
		shift
		shift
		;;
		-n|--number|--nbsolution)
		nbSolution=$2
		shift
		shift
		;;
		-p|--precision)
		prec=$2
		shift
		shift
		;;
		-h|--help)
echo "./modelling.sh -i Input_file [OPTIONS...]
-i : Declares the input file named Input_file here

OPTIONS :
-h : Display this list of options
-n : Indicates the number of solutions for the constraint solver (Default : 10)
-o : Declares the output file with its path (Default : directory named with date of the day in the current directory of the input file)
-t : Time (in hour) represented in the graph and for the simulation (Default : 72)
-p : Precision for the Solver (Default : 1e-3)

EXAMPLES :
./modelling.sh -i ./examples/path_test -n 100 -t 48
./modelling.sh -i ./examples/path_test -o ./examples/res/output_file"
exit 1
	esac
done


if [ ! $isDefinedInitialFile ]
	then
		echo "No input file defined.
Use ./modelling.sh --help for more information."
		exit 1
fi

if [ ! $isDefinedOutputFile ]
	then
		initialFilename=$(basename $initialFile)
		filenameFinal=cstr_$initialFilename
		absoluteFinalDir=$absoluteInitialDir/`date +%Y-%m-%d`
fi


# First we identify all constraints of the 
# input influence graph linked with a Hoare
# triple
cd ./constraints && \
./run.sh $initialFile $absoluteFinalDir/$filenameFinal $absoluteFinalDir/dinit  || exit 1 && \
cd ../

# Then we identify all variables using the solver Absolute
cd ./parameters/ && \
cp $absoluteFinalDir/$filenameFinal\_1.abs $absoluteFinalDir/final_cstr_$initialFilename.abs && \
#./identification_parameters.sh $absoluteFinalDir/final_cstr_$initialFilename.abs $pathSolver $pathSoft $absoluteFinalDir $nbSolution $prec || exit 1 &&\


# Now we simulate the dynamics of each component depending on the influence graph.
cd $pathSoft/simulator
./identify_cel.sh $absoluteFinalDir/cel_file  || exit 1 && \
./simu.sh $absoluteInitialDir/$initialFilename $absoluteFinalDir/data_cel.txt $absoluteFinalDir $maxTime.0  || exit 1 &&\

# Finally we create the graphs
cd $pathSoft/graph
./graph_config_file.sh $absoluteFinalDir/graph_data.txt $absoluteInitialDir/$initialFilename $absoluteFinalDir result\_$initialFilename\_$maxTime\H.pdf $maxTime  || exit 1 &&\
echo "Creation of graph achieved !"
