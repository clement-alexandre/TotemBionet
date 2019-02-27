#!/bin/bash

cstrFile=$1 && \
pathSolver=$2 && \
pathSoft=$3 && \
finalDir=$4 && \
nbSolution=$5 && \
nbLine=42 && \
prec=$6 && \

# We select a random integer between 1 and $nbSolution
# We keep the nth (randomNumber) set of variables + constants of the solver 
# and we identify a set of parameters
randomNumber=$(((RANDOM % $nbSolution) + 1)) && \
# echo "Random number between 1 and " $nbSolution " : " $randomNumber

# We use the constraint solver for 
# identifying one set of parameters satisfying
# the constraints
cd $pathSolver
./solver.opt $cstrFile -max_sol $nbSolution -p $prec -debug -t -d boxQS > $finalDir/result_solver
cd $finalDir

while [ $nbLine -gt 0 ]; do
	if grep -q "^sure:" result_solver;
		then
			nbSureSet=$(grep -c "^sure:" result_solver) &&\
			# Select all intervals from the first set of parameters
			# computed in the result file provided by AbSolute
			grep -E -o -m$nbSureSet '^sure:.*' result_solver | tail -n1 > temp
			sed -i 's/ = /:/g' temp &&\
			sed -i 's/; /;/g' temp &&\
			sed 's/sure: .*, (\(.*\))/\1/' temp  > temp2  &&\
			sed -ie 's/ /\n/g' temp2   &&\
			# Keep the constraint of the line $randomChoice
			sed "${randomChoice}q;d" temp2 > temp3
			sed 's/\(.*\)\:.\(.*\)./\1\:\2/' temp3 > temp4 &&\

			# We keep all variables 
			sed 's/sure: (.*), (\(.*\))/\1/' temp  > temp_simu  &&\
			sed -ie 's/ /\n/g' temp_simu   &&\

			sed 's/\(.*\)\:\(.*\)/\1\=\2/' temp_simu > temp_simu2 &&\
			sed '/^\(C_.*\)/!d' temp_simu2 > cel_file &&\
			sed -i 's/=\(-\?[0-9].*\)/=[\1;\1]/g' cel_file &&\
			sed '/\(pi_entrance_.*\)/!d' temp_simu2 > temp_piinit &&\
			sed -i 's/=\([0-9]\.*[0-9]*\)/=[\1;\1]/g' temp_piinit &&\

			# Select the Hybrid Condition of each variable for the simulator
			searchHybridConditionPrecondition=1 &&\
			maxValue=0;
			while [ $searchHybridConditionPrecondition -eq 1 ]; do
				if grep -q "pi_entrance_.*_$(($maxValue + 1))" temp_piinit
					then
						maxValue=$((maxValue+1))
					else
						searchHybridConditionPrecondition=0
						sed "/pi_entrance_.*_$maxValue.*/!d" temp_piinit > temp_piinit2 &&\
						sed 's/\(.*\)\=.\(.*\)./\1\:\2/' temp_piinit2 > temp_piinit3
						# sed "s/pi_entrance_\(.*\)_$maxValue\=.\(.*\);.*/pi(\1) \= \2/" temp_piinit2 > piinit

						nbPi=$(wc -l temp_piinit2 | awk '{print $1}') &&\
						lineRead=1;

						cd $pathSoft/parameters
						while [ $lineRead -le $nbPi ]; do
							ocamlc -c compare_min_max_piinit.ml &&\
							ocamlc -o find_min_and_max_piinit str.cma compare_min_max_piinit.cmo  &&\
							line=$(sed "$lineRead q;d" $finalDir/temp_piinit3) &&\
							# echo $line

							piValue=$(./find_min_and_max_piinit $line) &&\
							# echo $piValue
							rm -f *.cmi *.cmo  find_min_and_max_piinit

							echo $piValue | sed "s/pi_entrance_\(.*\)_$maxValue\=\(.*\)/pi(\1) \= \2/" >> $finalDir/temp_piinit_final
							(( lineRead += 1 ))
						done
						cp $finalDir/temp_piinit_final $finalDir/piinit
				fi
			done
			rm -f temp* &&\
			nbLine=0
		else
			if grep -q "unsure:" result_solver;
				then
					grep -E -o -m$randomNumber 'unsure:.*' result_solver | tail -n1 > temp
					sed -i 's/ = /:/g' temp &&\
					sed -i 's/; /;/g' temp &&\
					sed 's/unsure: (\(.*\)),.*/\1/' temp  > temp2  &&\
					sed -ie 's/ /\n/g' temp2   &&\
					sed '/\(C_.*\)\].*/!d' temp2 > temp2_cel &&\
					# TODO: Gérer toutes variables et pas uniquement les célérités
					# sed 's/unsure: (\(.*\)),.*/\1/' temp  > temp2_cel  &&\
					# sed -ie 's/ /\n/g' temp2_cel   &&\
					nbVariables=$(wc -l temp2_cel | awk '{print $1}') && \
					# echo $nbVariables && \
					randomChoice=$(((RANDOM % $nbVariables) + 1))
					# echo "Random constraint between 1 and " $nbVariables " : " $randomChoice
					# Keep the constraint of the line $randomChoice
					sed "${randomChoice}q;d" temp2_cel > temp3

					sed 's/\(.*\)\:.\(.*\)./\1\:\2/' temp3 > temp4 &&\

					# Read min and max of the interval of the celerity
					# and compute the new value (2min + max)/3
					cd $pathSoft/parameters && \
					ocamlc -c compare_min_max.ml &&\
					ocamlc -o find_min_and_max str.cma compare_min_max.cmo  &&\
					newConstant=$(./find_min_and_max $finalDir/temp4) &&\
					# echo $newConstant &&\
					cstName=$(echo $newConstant | cut -d '=' -f 1) &&\
					# echo $cstName &&\

					rm -f *.cmi *.cmo  find_min_and_max

					cd $finalDir && \
					sed -i "/real $cstName =/d" $cstrFile &&\
					# mv final_file $cstrFile &&\
					sed -i "/constants{/a \ $newConstant" $cstrFile &&\
					
					nbLine=$(wc -l temp2 | awk '{print $1}') &&\
					# nbLine=$(wc -l temp2_cel | awk '{print $1}') &&\
					# echo $nbLine

					rm -f temp*

					cd $pathSolver && \
					./solver.opt $cstrFile -max_sol $nbSolution -p $prec -debug -t -d boxQS > $finalDir/result_solver && \
					cd $finalDir
				else
					echo "No solution. " &&\
					exit 1
			fi
	fi
done

