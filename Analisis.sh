#!/bin/bash


for dir in $(/bin/ls -d Ising_*)
do
	cd ${dir}
	
	echo 'Analizando: ' $dir

	for temp in $(LANG=en_US.UTF-9 seq -f "%g" 0.05 0.05 4.00)
	do
		cd ${temp}_temp
		#CALCULO EL PROMEDIO DE LAS ENERGIAS Y ENERGIAS^2 CON SUS RESPCTIVOS DESVIOS ESTANDAR
		awk -v temp=$temp 'NR>1 {
			sumE+=$1; 
			sumEE+=$2; 
			sumE2+=$1*$1; 
			sumEE2+=$2*$2;
			
		} END {
			term = NR-1;
			print temp, sumE/term, sqrt(sumE2/term-(sumE/term)^2), sumEE/term, sqrt(sumEE2/term-(sumEE/term)^2)
		
		}' datosPROM.dat >> ../Energia_$dir.dat
	
		#CALCULO EL PROMEDIO DE LA MAGNETIZACION Y MAG^2 CON SUS RESPECTIVOS DESV. ESTANDAR
		awk -v temp=$temp 'NR>1 {
			sumM+=$3; 
			sumMM+=$4; 
			sumM2+=$3*$3; 
			sumMM2+=$4*$4;
			
		} END {
			term = NR-1;
			print temp, sumM/term, sqrt(sumM2/term-(sumM/term)^2), sumMM/term, sqrt(sumMM2/term-(sumMM/term)^2)
		
		}' datosPROM.dat >> ../Mag_$dir.dat

		#CALCULO EL PROMEDIO DE LA TAZA DE ACEPTACION CON SU DESV. ESTANDAR 
		awk -v temp=$temp 'NR>1 {
			sumA+=$5; 
			sumA2+=$5*$5; 
			
		} END {
			term = NR-1;
			print temp, sumA/term, sqrt(sumA2/term-(sumA/term)^2)
		
		}' datosPROM.dat >> ../Aceptados_$dir.dat


		cd ../

	done
	cd ../

	mv ${dir}/Energia_$dir.dat Energia_$dir.dat
	mv ${dir}/Mag_$dir.dat Mag_$dir.dat
	mv ${dir}/Aceptados_$dir.dat Aceptados_$dir.dat
done
