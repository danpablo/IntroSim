#!/bin/bash
# Bash script for running njobs time the same physics and saving each run in a directory

# This is a bash routine. It saves a configuration
save_run()
{
# ----- Find first empty directory
          save_dir=1

          while [ -e ${save_dir}_run ]
          do
          ((save_dir++))
          done

          mkdir ${save_dir}_run

# Files to be copied
         for cp_file in  input.dat matrix_in.dat matrix_out.dat seed.dat
         do
             if [ -e $cp_file  ] ; then
                   cp ${cp_file} ${save_dir}_run
             else
             echo "Warning: ${cp_file} does not exist in directory"
         fi
         done
# Files to be moved
         for mv_file in
         do
             if [ -e $mv_file  ] ; then
                 mv ${mv_file} ${save_dir}_run
             else
                  echo "Warning: ${mv_file} does not exist in directory"
             fi
         done

      }

############################## MAIN ################################


# Go to the current working dir


# ------ Parameters ------

# Executables


exe=$HOME/SimComp/Ising/ising

echo "Executing program: $exe"

# MAXIMUM number of succesive jobs that will run with this script

njobs=60

# Get the current run number

run_exists=$(/bin/ls -l RUN* |wc -l)

if [ $run_exists -ne 1 ] ; then
  echo "File RUN_x with x a number is needed"
  echo "This script is going until $njobs runs"
  exit 1
fi


# Read the current job


curr_job=$(/bin/ls RUN_* |awk -F"_" '{print $2} ')

echo "Current job: $curr_job"

currm1=$((curr_job - 1))

if  [ $currm1 -eq $njobs ] ; then
echo -e "\n \t Last allowed job has already run. Change file RUN_$curr_job \n \t to a lower number to get new runs \n"
exit 1
fi

if  [ $curr_job -le $njobs ] ;  then


# ------------------ Execution -------------------------

   echo we run the job $curr_job out of $njobs

# Remove echo for using this command
   time      $exe

#  Save the run

         save_run

# Remove the old RUN file and update to the new value

  rm RUN_$curr_job

  ((curr_job++))

  touch RUN_$curr_job


# Only if RUN exists, rerun the simulation and some checks follows setup and re-run

       if  [ -e RUN_* ]
       then
# Prepare files for a new run

#        Do here, what is needed

	 cp matrix_out.dat matrix_in.dat
	 rm matrix_out.dat
         echo  "---> Resending the job"
         ./send_job.sh
       else
           echo "---> Finishing the run here"
       fi

   fi # if  job is lower than the maximum
