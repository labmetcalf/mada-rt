#!/bin/bash
ssh -T mrajeev@della.princeton.edu <<HERE
    cd  mada-rt  # change to repo
    jid=\$(sbatch bash/rt_test.slurm | cut -c 21-)
    echo "Here's the job id: \$jid"
    jstat=\$(sacct -j "\$jid" -u mrajeev | head -n 3)
    echo "Here's the job stat: \$jstat"
    until grep -q "COMPLETED\|FAILED\|CANCELLED" <<< \$jstat  # if completed or failed
    do
        echo waiting   # updating
        jstat=\$(sacct -j "\$jid" -u mrajeev | head -n 3)
        echo "Here's the job stat: \$jstat"
        sleep 1m # time to sleep for (base it on how long the job should take)
    done
    if grep -q "FAILED\CANCELLED" <<< \$jstat
    then
        echo "Failed or cancelled"
        exit
    else
        logout
    fi
HERE
        sleep 1m    # sleep again as sometimes takes a while to write output
        rsync -rLvzt   
