#!/bin/bash
# from https://github.com/litz-lab/scarab-infra/blob/main/docs/slurm_install_guide.md
set -o errexit
apt-get install libmunge-dev libmunge2 munge
systemctl enable munge
systemctl start munge
apt-get install slurmctld -y
apt-get install slurmd -y
apt-get install slurm-client
cp slurm.conf /etc/slurm/slurm.conf
for d in /var/spool/slurmd /var/lib/slurm-llnl /var/lib/slurm-llnl/slurmd /var/lib/slurm-llnl/slurmctld /var/run/slurm-llnl /var/log/slurm-llnl; do
    mkdir -p $d
    chmod 755 $d
    chown slurm $d
done
systemctl restart slurmctld
systemctl restart slurmd
