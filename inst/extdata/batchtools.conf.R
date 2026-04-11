code <- system("sbatch --version")
if(code==0){
  slurm.tmpl <- system.file(
    package="mlr3resampling", "extdata", "slurm-afterok.tmpl",
    mustWork=TRUE)
  cluster.functions = makeClusterFunctionsSlurm(slurm.tmpl)
}
