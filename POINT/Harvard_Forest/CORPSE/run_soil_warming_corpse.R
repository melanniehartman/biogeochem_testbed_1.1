deltas = c("+0C","+2C","+4C","+6C")
for (i in 1:length(deltas))
{
    fileLst = paste("fcasacnp_clm_testbed_trans", deltas[i], ".lst", sep="")
    fileLst
    file.copy(fileLst, "fcasacnp_clm_testbed.lst", overwrite=TRUE)
    stdOutFile = paste("stdout_", deltas[i], ".log", sep="")
    stdErrFile = paste("stderr_", deltas[i], ".log", sep="")
    args2 = ""
    system2("./casaclm_mimics_corpse", args=args2, wait=TRUE, stdout=stdOutFile, stderr=stdErrFile)
    ptFile = paste("POINT_corpse_1_trans", deltas[i], ".csv", sep="")
    file.rename("POINT_corpse_1.csv", ptFile)
}
