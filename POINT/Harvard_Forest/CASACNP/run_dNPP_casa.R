deltas = c("NPPx0.5","NPPx1.0","NPPx1.5","NPPx2.0")
model = "casa"
for (i in 1:length(deltas))
{
    fileLst = paste("fcasacnp_clm_testbed_trans_", deltas[i], ".lst", sep="")
    fileLst
    file.copy(fileLst, "fcasacnp_clm_testbed.lst", overwrite=TRUE)
    stdOutFile = paste("stdout_", deltas[i], ".log", sep="")
    stdErrFile = paste("stderr_", deltas[i], ".log", sep="")
    args2 = ""
    system2("./casaclm_mimics_corpse", args=args2, wait=TRUE, stdout=stdOutFile, stderr=stdErrFile)
    ptFile1 = paste("POINT_", model, "_1.csv", sep="")
    ptFile2 = paste("POINT_", model, "_1_trans_", deltas[i], ".csv", sep="")
    file.rename(ptFile1, ptFile2)
}
