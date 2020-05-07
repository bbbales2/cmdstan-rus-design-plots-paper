Rscript generate_csr_runs.R
cd csr
cat run_fits.sh | parallel -j8
cd ../
Rscript process_csr.R
