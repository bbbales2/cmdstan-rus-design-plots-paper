Rscript generate_xyr_runs.R
cd xyr
cat run_fits.sh | parallel -j8
cd ../
Rscript process_xyr.R
