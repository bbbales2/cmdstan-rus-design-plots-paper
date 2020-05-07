Rscript generate_car_runs.R
cd car
cat run_fits.sh | parallel -j8
cd ../
Rscript process_car.R
