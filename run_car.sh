Rscript generate_car_runs.R
cd car
cat run_fits.sh | parallel -j16
cd ../
Rscript process_car.R
