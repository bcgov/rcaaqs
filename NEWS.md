# rcaaqs 0.3.0

* Made `format_date` function defunct, and replaced it with `format_caaqs_dt`, which puts the 
datetime into the correct timezone in POSIXct, and automatically assigns readings with the 
previous hour by subtracting one second from the timestamp.
* export `round_caaqs` function.
* Add more robust checking of columns in core functions (#23)

# rcaaqs 0.2.0

* Added a `NEWS.md` file to track changes to the package.
* Methods for PM2.5 now deal with exceptions to data completeness criteria (#13, #14)
* Methods for ozone (#2, #14)
* Preliminary methods for SO2 (#11, #14) and NO2 (#12, #14)
* Methods for dealing with exceptional events and transboundary flows (#5, #14)
* Added Aman Verma (@nograpes) as a package author for work on #2, #5, #11, #12, #13, #14 (via #17)
* Ensure rounding follows the CCME 'Guidance Document on Achievement Determination Canadian Ambient Air Quality Standards for Fine Particulate Matter and Ozone' standards (#21)
