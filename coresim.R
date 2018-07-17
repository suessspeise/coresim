# R-script to make "simulated core photographs" form color scan data
#
# Usage: 
#   coresim(<L-values>, <a-values>, <b-values>)
#       # takes CIE*Lab Color vectors and generates a plot that resembles a core scan
#
#   csv2coresim(<name of csv-table with color scan data>)
#       you may have to adjust either the table or the arguments used by csv2coresim()
#
#   lab2rgb(<L>, <a>, <b>)
#       color conversion from Lab to RGB
#
# Last revised: 22.06.2018 (H.Campos gb196@stud.uni-heidelberg.de)


# conversion from Lab to RGB implementing formulas from: http://www.easyrgb.com/en/math.php
lab2rgb<-function(l_s,a_s,b_s) {
    # CIE-L*ab → XYZ Colorspace
    var_Y = ( l_s + 16. ) / 116.
    var_X = a_s / 500. + var_Y
    var_Z = var_Y - b_s / 200.
    if ( var_Y**3 > 0.008856 ) {var_Y = var_Y**3} else {var_Y = ( var_Y - 16. / 116. ) / 7.787}
    if ( var_X**3 > 0.008856 ) {var_X = var_X**3} else {var_X = ( var_X - 16. / 116. ) / 7.787}
    if ( var_Z**3 > 0.008856 ) {var_Z = var_Z**3} else {var_Z = ( var_Z - 16. / 116. ) / 7.787}
    
    # Standard: Observer= 2°, Illuminant= D65
    X = 95.047 * var_X  
    Y = 100.000 * var_Y
    Z = 108.883 * var_Z
 
    # XYZ → Standard-RGB
    # X, Y and Z input refer to a D65/2° standard illuminant.
    var_X = X / 100
    var_Y = Y / 100
    var_Z = Z / 100

    var_R = var_X *  3.2406 + var_Y * -1.5372 + var_Z * -0.4986
    var_G = var_X * -0.9689 + var_Y *  1.8758 + var_Z *  0.0415
    var_B = var_X *  0.0557 + var_Y * -0.2040 + var_Z *  1.0570

    if ( var_R > 0.0031308 ) {var_R = 1.055 * ( var_R ^ ( 1 / 2.4 ) ) - 0.055} else {var_R = 12.92 * var_R}
    if ( var_G > 0.0031308 ) {var_G = 1.055 * ( var_G ^ ( 1 / 2.4 ) ) - 0.055} else {var_G = 12.92 * var_G}
    if ( var_B > 0.0031308 ) {var_B = 1.055 * ( var_B ^ ( 1 / 2.4 ) ) - 0.055} else {var_B = 12.92 * var_B}

    # bundle and return
    return(rgb(var_R * 255,var_G * 255,var_B * 255, maxColorValue=255))
}


# takes CIE*Lab Color vectors and generates a plot that resembles a core scan
coresim <-function(dat_L, dat_a, dat_b) {  

    # plot
    barplot(numeric(length(dat_L)) + 1, # uniform bar heigth = 1
        #xlab=dat_nr,
        col=lab2rgb(dat_L,dat_a,dat_b), # color array; takes Lab values
        
        # style
        asp=50, # longish plot (height << width)
        yaxt="n", # no labels on y axis
        space=0, # remove spacing between bars
        border = NA # remove black outline of bars
        ) 
}


# Loads Color Scan CSV-table and generates a plot that resembles a core scan
csv2coresim <-function(filename) {  
    # reading csv
    data <- read.csv(file=filename, header=TRUE, sep=",")
    dat_nr = data$Sample.no.
    dat_L = data$L...10..D65.
    dat_a = data$a...10..D65.
    dat_b = data$b...10..D65.

    coresim(dat_L, dat_a, dat_b)
}