library(stringi)
longio <- long
levels(longio$in_y) <- stri_rand_strings(6, 20, pattern = "[A-Za-z0-9]")
levels(longio$out_x) <- stri_rand_strings(7, 20, pattern = "[A-Za-z0-9]")
dput(longio, file = 'long.txt')
