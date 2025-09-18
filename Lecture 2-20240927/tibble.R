iris #data.frame
as_tibble(iris) #tibble

tb = tibble(x = 1:5,
       y = 2,
       z = x^2+1)
add_row(tb, x = 2) # add a new now
add_row(tb, x = 4, y = 0, z = 1, .before = 3) 
tb2 <- add_column(tb, w = -1:3, r = 0)

bind_rows(tb[1:2, ], tb[4:5, ]) #combine by row
bind_cols(tb2[, 1:2], tb2[, 4])  #combine by column
