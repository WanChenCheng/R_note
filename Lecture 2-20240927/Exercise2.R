df = tibble(sub = c(1,2,3), treat = c("A","B","C"),
            t_1 = c(0.1,0.2,0.3), t_2 = c(4,5,6),
            t_3 = c(7,8,9), t_4 = c(1,1,1), t_5 = c(1.3,2.3,3.3)
)


df |> 
  pivot_longer(
    cols = t_1:t_5,
    names_to = "t",
    values_to = "v"
  ) -> df1

df1 |> 
  pivot_wider(
    names_from = t,
    values_from = v
  ) ->df2

df1 |> separate(
  t, 
  into=c("tt", "number"), 
  sep = "_", 
  convert=TRUE
) -> df3

df3 |> unite(
  t,
  tt,number,
  sep = "_",
) -> df4

df3 |> unite(
  time,
  tt,number,
  sep = "",
) -> df5
