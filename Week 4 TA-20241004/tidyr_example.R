library(tidyr) #tidyr 處理資料格式

# spread(data:要處理的資料框, key:要成為新變數名稱的那一欄, value:填入每個新變數欄的值)
# pivot_wider()

# 原始資料：長格式
data_long <- data.frame(
  year = c(2010, 2010, 2011, 2011),
  variable = c("A", "B", "A", "B"),
  value = c(3, 5, 8, 7)
)

# 使用 spread() 將資料轉換為寬格式
data_wide <- spread(data_long, key = variable, value = value)
#第一個位置寫要處理的資料，key成為新變數，value是要填的變數


# gather(data:要處理的資料框, key:新的變數名稱欄, value:新的變數值欄, ...:需要合併的欄位名稱(散落的資料), na.rm:如果為 TRUE，會移除NA值, convert:是否將新變數欄中的類別轉換為相應的類型)
# pivot_longer()

# 原始資料：寬格式
data_wide <- data.frame(
  year = c(2010, 2011),
  A = c(3, 8),
  B = c(5, 7)
)

# 使用 gather() 將資料轉換為長格式
data_long <- gather(data_wide, key = "variable", value = "value", A:B)

