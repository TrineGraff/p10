data_raw = read.csv("../data/transformed_data.csv") %>% as.data.frame()
FRED_MD <- read.csv("../data/FRED_MD/current.csv", sep = ",", header = TRUE)
dato = as.Date(FRED_MD[-1, 1], format = "%m/%d/%Y")
FRED <- FRED_MD[-1, -1]
df_raw <- data.frame(dato, FRED)
FRED <- df_raw[-c(1:12, 704:708), -c(58, 60, 95, 123, 128)]

dato11 = FRED$dato
dato1 <- c(as.character(dato11))

df = data.frame(y = data_raw$UNRATE, date = as.Date(data_raw$dato))

stad = ggplot(df, aes(x = date ))  +
  geom_line(aes(y = df$y), col = "dimgray") +
  ylab("Rate") + xlab("") +
  theme(legend.title=element_blank())

df_nu = data.frame(y = FRED$UNRATE, date = as.Date(data_raw$dato))
tail(df_nu)

uden = ggplot(df_nu, aes(x = df_nu$date ))  +
  geom_line(aes(y = df_nu$y), col = "dimgray") +
  ylab("Procent") + xlab("") +
  theme(legend.title=element_blank())


grid.arrange(uden, stad)
p1 <- ggplotGrob(uden)
p2 = ggplotGrob(stad)

