install.packages("vroom")
install.packages("readxl")
install.packages("Hmisc")
install.packages('dplyr')
install.packages("psych")

# №1.1
# Расчет среднего арифметического
mean_value <- mean(analis[['Avg-Avg-Р43']], na.rm=TRUE)
mean_value
# Расчет среднего гармонического
hmean_value <- exp(mean(log(analis[['Avg-Avg-Р43']][analis[['Avg-Avg-Р43']]>0]), na.rm=TRUE))
hmean_value
# Расчет среднего геометрического
gmean_value <- exp(mean(log(analis[['Avg-Avg-Р43']]), na.rm=TRUE))
gmean_value
# Расчет медианы
median_value <- median(analis[['Avg-Avg-Р43']], na.rm=TRUE)
median_value


# №1.2
# Расчет первого квартиля (25-й процентиль)
q1 <- quantile(analis[['Avg-Avg-Р43']], probs = 0.25, na.rm = TRUE)
q1
# Расчет третьего квартиля (75-й процентиль)
q3 <- quantile(analis[['Avg-Avg-Р43']], probs = 0.75, na.rm = TRUE)
q3


# №1.3
# Построение гистограммы
hist(analis[['Avg-Avg-Р43']], breaks = "FD", main = "Гистограмма частот", xlab = "Значения признака X")

# Добавление отметок для мер центральной тенденции и квартилей
abline(v = mean_value, col = "red", lwd = 2, lty = 2)    # Среднее арифметическое
abline(v = hmean_value, col = "blue", lwd = 2, lty = 2)   # Среднее гармоническое
abline(v = gmean_value, col = "green", lwd = 2, lty = 2)  # Среднее геометрическое
abline(v = median_value, col = "purple", lwd = 2, lty = 2) # Медиана
abline(v = q1, col = "orange", lwd = 2, lty = 2)          # Первый квартиль
abline(v = q3, col = "brown", lwd = 2, lty = 2)           # Третий квартиль


# №1.4
# Загрузка пакета dplyr
library(dplyr)

# Функция для расчета моды
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Группировка и расчет статистик
grouped_data <- analis %>%
  group_by('Avg-Avg-Р43') %>%
  summarise(mean = mean('Avg-Avg-Р43'),
            median = median('Avg-Avg-Р43'),
            mode = as.character(Mode('Avg-Avg-Р43')))

# Расчет статистик для несгруппированных данных
mean_all <- mean(analis$'Avg-Avg-Р43')
median_all <- median(analis$'Avg-Avg-Р43')
mode_all <- as.character(Mode(analis$'Avg-Avg-Р43'))

# Вывод результатов
print("Сгруппированные данные:")
print(grouped_data)
cat("\n")
print("Несгруппированные данные:")
print(paste("Среднее арифметическое:", mean_all))
print(paste("Медиана:", median_all))
print(paste("Мода:", mode_all))


# №1.5
# Загрузка пакета psych
library(psych)

# Рассчет мер изменчивости данных
data_column <- analis$`Avg-Avg-Р43`
variability_measures <- c(
  размах = diff(range(data_column)),
  вариация = var(data_column),
  среднее_квадратическое_отклонение = sd(data_column),
  дисперсия = var(data_column),
  коэффициент_вариации = sd(data_column) / mean(data_column),
  среднее_линейное_отклонение = mad(data_column),
  интерквартильный_размах = IQR(data_column),
  коэффициент_осцилляции = (max(data_column) - min(data_column)) / (max(data_column) + min(data_column)),
  относительное_линейное_отклонение = mad(data_column) / median(data_column)
)

# Вывод результатов
print(variability_measures)


# №1.6
# Установка пакета psych (если еще не установлен)
install.packages("psych")

# Загрузка пакета psych
library(psych)

# Построение коробки с усами
boxplot(analis$`Avg-Avg-Р43`, main = "Коробка с усами")

# Рассчет показателей асимметрии и эксцесса
skewness <- skew(analis$`Avg-Avg-Р43`)
kurtosis <- kurtosi(analis$`Avg-Avg-Р43`)

# Вывод результатов
print(paste("Показатель асимметрии:", skewness))
print(paste("Показатель эксцесса:", kurtosis))
