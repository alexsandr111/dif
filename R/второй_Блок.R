install.packages("car")
install.packages("ppcor")

# №2.1
# Загрузка пакета car
library(car)

# Построение диаграммы квантиль-квантиль
qqPlot(analis$`Avg-Avg-Р43`, main = "Диаграмма квантиль-квантиль")


# №2.2
# Выполнение теста Шапиро-Уилка
shapiro_test <- shapiro.test(analis$`Avg-Avg-Р43`)

# Вывод результатов теста
print("Тест Шапиро-Уилка:")
print(paste("p-значение:", shapiro_test$p.value))
print(paste("Статистика теста:", shapiro_test$statistic))


# №2.3
# Расчет линейного коэффициента корреляции и его значимости
cor_test <- cor.test(analis$`Avg-Avg-Р43`, analis$`Avg-Avg-Р42`)

# Вывод результатов
print("Корреляция:")
print(paste("Коэффициент корреляции:", cor_test$estimate))
print(paste("p-значение:", cor_test$p.value))


# №2.4
# Выбор нужных столбцов
selected_columns <- analis[, c("Avg-Avg-Р43", "Avg-Avg-Р44")]

# Исключение влияния остальных количественных признаков
partial_cor <- cor(selected_columns, method = "pearson")

# Рассчитанный частный коэффициент корреляции
partial_cor_coef <- partial_cor[1, 2]

# Вывод результата
print("Частный коэффициент корреляции:")
print(paste("Значение:", partial_cor_coef))


# №2.5
# Выбор нужных столбцов
selected_columns <- analis[, c("Avg-Avg-Р43", "Avg-Avg-Р44")]

# Расчет коэффициента корреляции Спирмена
spearman_corr <- cor(selected_columns, method = "spearman")

# Расчет коэффициента корреляции Кендалла
kendall_corr <- cor(selected_columns, method = "kendall")

# Проверка значимости коэффициента корреляции Спирмена
spearman_test <- cor.test(selected_columns$`Avg-Avg-Р43`, selected_columns$`Avg-Avg-Р44`, method = "spearman")

# Проверка значимости коэффициента корреляции Кендалла
kendall_test <- cor.test(selected_columns$`Avg-Avg-Р43`, selected_columns$`Avg-Avg-Р44`, method = "kendall")

# Вывод результатов
print("Ранговый коэффициент корреляции Спирмена:")
print(spearman_corr)
print(paste("Значимость:", spearman_test$p.value))
cat("\n")
print("Ранговый коэффициент корреляции Кендалла:")
print(kendall_corr)
print(paste("Значимость:", kendall_test$p.value))