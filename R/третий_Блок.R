install.packages("scales")
install.packages("ggplot2")
install.packages("reshape2")
install.packages("stats")
install.packages("ggplot2")


# №3.1
# Проверка типов данных и преобразование в числовой формат, если необходимо
for (col in colnames(analis)) {
  if (!is.numeric(analis[[col]])) {
    analis[[col]] <- as.numeric(as.character(analis[[col]]))
  }
}

# Нормирование показателей по формуле (x - min) / (max - min) * 100
normalized_table <- analis
for (col in colnames(normalized_table)) {
  normalized_table[[col]] <- (normalized_table[[col]] - min(normalized_table[[col]])) / (max(normalized_table[[col]]) - min(normalized_table[[col]])) * 100
}


# №3.2
library(ggplot2)

# Создание корреляционной матрицы
cor_matrix <- cor(normalized_table)

# Преобразование матрицы в длинный формат
cor_data <- reshape2::melt(cor_matrix)

# Построение корреляционной матрицы с использованием цветовой гаммы
cor_plot <- ggplot(data = cor_data, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme_minimal() +
  labs(title = "Корреляционная матрица")

# Вывод корреляционной матрицы
print(cor_plot)


# №3.3
# Создание корреляционной матрицы
cor_matrix <- cor(analis)

# Преобразование матрицы в длинный формат
cor_data <- reshape2::melt(cor_matrix)

# Построение корреляционной матрицы с использованием цветовой гаммы
cor_plot1 <- ggplot(data = cor_data, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme_minimal() +
  labs(title = "Корреляционная матрица")

# Вывод корреляционной матрицы
print(cor_plot1)


# №3.4
# Создание корреляционной матрицы для нормализованных показателей
cor_matrix_normalized <- cor(normalized_table)

# Создание корреляционной матрицы для ненормализованных показателей
cor_matrix_unnormalized <- cor(analis)

# Разница между корреляционными матрицами
cor_diff <- cor_matrix_normalized - cor_matrix_unnormalized

# Построение корреляционной матрицы с использованием цветовой гаммы
cor_diff_plot <- ggplot(data = reshape2::melt(cor_diff), aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme_minimal() +
  labs(title = "Разница между корреляционными матрицами")

# Вывод корреляционной матрицы с разницей
print(cor_diff_plot)


# №3.5
# Выделение количественных показателей
quantitative_columns <- analis[, sapply(analis, is.numeric)]

# Проверка на константные столбцы
non_constant_columns <- quantitative_columns[, sapply(quantitative_columns, function(x) length(unique(x)) > 1)]

# Нормализация данных
normalized_data <- scale(non_constant_columns)

# Применение PCA
pca <- prcomp(normalized_data)

# Кластеризация
k <- 3  # Задайте количество кластеров
cluster_labels <- kmeans(pca$x, centers = k)$cluster

# Визуализация результатов
# Пример визуализации двух первых главных компонент
plot(pca$x[, 1], pca$x[, 2], col = cluster_labels, pch = 16)



# №3.6
# Выделение количественных показателей
quantitative_columns <- analis[, sapply(analis, is.numeric)]

# Проверка на низкую изменчивость и константные значения
filtered_columns <- quantitative_columns[, apply(quantitative_columns, 2, function(x) length(unique(x)) > 1)]

# Проверка наличия столбцов после фильтрации
if (ncol(filtered_columns) == 0) {
  stop("All columns have zero variance or contain constant values. Cannot perform PCA.")
}

# Нормализация данных
normalized_data <- scale(filtered_columns)

# Вычисление матрицы ковариации
covariance_matrix <- cov(normalized_data)

# Вычисление собственных значений и собственных векторов
eigen_result <- eigen(covariance_matrix)

# Сортировка собственных значений в убывающем порядке
sorted_eigenvalues <- eigen_result$values[order(eigen_result$values, decreasing = TRUE)]
sorted_eigenvectors <- eigen_result$vectors[, order(eigen_result$values, decreasing = TRUE)]

# Выбор компонент
k <- 2  # Задайте количество компонент
selected_components <- sorted_eigenvectors[, 1:k]

# Проецирование данных на выбранные компоненты
projected_data <- normalized_data %*% selected_components

# Визуализация результатов
plot(projected_data[, 1], projected_data[, 2])


# №3.7
# Выделение количественных показателей из базы данных "analis"
quantitative_columns <- analis[, sapply(analis, is.numeric)]

# Удаление столбцов с пропущенными значениями (NA)
quantitative_columns <- quantitative_columns[, colSums(is.na(quantitative_columns)) == 0]

# Разбиение количественных показателей на две группы
group1 <- quantitative_columns[, 1:5]  # Первая группа (выберите подходящий диапазон столбцов)
group2 <- quantitative_columns[, 6:10]  # Вторая группа (выберите подходящий диапазон столбцов)

# Канонический корреляционный анализ
result <- cancor(group1, group2)

# Получение канонических переменных
canonical_vars <- result$cor

# Определение максимальной корреляции между каноническими переменными
max_correlation <- max(canonical_vars)

# Вывод результатов
print(result)
print(paste("Максимальная корреляция:", max_correlation))


# №3.8
# Загрузка необходимых пакетов
library(stats)

# Выделение независимых переменных и зависимой переменной
independent_vars <- analis[, -c(1, 15)]  # Выберите нужные столбцы с независимыми переменными
dependent_var <- analis$'Avg-Avg-Р7'  # Зависимая переменная

# Разделение данных на обучающую и тестовую выборки
set.seed(123)  # Зафиксируем случайное разбиение для воспроизводимости
train_index <- sample(1:nrow(analis), nrow(analis) * 0.8)  # 80% данных для обучения
train_data <- independent_vars[train_index, ]
train_labels <- dependent_var[train_index]
test_data <- independent_vars[-train_index, ]

# Построение модели регрессии
model <- lm(train_labels ~ ., data = train_data)

# Проведение прогнозирования на тестовой выборке
predictions <- predict(model, newdata = test_data)

# Вывод результатов прогнозирования
print(predictions)

























