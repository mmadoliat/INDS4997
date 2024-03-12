library(keras);
library(abind)
c(c(x_train, y_train), c(x_test, y_test)) %<-% dataset_mnist()

size <- 60000
i <- sample(1:60000,size,replace=T)
j <- sample(1:60000,size,replace=T)
Y_train <- as.numeric(paste0(y_train[i],y_train[j]))
X_train <- abind(x_train[i,,], x_train[j,,], along=3)
size <- 10000
ii <- sample(1:10000,size,replace=T)
jj <- sample(1:10000,size,replace=T)
Y_test <- as.numeric(paste0(y_test[ii],y_test[jj]))
X_test <- abind(x_test[ii,,], x_test[jj,,], along=3)

digit <- 1
ind <- Y_train%/%10!=digit
X_train <- X_train[ind,,]
Y_train <- Y_train[ind]
ind <- Y_test%/%10==digit

################################################################################

x_train <- array_reshape(X_train, c(nrow(X_train), 1568))
x_test <- array_reshape(X_test, c(nrow(X_test), 1568))
x_train <- x_train / 255
x_test <- x_test / 255

# Convert class vectors to binary class matrices
y_train <- to_categorical(Y_train, 100)
y_test <- to_categorical(Y_test, 100)

### Step 2 : Model definition ###
model <- keras_model_sequential()
model %>%
  layer_dense(units = 256, activation = 'relu', input_shape = c(1568)) %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 100, activation = 'softmax')

### Step 3 : Compile Model ###
model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)

### Step 4 : Model Training ###
history <- model %>% fit(
  x_train, y_train,
  batch_size = 128,
  epoch = 10,
  validation_split = 0.2
)

################################################################################

batch_size <- 128
num_classes <- 100
epochs <- 12

# Input image dimensions
img_rows <- 28
img_cols <- 28*2

# Redefine  dimension of train/test inputs
x_train <- array_reshape(X_train, c(nrow(X_train), img_rows, img_cols, 1))
x_test <- array_reshape(X_test, c(nrow(X_test), img_rows, img_cols, 1))
input_shape <- c(img_rows, img_cols, 1)

# Transform RGB values into [0,1] range
x_train <- x_train / 255
x_test <- x_test / 255

cat('x_train_shape:', dim(x_train), '\n')
cat(nrow(x_train), 'train samples\n')
cat(nrow(x_test), 'test samples\n')

# Convert class vectors to binary class matrices
y_train <- to_categorical(Y_train, num_classes)
y_test <- to_categorical(Y_test, num_classes)

# Define Model -----------------------------------------------------------
#
# # Define model
# model0 <- keras_model_sequential() %>%
#   layer_conv_2d(filters = 32, kernel_size = c(3,3), activation = 'relu',
#                 input_shape = input_shape) %>%
#   layer_conv_2d(filters = 64, kernel_size = c(3,3), activation = 'relu') %>%
#   layer_max_pooling_2d(pool_size = c(2, 2)) %>%
#   layer_dropout(rate = 0.25) %>%
#   layer_flatten() %>%
#   layer_dense(units = 128, activation = 'relu') %>%
#   layer_dropout(rate = 0.5) %>%
#   layer_dense(units = num_classes, activation = 'softmax')
#
# # Compile model
# model0 %>% compile(
#   loss = loss_categorical_crossentropy,
#   optimizer = optimizer_adadelta(),
#   metrics = c('accuracy')
# )
#
# # Train model
# model0 %>% fit(
#   x_train, y_train,
#   batch_size = batch_size,
#   epochs = epochs,
#   validation_split = 0.2
# )
#
# scores <- model0 %>% evaluate(
#   x_test, y_test, verbose = 0
# )
#
# # Output metrics
# cat('Test loss:', scores[[1]], '\n')
# cat('Test accuracy:', scores[[2]], '\n')

##########################################################################

model1 <- keras_model_sequential(input_shape = input_shape)
model1 |>
  layer_conv_2d(filters = 32, kernel_size = c(3, 3), activation = "relu") |>
  layer_max_pooling_2d(pool_size = c(2, 2)) |>
  layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu") |>
  layer_max_pooling_2d(pool_size = c(2, 2)) |>
  layer_flatten() |>
  layer_dropout(rate = 0.5) |>
  layer_dense(units = num_classes, activation = "softmax")

summary(model1)

batch_size <- 128
epochs <- 15

model1 |> compile(
  loss = "categorical_crossentropy",
  optimizer = "adam",
  metrics = "accuracy"
)

model1 |> fit(
  x_train, y_train,
  batch_size = batch_size,
  epochs = epochs,
  validation_split = 0.1
)

score <- model1 |> evaluate(x_test, y_test, verbose=0)
score
img_test <- array_reshape(x_test[ind,,,], c(sum(ind), img_rows, img_cols, 1))
model1 |> evaluate(img_test, y_test[ind,], verbose=0)
pred_test <- apply(predict(model1,img_test),1,which.max)-1

K<-20
par(ask=T)
for (k in sample(1:length(pred_test),K)) {
  image(t(img_test[k,28:1,,1]), main = paste("Model prediction is",pred_test[k]), axes=FALSE, col = grey(seq(0, 1, length = 256)))
}
pie(table(pred_test%/%10))

for (k in which(pred_test%%10!=Y_test[ind]%%10)) {
  image(t(img_test[k,28:1,,1]), main = paste("Model predicted",Y_test[ind][k],"as",pred_test[k]), axes=FALSE, col = grey(seq(0, 1, length = 256)))
}
