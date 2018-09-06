df = read.csv("iris_processed.csv")

eta = 0.1
x1 = df$Sepal.Length
x2 = df$Petal.Length
t = df$Species

weight = runif(2, min = 1e-3, max = 1e-2)
bias = runif(1)

weight1 = list()
weight2 = list()

for (generation in 1:1000){
  a = x1 * weight[1] + x2 * weight[2]
  y = 1 / (1 + exp(-a - bias))
  
  e = t - y
  g1 = -e * x1
  g2 = -e * x2
  gbias = -e
  
  weight[1] = weight[1] - eta * sum(g1)
  weight[2] = weight[2] - eta * sum(g2)
  bias[1] = bias[1] - eta * sum(gbias)
  
  weight1 = append(weight1, weight[1])
  weight2 = append(weight2, weight[2])
  
  print(bias)
}
