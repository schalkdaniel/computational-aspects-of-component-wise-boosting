# Call valgrind:
# R -d "valgrind --tool=callgrind --log-file=binary-calltree.txt" -e "source('callgrind-binary.R')"
# Visualize:
# gprof2dot -f callgrind callgrind.out.x | dot -Tsvg -o output.svg

devtools::load_all( "~/repos/compboost")

nclasses = 10L
nsim = 500000L

classes = sample(x = LETTERS, size = nclasses)
gmean = sample(nclasses) * rnorm(1)

idx = sample(x = seq_len(nclasses), size = nsim, replace = TRUE)

x = classes[idx]
y = gmean[idx] + rnorm(nsim)

df_cat = data.frame(x = x, y = y)

learning_rate = 0.05
iter_max      = 1000L

response = ResponseRegr$new("mpg", as.matrix(df_cat$y))

cdata_source = CategoricalData$new(df_cat$x, "x")

bl_list = lapply(classes, function (cl) BaselearnerCategoricalBinary$new(cdata_source, cl))

factory_list = BlearnerFactoryList$new()
temp = lapply(bl_list, function (bl) factory_list$registerFactory(bl))

loss_quadratic = LossQuadratic$new()
optimizer = OptimizerCoordinateDescent$new()

log_iterations = LoggerIteration$new(" iterations", TRUE, iter_max)
logger_list = LoggerList$new()
logger_list$registerLogger(log_iterations)

cboost_binary = Compboost_internal$new(
  response      = response,
  learning_rate = learning_rate,
  stop_if_all_stopper_fulfilled = FALSE,
  factory_list = factory_list,
  loss         = loss_quadratic,
  logger_list  = logger_list,
  optimizer    = optimizer
)
cboost_binary$train(trace = as.integer(iter_max / 4))




mem_consumption = data.frame(
  run = c(1),
  before_init = c(2636),
  before_train = c(2641)
  after = c(2649)
)
