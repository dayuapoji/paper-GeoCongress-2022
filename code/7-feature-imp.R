# ==============================================================================
# FEATURE IMPORTANCE
# ==============================================================================
set.seed(1)

# ------------------------------------------------------------------------------
# DATA SPLITTING
# ------------------------------------------------------------------------------

# create df labeled only
rf_df_labeled <- rf_df %>%
  .[(.$SoilLabel != 'Unlabeled'), ]

# training data set
train_index <- sample(nrow(rf_df_labeled), 0.8 * nrow(rf_df_labeled), 
                      replace = FALSE)
rf_train_chainage <- rf_df_labeled[train_index, ]
rf_train <- prep_df(rf_train_chainage)

# up-sampling for imbalance data
rf_train_up <- upSample(x = rf_train[1:(ncol(rf_train)-1)],
                        y = factor(rf_train$SoilLabel),
                        yname = "SoilLabel") 

# ------------------------------------------------------------------------------
# TRAINING CONTROL
# ------------------------------------------------------------------------------

# training and cross validation with upsampling
ctrl <- trainControl(method = "repeatedcv",
                     number = 10,
                     repeats = 5,
                     verboseIter = FALSE,
                     search = 'grid')

p <- ncol(rf_train)-1

tunegrid <- expand.grid(mtry = round(sqrt(p)),
                        splitrule = "extratrees",
                        min.node.size = 10)

# ------------------------------------------------------------------------------
# RF IMPURITY
# ------------------------------------------------------------------------------

rf_model_impurity <- train(SoilLabel~., data = rf_train_up,
                  method = 'ranger',
                  trControl = ctrl,
                  tuneGrid = tunegrid,
                  num.trees = 500,
                  importance = 'impurity',
                  num.threads = 7,
                  verbose = FALSE)

rf_impurity_df <- varImp(rf_model_impurity)$importance
colnames(rf_impurity_df) <- "Impurity"
rf_impurity_df$Features <- rownames(rf_impurity_df)
rownames(rf_impurity_df) <- NULL
rf_impurity_df$Index <- as.numeric(rownames(rf_impurity_df))

# ------------------------------------------------------------------------------
# RF PERMUTATION
# ------------------------------------------------------------------------------

rf_model_permutation <- train(SoilLabel~., data = rf_train_up,
                           method = 'ranger',
                           trControl = ctrl,
                           tuneGrid = tunegrid,
                           num.trees = 500,
                           importance = 'permutation',
                           num.threads = 7,
                           verbose = FALSE)

rf_permutation_df <- varImp(rf_model_permutation)$importance
colnames(rf_permutation_df) <- "Permutation"
rf_permutation_df$Features <- rownames(rf_permutation_df)
rownames(rf_permutation_df) <- NULL
rf_permutation_df$Index <- as.numeric(rownames(rf_permutation_df))

# ------------------------------------------------------------------------------
# CONDITIONAL PERMUTATION
# ------------------------------------------------------------------------------

# cf analysis
cf_model <- cforest(SoilLabel~., data = rf_train_up,
                    control = cforest_unbiased(ntree = 500, 
                                               mtry = round(sqrt(p))))
cf_varimp <- varimp(cf_model, conditional = TRUE)

# create df
cf_varimp_df <- data.frame(cf_varimp)
cf_varimp_df$ConditionalForests <- 100 * cf_varimp_df$cf_varimp / max(cf_varimp_df$cf_varimp)
cf_varimp_df$Features <- rownames(cf_varimp_df)
rownames(cf_varimp_df) <- NULL
cf_varimp_df$Index <- as.numeric(rownames(cf_varimp_df))

# grouping
cf_varimp_df$Groups <- ifelse(
  grepl("Cutter", cf_varimp_df$Features, ignore.case = TRUE), "Cutter",
  ifelse(
    grepl("Thrust|Advance", cf_varimp_df$Features, ignore.case = TRUE), "Thrust",
    ifelse(
      grepl("EP|Density", cf_varimp_df$Features, ignore.case = TRUE), "Chamber",
      ifelse(
        grepl("Screw", cf_varimp_df$Features, ignore.case = TRUE), "Screw",
        ifelse(
          grepl("Foam|Air", cf_varimp_df$Features, ignore.case = TRUE), "Foam",
          ifelse(
            grepl("Polymer", cf_varimp_df$Features, ignore.case = TRUE), "Polymer",
            ifelse(
              grepl("Belt", cf_varimp_df$Features, ignore.case = TRUE), "BeltMuck",
              ifelse(
                grepl("Pitch|Roll|Yaw", cf_varimp_df$Features, ignore.case = TRUE), "Attitude",
                ifelse(
                  grepl("Grout", cf_varimp_df$Features, ignore.case = TRUE), "TailGrout", 
                  "Other")))))))))

# ------------------------------------------------------------------------------
# COMBINE RESULTS
# ------------------------------------------------------------------------------

varimp_df <- merge(rf_impurity_df, rf_permutation_df, 
                   by = c("Features", "Index"))
varimp_df <- merge(varimp_df, cf_varimp_df, 
                   by = c("Features", "Index"))

# standardize to max = 1.0
varimp_df['Impurity'] <- varimp_df['Impurity'] / 100
varimp_df['Permutation'] <- varimp_df['Permutation'] / 100
varimp_df['ConditionalForests'] <- varimp_df['ConditionalForests'] / 100

write.csv(varimp_df,'../results/varimp.csv', row.names = FALSE)
