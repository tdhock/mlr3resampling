AutoTunerTorch_epochs = R6::R6Class(
  "AutoTunerTorch_epochs",
  inherit = mlr3tuning::AutoTuner,
  public = list(
    module_learner=NULL,
    initialize=function(id, module_generator, max_epochs, batch_size, measure_list, validate=0.5){
      if(!is.list(measure_list)){
        measure_list <- list(measure_list)
      }
      M <- measure_list[[1]]
      task_type <- sub("[.].*", "", M$id)
      self$module_learner = mlr3::lrn( 
        paste0(task_type, ".module"),
        module_generator = module_generator,
        ingress_tokens = list(x = mlr3torch::ingress_num()),
        epochs = paradox::to_tune(upper=max_epochs, internal=TRUE),
        patience = max_epochs,
        batch_size = batch_size,
        measures_valid=measure_list,
        measures_train=measure_list,
        callbacks = mlr3torch::t_clbk("history"))
      if(task_type=="classif")self$module_learner$predict_type <- "prob"
      mlr3::set_validate(self$module_learner, validate = validate)
      terminator <- mlr3tuning::mlr_terminators$get("evals")
      terminator$param_set$set_values(n_evals=1)
      super$initialize(
        learner = self$module_learner,
        tuner = mlr3tuning::tnr("internal"),
        resampling = mlr3::rsmp("insample"),
        measure = mlr3::msr("internal_valid_score", minimize = TRUE),
        terminator=terminator,
        id=id,
        store_models = TRUE)
    },
    save_learner=function(){
      list(history=self$archive$learners(1)[[1]]$model$callbacks$history)
    },
    edit_learner=function(){
      self$learner$param_set$set_values(
        patience=2,
        epochs=paradox::to_tune(upper=2, internal=TRUE)
      )
    }
  )
)

save_learner_glmnet <- function(x){
  weight <- as.matrix(coef(x$model))[-1,]
  list(weights=data.table(feature=names(weight), weight))
}

LearnerRegrCVGlmnetSave = R6::R6Class(
  "LearnerRegrCVGlmnetSave",
  inherit = mlr3learners::LearnerRegrCVGlmnet,
  public = list(
    save_learner = function()save_learner_glmnet(self)
  )
)

LearnerClassifCVGlmnetSave = R6::R6Class(
  "LearnerClassifCVGlmnetSave",
  inherit = mlr3learners::LearnerClassifCVGlmnet,
  public = list(
    initialize = function(...){
      super$initialize(...)
      self$predict_type <- "prob"
    },
    save_learner = function()save_learner_glmnet(self)
  )
)
