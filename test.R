source("Surrogate.R")

fail_handle = fail("fail_data")

s = Surrogate$new(fail_handle, oml_task_id = 12, baselearner_name = "knn", 
  measure_name = "auc", hp_names = "lambda")
print(s)
print(s$in_cache_rtask)
print(s$in_cache_model)
# s$file_rtask_to_disk()
# print(s$in_cache_rtask)
# print(s$in_cache_model)
s$acquire_rtask()
s$acquire_model()
print(s)

