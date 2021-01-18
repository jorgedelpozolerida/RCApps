#This file must disappear and be subtituted by a cron tab




# # Code to update csv list based on csv files in folder
# 
# path_to_csv<-file.path(
#   read_json("../../info/paths.json")$project_dir,
#   read_json("../../info/paths.json")$testCSV_dir # path to folder with csv
# )
# 
# testIDlist_path<-file.path(
#   read_json("../../info/paths.json")$project_dir,
#   read_json("../../info/paths.json")$testIDlist_file # path to folder with csv
# )
# 
# cartridges_names<-list.files(path=path_to_csv, pattern='\\.csv$') %>% # \ to escape \, \ to escape .
#   as.data.frame() %>% 
#   rename(filename ='.') %>% 
#   mutate(testID = str_sub(filename, 1, -5)) #take out .csv from name
# 
# write.table(cartridges_names, file=testIDlist_path, sep=';')
