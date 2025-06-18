save_ifnot_exists = function(object, path){

    if(!file.exists(path)){
        write.csv(object, path)
    }
}
