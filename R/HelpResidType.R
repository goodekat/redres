# Help function to compute the residual type




helper_resid <- function(type = NA, model){
  # lm residuals
 if(class(model)[1] == "lm"){
    if(is.na(type) | type == "response"){
      return(resid(model, type = "response"))
    }else if(type == "pearson"){
      return(resid(model, type = "response"))
    }else if(type == "standardized"){
      return(stdres(model))
    }
  }
  # glm residuals
  else if (class(model)[1] == "glm"){
    if(is.na(type) | type == "deviance"){
      return(resid(model, type = "deviance"))
    }else if (type == "response"){
      return(resid(model, type = "response"))
    }else if (type == "pearson"){
      return(resid(model, type = "pearson"))
    }
  }
  # lmer residuals
  else if (class(model)[1] == "lmerMod"){
    if(is.na(type) | type == "raw_cond"){
      return(redres(model, type = "raw_cond"))
    }else if (type == "raw_mar"){
      return(redres(model, type = "raw_mar"))
    }else if (type == "pearson_cond"){
      return(redres(model, type = "pearson_cond"))
    }else if (type == "pearson_mar"){
      return(redres(model, type = "pearson_mar"))
    }else if (type == "std_cond"){
      return(redres(model, type = "std_cond"))
    } else if(type=="std_mar"){
      return(redres(model, type = "std_mar"))
    }else if (type == "genres"){
      return(redres(model, type = "genres"))
    }
  }
}
