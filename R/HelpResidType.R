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
    if(is.na(type) | type == "pearson"){
      return(resid(model, type = "pearson"))
    }else if (type == "response"){
      return(resid(model, type = "response"))
    }else if (type == "condres"){
      return(redres(model, type = "condres"))
    }else if (type == "stdres"){
      return(redres(model, type = "stdres"))
    }else if (type == "genres"){
      return(redres(model, type = "genres"))
    }
  }
}
