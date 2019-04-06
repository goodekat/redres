helper_resid <- function(type = NA, model){
  # redres residuals
  if (class(model)[1] == "redres"){
      ifelse(type == "condres", resid(model, type = "condres"),
              ifelse(type == "stdres"), resid(model, type = "stdres"),
              stdres(model, type = "genres"))
  }
  # lm residuals
  else if(class(model)[1] == "lm"){
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
    }
  }
}
