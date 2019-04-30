# Code for creating package sticker

# Load libraries
library(hexSticker)
library(magick)

# Read in image
dress_raw <- image_read("./inst/sticker/re-dress-trans.png")

# Print the image
print(dress_raw)

# Sticker with green text
sticker1 <- sticker(

  # dress image
  subplot = dress_raw,
  s_x = 1,
  s_y = .9,
  s_width = 1.2,
  s_height = 1.3,

  # package name
  package = "redres",
  p_y = 1.63,
  p_size = 5,
  p_family = "sans",
  p_color = "black",

  # background format
  h_fill = "aliceblue",
  h_color = "black",
  h_size = 2,

  # url
  url = "https://goodekat.github.io/redres/",
  u_size = 1.1,
  u_family = "sans",
  u_color = "black",
  u_y = 0.075,

  # save sticker
  filename = "./inst/sticker/sticker1.png"

)

print(sticker1)

# Sticker with red text
sticker2 <- sticker(

  # dress image
  subplot = dress_raw,
  s_x = 1,
  s_y = .9,
  s_width = 1.2,
  s_height = 1.3,

  # package name
  package = "redres",
  p_y = 1.63,
  p_size = 5,
  p_family = "sans",
  p_color = "black",

  # background format
  h_fill = "bisque1",
  h_color = "black",
  h_size = 2,

  # url
  url = "https://goodekat.github.io/redres/",
  u_size = 1.1,
  u_family = "sans",
  u_color = "black",
  u_y = 0.075,

  # save sticker
  filename = "./inst/sticker/sticker2.png"

)

print(sticker2)

# Sticker with black text
sticker3 <- sticker(

  # dress image
  subplot = dress_raw,
  s_x = 1,
  s_y = .9,
  s_width = 1.2,
  s_height = 1.3,

  # package name
  package = "redres",
  p_y = 1.63,
  p_size = 5,
  p_family = "sans",
  p_color = "black",

  # background format
  h_fill = "lavender",
  h_color = "black",
  h_size = 2,

  # url
  url = "https://goodekat.github.io/redres/",
  u_size = 1.1,
  u_family = "sans",
  u_color = "black",
  u_y = 0.075,

  # save sticker
  filename = "./inst/sticker/sticker3.png"

)

print(sticker3)

# Sticker with black text
sticker4 <- sticker(

  # dress image
  subplot = dress_raw,
  s_x = 1,
  s_y = .9,
  s_width = 1.2,
  s_height = 1.3,

  # package name
  package = "redres",
  p_y = 1.63,
  p_size = 5,
  p_family = "sans",
  p_color = "black",

  # background format
  h_fill = "azure1",
  h_color = "black",
  h_size = 2,

  # url
  url = "https://goodekat.github.io/redres/",
  u_size = 1.1,
  u_family = "sans",
  u_color = "black",
  u_y = 0.075,

  # save sticker
  filename = "./inst/sticker/sticker4.png"

)

print(sticker4)

# Sticker with black text
sticker5 <- sticker(

  # dress image
  subplot = dress_raw,
  s_x = 1,
  s_y = .9,
  s_width = 1.2,
  s_height = 1.3,

  # package name
  package = "redres",
  p_y = 1.63,
  p_size = 5,
  p_family = "sans",
  p_color = "black",

  # background format
  h_fill = "mintcream",
  h_color = "black",
  h_size = 2,

  # url
  url = "https://goodekat.github.io/redres/",
  u_size = 1.1,
  u_family = "sans",
  u_color = "black",
  u_y = 0.075,

  # save sticker
  filename = "./inst/sticker/sticker5.png"

)

print(sticker5)
