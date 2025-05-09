# Creating data
set.seed(123)
df <- data.frame(
  age = rpois(100000, 65),
  gender = sample(
    c("Woman", "Woman", "Man"), 100000,
    replace = TRUE
  )
)

# Age pyramid
age_pyramid(df,
  age_var = "age", fill_var = "gender",
  man_level = "Man", title = "This is an age pyramid",
  obfuscate = FALSE
)

# Age pyramid with percent = FALSE
age_pyramid(df,
  age_var = "age", fill_var = "gender",
  man_level = "Man", percent = FALSE, x_breaks = 5000,
  title = "This is an age pyramid", obfuscate = FALSE
)

df <- data.frame(
  age = rpois(1083, 65),
  gender = sample(
    c("Woman", "Woman", "Man"), 1083,
    replace = TRUE
  )
)

age_pyramid(df,
  age_var = "age", fill_var = "gender",
  man_level = "Man", percent = FALSE, x_breaks = 5000,
  title = "This is an age pyramid", obfuscate = TRUE
)
