config <- list(
    output_format = list(
        static = list(
            list(
                name = "Name",
                validation = function(input) {
                    return(TRUE)
                }
            ),
            list(
                name = "Date",
                validation = function(input) {
                    return(TRUE)
                }
            ),
            list(
              name = "Other",
              validation = function(input) {
                return(TRUE)
              } 
            )
        ),
        dynamic = function(output_col_name) {
            mock_dynamic_name <- c("dynamic1", "dynamic2")
            return(setdiff(mock_dynamic_name, output_col_name))
        }

    )
)
