config <- list(
    output_format = list(
        static = list(
            list(
                name = "Name",
                validation = function() {
                    return(TRUE)
                }
            ),
            list(
                name = "Date",
                validation = function() {
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