#' A custom formatter using flextable
#'
#' @param data Table to make into flextable object
#' @returns A custom flextable output
#' @export
#'
#' @examples
#' myflextable(mtcars)
myflextable = function(data) {
        flextable::set_flextable_defaults(na_str = "NA", font.size = 12, padding.bottom = 1, padding.top = 1)
        x = flextable::flextable(data)
        x = flextable::colformat_int(x, big.mark = "")
        x = flextable::colformat_double(x, big.mark = "", digits = 2, na_str = "NA")
        x = flextable::theme_booktabs(x)
        return(x)
}

#' Generate individual country estimates over time for the indicator, setting, and dimension of choice.
#'
#' @param dataset Choose the dataset
#' @param indicator_name_choice # Choose the indicator
#' @param dimension_choice # Choose the dimension
#'
#' @return A ggplot lineplot
#' @export
#'
#' @examples make_lineplot(rep_imm)
make_lineplot <- function(
                dataset, 
                # whoreg6_choice = "European",
                # setting_choice = NULL, 
                indicator_name_choice = NULL, 
                dimension_choice = NULL
){
        # if(whoreg6_choice != "All"){
        #         dataset <- dataset |> 
        #                 filter(whoreg6 == whoreg6_choice)
        # } else{
        #         dataset <- dataset #|> 
        #         # filter(setting %in% c(setting_choice)) |> 
        #         # mutate(setting = fct_relevel(setting, setting_choice))
        # }
        
        # if(is.null(setting_choice)){setting_choice = unique(dataset$setting)[1]}
        if(is.null(indicator_name_choice)){indicator_name_choice = unique(dataset$indicator_name)[1]}
        if(is.null(dimension_choice)){dimension_choice = unique(dataset$dimension)[1]}
        
        
        # prepare the data
        p <- dataset |> 
                # filter(setting %in% c(setting_choice, setting_compare)) |> 
                # mutate(setting = fct_relevel(setting, setting_choice)) |> 
                filter(indicator_name == indicator_name_choice) |> 
                filter(dimension == dimension_choice) |>
                droplevels() |> 
                arrange(subgroup_order) |> 
                mutate(subgroup = fct_inorder(subgroup)) |> 
                
                # Generate the plot
                ggplot(aes(x = date, y = estimate, color = subgroup))+
                geom_point(
                        # aes(size = population)
                )+
                # geom_ribbon(aes(ymin = ci_lb, ymax = ci_ub, fill = subgroup), alpha = 0.2, color = NA)+
                # geom_errorbar(aes(ymin = ci_lb, ymax = ci_ub), width = 100)+
                geom_path()+
                ggtitle(label = indicator_name_choice)+
                labs(x = "Year", y = "%", color = dimension_choice
                     # , size = "Population"
                )+
                facet_wrap(.~setting, labeller = label_wrap_gen())+
                theme_classic()
        p
}

#' Generate a table based on the filtered dataset
#'
#' @param dataset 
#' @param caption_indicator 
#' @param caption_dimension 
#'
#' @return
#' @export
#'
#' @examples
make_table <- function(
                dataset, 
                caption_indicator = "indicator",
                caption_dimension = "dimension"
){
        a<-dataset |> 
                # mutate(indicator_name = str_remove(indicator_name, " \\(\\%\\)")) |> 
                arrange(setting, year, indicator_name, subgroup, subgroup_order) |> 
                select(setting, year, subgroup, full_estimate, population)
        
        # Automatically get linenumbers to visually break up the table
        b <- a |> 
                group_by(setting, year) |>
                mutate(
                        setting = ifelse(!duplicated(setting), setting, ""),
                        year = ifelse(!duplicated(year), year, "")
                ) |>
                ungroup() |> 
                mutate(lineref = case_when(setting=="" ~ FALSE, .default = TRUE)) |> 
                select(lineref) |> 
                rowid_to_column() |> 
                filter(lineref)
        groupnames <- setdiff(b$rowid-1, 0) # remove 0, first line not necessary
        
        table_output <- myflextable(a) |> 
                set_header_labels(values = c("Setting", "Year", "Subgroup", 
                                             "Estimate [95% CI]*", "Population")) |> 
                hline(i = groupnames, border = fp_border_default(color = "black")) |>
                autofit() |> 
                add_footer_lines(c(
                        paste0("Dimension: ", caption_dimension),
                        paste0("Source: ", "Health Inequality Data Repository (WHO)")
                )) |>
                set_caption(caption = paste0(caption_indicator),
                            align_with_table = FALSE)
        
        return(table_output)
}