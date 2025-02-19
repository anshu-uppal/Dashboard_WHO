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