# helpers.R
# helper functions for cs_crispr app.R
require(shiny)
require(ggplot2)
require(ggrepel)
require(stringr)
require(DT)

get_axes <- function(var1=NULL, var2=NULL, var3=NULL, plot_type){
  # convenience function to extract axis label strings for plots
  if (plot_type == 1){ # for plotting one expt rank-order vs beta score
    x_ax = paste(var3, 'mean', 'rank', sep='_') # x axis is rank-order
    y_ax = paste(var3, 'mean', 'beta', sep='_') # y axis is beta score
  }else{
    x_ax = paste(var1, 'mean', 'beta', sep='_') # x axis is rank-order
    y_ax = paste(var2, 'mean', 'beta', sep='_') # y axis is beta score
  }
  return(list(x_ax=x_ax, y_ax=y_ax))
}

get_x_scale <- function(expt_1, expt_2=NULL){
  # depending upon whether there are one or two experiment inputs,
  # determines if x axis is reversed or not.
  if (is.null(expt_2)){
    return(scale_x_reverse())
  }else{
    return(scale_x_continuous())
  }
}

plot_expts <- function(df, expt_1, expt_2=NULL, gene_vec=NULL){
  # generates ggplot2 scatterplots for either 1-expt or 2-expt plots,
  # with or without added gene_lists plotted in another color.
  # THIS ONLY WORKS WITH crispr_df (combined_spread.rds) PASSED AS DF.
  if (is.null(expt_2)){ # for plotting one expt rank-order vs beta score
    x_ax = paste(expt_1, 'mean', 'rank', sep='_') # x axis is rank-order
    y_ax = paste(expt_1, 'mean', 'beta', sep='_') # y axis is beta score
    ax_labels = labs(x=paste(expt_1, 'reporter beta score rank'),
                     y=paste(expt_1, 'reporter beta score'))
  }else{
    x_ax = paste(expt_1, 'mean', 'beta', sep='_') # x axis is rank-order
    y_ax = paste(expt_2, 'mean', 'beta', sep='_') # y axis is beta score
    ax_labels = labs(x=paste(expt_1, 'reporter beta score'),
                     y=paste(expt_2, 'reporter beta score'))
  }
  return(
    ggplot(df, aes_string(x=x_ax, y=y_ax)) +
      geom_point(color = 'grey60') +
      get_x_scale(expt_1, expt_2) +
      geom_point(
        data=subset(df, Gene %in% gene_vec), color='red' # for labeled subset
      ) + 
      geom_text_repel(
        data=subset(df, Gene %in% gene_vec), # for labeled subset
        color='red',
        fontface='bold', aes(label=Gene)
      ) +
      theme(
        panel.background = element_rect(fill=NA, color='black'),
        panel.grid = element_blank(),
        axis.title = element_text(size = 16, face='bold'),
        axis.text = element_text(color = 'black', size=10)
      ) +
      ax_labels
  )
}

split_genes <- function(expt_genes, entered_string){
  # Args:
  # expt_genes: a vector of genes (from crispr_df$Gene)
  # entered_string: a string (provided by the user in input$highlighted_gene)
  # Returns:
  # a two-element named list.
  # $present: a vector of genes present in crispr_df$Gene
  # $absent: a vector of genes absent from crispr_df$Gene
  
  # first, capitalize and remove blanks from string
  gene_list_str = toupper(gsub( " ", "", entered_string, fixed=TRUE))
  gene_vec = unlist(str_split(gene_list_str, '\\,')) # split by commas
  present_genes = intersect(gene_vec, expt_genes)
  absent_genes = setdiff(gene_vec, expt_genes)
  return(list(present=present_genes, absent=absent_genes))
}

plot_ax_select <- function(var_name, selector_label, initial='LC3'){
  # convenience function to clean up code for selectInputs which define
  # the possible choices for the experiment to plot. default selected value
  # is LC3, but this can be changed by changing the selected argument.
  selectInput(var_name,
              label = selector_label,
              choices = list ("LC3",
                              "SQSTM1",
                              "NBR1",
                              "TAX1BP1",
                              "NDP52"),
              selected = initial)
}

plot_get_cols <- function(crispr_df, var1=NULL, var2=NULL, var3=NULL, plot_type,
                         sample_rb, expts_rb, format_rb){
  # get the integer column indices for crispr_df which fulfill the
  # filtering requirements established by the three different radio buttons
  
  if (expts_rb == 1){
    if (plot_type == 1){ # if only visualizing one expt, using input$var3
      # first, escape the '.' charatcters in var3 so they survive the regex
      v3 <- gsub('.', '\\.', var3, fixed=TRUE)
      re_experiment <- paste0("^", v3, '_.*')
    }else{ # if visualizing two expts using input$var1 and input$var2
      # first, escape the '.' characters in var1 and var2
      v1 <- gsub('.', '\\.', var1, fixed=TRUE)
      v2 <- gsub('.', '\\.', var2, fixed=TRUE)
      # first, generate a vector with the two possible options contained
      re_experiment <- c(paste0("^", v1, '_.*'),
                         paste0("^", v2, '_.*'))
      # then stitch them together with an 'or' operator
      re_experiment <- paste(re_experiment, collapse='|')
    }
  }else{
    re_experiment <- '^[A-Za-z0-9\\.]*_.*' # match any experiment
  }
  # next, generate re to identify desired samples
  if (sample_rb == 1){
    re_sample <- '.*_mean_.*' # if only showing mean
  }else if (sample_rb == 2){
    re_sample <- '.*_[1-9]_.*' # if only showing replicates
  }else if (sample_rb == 3){
    re_sample <- '.*_mean_.*|.*_[1-9]_.*'
  }
  # next, generate re to identify sample format
  if (format_rb == 1){
    re_format <- '.*_beta$'
  }else if (format_rb == 2){
    re_format <- '.*_rank$'
  }else if (format_rb == 3){
    re_format <- '.*_beta$|.*_rank$'
  }
  expt_matches <- grep(re_experiment, names(crispr_df))
  sample_matches <- grep(re_sample, names(crispr_df))
  format_matches <- grep(re_format, names(crispr_df))
  
  all_matches <- expt_matches[expt_matches %in% sample_matches & expt_matches %in% format_matches]
  return(all_matches)
}

main_table_get_cols <- function(crispr_df, expt_vec, sample_rb, format_rb){
  # get the integer column indices for crispr_df which fulfill the
  # filtering requirements established by the checkboxes and 
  # the two different radio buttons
  # first, escape the '.' charatcters in var3 so they survive the regex
  expt_vec <- gsub('.', '\\.', expt_vec, fixed=TRUE)
  # add the start of string and underscore followed by any chars
  re_experiment <- paste0("^", expt_vec, '_.*')
  # then stitch them together with an 'or' operator
  re_experiment <- paste(re_experiment, collapse='|')
  # next, generate re to identify desired samples
  if (sample_rb == 1){
    re_sample <- '.*_mean_.*' # if only showing mean
  }else if (sample_rb == 2){
    re_sample <- '.*_[1-9]_.*' # if only showing replicates
  }else if (sample_rb == 3){
    re_sample <- '.*_mean_.*|.*_[1-9]_.*'
  }
  # next, generate re to identify sample format
  if (format_rb == 1){
    re_format <- '.*_beta$'
  }else if (format_rb == 2){
    re_format <- '.*_rank$'
  }else if (format_rb == 3){
    re_format <- '.*_beta$|.*_rank$'
  }
  expt_matches <- grep(re_experiment, names(crispr_df))
  sample_matches <- grep(re_sample, names(crispr_df))
  format_matches <- grep(re_format, names(crispr_df))
  
  all_matches <- expt_matches[expt_matches %in% sample_matches & expt_matches %in% format_matches]
  return(all_matches)
}


make_gene_selector <- function(value=''){
  # uses renderUI to generate the gene selector. 
  # value indicates the currently stored value
  # (can be altered by user or by plot_click or reset button)
  renderUI({
    textInput("highlighted_gene",
              label=h5('Enter comma-separated gene names'),
              value=value,
              placeholder='Optional: Enter gene names for source data')
  })
}


vector_to_hl_string <- function(df){
  # takes a subsettted crispr_df (generated by nearPoints) and generates a
  # comma separated string from $Gene to pass to input$highlighted_gene.
  # if there are more than 10 rows in the subsetted df, only returns the top
  # 10 closest points.
  if (nrow(df) > 10){ # if the nearPoints gave more than 10 points
    sub_df <- df[1:10, ]
    return(paste(sub_df$Gene, collapse=','))
  }else{
  return(paste(df$Gene, collapse=','))
  }
}