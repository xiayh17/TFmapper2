#' search_options UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_search_options_ui <- function(id){
  ns <- NS(id)
  tagList(

    fluidRow(
      # tags$head(
      #   tags$style(
      #     HTML(
      #       "table.dataTable tbody tr.selected td {
      #  color: white !important;
      #  background-color: #ff8d00 !important;}"
      #     )
      #   )
      # ),
      shinyjs::useShinyjs(),
      column(width = 4,

             bs4Dash::bs4Card(
               #icon = icon("laptop-code"),
               title = "Start Search: ",
               width = NULL,
               maximizable = TRUE,
               boxToolSize = "lg",
               elevation = 3,
               headerBorder = FALSE,
               shinyWidgets::prettyRadioButtons(
                 inputId = ns("species"),
                 label = "Choices Species: ",
                 choices = species_pull,
                 icon = icon("eye-dropper"),
                 bigger = TRUE,
                 inline = TRUE,
                 fill = TRUE,
                 plain = TRUE,
                 status = "success",
                 animation = "pulse"
               ),
               shinyWidgets::prettyCheckboxGroup(
                 inputId = ns("ip"),
                 label = "Choose IP: ",
                 choices = c("Trans-acting factors" = "factor", "Histone marks" = "hm", "Chromatin accessibility" = "ca"),
                 selected = "factor",
                 icon = icon("eye-dropper"),
                 bigger = TRUE,
                 inline = TRUE,
                 fill = TRUE,
                 plain = TRUE,
                 status = "success",
                 animation = "pulse"
               )

               # selectizeInput('cellline', label = "Biological source",
               #                choices = NULL,# width = 275,
               #                options = list(placeholder = "Search by Cell Line; Tissue; Organ,Example: MCF-7;Epithelium;Mammary Gland",
               #                               maxOptions = 1000)
               # )
             ), ## end of card 1
             bs4Dash::bs4TabCard(
               title = NULL,
               footer = NULL,
               status = "success",
               solidHeader = FALSE,
               background = NULL,
               width = NULL,
               height = NULL,
               side = "right",
               collapsible = TRUE,
               collapsed = FALSE,
               closable = FALSE,
               maximizable = TRUE,
               gradient = FALSE,
               boxToolSize = "xs",
               elevation = 3,
               headerBorder = FALSE,
               label = NULL,
               dropdownMenu = NULL,
               sidebar = NULL,
               id = ns("search_tabs"),
               selected = "Search by gene",
               type = "tabs",
               #icon = icon("question-circle"),
               tabPanel("Search by gene",
                        value = "search_gene",
                        icon = icon("dna"),
                        selectInput(
                          inputId = ns("portion"),
                          label = "Which portion of the gene to query?",
                          choices = portion_pull,
                          multiple = TRUE
                        ),
                        selectizeInput(
                          inputId = ns("gene"),
                          label = "Please select a gene name (HUGO symbol)",
                          choices = NULL,
                          selected = character(0),
                          multiple = FALSE,
                          options = list(
                            placeholder = 'Search by Gene ID,e.g: EZH2',
                            onInitialize = I('function() { this.setValue(""); }'),
                            maxOptions = 200
                          )),
                        shinyWidgets::actionBttn(
                          inputId = ns("do_gene"),
                          label = "Search",
                          icon =  icon("search"),
                          style = "jelly",
                          color = "success",
                          size = "md",
                          block = F
                        )
               ), # end of tabPanel 1
               tabPanel("Search by genomic coordinates",
                        value = "search_coordinate",
                        icon = icon("map-marked-alt"),
                        fluidRow(
                          column(
                            9,
                            p("Please input the coordinates:"),
                            textInput(
                              ns("position"),
                              label = NULL,
                              "chr18:28176327-28178670",
                              width = "100%"
                            ),
                            fluidRow(
                              column(
                                5,
                                shinyWidgets::actionBttn(
                                  inputId = ns("zoom_in"),
                                  label = "Zoom in",
                                  icon = icon("search-minus"),
                                  style = "stretch",color = "success",
                                  size = "sm",block = T)
                              ),
                              column(
                                5,
                                shinyWidgets::actionBttn(
                                  inputId = ns("zoom_out"),
                                  label = "Zoom out",
                                  icon = icon("search-minus"),
                                  style = "stretch",color = "success",
                                  size = "sm",block = T)
                              )
                            )
                          ),
                          column(
                            3,
                            p(HTML("&nbsp;")),
                            shinyWidgets::actionBttn(
                              inputId = ns("do_position"),
                              label = "Search",
                              icon =  icon("search"),
                              style = "jelly",
                              color = "success",
                              size = "md",
                              block = F
                            ),
                            column(12,
                                   br(),
                                   helpText(" (by 1 kb)"))
                          )
                        )
                        # h3("you can ajust the postion by increase or decrease 1 kb at the region in box "),
               )
             ), # end of card 2
             conditionalPanel(
               condition = paste0("input['", ns("do_gene"), "'] > 0|input['", ns("do_position"), "']>0"),
               bs4Dash::bs4Card(
                 #icon = icon("laptop-code"),
                 title = "Filter by Biological source",
                 width = NULL,
                 maximizable = TRUE,
                 boxToolSize = "lg",
                 elevation = 3,
                 headerBorder = FALSE,
                 selectizeInput(
                   inputId = ns("tissue_type"),
                   label = "Please select a Tissue Type",
                   choices = NULL,
                   selected = character(0),
                   multiple = F,
                   options = list(
                     placeholder = 'Filter by Tissue Type',
                     onInitialize = I('function() { this.setValue(""); }'),
                     maxOptions = 100
                   )),
                 selectizeInput(
                   inputId = ns("cell_line"),
                   label = "Please select a Cell Line",
                   choices = NULL,
                   selected = character(0),
                   multiple = F,
                   options = list(
                     placeholder = 'Filter by Cell Line',
                     onInitialize = I('function() { this.setValue(""); }'),
                     maxOptions = 100
                   )),
                 selectizeInput(
                   inputId = ns("cell_type"),
                   label = "Please select a Cell Type",
                   choices = NULL,
                   selected = character(0),
                   multiple = F,
                   options = list(
                     placeholder = 'Filter by Cell Type',
                     onInitialize = I('function() { this.setValue(""); }'),
                     maxOptions = 100
                   ))
                 # shinyWidgets::actionBttn(
                 #   inputId = ns("do_filter"),
                 #   label = "Search",
                 #   icon =  icon("search"),
                 #   style = "jelly",
                 #   color = "success",
                 #   size = "md",
                 #   block = F
                 # )
               ) ## end of card 3
             ),
             textOutput(ns("text"))
      ),
      column(width = 8,
             bs4Dash::bs4Card(
               status = "success",
               width = NULL,
               height = '100%',
               title = "Results (Choose One To Show after Click Search)",
               conditionalPanel( ## only show results after user click the search button !
                 condition = paste0("input['", ns("do_gene"), "'] > 0|input['", ns("do_position"), "']>0"),
                 ## we should change the link behind this UI in server client.
                 # uiOutput('washUlink'),
                 bs4Dash::bs4TabCard(
                   title = NULL,
                   footer = NULL,
                   status = "success",
                   solidHeader = FALSE,
                   background = NULL,
                   width = NULL,
                   height = '100%',
                   side = "right",
                   collapsible = TRUE,
                   collapsed = FALSE,
                   closable = FALSE,
                   maximizable = TRUE,
                   gradient = FALSE,
                   boxToolSize = "xs",
                   elevation = 3,
                   headerBorder = FALSE,
                   label = NULL,
                   dropdownMenu = NULL,
                   sidebar = NULL,
                   id = NULL,
                   selected = "Search by gene",
                   type = "tabs",
                   tabPanel(
                     title = "Genes",
                     DT::DTOutput(ns("gene_dat"))
                   ),
                   tabPanel(
                     title = "Download"
                     # downloadLink('downloadData_csv', 'Download the CSV file'),
                     # downloadLink('downloadData_bed', 'Download the BED file')
                   )
                 )
               )## end for conditionalPanel
             )
      )
    )

  )
}

#' search_options Server Functions
#'
#' @noRd
mod_search_options_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    shinyOptions(cache = cachem::cache_mem(max_size = 1000e6))
    shinyWidgets::useSweetAlert()
    ## 连接数据库
    tfmapperdb <- pool::dbPool(RSQLite::SQLite(), dbname = "/Users/yonghe/Desktop/TFmapperTool/data-raw/TFmapperDB.sqlite")

    possible_gene <- reactiveVal(value = NULL)
    gene_name <- reactiveVal(value = NULL)
    gene_coord <- reactiveVal(value = NULL)
    results_o <- reactiveVal(value = NULL)
    ## 获取用户选项
    ## 判断基因还是坐标做出选择
    observeEvent(input$search_tabs,{

      search_tabs <- input$search_tabs ## 判断依据

      if(search_tabs == "search_gene") {

        ## 如果是查询基因
        ## 获取关于基因的选项

        observeEvent(
          # req(input$portion,input$species,input$ip), ## 全部条件变动，候选项都才变
          {
            input$portion
            input$species
            input$ip
          },
          {

          species_key <- input$species
          ip_key <- input$ip
          portion_key <- input$portion

          possible_gene <- TFmapperTool::gene_pull(tfmapperdb = tfmapperdb,
                                                   species_key = species_key,
                                                   ip_key = ip_key,
                                                   portion_key = portion_key)

          updateSelectizeInput(session, 'gene',
                               choices = as.list(possible_gene[,"GeneSymbol"]),
                               selected = character(0),
                               server = TRUE)

          output$text <- renderText({paste0("You are viewing tab \"",
                                            input$species,input$ip,input$search_tabs,
                                            input$portion,input$gene,length(input$gene),
                                            # possible_gene[1,1],
                                            "\"")})
        })

      } else if (search_tabs == "search_coordinate") {

        ## 如果查询坐标
        ## 获取坐标相关选项
        species_key <- input$species
        ip_key <- input$ip
        position <- input$position
        chr_key = strsplit(position,split = ":|-")[[1]][1]
        start_key = as.numeric(strsplit(position,split = ":|-")[[1]][2])
        end_key = as.numeric(strsplit(position,split = ":|-")[[1]][3])

        observeEvent(input$zoom_in, {
          position <- input$position
          chr_key = strsplit(position,split = ":|-")[[1]][1]
          start_key = as.numeric(strsplit(position,split = ":|-")[[1]][2])
          end_key = as.numeric(strsplit(position,split = ":|-")[[1]][3])
          chrom=chr_key
          start=start_key
          end=end_key
          if((end-start)>2000){
            start=start+1000
            end=end-1000
          }
          tmp=paste0(chrom,":",start,"-",end)
          updateTextInput(session, 'position', label = NULL, value = tmp)
        })
        observeEvent(input$zoom_out, {
          position <- input$position
          chr_key = strsplit(position,split = ":|-")[[1]][1]
          start_key = as.numeric(strsplit(position,split = ":|-")[[1]][2])
          end_key = as.numeric(strsplit(position,split = ":|-")[[1]][3])
          chrom=chr_key
          start=start_key
          end=end_key
          start=start-1000
          end=end+1000
          tmp=paste0(chrom,":",start,"-",end)
          updateTextInput(session, 'position', label = NULL, value = tmp)
        })

        output$text <- renderText({paste0("You are viewing tab \"",
                                          input$species,input$ip,input$search_tabs,
                                          input$position,chr_key,start_key,end_key,
                                          # posssible_coord[1],posssible_coord[2],
                                          "\"")})

      }

    }) ## 分别对基因和坐标筛选选项完成

    ## 根据基因和坐标做出搜索
    observeEvent(input$do_gene,{
      results_o(NULL)
      species_key <- input$species
      ip_key <- input$ip
      portion_key <- input$portion
      gene_key <- input$gene
      search_tabs <- input$search_tabs
      print(search_tabs)
      ## 检查用户点击search之前有没有选择基因和Portion
      if(is.null(input$portion)) {

        shinyWidgets::sendSweetAlert(
          session = session,
          title = "Warning !!!",
          text = "Please choose at least one Portion",
          type = "warning"
        )

      }

      if(identical(input$gene,"")) {

        shinyWidgets::sendSweetAlert(
          session = session,
          title = "Warning !!!",
          text = "Please choose at least one Gene",
          type = "warning"
        )

      }

      if(!is.null(input$portion)&!identical(input$gene,"")){

        ## 查询基因
        gene_name <- reactive({

          TFmapperTool::query_gene(tfmapperdb = tfmapperdb,
                                   species_key = species_key,
                                   ip_key = ip_key,
                                   portion_key = portion_key,
                                   gene_key = gene_key)

        }) %>% bindCache(input$species, input$ip, input$portion, input$gene) %>%
          bindEvent(input$do_gene)

        possible_bio <- reactive({

          TFmapperTool:::query_bio(

            tfmapperdb = tfmapperdb,
            gene_name()

          )

        })

        cell_line_pull <- possible_bio()$Cell_line
        cell_type_pull <- possible_bio()$Cell_type
        tissue_type_pull <- possible_bio()$Tissue_type

        ## 根据生物来源进行信息整合
        ## 获取基因搜索结果的注释信息
        gene_bio <-  reactive({

          gene_name <- gene_name()

          ## 注释信息没有筛选
          if(any(class(gene_name) %in% c("data.frame", "tbl", "tbl_df"))) {

            TFmapperTool::query_anno(tfmapperdb = tfmapperdb,
                                     gene_name,
                                     cell_line_key = cell_line_pull,
                                     cell_type_key = cell_type_pull,
                                     tissue_type_key = tissue_type_pull) }

        })

        ## 合并基因搜索结果和注释信息得到最终结果
        gene_res <- reactive({

          gene_name <- gene_name()
          gene_bio <- gene_bio()

          if(any(class(gene_name) %in% c("data.frame", "tbl", "tbl_df")) & any(class(gene_bio) %in% c("data.frame", "tbl", "tbl_df"))) {

            TFmapperTool::merge_query(gene_data = gene_name,anno_info = gene_bio)

          }

        })

        updateSelectizeInput(session, 'cell_type',
                             choices = as.list(gene_res()[,"Cell_type"]),
                             selected = character(0),
                             server = TRUE)

        updateSelectizeInput(session, 'cell_line',
                             choices = as.list(gene_res()[,"Cell_line"]),
                             selected = character(0),
                             server = TRUE)

        updateSelectizeInput(session, 'tissue_type',
                             choices = as.list(gene_res()[,"Tissue_type"]),
                             selected = character(0),
                             server = TRUE)

        print(input$cell_line)

        ##首先筛选条件
        gene_filter <- reactive({

          gene_res <- gene_res()
          cell_line_key = input$cell_line
          tissue_type_key = input$tissue_type
          cell_type_key = input$cell_type

          ## 如果用户还没有选择，就展示全部，即不筛选
          if(all(identical(cell_line_key,""),identical(cell_type_key,""),identical(tissue_type_key,""))) {

            gene_filter = gene_res

            # gene_filter

          } else {

            ## 用户可能只选择了部分呢，所以添加转换没选择的为全部
            if(identical(cell_line_key,"")){

              cell_line_key = cell_line_pull

            }

            if(identical(cell_type_key,"")){

              cell_type_key = cell_type_pull

            }

            if(identical(tissue_type_key,"")){

              tissue_type_key = tissue_type_pull

            }

            gene_filter = gene_res[gene_res[,"Cell_line"] %in% cell_line_key&gene_res[,"Cell_type"] %in% cell_type_key&gene_res[,"Tissue_type"] == tissue_type_key,]

            # gene_filter
          }

          print(cell_line_key)
          print(cell_type_key)
          print(tissue_type_key)
          print(dim(gene_filter))

          if (search_tabs == "search_gene"&nrow(gene_filter) == 0) {

            print("should be in search_gene no data")
            # ## 用户的筛选结果为0，弹出提示
            shinyWidgets::sendSweetAlert(
              session = session,
              title = "Warning !!!",
              text = "No data here Under Your choosies",
              type = "warning"
            )

          }

          gene_filter

        })

        if (search_tabs == "search_gene"&nrow(gene_filter()) > 0) {
          print("should be in search_gene have data")

          observeEvent({input$cell_line
            input$tissue_type
            input$cell_type},{
              print("here is gene")
              print(dim(gene_filter()))
              results_o(gene_filter())
              print(dim(results_o()))
            })

        }

        print(dim(gene_filter()))

        # # 测试结果
        # output$gene_dat <- DT::renderDT({
        #   gene_filter = gene_filter()
        #   DT::datatable(gene_filter)
        # })

        print(search_tabs)
        search_tabs = NULL
        print(search_tabs)

      }

    }) ## end of do_gene

    # # 测试结果
    # output$gene_dat <- DT::renderDT({
    #   gene_filter = results_o()
    #   DT::datatable(gene_filter)
    # })

    observeEvent(input$do_position,{
      results_o(NULL)
      species_key <- input$species
      ip_key <- input$ip
      search_tabs <- input$search_tabs

      position <- input$position
      chr_key = strsplit(position,split = ":|-")[[1]][1]
      start_key = as.numeric(strsplit(position,split = ":|-")[[1]][2])
      end_key = as.numeric(strsplit(position,split = ":|-")[[1]][3])

      ## 是否进行下一步的判断以及给用户的提示信息
      ## 基因位置 至少起始和终止不能同时小于最小值或者大于最大值
      ## 计算最小和最大位置数据
      posssible_coord <- TFmapperTool::coor_pull(
        tfmapperdb = tfmapperdb,
        species_key = species_key,
        ip_key = ip_key,
        chr_key = chr_key
      )

      ## 如果同时小于最小值
      if(all(start_key<posssible_coord[1],end_key<posssible_coord[1])|all(start_key>posssible_coord[2],end_key>posssible_coord[2])){

        ## 弹窗提醒
        shinyWidgets::sendSweetAlert(
          session = session,
          title = "Warning !!!",
          text = paste0("Please enter a more reasonable coordinate between",posssible_coord[1]," to ",posssible_coord[2]),
          type = "warning"
        )

      } else {

        ## 合并基因搜索结果和注释信息得到最终结果
        withProgress(message = 'Searching in progress', {
          for(i in 1:10){

            # Long Running Task
            Sys.sleep(1)

            # Update progress
            incProgress(1/10)
          }
          ## 查询位置
          gene_coord <- reactive({

            TFmapperTool::query_coord(tfmapperdb = tfmapperdb,
                                      species_key = species_key,
                                      chr_key = chr_key,
                                      ip_key = ip_key,
                                      start_key = start_key,
                                      end_key = end_key)

          }) %>% bindCache(input$species, input$ip, input$position) %>%
            bindEvent(input$do_position)

          possible_bio_coord <- reactive({

            TFmapperTool:::query_bio(

              tfmapperdb = tfmapperdb,
              gene_coord()

            )

          })

          cell_line_pull <- possible_bio_coord()$Cell_line
          cell_type_pull <- possible_bio_coord()$Cell_type
          tissue_type_pull <- possible_bio_coord()$Tissue_type



          ## 根据生物来源进行信息整合
          ## 获取基因搜索结果的注释信息
          coord_bio <-  reactive({

            gene_coord <- gene_coord()

            ## 注释信息没有筛选
            if(any(class(gene_coord) %in% c("data.frame", "tbl", "tbl_df"))) {

              TFmapperTool::query_anno(tfmapperdb = tfmapperdb,
                                       gene_coord,
                                       cell_line_key = cell_line_pull,
                                       cell_type_key = cell_type_pull,
                                       tissue_type_key = tissue_type_pull) }

          })
          coord_res <- reactive({

            gene_coord <- gene_coord()
            coord_bio <- coord_bio()

            if(any(class(gene_coord) %in% c("data.frame", "tbl", "tbl_df")) & any(class(coord_bio) %in% c("data.frame", "tbl", "tbl_df"))) {

              TFmapperTool::merge_query(gene_data = gene_coord,anno_info = coord_bio)

            }

          })
        })

        updateSelectizeInput(session, 'cell_type',
                             choices = as.list(coord_res()[,"Cell_type"]),
                             selected = character(0),
                             server = TRUE)

        updateSelectizeInput(session, 'cell_line',
                             choices = as.list(coord_res()[,"Cell_line"]),
                             selected = character(0),
                             server = TRUE)

        updateSelectizeInput(session, 'tissue_type',
                             choices = as.list(coord_res()[,"Tissue_type"]),
                             selected = character(0),
                             server = TRUE)

        print(input$cell_line)

        ##首先筛选条件
        coord_filter <- reactive({

          coord_res <- coord_res()
          cell_line_key = input$cell_line
          tissue_type_key = input$tissue_type
          cell_type_key = input$cell_type

          ## 如果用户还没有选择，就展示全部，即不筛选
          if(all(identical(cell_line_key,""),identical(cell_type_key,""),identical(tissue_type_key,""))) {

            coord_filter = coord_res

            # gene_filter

          } else {

            ## 用户可能只选择了部分呢，所以添加转换没选择的为全部
            if(identical(cell_line_key,"")){

              cell_line_key = cell_line_pull

            }

            if(identical(cell_type_key,"")){

              cell_type_key = cell_type_pull

            }

            if(identical(tissue_type_key,"")){

              tissue_type_key = tissue_type_pull

            }

            coord_filter = coord_res[coord_res[,"Cell_line"] %in% cell_line_key&coord_res[,"Cell_type"] %in% cell_type_key&coord_res[,"Tissue_type"] == tissue_type_key,]

            # gene_filter
          }

          print(cell_line_key)
          print(cell_type_key)
          print(tissue_type_key)
          print(dim(coord_filter))

          if (search_tabs == "search_coordinate"&nrow(coord_filter) == 0) {

            print("should be in search_coordinate no data")
            # ## 用户的筛选结果为0，弹出提示
            shinyWidgets::sendSweetAlert(
              session = session,
              title = "Warning !!!",
              text = "No data here Under Your choosies",
              type = "warning"
            )

          }


          coord_filter

        })

        if (search_tabs == "search_coordinate"&nrow(coord_filter()) > 0) {
          print("should be in search_coordinate have data")

          observeEvent({input$cell_line
            input$tissue_type
            input$cell_type},{
              print("here is coord")
              print(dim(coord_filter()))
              results_o(coord_filter())
              print(dim(results_o()))
            })

        }

        print(dim(coord_filter()))

      }

    }) ## end of do position

    ## 整理结果

    res_re <- reactive({

      results_o <- results_o()
      vip_col <- c("GSMID","Factor","Portion","IP","GeneSymbol","Tissue_type","Cell_line","Cell_type","Species","Chr","Start","Length")
      all_col <- colnames(results_o)
      other_col <- setdiff(all_col,vip_col)
      res_re <- results_o[,c(vip_col,other_col)]

      ## 拼接坐标
      ## 这一步的结果可作为用户下载表格的来源
      tibble::add_column(res_re,
                           Coordinate = paste0(res_re[,"Chr"], ":", res_re[,"Start"],"-",res_re[,"Start"] + res_re[,"Length"]),
                           .after = "GeneSymbol")

    })

    ## 优化UI表格
    res_dt <- reactive({

      res_re <- res_re()
      res_dt <- res_re()
      res_dt[,"GSMID"] <- TFmapperTool:::gsmLink(res_re$GSMID,res_re$GSMID)
      res_dt[,"GeneSymbol"] <- TFmapperTool:::geneCardsLink(res_re$GeneSymbol,res_re$GeneSymbol)
      res_dt <- tibble::add_column(res_dt,
                           `Genome Browser` = paste(TFmapperTool:::washuLink(res_re$Species,res_re$Coordinate,"WashU"),
                                                    TFmapperTool:::ucscLink(res_re$Species,res_re$Coordinate,"UCSC")),
                           .after = "Coordinate")

      # DT::datatable(res_dt,style = "bootstrap4", escape = FALSE)
      res_dt

    })


    # 测试结果
    output$gene_dat <- DT::renderDT({
      res_dt = res_dt()
      DT::datatable(res_dt,
                    rownames = FALSE,
                    # style = "bootstrap4",
                    escape = FALSE,
                    extensions = 'Responsive',
                    caption = htmltools::tags$caption(
                      style = 'caption-side: bottom; text-align: center;',
                      'Table ', htmltools::em('Result of Search')
                    )
      )
    })

    # ## 返回结果
    # return(
    #   list(
    #     results_o = observe({
    #       results_o()
    #     }),
    #     do_gene = reactive({ input$do_gene }),
    #     do_position = reactive({ input$do_position })
    #   ))

  })
}

## To be copied in the UI
# mod_search_options_ui("search_options_ui_1")

## To be copied in the server
# mod_search_options_server("search_options_ui_1")
