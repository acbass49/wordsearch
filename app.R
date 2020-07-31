library(shiny)
library(tidyverse)

# This is a shiny app in R. Read more online if unfamiliar
ui <- fluidPage(
        mainPanel(width = 4,
            #This UI is fairly readable
            textOutput("title"),
            textAreaInput("words","Enter Words Here (every word on new line)", rows = 5),
            selectInput("dimensions", "Select # of rows/columns", 
                        choices = 10:50, selected = 15),
            actionButton("execute", "Generate", class = "btn-success"),
            verbatimTextOutput("wordlist"),
            verbatimTextOutput("wordsearch"),
            verbatimTextOutput("key"),
            downloadButton("downloadData", "Download CSV")
        )
    )

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    output$title <- renderText(print("Word Search Generator"))
    #observe event activates code when input$execute is activated    
    observeEvent(input$execute, {
            letters_only <- function(x) !grepl("[^A-Za-z]", x)
            
            letters_only_vector <- function(x){
                for (word in x){
                    if ((!grepl("[^A-Za-z]", word))==FALSE){
                        return(FALSE)
                    }
                }
                return(TRUE)
            }
            
            #function to generate random point on grid
            random_point <- function(matrx) round(runif(1, .5, sqrt(length(matrx))+.4999))
            
            #function to check if word can fit on grid
            check_matrix <- function(x, y, matrx, string_value){
                if (matrx[y, x]==string_value | is.na(matrx[y, x])){
                    result <- "PASS"
                    return(result)
                }else{
                    result <- "FAIL"
                    return(result)
                }
            }
            
            #I made 8 plots signifying each of the directions a word can go on a word search puzzle
            plot_1 <- function(x, y, matrx, string){
                string <- unlist(str_split(string, ""))
                if (y - length(string) >= 1 & check_matrix(x, y, matrx, string[1])=="PASS"){
                    z <- 1
                    for (char in string){
                        if(check_matrix(x, y, matrx, string[z])=="PASS"){
                            matrx[y, x] <- string[z]
                        } else {result <- "error"
                        return(list(result))}
                        z <- z + 1
                        y <- y - 1
                    } 
                    result <- "PASS"
                    return(return(list(result, matrx)))
                } else {result <- "error"
                return(list(result))}
            }
            
            plot_2 <- function(x, y, matrx, string){
                string <- unlist(str_split(string, ""))
                if (y - length(string) >= 1 & 
                    x + length(string) <= sqrt(length(matrx)) & 
                    check_matrix(x, y, matrx, string[1])=="PASS"){
                    z <- 1
                    for (char in string){
                        if(check_matrix(x, y, matrx, string[z])=="PASS"){
                            matrx[y, x] <- string[z]
                        } else {result <- "error"
                        return(list(result))}
                        z <- z + 1
                        y <- y - 1
                        x <- x + 1
                    } 
                    result <- "PASS"
                    return(return(list(result, matrx)))
                } else {result <- "error"
                return(list(result))}
            }
            
            plot_3 <- function(x, y, matrx, string){
                string <- unlist(str_split(string, ""))
                if (x + length(string) <= sqrt(length(matrx)) & check_matrix(x, y, matrx, string[1])=="PASS"){
                    z <- 1
                    for (char in string){
                        if(check_matrix(x, y, matrx, string[z])=="PASS"){
                            matrx[y, x] <- string[z]
                        } else {result <- "error"
                        return(list(result))}
                        z <- z + 1
                        x <- x + 1
                    } 
                    result <- "PASS"
                    return(return(list(result, matrx)))
                } else {result <- "error"
                return(list(result))}
            }
            
            plot_4 <- function(x, y, matrx, string){
                string <- unlist(str_split(string, ""))
                if (y + length(string) <= sqrt(length(matrx)) & 
                    x + length(string) <= sqrt(length(matrx)) & 
                    check_matrix(x, y, matrx, string[1])=="PASS"){
                    z <- 1
                    for (char in string){
                        if(check_matrix(x, y, matrx, string[z])=="PASS"){
                            matrx[y, x] <- string[z]
                        } else {result <- "error"
                        return(list(result))}
                        z <- z + 1
                        y <- y + 1
                        x <- x + 1
                    } 
                    result <- "PASS"
                    return(return(list(result, matrx)))
                } else {result <- "error"
                return(list(result))}
            }
            
            plot_5 <- function(x, y, matrx, string){
                string <- unlist(str_split(string, ""))
                if (y + length(string) <= sqrt(length(matrx)) & check_matrix(x, y, matrx, string[1])=="PASS"){
                    z <- 1
                    for (char in string){
                        if(check_matrix(x, y, matrx, string[z])=="PASS"){
                            matrx[y, x] <- string[z]
                        } else {result <- "error"
                        return(list(result))}
                        z <- z + 1
                        y <- y + 1
                    } 
                    result <- "PASS"
                    return(return(list(result, matrx)))
                } else {result <- "error"
                return(list(result))}
            }
            
            plot_6 <- function(x, y, matrx, string){
                string <- unlist(str_split(string, ""))
                if (y + length(string) <= sqrt(length(matrx)) & 
                    x - length(string) >= 1 & 
                    check_matrix(x, y, matrx, string[1])=="PASS"){
                    z <- 1
                    for (char in string){
                        if(check_matrix(x, y, matrx, string[z])=="PASS"){
                            matrx[y, x] <- string[z]
                        } else {result <- "error"
                        return(list(result))}
                        z <- z + 1
                        y <- y + 1
                        x <- x - 1
                    } 
                    result <- "PASS"
                    return(return(list(result, matrx)))
                } else {result <- "error"
                return(list(result))}
            }
            
            plot_7 <- function(x, y, matrx, string){
                string <- unlist(str_split(string, ""))
                if (x - length(string) >= 1 & check_matrix(x, y, matrx, string[1])=="PASS"){
                    z <- 1
                    for (char in string){
                        if(check_matrix(x, y, matrx, string[z])=="PASS"){
                            matrx[y, x] <- string[z]
                        } else {result <- "error"
                        return(list(result))}
                        z <- z + 1
                        x <- x - 1
                    } 
                    result <- "PASS"
                    return(return(list(result, matrx)))
                } else {result <- "error"
                return(list(result))}
            }
            
            plot_8 <- function(x, y, matrx, string){
                string <- unlist(str_split(string, ""))
                if (y - length(string) >= 1 & 
                    x - length(string) >= 1 & 
                    check_matrix(x, y, matrx, string[1])=="PASS"){
                    z <- 1
                    for (char in string){
                        if(check_matrix(x, y, matrx, string[z])=="PASS"){
                            matrx[y, x] <- string[z]
                        } else {result <- "error"
                        return(list(result))}
                        z <- z + 1
                        y <- y - 1
                        x <- x - 1
                    } 
                    result <- "PASS"
                    return(return(list(result, matrx)))
                } else {result <- "error"
                return(list(result))}
            }
            
            #this takes the coordinate and word and plots it in the matrix
            random_map <- function(x, y, matrx, string) {
                num <- round(runif(1, 0.5,8.4999))
                if (num==1) return(plot_1(x, y, matrx, string))
                else if (num==2) return(plot_2(x, y, matrx, string))
                else if (num==3) return(plot_3(x, y, matrx, string))
                else if (num==4) return(plot_4(x, y, matrx, string))
                else if (num==5) return(plot_5(x, y, matrx, string))
                else if (num==6) return(plot_6(x, y, matrx, string))
                else if (num==7) return(plot_7(x, y, matrx, string))
                else if (num==8) return(plot_8(x, y, matrx, string))
            }
            
            #taking word list from UI and making a vector
            word_vector <- unlist(str_split(input$words,"\n"))
            word_vector <- sapply(word_vector, toupper)
            
            #checking to see if word list will fit in grid specified grid
            number <- as.numeric(input$dimensions)
            
            if (max(str_count(word_vector))>number){
                condition <- FALSE
            } else{
                matrx <- matrix(nrow = number, ncol = number)
                condition <- TRUE
            }
            
            
            
            #putting words in matrix
            for (word in word_vector){
                check_plot_valid <- FALSE
                while (check_plot_valid==FALSE){
                    x <- random_point(matrx)
                    y <- random_point(matrx)
                    lis <- random_map(x, y, matrx, word)
                    if (lis[[1]]=="PASS"){
                        matrx <- lis[[2]]
                        check_plot_valid <- TRUE
                    } else {next()}
                }
            }
            
            #this is the key
            key <- matrx
            
            #putting random letters in the rest of matrix
            alphabet <- unlist(str_split("ABCDEFGHIJKLMNOPQRSTUVWXYZ", ""))
            
            for (row in seq_len(sqrt(length(matrx)))){
                for (column in seq_len(sqrt(length(matrx)))){
                    if (!(matrx[row,column] %in% alphabet))
                        matrx[row,column] <- sample(alphabet, 1)
                }
            }
            
            matrx_for_csv <- matrx
            
            #reordering matrix to prepare for print command
            for (i in seq_len(sqrt(length(matrx)))){
                if (i == 1){
                    n_matrx <- c(matrx[i, ])
                } else{
                    next_row <- c(matrx[i, ])
                    n_matrx <- cbind(n_matrx, next_row)
                }
            }
            
            extra_row <- c("\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n")
            
            n_matrx <- rbind(n_matrx, extra_row)
            
            #print out error message if words list too long/ word input incorrrect otherwise display result
            if (letters_only_vector(word_vector)==FALSE | condition==FALSE){
            condition <- FALSE
            
            }else{ condition <- TRUE}
            
            if (condition==FALSE){
            output$wordsearch <- renderText({
                print(
                "
                ERROR:
                Please make sure you are using 
                only letters and entering a new line 
                for each new word. 
                
                Also please NO SPACES.
                
                If these aren't working make sure each 
                word isn't longer than the dimensions
                you selected for the crossword.
                "
                )})
                output$wordlist <- renderText({print("")})
                output$key <- renderText({print("")})
            }else{
                output$wordlist <- renderText({
                    #creates a new world list that has new line spacing
                    new_word_vector <- c("\n")
                    for (word in word_vector){
                        new_word_vector <- c(new_word_vector, word, "\n")
                    }
                    print(c("Here is your list of words...\n", new_word_vector))})
                output$wordsearch <- renderText({print(c("", n_matrx))})
                output$key <- renderText({
                    #puts . for every blank space for neat key
                    for (row in seq_len(sqrt(length(matrx)))){
                        for (column in seq_len(sqrt(length(matrx)))){
                            if (!(key[row,column] %in% alphabet))
                                key[row,column] <- "."
                        }
                    }
                    #reorders key for neat printing
                    for (i in seq_len(sqrt(length(matrx)))){
                        if (i == 1){
                            key_1 <- c(key[i, ])
                        } else{
                            next_row <- c(key[i, ])
                            key_1 <- cbind(key_1, next_row)
                        }
                    }
                    extra_row <- c("\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n")
                    
                    key <- rbind(key_1, extra_row)
                    
                    print(c("Here is the key!\n\n",key))})
                
                output$downloadData <- downloadHandler(
                    #download option made easy by shiny
                    filename = "wordsearch.csv",
                    content = function(file){
                        write.csv(matrx_for_csv, file, row.names = FALSE)
                    }
            )
        }
        })
}

# Run the application 
shinyApp(ui = ui, server = server)