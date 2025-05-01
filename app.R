# shiny
library(shiny)
library(bslib)
# database and word2vec
library(DBI)
library(RSQLite)
library(word2vec)
library(rlang)
# graphs
library(dplyr)
library(tidyr)
library(igraph)
library(visNetwork)


## day.words = scan('random.words.txt', '', quiet=TRUE)
## day.words = scan('random.words.3000.txt', '', quiet=TRUE)
## day.words = day.words[which(nchar(day.words)>3)]
## set.seed = (123456)
## day.words = matrix(sample(day.words), ncol=2, byrow=2)

## read pre-computed word pairs
day.comp = read.table('word_pairs_computer.tsv', as.is=TRUE)
colnames(day.comp) = c('word1', 'word2', 'score', 'solution', 'difficulty')
day.words = day.comp %>% select(word1, word2) %>% unique %>% as.matrix
day.one = as.Date('2025-05-01')

ndims = 300
sim.method = 'cosine'
if(sim.method == 'cosine'){
  sim.slider = c(.1, .9, .5, .1)
  sim.levels = c(.3, .4, .5, .6)
}
if(sim.method == 'dot'){
  sim.slider = c(0.05, .15, .1, .01)
  sim.levels = c(.08, .1, .15, .2)
}

## con <- dbConnect(SQLite(), dbname = "googlenews.simple_words.opt.db")
con <- dbConnect(SQLite(), dbname = "glove.6B.300d.opt.db")
## dbListTables(con)

## list of words to debug
## words = unlist(strsplit('politician politics strong strength power international business product coffee healed cured restored restore fort fortified improved improve milk water drink trade growth grown ground beef chemical chemicals additive healthy', ' '))

cache <<- list()

getEmb <- function(word){
  if(length(cache) > 1000){
    ## empty cache when it's too big
    message('emptying the cache')
    cache = list()
  }
  if(is.null(cache[[word]])){
    bucket = strtoi(substr(hash(word), 1, 5), 16) %% 100
    res = dbGetQuery(con, paste0("SELECT * FROM words_", bucket, " WHERE word = ?"), params=word)
    message('queryied ', word)
    if(nrow(res) == 1){
      cache[[word]] = as.numeric(unlist(strsplit(res$emb, '_')))
    } else {
      cache[[word]] = rep(NA, ndims)
    }
    cache <<- cache
    ## assign("cache", cache, envir = .GlobalEnv)
  }
  return(cache[[word]])
}

addHighscore <- function(name, steps, difficulty, path){
  day = format(Sys.time(), "%Y%m%e")
  hs = data.frame(name=name, date=format(Sys.time(), "%Y%m%e"),
                  step=steps, difficulty=difficulty, path=path)
  dbWriteTable(con, 'highscores', hs, append=TRUE)
}

checkWin <- function(wvs, min.sim, check.highscores=FALSE){
  words = rownames(wvs)
  sim.m = word2vec_similarity(wvs, wvs, type=sim.method)
  nk = graph_from_adjacency_matrix(sim.m>=min.sim)
  nk.cmp = components(nk)
  nodes = tibble(label=names(nk.cmp$membership),
                 group=factor(as.numeric(nk.cmp$membership)))
  win = nodes %>% filter(label %in% words[1:2]) %>% .$group %>% unique %>% length
  win = win == 1 && all(words[1:2] %in% nodes$label)
  res = list(score=NA, path=NA, msg='', add.highscore=FALSE)
  if(win){
    win_path = shortest_paths(nk, words[1], words[2])
    res$score = length(win_path$vpath[[1]]) - 2
    res$path = paste0(names(V(nk))[win_path$vpath[[1]]], collapse='->')
    if(check.highscores){
      ## read highscores from DB
      hs = dbReadTable(con, 'highscores')
      day = format(Sys.time(), "%Y%m%e")
      hs = subset(hs, date==day & difficulty == min.sim & name != '')
      print(hs)
      print(nrow(hs))
      print(res)
      ## check if champion
      if(nrow(hs) == 0 | !any(hs$step <= res$score)){
        res$msg = "You're the champion!"
        res$add.highscore = TRUE
      } else if(!any(hs$step < res$score | hs$path == res$path)){
        res$msg = "You found a different path, you're a champion!"
        res$add.highscore = TRUE
      } else if(!any(hs$step < res$score)){
        res$msg = "Someone found your solution already..."
      }
    }
  }
  ## return win information
  return(res)
}

btn.col = '#EEEEEE'

# Define UI for app ----
ui <- page_sidebar(
  sidebar = sidebar(
    actionButton("help", "Help", style=paste0('background-color: ', btn.col)),
    selectInput(inputId="min.sim", label="Level", choices = 1:4, selected = 1),
    ## sliderInput(inputId = "min.sim", label = "Difficulty",
    ##             min=sim.slider[1], max=sim.slider[2],
    ##             value=sim.slider[3], step=sim.slider[4]),
    textInput(inputId = "new.word", label = "New word", value = ''),
    actionButton('cleanup', 'Cleanup singletons',
                 style=paste0('background-color: ', btn.col)),
    conditionalPanel('false',
                     textInput('words', 'hidden', value='')),
    textOutput('status'),
    textInput('name', 'Your name', value=''),
    actionButton('refresh_highscore', 'Save/refresh leaderboard',
                 style=paste0('background-color: ', btn.col)),
    h4('Leaderboard'),
    verbatimTextOutput('highscores'),
    textOutput('word.update.trigger'),
    textOutput('cleanup.trigger'),
    open='always',
    bg='#E8F5E9'
  ),
  visNetworkOutput('visnk', height='800px')
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  loadEmbs <- reactive({
    message('Loading embeddings')
    today = as.Date(Sys.time())
    today.words = day.words[as.numeric(today-day.one) + 1,]
    words = c(today.words, unlist(strsplit(input$words, ' ')))
    words = gsub(' ', '', words)
    words = unique(setdiff(words, c(' ', '')))
    print(words)
    wvs = t(sapply(words, getEmb))
    if(is.na(wvs[nrow(wvs), 1])){
      showNotification(paste("Unknown word: ", words[length(words)]),
                       type='error', duration=10)
    }
    words = words[which(!is.na(wvs[,1]))]
    wvs[which(!is.na(wvs[,1])),]
  })

  observeEvent(input$help, {
    showModal(modalDialog(
      p(strong('Goal'), 'Connect the two target words (within circles). The target words are selected randomly every day from a list of common English words.'),
      p(strong('How'), 'Type new words in the ', em('New word'), 'field. The word is sent when you press ', em('space'), '. Remove a word by starting with ', em('-'),
        '. If it is close enough to another word it will be ', strong('connected and colored'),
        ' in the network. A  grey word is not connected. A grey link means that two words are almost connected.'),
      p(strong('Difficulty '), 'Use the slider at the top to change the minimum similarity used to define if words are ',
        em('connected'), ' to adjust the difficulty.'),
      p(strong('Leaderboard '), 'When you win, you can save your score (minimum steps between the target words) to the leaderboard if you’ve found the shortest path and no one has found your solution yet.',
        'Type you name in the ', em('Your name'), ' field and click on ', em('Save/refresh champion(s)'),
        '. The leaderboard is specific to a difficulty level.'),
      p(strong('Word similarity'), 'Words were placed in a high-dimentional space based on their usage context using the ', a('word2vec technique', href='https://en.wikipedia.org/wiki/Word2vec'), '. The similarity between two words is based on their distance in this space.'),
      easyClose = TRUE
    ))
  })
  
  output$status <- renderText({
    message("Win check")
    wvs = loadEmbs()
    min.sim = sim.levels[as.numeric(input$min.sim)]
    win = checkWin(wvs, min.sim)
    win.msg = ''
    if(!is.na(win$score)){
      win.msg = paste0('You won in ', win$score, ' steps: ', win$path, '!')
    }
    return(win.msg)
  })
  
  output$word.update.trigger <- renderText({
    newword = input$new.word
    if(nchar(newword) == 0){
      return('')
    }
    if(substr(newword, nchar(newword), nchar(newword)) == ' ' &&
       nchar(newword)>2){
      newword = substr(newword, 1, nchar(newword) - 1)
      words = unlist(strsplit(isolate(input$words), ' '))
      ## if starts with '-', remove word
      if(substr(newword, 1, 1) == '-'){
        newword = substr(newword, 2, nchar(newword))
        message('removing ', newword)
        words = setdiff(words, c(newword, ' ', ''))
      } else {
        words = c(words, newword)
      }
      updateTextInput(inputId='new.word',
                      value='')
      updateTextInput(inputId='words',
                      value=paste(words, collapse=' '))
    }
    return('')
  })

  output$cleanup.trigger <- renderText({
    input$cleanup
    wvs = isolate(loadEmbs())
    min.sim = sim.levels[as.numeric(isolate(input$min.sim))]
    ## compute similarity and distance
    sim.m = word2vec_similarity(wvs, wvs, type=sim.method)
    nk = graph_from_adjacency_matrix(sim.m>=min.sim-sim.slider[4])
    nk.cmp = components(nk)
    ## words to remove
    words.torm = tibble(label=names(nk.cmp$membership),
                        group=factor(as.numeric(nk.cmp$membership))) %>%
      group_by(group) %>% filter(n()==1) %>%
      .$label %>% unique
    words = unlist(strsplit(isolate(input$words), ' '))
    words = setdiff(words, words.torm)
    updateTextInput(inputId='words',
                    value=paste(words, collapse=' '))    
    return('')
  })

  output$highscores <- renderText({
    input$refresh_highscore
    ## check for a win and add highscore if valid
    wvs = isolate(loadEmbs())
    words = rownames(wvs)
    min.sim = sim.levels[as.numeric(input$min.sim)]
    win = checkWin(wvs, min.sim, check.highscores=TRUE)
    if(win$add.highscore){
      addHighscore(isolate(input$name), steps=win$score,
                   difficulty=min.sim, path=win$path)
    }
    if(win$msg != ''){
      showNotification(win$msg, duration=15)
    }
    ## read highscores from DB
    hs = dbReadTable(con, 'highscores')
    day = format(Sys.time(), "%Y%m%e")
    hs = hs %>% filter(date==day, difficulty == min.sim, name != '') %>%
      select(name, step)
    ## add computer scores
    print(words)
    day.comp = day.comp %>% filter(word1==words[1], word2==words[2]) %>%
      mutate(step=score-2, name='Computer') %>%
      filter(step >= 0, difficulty==min.sim) %>%
      select(name, step)
    print(day.comp)
    ## find best score for each winner and rank
    hs = rbind(hs, day.comp) %>% group_by(name) %>%
      arrange(step) %>% do(head(., 1)) %>%
      ungroup %>% arrange(step) %>%
      mutate(rk=rank(step, ties.method='min')) %>% 
      filter(rk<=5) %>%
      mutate(toprint=paste0(rk, ' - ', name, ' (', step, ')'))
    print(hs)
    ## output high scores
    if(nrow(hs) == 0){
      return('No winner yet.')
    }
    return(paste(hs$toprint, collapse='\n'))
  })

  output$visnk <- renderVisNetwork({
    wvs = loadEmbs()
    words = rownames(wvs)
    min.sim = sim.levels[as.numeric(input$min.sim)]
    ## compute similarity and distance
    sim.m = word2vec_similarity(wvs, wvs, type=sim.method)
    nk = graph_from_adjacency_matrix(sim.m>=min.sim)
    nk.cmp = components(nk)
    ## nodes
    node.colpal = scales::muted(rainbow(length(nk.cmp$csize)), l=80)
    nodes = tibble(label=names(nk.cmp$membership),
                   group=factor(as.numeric(nk.cmp$membership)),
                   font=ifelse(label== words[length(words)] & !(label %in% words[1:2]), '22px', '14px'),
                   shape=ifelse(label %in% words[1:2], 'circle', 'box'),
                   color=node.colpal[as.numeric(group)]) %>%
      mutate(id=paste0('id_', label)) %>%
      group_by(group) %>% mutate(color=ifelse(n()>1, color, 'lightgrey'))
    node.col = nodes$color
    names(node.col) = nodes$label
    ## all edges
    edges = as.data.frame(sim.m) %>% mutate(word=rownames(sim.m)) %>%
      pivot_longer(-word, names_to='to') %>%
      mutate(from=paste0('id_', word), to=paste0('id_', to)) %>%
      select(from, to, value) %>% 
      filter(from < to, value>min.sim - sim.slider[4]) %>%
      mutate(label=round(value, 3),
             color=ifelse(value<min.sim, 'grey', node.col[from]))
    ## have we won?
    win = checkWin(wvs, min.sim)
    if(!is.na(win$score)){
      win_path = shortest_paths(nk, words[1], words[2])
      win_score = length(win_path$vpath[[1]]) - 2
      win_path = paste0(names(V(nk))[win_path$vpath[[1]]], collapse='->')
      win.msg = paste0('You won in ', win_score, ' steps: ', win_path, '!')
      showNotification(win.msg, duration=5)
      # addHighscore(isolate(input$name), steps=win_score, difficulty=input$min.sim, path=win_path)
    }
    ## network 'barnesHut', 'repulsion', 'forceAtlas2Based'
    visNetwork(nodes, edges, height = "800px", width = "100%") %>%
      visPhysics(solver='forceAtlas2Based') %>% 
      visLayout(randomSeed = 123)
  }) 
  
}

shinyApp(ui = ui, server = server)
