library(gapminder)
library(ggplot2)
library(mongolite)
library(jpeg)

# CONECTAR AO SERVIDOR LOCAL MONGODB
mongo_url <- "mongodb://localhost:27017/piiia"
mongo <- mongo(collection = "graphics", url = mongo_url)

# FUNÇÃO PARA RENOMEAR OS EIXOS BASEADO NA COLUNA DO DATASET
get_ax_name <- function(field_name){
  out<-''
  if(field_name==substitute(country)){
    out<-'Country'
  }
  else if(field_name==substitute(continent)){
    out<-'Continent'
  }
  else if(field_name==substitute(year)){
    out<-'Year'
  }
  else if(field_name==substitute(lifeExp)){
    out<-'Life expectancy'
  }
  else if(field_name==substitute(pop)){
    out<-'Population'
  }
  else if(field_name==substitute(gdpPercap)){
    out<-'GDP per capita'
  }
  return(out)
  
}

# FUNÇÃO PARA GERAR O GRÁFICO DESEJADO OU LER O GRÁFICO A PARTIR DO BD
get_plot <- function(X_axis, Y_axis, highlight=NULL, plot_type = "scatter") {

#     
data<-gapminder
X_name<-get_ax_name(substitute(X_axis))
Y_name<-get_ax_name(substitute(Y_axis))
ext = '.jpg'

graph_name<-paste0(paste(sep="_",
                  plot_type,
                  deparse(substitute(X_axis)),
                  deparse(substitute(Y_axis)),
                  deparse(substitute(highlight))
                  ),ext)
req<-paste0('{\"name\":\"',graph_name,'\"}')
# AGORA PROCURAR O GRÁFICO NA BASE DE DADOS, BASEADO NO NOME

# SE A IMAGEM NÃO EXISTIR, CRIAR O PLOT E DEPOIS SALVAR
if (mongo$count(query = req) == 0) {


  print("Gráfico não gerado previamente, gerando agora e salvando no BD...")
  
  X_axis<-eval(substitute(X_axis),data)
  Y_axis<-eval(substitute(Y_axis),data)
  highlight<-eval(substitute(highlight),data)
  
  if(plot_type == "scatter") {
  plot <- ggplot(data, aes(x = X_axis, y = Y_axis, color=highlight, highlight=target)) + 
    geom_point(alpha = 0.5) +
    labs(x = X_name, y = Y_name, 
         title = paste(X_name,' vs ',Y_name))
} else if(plot_type == "line") {
  plot <- ggplot(data, aes(x = X_axis, y = Y_axis, color = highlight, group = highlight)) +
    geom_line(linewidth = 1) +
    labs(x = X_name, y = Y_name, color = highlight)
} else if(plot_type == "box") {
  plot <- ggplot(data, aes(x = X_axis, y = Y_axis)) +
    geom_boxplot() +
    labs(x = "Group", y = Y_name)
} else if(plot_type == "histogram") {
  plot <- ggplot(data, aes(x = X_axis)) +
    geom_histogram(binwidth = 1) +
    labs(x = X_name, y = Y_name)
} else if(plot_type == "density") {
  plot <- ggplot(data, aes(x = X_axis)) +
    geom_density() +
    labs(x = X_name)
}else if(plot_type == "bar") {
  plot <- ggplot(data, aes(x = X_axis)) +
    geom_bar() +
    labs(x = X_axis, y = Y_axis)
} else if(plot_type == "violin") {
  plot <- ggplot(data, aes(x = X_axis, y = Y_axis)) +
    geom_violin() +
    labs(x = X_name, y = Y_name)
} else if(plot_type == "area") {
  plot <- ggplot(data, aes(x=X_axis,y=Y_axis)) + 
    geom_area(aes(fill=highlight),alpha=0.5) + 
    labs(title=paste(X_name,' vs ',Y_name),x=X_name,y=Y_name)
} else if(plot_type == "heat_map") {
  plot <- ggplot(data,aes(X_axis,Y_axis)) + 
    geom_tile(aes(fill=Y_axis),colour="white") + 
    scale_fill_gradient(low="white",high="steelblue") + 
    labs(title=paste(X_name,' vs ',Y_name),x=X_name,y=Y_name)
} else if(plot_type == "pie_chart") {
  plot <- ggplot(data,aes(x=X_axis,y=Y_axis)) + 
    geom_bar(stat="identity",width=1) + 
    coord_polar("y",start=0) + 
    labs(title=paste(Y_name," Distribution"),x="",y="")
} else {
  stop("Invalid plot type. Please choose 'scatter', 'line', 'box', 'histogram', 'density', 'bar', 'violin', 'area', 'heat_map', or 'pie_chart'.")
}
  
  image_plot<-ggsave(graph_name, plot = plot, dpi = 300, width = 6, height = 4)
  #con$insert(list(image = image_plot))
  # Insert image into MongoDB
  mongo$insert(list(name = graph_name, data = readBin(image_plot, "raw")))
  print(paste('Gráfico ',graph_name,' salvo no BD.'))
  print(plot)
} else {
  print(paste('Gráfico ',graph_name,' carregado a partir do BD.'))
  img <- mongo$find(query = req)
  img_data <- readJPEG(graph_name)
  plot<-plot(1:10, type = "n", xlab = "", ylab = "")
  rasterImage(img_data, 1, 1, 10, 10)
}
 
 
}

get_plot(gdpPercap,lifeExp,country,'pie_chart')
