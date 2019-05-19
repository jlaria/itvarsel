library(ggplot2)

#load("GM-Data.RData")
load("Real.RData")

ggplot()+
  aes(x = 1:30, y = sort(incidencias, decreasing = T)[1:30])+
  geom_point(shape = 1)+
  geom_line()+
  geom_vline(aes(xintercept = 7), linetype="dashed", color = "Red")+
  labs(x = expression("Variable index (sorted by importance "*I[j]*")"),
       y = expression("Importance "*(I[j])))+
  geom_text(aes(x = 6, y = 0.6, label = "cutoff value"), angle = 90, color = "Red")+
  theme_minimal()

nvar_reduced = 7
vars = order(incidencias, decreasing = T)[1:nvar_reduced]
imp = sort(incidencias, decreasing = T)[1:nvar_reduced]
power = rep(0, nrow(result))
for(r in 1:nrow(result)){
  coef = abs(result$selected.coef[[r]][result$selected.vars[[r]]%in% vars])
  if(length(coef)>0)
  {
    coef = coef/max(coef)
    power[r] = sum(imp[vars %in% result$selected.vars[[r]]]*coef)/sum(imp)
  }else{
    power[r] = 0
  }
}

id = order(power, decreasing = T)
b = which.max(power[id] + result$ccr[id])

# ggplot()+
#   geom_line(aes(x = 1:200, y = power[id], linetype = "power"))+
#   geom_line(aes(x = 1:200, y = result$ccr[id], linetype = "ccr"))+
#   geom_point(aes(x = b, y = result$ccr[id][b], color = "Best model"))+
#   geom_point(aes(x = b, y = power[id][b], color = "Best model"))+
#   labs(color=NULL, linetype = "Measure", 
#        x = "Model index (sorted by power)",
#        y = "Value")

df = data.frame(index = c(1:200, 1:200),
                value = c(result$ccr[id], power[id]),
                Measure = factor(rep(c("ccr", "power"), each = 200)) )

ggplot(df)+
  aes(x = index, y = value, group = Measure)+
  geom_line(aes(linetype = Measure, color  = Measure))+
  geom_point(aes(x = b, y = result$ccr[id][b], shape = "Best model"))+
  geom_point(aes(x = b, y = power[id][b], shape = "Best model"))+
  labs(shape = NULL,  
       x = "Model index (sorted by power)",
       y = "Value")+
  theme_minimal()




group.index = as.numeric(cluster.pca(data.train$x))
length(unique(group.index))
max(group.index)

included = rep(FALSE, length(group.index))
included[mC$selected.vars] = TRUE

included.groups = sort(unique(group.index[mC$selected.vars]))
df = NULL 
for (gr in included.groups) {
  var_in = sum(included[group.index==gr])
  var_out = sum(!included[group.index==gr])
  df = rbind(df, 
             data.frame(count = c(var_in, var_out),
                        type = c("included", "excluded"),
                        gr = rep(paste0("Group ", gr),2)) )
}

plot_gr  = ggplot(df)+
  aes(x = "", y = count, 
      fill = type)+
  geom_bar(stat = "identity")+
  coord_flip()+
  labs(y = "", fill = "Status", title = "Groupwise view")+
  theme_minimal()+
  theme(legend.position = "top")+
  scale_fill_brewer(palette = "Reds")+
  facet_wrap(~gr, ncol = 3)

# overall
plot_all = ggplot()+
  aes(x = "", y = c(sum(included), sum(!included)),
      fill = c("included", "excluded"))+
  geom_bar(stat = "identity")+
  coord_flip()+
  labs(y = "Number of transcriptomes", 
       title = "Overall view")+
  guides(fill = F)+
  scale_fill_brewer(palette = "Reds")+
  theme_minimal()

library(gridExtra)
grid.arrange(plot_gr, plot_all, heights = c(4,1))
