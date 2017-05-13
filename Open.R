library(dplyr);library(tidyr);library(readxl);library(readr);library(ggplot2);

od <- read_xlsx("OpenBank/OpenRisk.xlsx", 
                col_names = c("Bank","Operation type",paste(rep("M",24),1:24,sep = "")),
                skip = 2)
od <- od %>%
        gather(Month, number, M1:M24) %>%
        mutate (Bank = rep(LETTERS[1:5],each = 3,times = 24)) %>%
        spread(`Operation type`, number) %>%
        mutate(Month = parse_number(Month)) %>%
        arrange(Bank,Month) %>%
        select (1,2,Requests = 4,Approval = 5,Issues = 3)


od <- od %>%
        mutate(Approved = Requests*Approval,Acceptance = Issues/Approved) %>%
        select(c(1,2,3,6,5,4,7))
odl <- od %>%
        gather(variable,value,Requests:Acceptance)

########################## BOXPLOT ######################################

# Boxplot (we do subset in order to show inlyu amounts not %%)
ggplot(subset(odl, variable %in% c("Requests", "Approved", "Issues")), 
       aes(x = Bank, y = value, fill = Bank)) + 
        geom_boxplot() +
        facet_grid(. ~ variable) + 
        theme(legend.position = "top")

# Boxplot everything
ggplot(odl, 
       aes(x = Bank, y = value, fill = Bank)) + 
        geom_boxplot() +
        facet_grid(. ~ variable) + 
        theme(legend.position = "top")

# To avoid scaling
ggplot(odl, 
       aes(x = Bank, y = value, fill = Bank)) + 
        geom_boxplot() +
        facet_grid(variable ~ ., scale = "free_y")

########################## HISTOGRAMM ######################################

# Histogramm
ggplot(odl, 
       aes(x = value, fill = Bank)) + 
        geom_histogram(alpha = 0.5) +
        facet_grid(. ~ variable, scales = "free_x") + 
        theme(legend.position = "top")

# Density
ggplot(odl, 
       aes(x = value, fill = Bank)) + 
        geom_density(alpha = 0.5) +
        facet_grid(variable ~ ., scales = "free_y")

# Density of logarithm
ggplot(odl, 
       aes(x = log(value), fill = Bank)) + 
        geom_density(alpha = 0.5) +
        facet_grid(variable ~ ., scales = "free_y")

# Density of square root
ggplot(odl, 
       aes(x = log(value), fill = Bank)) + 
        geom_density(alpha = 0.5) +
        facet_grid(variable ~ ., scales = "free_y")

########################## BAR CHART ######################################

#  We do subset in order to show inlyu amounts not %%
ggplot(subset(odl, variable %in% c("Requests", "Approved", "Issues")), 
       aes(Bank, value)) + 
        geom_bar(stat = "identity", aes(fill = variable))

# Separate bars
ggplot(subset(odl, variable %in% c("Requests", "Approved", "Issues")), 
       aes(Bank, value)) + 
        geom_bar(stat = "identity", aes(fill = variable), position = "dodge")

########################## LINE PLOT ######################################

# Row line plot
ggplot(odl, 
       aes(x = Month, y = value, color = Bank)) + 
        geom_line() +
        facet_grid(variable ~ ., scale = "free_y")

# Normazile to 0-1
normalize <- function(vec) {
        answer <- (vec - min(vec, na.rm = T))/(max(vec, na.rm = T) - min(vec, na.rm = T))
        return(answer)
}

dtLong$Normalized <- ave(dtLong$value, dtLong$Bank, dtLong$variable, FUN = normalize)

ggplot(dtLong, 
       aes(x = Month, y = Normalized, color = Bank)) + 
        geom_line() +
        facet_grid(variable ~ .)

########################## DENSITY ######################################

# First of all let is just compare their distributions visually.

# Request
ggplot(od, aes(Requests, fill = Bank)) + 
        geom_density(alpha = 0.2) +
        facet_grid(Bank ~ .) + 
        theme(legend.position = "top",
              legend.direction = "horizontal",
              legend.title = element_blank())

# Approved
ggplot(od, aes(Requests, fill = Bank)) + 
        geom_density(alpha = 0.2) +
        facet_grid(Bank ~ .) + 
        theme(legend.position = "top",
              legend.direction = "horizontal",
              legend.title = element_blank())

# And so on!

########################## MEAN + 95 CI ######################################

# Plot means of the sets together with 95% confidence interval

# Request
ggplot(od, aes(x = Bank, y = Requests)) +
        stat_summary(fun.y = "mean", colour = "black", geom = "point") + 
        stat_summary(fun.data = "mean_cl_normal", 
                     aes(colour = Bank), 
                     geom = "errorbar")

# Approved
ggplot(od, aes(x = Bank, y = Approved)) +
        stat_summary(fun.y = "mean", colour = "black", geom = "point") + 
        stat_summary(fun.data = "mean_cl_normal", 
                     aes(colour = Bank), 
                     geom = "errorbar")

# And so on!

########################## ANOVA ######################################

# D is different
pairwise.t.test(od$Requests, od$Bank, p.adj = "none")

# D is different
pairwise.t.test(od$Approved, od$Bank, p.adj = "none")

# A-C are the same, others are different
pairwise.t.test(od$Approval, od$Bank, p.adj = "none")

# And so on!!

