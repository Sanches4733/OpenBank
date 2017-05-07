library(dplyr);library(tidyr);library(readxl);library(readr);library(ggplot2);

od <- read_xlsx("Open/OpenRisk.xlsx", 
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
        mutate(Approved = Requests*Approval,Accepted = Issues/Approved) %>%
        select(c(1,2,3,6,5,4,7))

grequests <- ggplot(od, aes(x=Month, y=Requests, colour=Bank)) +
        geom_line() +
        ggtitle("Заявки")
gapproved <- ggplot(od, aes(x=Month, y=Approved, colour=Bank)) +
        geom_line() +
        ggtitle("Одобрено")
gissues <- ggplot(od, aes(x=Month, y=Issues, colour=Bank)) +
        geom_line() +
        ggtitle("Выдано")