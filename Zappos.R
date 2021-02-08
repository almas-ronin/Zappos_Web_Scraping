library(dplyr)
library(ggplot2)
library(tidyr)


# reading sandal 
sandal = read.csv("sandal.csv", TRUE)
sandal2 = sandal %>%  mutate(.,category = "Sandals") 
head(sandal2)

# reading sneakers
sneakers = read.csv("sneakers.csv", TRUE)
sneakers2 = sneakers %>% mutate(.,category = " Casual Sneakers")
head(sneakers2)
  
# reading boot
boot = read.csv("boot.csv", TRUE)
boot2 = boot %>% mutate(.,category = "Boots")
head(boot2) 

# reading oxford
oxford = read.csv("oxford.csv", TRUE)
oxford2 = oxford %>% mutate(.,category = "Oxfords")
head(oxford2) 

# reading loafer
loafer = read.csv("loafer.csv", TRUE)
loafer2 = loafer %>% mutate(.,category = "Loafers")
head(loafer2)

# reading running
running = read.csv("running.csv", TRUE)
running2 = running %>% mutate(.,category = "Running Shoes")
head(running2)

##################### Categories Analysis ###################
shoes = rbind(sandal2,sneakers2,boot2,oxford2,loafer2,running2)
shoes2 = shoes %>% group_by(.,brand)
shoes3 = shoes %>% filter(.,rating != 0)
shoes_cat =shoes3 %>% mutate(.,m_fit = (true_fit+true_width)/2.0 ) %>% filter(.,true_fit != 0)


# comparing true size
g = ggplot (shoes_cat, aes(x= reorder(category,m_fit,median), y=m_fit, fill = category))
g+geom_boxplot() + xlab("") + ylab("Average Fit Score") + 
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  theme(legend.position = "none")

# comparing price
g = ggplot (shoes3, aes(x= reorder(category,price,median), y=price, fill = category))
g+geom_boxplot() + xlab("") + ylab("Price $") + 
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  theme(legend.position = "none")
shoes %>% arrange(.,desc(price))

# comparing rating   
shoes4 = shoes3 %>% group_by(.,category) %>% summarise(.,rating = mean(rating))
g = ggplot (shoes4, aes(x= reorder(category,rating), y=rating))
g+geom_point(colour = "Blue", size = 3) + xlab("") + ylab("Average Rating") + 
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  theme(legend.position = "none")

##################### Sandal's Analysis By Brand ##################
sandal_g_r = shoes %>% filter(.,category == "Sandals") %>% group_by(.,brand) %>% mutate(., n_model = n()) %>% 
  filter(., n_model >= 10 & num_review >=10)
sandal_g_p = shoes %>% filter(.,category == "Sandals") %>% group_by(.,brand) %>% mutate(., n_model = n()) %>% 
  filter(., n_model >= 10) 
sandal_g_f = shoes %>% filter(.,category == "Sandals") %>% group_by(.,brand) %>% mutate(., n_model = n()) %>% 
  filter(., n_model >= 10 & num_review >=10)

#### Price
sandal_p = sandal_g_p %>% filter(.,category == "Sandals" & brand != "" & rating != 0) %>% group_by(.,brand) %>%
  summarise(.,m_price=median(price),m_rating =mean(rating),m_comf=(mean(true_fit)+mean(true_width))/2) %>%
  arrange(.,desc(m_price)) 

sandal_p_low = sandal_p %>% top_n(.,-3, m_price)
sandal_p_high = sandal_p %>% top_n(.,3, m_price) 
sandal_p_low
sandal_p_high
sandal_p_ten = rbind(sandal_p_low, sandal_p_high)

g = ggplot(sandal_p_ten, aes(x= reorder(brand,m_price), y=m_price))
g + geom_point(colour = "Blue", size = 3) + xlab("") + ylab("Sandals' Median Price $") + 
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  theme(legend.position = "none") 

#### Fit
sandal_f = sandal_g_f %>% filter(.,category == "Sandals" & brand != "" & rating != 0 & true_fit != 0) %>% group_by(.,brand) %>%
  summarise(.,m_price=median(price),m_rating =mean(rating),m_comf=(mean(true_fit)+mean(true_width))/2) %>%
  arrange(.,desc(m_comf)) 
sandal_f_low = sandal_f %>% top_n(.,-3, m_comf)
sandal_f_high = sandal_f %>% top_n(.,3, m_comf) 
sandal_f_low
sandal_f_high
sandal_f_ten = rbind(sandal_f_low, sandal_f_high)

g = ggplot(sandal_f_ten, aes(x= reorder(brand,m_comf), y=m_comf))
g + geom_point(colour = "red", size = 3) + xlab("") + ylab("Sandals' Average Fit Score") + 
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  theme(legend.position = "none") 

#### rating
sandal_r = sandal_g_r %>% filter(.,category == "Sandals" & brand != "" & rating != 0 ) %>% group_by(.,brand) %>%
  summarise(.,m_price=median(price),m_rating =mean(rating),m_comf=(mean(true_fit)+mean(true_width))/2) %>%
  arrange(.,desc(m_comf)) 
sandal_r_low = sandal_r %>% top_n(.,-3, m_rating)
sandal_r_high = sandal_r %>% top_n(.,3, m_rating) 
sandal_r_low
sandal_r_high
sandal_r_ten = rbind(sandal_r_low, sandal_r_high)

g = ggplot(sandal_r_ten, aes(x= reorder(brand,m_rating), y=m_rating))
g + geom_point(colour = "green", size = 3) + xlab("") + ylab("Sandal's Average Customer Rating") + 
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  theme(legend.position = "none") 



##################### Loafer's Analysis By Brand ##################
loafer_g_r = shoes %>% filter(.,category == "Loafers") %>% group_by(.,brand) %>% mutate(., n_model = n()) %>% 
  filter(., n_model >= 10 & num_review >=10)
loafer_g_p = shoes %>% filter(.,category == "Loafers") %>% group_by(.,brand) %>% mutate(., n_model = n()) %>% 
  filter(., n_model >= 10) 
loafer_g_f = shoes %>% filter(.,category == "Loafers") %>% group_by(.,brand) %>% mutate(., n_model = n()) %>% 
  filter(., n_model >= 10 & num_review >=10)

#### Price
loafer_p = loafer_g_p %>% filter(.,category == "Loafers" & brand != "" & rating != 0) %>% group_by(.,brand) %>%
  summarise(.,m_price=median(price),m_rating =mean(rating),m_comf=(mean(true_fit)+mean(true_width))/2) %>%
  arrange(.,desc(m_price)) 

loafer_p_low = loafer_p %>% top_n(.,-3, m_price)
loafer_p_high = loafer_p %>% top_n(.,3, m_price) 
loafer_p_low
loafer_p_high
loafer_p_ten = rbind(loafer_p_low, loafer_p_high)

g = ggplot(loafer_p_ten, aes(x= reorder(brand,m_price), y=m_price))
g + geom_point(colour = "Blue", size = 3) + xlab("") + ylab("Loafers' Median Price $") + 
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  theme(legend.position = "none") 

#### Fit
loafer_f = loafer_g_f %>% filter(.,category == "Loafers" & brand != "" & rating != 0 & true_fit != 0) %>% group_by(.,brand) %>%
  summarise(.,m_price=median(price),m_rating =mean(rating),m_comf=(mean(true_fit)+mean(true_width))/2) %>%
  arrange(.,desc(m_comf)) 
loafer_f_low = loafer_f %>% top_n(.,-3, m_comf)
loafer_f_high = loafer_f %>% top_n(.,3, m_comf) 
loafer_f_low
loafer_f_high
loafer_f_ten = rbind(loafer_f_low, loafer_f_high)

g = ggplot(loafer_f_ten, aes(x= reorder(brand,m_comf), y=m_comf))
g + geom_point(colour = "red", size = 3) + xlab("") + ylab("Loafers' Average Fit Score") + 
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  theme(legend.position = "none") 

#### rating
loafer_r = loafer_g_r %>% filter(.,category == "Loafers" & brand != "" & rating != 0 ) %>% group_by(.,brand) %>%
  summarise(.,m_price=median(price),m_rating =mean(rating),m_comf=(mean(true_fit)+mean(true_width))/2) %>%
  arrange(.,desc(m_comf)) 
loafer_r_low = loafer_r %>% top_n(.,-3, m_rating)
loafer_r_high = loafer_r %>% top_n(.,3, m_rating) 
loafer_r_low
loafer_r_high
loafer_r_ten = rbind(loafer_r_low, loafer_r_high)

g = ggplot(loafer_r_ten, aes(x= reorder(brand,m_rating), y=m_rating))
g + geom_point(colour = "green", size = 3) + xlab("") + ylab("Loafers' Average Customer Rating") + 
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  theme(legend.position = "none")

##################### Boot's Analysis By Brand ##################
boot_g_r = shoes %>% filter(.,category == "Boots") %>% group_by(.,brand) %>% mutate(., n_model = n()) %>% 
  filter(., n_model >= 10 & num_review >=10)
boot_g_p = shoes %>% filter(.,category == "Boots") %>% group_by(.,brand) %>% mutate(., n_model = n()) %>% 
  filter(., n_model >= 10) 
boot_g_f = shoes %>% filter(.,category == "Boots") %>% group_by(.,brand) %>% mutate(., n_model = n()) %>% 
  filter(., n_model >= 10 & num_review >=10)

#### Price
boot_p = boot_g_p %>% filter(.,category == "Boots" & brand != "" & rating != 0) %>% group_by(.,brand) %>%
  summarise(.,m_price=median(price),m_rating =mean(rating),m_comf=(mean(true_fit)+mean(true_width))/2) %>%
  arrange(.,desc(m_price)) 

boot_p_low = boot_p %>% top_n(.,-3, m_price)
boot_p_high = boot_p %>% top_n(.,3, m_price) 
boot_p_low
boot_p_high
boot_p_ten = rbind(boot_p_low, boot_p_high)

g = ggplot(boot_p_ten, aes(x= reorder(brand,m_price), y=m_price))
g + geom_point(colour = "Blue", size = 3) + xlab("") + ylab("Boots' Median Price $") + 
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  theme(legend.position = "none") 

#### Fit
boot_f = boot_g_f %>% filter(.,category == "Boots" & brand != "" & rating != 0 & true_fit != 0) %>% group_by(.,brand) %>%
  summarise(.,m_price=median(price),m_rating =mean(rating),m_comf=(mean(true_fit)+mean(true_width))/2) %>%
  arrange(.,desc(m_comf)) 
boot_f_low = boot_f %>% top_n(.,-3, m_comf)
boot_f_high = boot_f %>% top_n(.,3, m_comf) 
boot_f_low
boot_f_high
boot_f_ten = rbind(boot_f_low, boot_f_high)

g = ggplot(boot_f_ten, aes(x= reorder(brand,m_comf), y=m_comf))
g + geom_point(colour = "red", size = 3) + xlab("") + ylab("Boots' Average Fit Score") + 
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  theme(legend.position = "none") 

#### rating
boot_r = boot_g_r %>% filter(.,category == "Boots" & brand != "" & rating != 0 ) %>% group_by(.,brand) %>%
  summarise(.,m_price=median(price),m_rating =mean(rating),m_comf=(mean(true_fit)+mean(true_width))/2) %>%
  arrange(.,desc(m_comf)) 
boot_r_low = boot_r %>% top_n(.,-3, m_rating)
boot_r_high = boot_r %>% top_n(.,3, m_rating) 
boot_r_low
boot_r_high
boot_r_ten = rbind(running_r_low, running_r_high)

g = ggplot(boot_r_ten, aes(x= reorder(brand,m_rating), y=m_rating))
g + geom_point(colour = "green", size = 3) + xlab("") + ylab("Boots' Average Customer Rating") + 
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  theme(legend.position = "none")


##################### Running shoes Analysis By Brand ##################
running_g_r = shoes %>% filter(.,category == "Running Shoes") %>% group_by(.,brand) %>% mutate(., n_model = n()) %>% 
  filter(., n_model >= 10 & num_review >=10)
running_g_p = shoes %>% filter(.,category == "Running Shoes") %>% group_by(.,brand) %>% mutate(., n_model = n()) %>% 
  filter(., n_model >= 10) 
running_g_f = shoes %>% filter(.,category == "Running Shoes") %>% group_by(.,brand) %>% mutate(., n_model = n()) %>% 
  filter(., n_model >= 10 & num_review >=10)
#### Price
running_p = running_g_p %>% filter(.,category == "Running Shoes" & brand != "" & rating != 0) %>% group_by(.,brand) %>%
  summarise(.,m_price=median(price),m_rating =mean(rating),m_comf=(mean(true_fit)+mean(true_width))/2) %>%
  arrange(.,desc(m_price)) 

running_p_low = running_p %>% top_n(.,-3, m_price)
running_p_high = running_p %>% top_n(.,3, m_price) 
running_p_low
running_p_high
running_p_ten = rbind(running_p_low, running_p_high)

g = ggplot(running_p_ten, aes(x= reorder(brand,m_price), y=m_price))
g + geom_point(colour = "Blue", size = 3) + xlab("") + ylab("Running Shoes' Median Price $") + 
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  theme(legend.position = "none") 

#### Fit
running_f = running_g_f %>% filter(.,category == "Running Shoes" & brand != "" & rating != 0 & true_fit != 0) %>% group_by(.,brand) %>%
  summarise(.,m_price=median(price),m_rating =mean(rating),m_comf=(mean(true_fit)+mean(true_width))/2) %>%
  arrange(.,desc(m_comf)) 
running_f_low = running_f %>% top_n(.,-3, m_comf)
running_f_high = running_f %>% top_n(.,3, m_comf) 
running_f_low
running_f_high
running_f_ten = rbind(running_f_low, running_f_high)

g = ggplot(running_f_ten, aes(x= reorder(brand,m_comf), y=m_comf))
g + geom_point(colour = "red", size = 3) + xlab("") + ylab("Running Shoes' Average Fit Score") + 
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  theme(legend.position = "none") 

#### rating
running_r = running_g_r %>% filter(.,category == "Running Shoes" & brand != "" & rating != 0) %>% group_by(.,brand) %>%
  summarise(.,m_price=median(price),m_rating =mean(rating),m_comf=(mean(true_fit)+mean(true_width))/2) %>%
  arrange(.,desc(m_comf)) 
running_r_low = running_r %>% top_n(.,-3, m_rating)
running_r_high = running_r %>% top_n(.,3, m_rating) 
running_r_low
running_r_high
running_r_ten = rbind(running_r_low, running_r_high)

g = ggplot(running_r_ten, aes(x= reorder(brand,m_rating), y=m_rating))
g + geom_point(colour = "green", size = 3) + xlab("") + ylab("Running Shoes'Average Rating") + 
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  theme(legend.position = "none")



########### Variety Analysis for oxfords #################


oxford_g = shoes %>% filter(.,category == "Oxfords" & brand != "") %>% group_by(.,brand) %>%
  summarise(., n_model = n()) %>% top_n(.,10, n_model)
top_5 = oxford_g$brand
oxford_g
g = ggplot(oxford_g, aes(x= reorder(brand,n_model), y=n_model))
g + geom_bar(stat = "identity", fill ='navy') + xlab("") + ylab("Number of Models") + 
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  theme(legend.position = "none") 

# g = ggplot(oxford_g, aes(x= "", y=n_model, fill = brand))
# g + geom_bar(stat = "identity", width = 1) + coord_polar("y", start=0) + theme_void()


oxford_5_p = shoes %>% filter(.,category == "Oxfords" & brand %in% top_5) %>% group_by(.,brand)
g = ggplot (oxford_5_p, aes(x= reorder(brand,price,median), y=price, fill = brand))
g+geom_boxplot() + xlab("") + ylab("Brand's Price $") + 
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  theme(legend.position = "none")


# shoes_percentage = shoes %>% filter(.,category == "Oxfords" & brand != "") %>% group_by(.,brand) %>%
#   summarise(., n_model = n()) %>% arrange(., desc(n_model)) %>% mutate(., percentage = n_model/sum(n_model))













