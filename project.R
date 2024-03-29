library(MASS)
raw = read.csv(file="./imports-85.data", header=FALSE, sep=",", na.strings = "?")
data = na.omit(raw)

cname = c("symboling", "normalized_losses", "make", "fuel_type", "aspiration",
          "num_of_doors", "body_style", "drive_wheels", "engine_location", "wheel_base",
          "length", "width", "height", "curb_weight", "engine_type", "num_of_cylinders",
          "engine_size", "fuel_system", "bore", "stroke", "compression_ratio", "horsepower",
          "peak_rpm", "city_mpg", "highway_mpg", "price")

colnames(data) = cname
data <- transform(data, 
                  #symboling = as.factor(symboling),
                  make = as.factor(make),
                  fuel_type = as.factor(fuel_type),
                  aspiration = as.factor(aspiration),
                  num_of_doors = as.factor(num_of_doors),
                  body_style = as.factor(body_style),
                  drive_wheels = as.factor(drive_wheels),
                  engine_type = as.factor(engine_type),
                  num_of_cylinders = as.factor(num_of_cylinders),
                  fuel_system = as.factor(fuel_system))

## won't need engine_location variable because there's only one type of engine location 
data <- subset(data, select = -c(engine_location))

## Apply factor() again to remove excessive level
data[] <- lapply(data, function(x) if(is.factor(x)) factor(x) else x)

## Do the stepwise regression for feature selection
stepAIC(lm(price~.,data=data), direction='both', trace = FALSE)

## The result of stepAIC
summary(lm(formula = price ~ symboling + make + aspiration + num_of_doors + 
     body_style + drive_wheels + wheel_base + length + width + 
     height + curb_weight + engine_type + fuel_system + bore + 
     compression_ratio + horsepower + peak_rpm, data = data))
## Adjusted R^2: 0.9641

alias(lm(formula = price ~ symboling + make + aspiration + num_of_doors + 
           body_style + drive_wheels + wheel_base + length + width + 
           height + curb_weight + engine_type + fuel_system + bore + 
           compression_ratio + horsepower + peak_rpm, data = data))
## found engine_type is an alias

car::vif(lm(formula = price ~ symboling + make + aspiration + num_of_doors + 
              body_style + drive_wheels + wheel_base + length + width + 
              height + curb_weight + fuel_system + bore + 
              compression_ratio + horsepower + peak_rpm, data = data))
##After vif, we can remove compression_ratio (since > 10)

## remove num_doors, width, horsepower, peak_rpm
summary(lm(formula = price ~ symboling + make + aspiration + 
             body_style + drive_wheels + wheel_base + length + engine_type +
             height + curb_weight + fuel_system + bore, data = data))
## Adjusted R^2: 0.9604

## remove symboling
summary(lm(formula = price ~ make + aspiration + 
             body_style + drive_wheels + wheel_base + length  + 
             height + curb_weight + fuel_system + bore, data = data))
## Adjusted R^2: 0.9566


######################################### ANOVA
## ANOVA
boxplot(data$price ~ data$engine_type,col=rainbow(7))

aov_cont <- aov(data$price ~ data$engine_type)
shapiro.test(aov_cont$residuals)
#summary(aov_cont)

bc <- boxcox(data$price ~ data$engine_type)
(trans <- bc$x[which.max(bc$y)])

aov_cont_new = aov(((data$price^trans-1)/trans) ~ data$engine_type)
summary(aov_cont_new)
#summary.lm(aov_cont_new)

plot(aov_cont_new)


#kruskal.test


shapiro.test(aov_cont_new$residuals)


## Check assumption of homogeneity of variance
bartlett.test(((data$price^trans-1)/trans) ~ data$engine_type)

## Assumption of Normality
qqnorm(aov_cont_new$residuals)
qqline(aov_cont_new$residuals)

tuk <- TukeyHSD(aov_cont_new)
tuk
# ohc-dohc, ohcf-dohc, ohcv-ohc, ohcv-ohcf

pairwise.t.test(data$price, data$engine_type, p.adj = "bonf")
# ohc-dohc, ohcf-dohc, ohc-ohcv, ohcf-ohcv
scheffe.test(aov_cont_new,"engine_type")

PostHocTest(aov_cont_new, method = "lsd")
PostHocTest(aov_cont_new, method = "hsd")
PostHocTest(aov_cont_new, method = "scheffe")
PostHocTest(aov_cont_new, method = "bonferroni")
