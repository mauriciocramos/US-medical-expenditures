
# Subset necessary columns
payments <- read.csv("payments.csv")[, c(1,6,10,11)]



# What is the relationship between mean covered charges
# (Average.Covered.Charges) and mean total payments (Average.Total.Payments) in
# New York?

# Subset NY State
ny <- subset(payments, Provider.State=="NY")

# Calculate correlation to annotate the plot
correlation <- with(ny, cor(Average.Covered.Charges, Average.Total.Payments))

# Calculate regression model
model <- lm(formula = Average.Total.Payments ~ Average.Covered.Charges, data = ny)
            
# Scatterplot
par(mfrow = c(1,1), mar=c(4,4,2,1))
plot(Average.Total.Payments ~ Average.Covered.Charges, data = ny,
     main="Relationship between covered charges and total payments in NY",
     xlab = "Mean covered charges ($)", ylab = "Mean total payments ($)",
     col = rgb(0, 0, 0, 0.5))

# Regression Line annotation
abline(reg = model, lwd = 2)

# Correlation legend annotation
legend(x = "bottomright", legend = paste0("correlation=", round(correlation,2)), bty="n")

# export PDF supressing console message
invisible(dev.copy2pdf(file = "plot1.pdf"))



# How does the relationship between mean covered charges
# (Average.Covered.Charges) and mean total payments (Average.Total.Payments)
# vary by medical condition (DRG.Definition) and the state in which care was
# received (Provider.State)?

# Custom function for multiple scatterplot, regression line and correlation annotation 
myPanelPlot <- function(data) {
    
    # Extract values for titles
    State = unique(data$Provider.State)
    DRG = unique(data$DRG.Definition)
    
    # Calculate correlation to annotate the plot
    correlation <- with(data, cor(Average.Covered.Charges, Average.Total.Payments))
    
    # Calculate regression model
    model <- lm(formula = Average.Total.Payments ~ Average.Covered.Charges, data = data)
    
    # Scatterplot
    with(data,
         plot(Average.Covered.Charges, Average.Total.Payments,
              pch = 20, xaxt='n', yaxt='n', cex = 0.5, col = DRG.Definition,
              main = paste0(paste0(State, collapse=","), " (DRG ", 
                            paste0(substr(DRG, 1 , 3), collapse=","), ")")))
    
    # Regression Line annotation
    abline(reg = lm(formula = Average.Total.Payments ~ Average.Covered.Charges, data = data),
           lwd = 1, lty = 1)
    
    # Correlation legend annotation
    with(data, legend("topright", bty = "n", legend = paste0("cor=", round(correlation,3))))
    
    return(NULL)
}

# Automatic panel layout
with(payments, par(mfcol = c(nlevels(DRG.Definition), nlevels(Provider.State)), mar = c(0, 0, 1, 0)))

# Multi-panel Scatterplot with Regression Line and Correlation
dflist <- split(payments, list(payments$DRG.Definition, payments$Provider.State))
for (df in dflist) {
    myPanelPlot(df)
}

# export PDF supressing console message
invisible(dev.copy2pdf(file = "plot2.pdf"))