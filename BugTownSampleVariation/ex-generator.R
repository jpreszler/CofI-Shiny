
df <- data.frame(id=1:500, method=rep("area2",500), 
                samp = sapply(1:500, function(x) 
                  toString(round(rnorm(5,50,20),0))))
df <- rbind.data.frame(df, data.frame(id=1:500, 
                              method=rep("area3",500), 
                              samp = sapply(1:500, function(x) 
                                toString(round(runif(5,1,100),0)))))
write.csv(df, file = "example.csv", row.names = FALSE)
