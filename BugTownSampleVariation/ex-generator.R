
df <- data.frame(id=1:25, method=rep("area2",25), 
                samp = sapply(1:25, function(x) 
                  toString(round(rnorm(5,50,20),0))))
df <- rbind.data.frame(df, data.frame(id=1:25, 
                              method=rep("area3",25), 
                              samp = sapply(1:25, function(x) 
                                toString(round(runif(5,1,100),0)))))
write.csv(df, file = "example.csv", row.names = FALSE)
