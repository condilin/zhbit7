#为泛型函数mysummary添加一个lm类:
mysummary.lm <- function(regmodel, ...) {
  z <- regmodel
  ans <- z[c("call")]
  #用summary函数求出残差的最小,最大值,1/4,3/4分位数
  ans$resis <- summary(z$residuals)
  coef <- z$coefficients
  #vcov函数求出回归系数的协方差矩阵,diag取出
  #对角线元素(即方差),最后sqrt求出标准差.
  sd <- sqrt(diag(vcov(z)))
  #t值等于回归系数估计值除以标准差
  tv <- coef(z) / sd
  #回归分析中t值服从自由度为n-2的t分布
  tm <- qt(0.975, df = df.residual(z))
  #t统计量的p值,当t值为正数时,参数lower.tail=F(即p(X<x)),
  #t值为负数时,lower.tail=T(即p(X>x)),为了避免麻烦,使用
  #绝对值后,都为正数,lower.tail=F.
  #计算P值时要乘以2!
  tp <- 2 * pt(abs(tv), df = df.residual(z),lower.tail = FALSE)
  ans$Coefficients <- cbind(z$coef,tv,tm,tp)
  colnames(ans$Coefficients) <- c("Estimate","t Values",
                                  "Margin","Pr(>|t|)")
  #直接调用anova函数,结果存储在an中.在print.summary.lm函数中
  #直接打印出an，即可得到方差分析结果.
  an <- anova(z)
  ans$an <- an
  ans$sigma <- summary(z)$sigma
  ans$rdf <- summary(z)$df[2L]
  ans$r.squared <- summary(z)$r.squared
  ans$adj.r.squared <- summary(z)$adj.r.squared
  ans$fstatistic <- summary(z)$fstatistic
  #设置mysummary.lm类
  structure(ans,class = "mysummary.lm")
}
