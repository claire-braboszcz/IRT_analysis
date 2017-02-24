mediate=function(x,y,m,names=NULL)  {
	reg0=lm(y~x)
	reg1=lm(m~x)
	reg2=lm(y~m+x)
	c=summary(reg0)$coefficients[2,1]
	sc=summary(reg0)$coefficients[2,2]
	a=summary(reg1)$coefficients[2,1]
	sa=summary(reg1)$coefficients[2,2]
	b=summary(reg2)$coefficients[2,1]
	sb=summary(reg2)$coefficients[2,2]
	cp=summary(reg2)$coefficients[3,1]
	scp=summary(reg2)$coefficients[3,2]
	sobel=(a*b)/sqrt(b^2*sa^2+a^2*sb^2+sa^2*sb^2)
	psobel=pnorm(abs(sobel),lower.tail=FALSE)*2
    plot(c(0,100),c(0,110),col="white",ann=F,tck=0,col.axis="white")
    rect(c(10,10,70,70,40),c(10,50,10,50,80),c(30,30,90,90,60),c(30,70,30,70,100))
    arrows(c(30,30,20,60),c(20,60,70,90),c(70,70,40,80),c(20,60,90,70),length=.15)
    text(c(20,20,80,80,50),c(20,60,20,60,90),c("X","X","Y","Y","M"),cex=2)
    text(24,82,round(a,4))
    text(76,82,round(b,4))
    text(50,17,round(c,4))
    text(50,57,round(cp,4))
    text(50,2,paste("Sobel z =",round(sobel,4),"; p= ",round(psobel,4)),cex=1.3)
    if(length(names)==3) {
    	text(20,34,names[1])
    	text(80,34,names[2])
    	text(50,104,names[3])
    }
    output=c(a=a,b=b,c=c,cp=cp,sobel.z=sobel,p=psobel)
	round(output,6)
}