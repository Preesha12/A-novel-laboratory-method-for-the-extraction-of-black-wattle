Preesha.data<-Data_for_Stats_1_
attach(Preesha.data)
Method<-as.factor(Method)
Replicate<-as.factor(Replicate)
library(agricolae)
Mod1<-aov(Extractives~Method+Replicate)
anova(Mod1)
LSD1<-LSD.test(Extractives,Method,DFerror = Mod1$df.residual,MSerror = deviance(Mod1)/Mod1$df.residual,p.adj = "bonferroni")
LSD1
Mod2<-aov(Tannin~Method+Replicate)
anova(Mod2)
LSD2<-LSD.test(Tannin,Method,DFerror = Mod2$df.residual,MSerror = deviance(Mod2)/Mod2$df.residual,p.adj = "bonferroni")
LSD2
Mod3<-aov(NonTannin~Method+Replicate)
anova(Mod3)
LSD3<-LSD.test(NonTannin,Method,DFerror = Mod3$df.residual,MSerror = deviance(Mod3)/Mod3$df.residual,p.adj = "bonferroni")
LSD3
Mod4<-aov(Insolubles~Method+Replicate)
anova(Mod4)
LSD4<-LSD.test(Insolubles,Method,DFerror = Mod4$df.residual,MSerror = deviance(Mod4)/Mod4$df.residual,p.adj = "bonferroni")
LSD4
Mod5<-aov(`Tannin n Non-Tannin Ratio`~Method+Replicate)
anova(Mod5)
LSD5<-LSD.test(`Tannin n Non-Tannin Ratio`,Method,DFerror = Mod5$df.residual,MSerror = deviance(Mod5)/Mod5$df.residual,p.adj = "bonferroni")
LSD5
Mod6<-aov(`Lovibond Red`~Method+Replicate)
anova(Mod6)
LSD6<-LSD.test(`Lovibond Red`,Method,DFerror = Mod6$df.residual,MSerror = deviance(Mod6)/Mod6$df.residual,p.adj = "bonferroni")
LSD6
Mod7<-aov(`Lovibond Yellow`~Method+Replicate)
anova(Mod7)
LSD7<-LSD.test(`Lovibond Red`,Method,DFerror = Mod7$df.residual,MSerror = deviance(Mod7)/Mod7$df.residual,p.adj = "bonferroni")
LSD7


