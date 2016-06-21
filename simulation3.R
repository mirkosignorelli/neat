library(neat)

# first step: load data
load('data-sim3.Rdata')

# first: tests under H0 (no enrichment) - compute specificity
network = networkmatrix(adj, nodes, 'directed')

pvaldistr = function(alist, blist, network, nodes, nsims) {
  pvalue = numeric(nsims)
  for (i in 1:nsims) {
    pvalue[i] = neat(alist = alist[i], blist = blist[i], network, 'directed',
                     nodes, anames = 'a', bnames = 'b')$pvalue
  }
  return(pvalue)
}

o1 = pvaldistr(overlap1$alist, overlap1$blist, network, nodes, nsims)
o2 = pvaldistr(overlap2$alist, overlap2$blist, network, nodes, nsims)
o3 = pvaldistr(overlap3$alist, overlap3$blist, network, nodes, nsims)
o4 = pvaldistr(overlap4$alist, overlap4$blist, network, nodes, nsims)
o5 = pvaldistr(overlap5$alist, overlap5$blist, network, nodes, nsims)
o6 = pvaldistr(overlap6$alist, overlap6$blist, network, nodes, nsims)
o7 = pvaldistr(overlap7$alist, overlap7$blist, network, nodes, nsims)
o8 = pvaldistr(overlap8$alist, overlap8$blist, network, nodes, nsims)
o9 = pvaldistr(overlap9$alist, overlap9$blist, network, nodes, nsims)
o10 = pvaldistr(overlap10$alist, overlap10$blist, network, nodes, nsims)
o0 = pvaldistr(overlap0$alist, overlap0$blist, network, nodes, nsims)


spec = function(pvec, alpha) sum(pvec >= alpha)/length(pvec)

alpha = 0.05
sp = numeric(11)
sp[1] = spec(o0,alpha)
sp[2] = spec(o1,alpha)
sp[3] = spec(o2,alpha)
sp[4] = spec(o3,alpha)
sp[5] = spec(o4,alpha)
sp[6] = spec(o5,alpha)
sp[7] = spec(o6,alpha)
sp[8] = spec(o7,alpha)
sp[9] = spec(o8,alpha)
sp[10] = spec(o9,alpha)
sp[11] = spec(o10,alpha)

rm(list = setdiff(ls(), c('sp')))



# second: tests under H1 (enrichment) - compute sensitivity
load('data-sim3.Rdata')
net0 = networkmatrix(adj, nodes, nettype = 'directed')

pvaldistr = function(alist, blist, adj, net0, nodes, nsims, addnab) {
  pvalue = numeric(nsims)
  for (i in 1:nsims) {
    add = 0
    adjnew = adj
    netnew = net0
    while (add < addnab) {
      n1 = sample(alist[[i]],1)
      n2 = sample(blist[[i]],1)
      if (adjnew[n1,n2] == 0) {
        adjnew[n1,n2] = 1 # remember that the netw is DIRECTED!
        netnew = rbind(netnew,c(n1,n2))
        add = add+1
      }
    }
    pvalue[i] = neat(alist = alist[i], blist = blist[i], netnew, 'directed',
                     nodes, anames = 'a', bnames = 'b')$pvalue
    if (i%%50 == 0) print(i)
  }
  return(pvalue)
}

addnab = 35
o0 = pvaldistr(overlap0$alist, overlap0$blist, adj, net0, nodes, nsims, addnab)
o1 = pvaldistr(overlap1$alist, overlap1$blist, adj, net0, nodes, nsims, addnab)
o2 = pvaldistr(overlap2$alist, overlap2$blist, adj, net0, nodes, nsims, addnab)
o3 = pvaldistr(overlap3$alist, overlap3$blist, adj, net0, nodes, nsims, addnab)
o4 = pvaldistr(overlap4$alist, overlap4$blist, adj, net0, nodes, nsims, addnab)
o5 = pvaldistr(overlap5$alist, overlap5$blist, adj, net0, nodes, nsims, addnab)
o6 = pvaldistr(overlap6$alist, overlap6$blist, adj, net0, nodes, nsims, addnab)
o7 = pvaldistr(overlap7$alist, overlap7$blist, adj, net0, nodes, nsims, addnab)
o8 = pvaldistr(overlap8$alist, overlap8$blist, adj, net0, nodes, nsims, addnab)
o9 = pvaldistr(overlap9$alist, overlap9$blist, adj, net0, nodes, nsims, addnab)
o10 = pvaldistr(overlap10$alist, overlap10$blist, adj, net0, nodes, nsims, addnab)

sens = function(pvec, alpha) sum(pvec < alpha)/length(pvec)

alpha = 0.05
se = numeric(11)
se[1] = sens(o0,alpha)
se[2] = sens(o1,alpha)
se[3] = sens(o2,alpha)
se[4] = sens(o3,alpha)
se[5] = sens(o4,alpha)
se[6] = sens(o5,alpha)
se[7] = sens(o6,alpha)
se[8] = sens(o7,alpha)
se[9] = sens(o8,alpha)
se[10] = sens(o9,alpha)
se[11] = sens(o10,alpha)

overlapperc = function(setsize, ovsize) { ovsize/(2*setsize-ovsize) }
x = overlapperc(50,seq(0,50,by=5))

rm(list = setdiff(ls(), c('sp','se','x')))



# third: plot results
sps = 100*sp
ses = 100*se
xs = x*100

plot(xs,sps,ylim=c(70,100),type='b',pch=2,lty=1, xlab=expression('Overlap '(J[AB])),
     ylab='Percentage',main='Specificity and sensitivity for different 
     values of overlap',cex=1.2,cex.axis=1.2,cex.lab=1.2)
abline(h=95, lty=2)
points(xs,ses,type='b',pch=18,lty=1)
legend(60,77,c('Specificity','Sensitivity'),lty=c(1,1),pch=c(2,18),cex=1.2)