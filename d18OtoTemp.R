###Temperature calculation function for aragonite and calcite

carbtemp<-function(d18Ow=-1,d18Oa,mineral='aragonite'){

mineral<-'All'

nd18Oa<-length(d18Oa)
d18O.w<-rep(d18Ow,n=nd18Oa)

d18O.s.VSMOW<-1.03091*d18Oa+30.91


#tempdframe<-  #dataframe that is equal in length to the d18Oa vector by the number of equations that will be used. If aragonite it is x and if calcite it is y. If all then x+y.

if(mineral=='aragonite'|mineral=='All'){ 1

#for grossman and ku 1986, t=... as is used in many publications including what Neil has published
gk_1986_a<-21.8-4.69*(d18Oa-d18O.w)

#alpha notation for grossman and ku 1986
agk_1986_a<-1000/(((d18O.s.VSMOW-d18O.w)+31.12)/18.04)-273

#for Kim et al 2007

ket_2007_a<-1000/((1000*ln(((1000+d18O.s.VSMOW)/(1000+d18O.w)))+31.14)/17.88)-273

#for Patterson et al. 1993

pet_1993_a<-1000/(((d18O.s.VSMOW-d18O.w)+33.49)/18.56)-273

#for Thorrold et al. 1997

tet_1997_a<-1000/(((d18O.s.VSMOW-d18O.w)+33.54)/18.56)-273

#for Radtke et al 1998

ret_1998_a<-1000/(((d18O.s.VSMOW-d18O.w)+33.13)/18.70)-273

#for White et al 1999

wet_1999_a<-1000/(((d18O.s.VSMOW-d18O.w)+26.39)/16.74)-273

#for Bohm et al 2000

bet_2000_a<-1000/(((d18O.s.VSMOW-d18O.w)+32.54)/18.45)-273
tempdframe<-as.data.frame(cbind(gk_1986_a,agk_1986_a,ket_2007_a,pet_1993_a,tet_1997_a,ret_1998_a,wet_1999_a,bet_2000_a))

}

if(mineral=='calcite'|mineral=='All'){ 2

ko_1997_c<-1000/(((d18O.s.VSMOW-d18O.w)+32.42)/18.03)-273

tempdframe<-as.data.frame(cbind(tempdframe,ko_1997_c))##Add in the calcite equations

}

return(tempdframe)

}
